library(tidyverse)
library(httr)
library(jsonlite)

# -- I. Sélection de l'échantillon --

if (!dir.exists("data")) dir.create("data")

# --- 1. RÉCUPÉRER 100% DE LA BASE MÈRE ---
steam_api_key <- "STEAM_API_KEY"

all_apps_list <- list()
last_id <- 0
have_more <- TRUE
page <- 1

cat("Téléchargement complet du catalogue Steam...\n")

while (have_more) {
  # include_games=true permet de filtrer directement les vrais jeux (sans DLC/vidéos)
  url_app_list <- paste0(
    "https://api.steampowered.com/IStoreService/GetAppList/v1/?key=", steam_api_key,
    "&include_games=true&include_dlc=false&max_results=50000&last_appid=", last_id
  )
  
  res_apps <- GET(url_app_list)
  parsed <- fromJSON(content(res_apps, "text", encoding = "UTF-8"))$response
  
  all_apps_list[[page]] <- parsed$apps
  cat("Page", page, "récupérée :", nrow(parsed$apps), "jeux\n")
  
  have_more <- isTRUE(parsed$have_more_results)
  last_id <- parsed$last_appid
  page <- page + 1
  Sys.sleep(1)
}

clean_apps <- bind_rows(all_apps_list) %>% filter(name != "")
cat("✅ Total catalogue récupéré :", nrow(clean_apps), "jeux !\n")

# --- 2. SCAN RAPIDE DES PRIX US PAR PAQUETS DE 100 ---
# On découpe les IDs en blocs de 100
ids <- clean_apps$appid
chunks <- split(ids, ceiling(seq_along(ids) / 100))

us_prices_file <- "data/steam_us_prices_all.csv"
cat("app_id,price_us\n", file = us_prices_file)

cat("Début du scan sur", length(chunks), "paquets de 100 jeux...\n")

for (i in seq_along(chunks)) {
  batch_ids <- paste(chunks[[i]], collapse = ",")
  url <- paste0("https://store.steampowered.com/api/appdetails?appids=", 
                batch_ids, "&cc=US&filters=price_overview")
  
  Sys.sleep(1) # Pause anti-ban
  
  if (i %% 25 == 0) cat("Paquet", i, "/", length(chunks), "(", round(i/length(chunks)*100, 1), "%)\n")
  
  res <- tryCatch(GET(url, timeout(15)), error = function(e) return(NULL))
  
  if (!is.null(res) && status_code(res) == 200) {
    # simplifyVector = FALSE évite que R plante sur les listes vides de Steam
    parsed <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    
    # On extrait uniquement les jeux qui ont bien un prix US
    batch_df <- map_dfr(names(parsed), function(id_str) {
      item <- parsed[[id_str]]
      if (isTRUE(item$success) && is.list(item$data) && !is.null(item$data$price_overview)) {
        tibble(
          app_id = as.numeric(id_str),
          price_us = item$data$price_overview$initial / 100
        )
      } else {
        NULL # On ignore directement les gratuits / invalides
      }
    })
    
    if (nrow(batch_df) > 0) {
      write_csv(batch_df, us_prices_file, append = TRUE)
    }
  }
}

cat("✅ Scan US terminé !\n")

# -- II. Délimitation de l'échantillon--

us_prices <- read_csv("data/steam_us_prices_all.csv", show_col_types = FALSE)

# 1. Créer les tranches de prix (tu peux ajuster les bornes selon la distribution)
us_prices_tiered <- us_prices %>%
  filter(price_us > 0) %>%
  mutate(
    price_tier = case_when(
      price_us < 5 ~ "1_Low_+ (<5$)",
      price_us >= 5 & price_us < 20 ~ "2_Low (5-20$)",
      price_us >= 20 & price_us < 45 ~ "3_Mid (20-45$)",
      price_us >= 45 ~ "4_High (45$+)"
    )
  )

# Vérifier combien de jeux tu as dans chaque tranche avant de tirer
print(count(us_prices_tiered, price_tier))

# 2. Tirage aléatoire stratifié (ex: 500 jeux par tranche = 2000 jeux)
set.seed(0)
sampled_ids <- us_prices_tiered %>%
  group_by(price_tier) %>%
  slice_sample(n = 500) %>% # Si une tranche a moins de 500 jeux, R te préviendra
  ungroup()

write_csv(sampled_ids, "data/sampled_app_ids.csv")
cat("✅ Échantillon stratifié sauvegardé :", nrow(sampled_ids), "jeux !\n")

# -- III. Extraction de la data-base de travail --

# --- 1. FONCTION D'EXTRACTION ---
get_steam_raw_data <- function(app_id, country_code) {
  url <- paste0("https://store.steampowered.com/api/appdetails?appids=", app_id, "&cc=", country_code)
  
  response <- tryCatch(GET(url, timeout(10)), error = function(e) return(NULL))
  
  # Erreur HTTP (15 colonnes)
  if (is.null(response) || status_code(response) != 200) {
    return(tibble(
      app_id = app_id, country_code = country_code, name = NA,
      type = NA, is_free = NA, currency = NA,
      price_initial = NA, price_final = NA, discount_percent = NA,
      release_date = NA, is_indie = NA, developers = NA, publishers = NA,
      metacritic = NA, status = "HTTP Error"
    ))
  }
  
  raw_text <- content(response, "text", encoding = "UTF-8")
  data <- fromJSON(raw_text)
  app_data <- data[[as.character(app_id)]]
  
  # AppID invalide ou jeu non disponible dans le pays
  if (is.null(app_data) || isFALSE(app_data$success)) {
    return(tibble(
      app_id = app_id, country_code = country_code, name = NA,
      type = NA, is_free = NA, currency = NA,
      price_initial = NA, price_final = NA, discount_percent = NA,
      release_date = NA, is_indie = NA, developers = NA, publishers = NA,
      metacritic = NA, status = "Invalid AppID"
    ))
  }
  
  name_app <- if (!is.null(app_data$data$name)) app_data$data$name else NA
  type_app <- if (!is.null(app_data$data$type)) app_data$data$type else NA
  is_free_app <- if (!is.null(app_data$data$is_free)) app_data$data$is_free else NA
  
  currency_app <- NA
  price_init <- NA
  price_fin <- NA
  discount_pct <- NA
  
  if (!is.null(app_data$data$price_overview)) {
    currency_app <- app_data$data$price_overview$currency
    price_init <- app_data$data$price_overview$initial / 100
    price_fin <- app_data$data$price_overview$final / 100
    discount_pct <- app_data$data$price_overview$discount_percent
  }
  
  release_date <- if (!is.null(app_data$data$release_date$date)) app_data$data$release_date$date else NA
  
  genres <- if (!is.null(app_data$data$genres)) paste(app_data$data$genres$description, collapse = ", ") else NA
  is_indie <- if (!is.na(genres)) str_detect(genres, "Indie") else NA
  
  developers <- if (!is.null(app_data$data$developers)) paste(app_data$data$developers, collapse = ", ") else NA
  publishers <- if (!is.null(app_data$data$publishers)) paste(app_data$data$publishers, collapse = ", ") else NA
  metacritic <- if (!is.null(app_data$data$metacritic$score)) app_data$data$metacritic$score else NA
  
  return(tibble(
    app_id = app_id,
    country_code = country_code,
    name = name_app,
    type = type_app,
    is_free = is_free_app,
    currency = currency_app,
    price_initial = price_init,
    price_final = price_fin,
    discount_percent = discount_pct,
    release_date = release_date,
    is_indie = is_indie,
    developers = developers,
    publishers = publishers,
    metacritic = metacritic,
    status = "OK"
  ))
}
# --- 2. PRÉPARATION DE L'ÉCHANTILLON STRATIFIÉ ---

sample_apps <- read_csv("data/sampled_app_ids.csv", show_col_types = FALSE)

countries <- c("US", "FR", "DE", "CH", "IN", "ID", "BR", "MX", "AR", "TR", "ZA", "JP")

grid <- expand_grid(app_id = sample_apps$app_id, country_code = countries)

# Petite sécurité : si app_id est manquant, le script s'arrête tout de suite
stopifnot("app_id" %in% names(grid), nrow(grid) > 0)

# --- 3. BOUCLE D'EXTRACTION SÉCURISÉE ---
csv_file <- "data/steam_raw_price.csv"

# Vérifier ce qui a déjà été fait
if (file.exists(csv_file)) {
  cat("Fichier CSV trouvé. Analyse de la progression...\n")
  
  # On lit le CSV et on force app_id en numérique pour éviter le conflit de type
  already_done <- read_csv(csv_file, show_col_types = FALSE) %>%
    mutate(app_id = as.numeric(app_id))
  
  grid_remaining <- grid %>%
    mutate(app_id = as.numeric(app_id)) %>%
    anti_join(already_done, by = c("app_id", "country_code"))
  
} else {
  cat("Nouveau téléchargement. Création du fichier CSV...\n")
  file.create(csv_file)
  cat("app_id,country_code,name,type,is_free,currency,price_initial,price_final,discount_percent,release_date,is_indie,developers,publishers,metacritic,status\n", file = csv_file)
  grid_remaining <- grid
}

cat(paste("Il reste", nrow(grid_remaining), "requêtes à effectuer sur", nrow(grid), "au total.\n"))

if (nrow(grid_remaining) > 0) {
  for (i in 1:nrow(grid_remaining)) {
    id <- grid_remaining$app_id[i]
    cc <- grid_remaining$country_code[i]
    
    Sys.sleep(1.5)
    
    
    if (i %% 12 == 0) cat("Requête", i, "/", nrow(grid_remaining), "(", round(i/nrow(grid_remaining)*100, 1), "% )\n")
    
    row_data <- get_steam_raw_data(id, cc)
    write_csv(row_data, csv_file, append = TRUE)
  }
  cat("✅ Extraction terminée avec succès !\n")
}