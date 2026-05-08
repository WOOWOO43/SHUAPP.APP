source("scraper_functions.R")

all_ca <- NULL
for (nm in names(SCHOOLS)) {
  message("Scraping ", nm, "...")
  df <- scrape_offers(SCHOOLS[[nm]]$slug, nm)
  if (!is.null(df) && nrow(df) > 0) {
    ca <- df |> filter(toupper(trimws(State)) == "CA")
    all_ca <- bind_rows(all_ca, ca)
  }
  Sys.sleep(runif(1, 1.5, 3))
}

if (!is.null(all_ca) && nrow(all_ca) > 0) {
  save_cache(all_ca)
  
  # Push updated CSV to GitHub so shinyapps.io picks it up
  system('git -C "C:/Users/warre/Desktop/Football Apps/CalPolyAnalyst/SHUAPP" add ca_offers_cache.csv ca_offers_cache_timestamp.txt')
  system('git -C "C:/Users/warre/Desktop/Football Apps/CalPolyAnalyst/SHUAPP" commit -m "Auto-update cache"')
  system('git -C "C:/Users/warre/Desktop/Football Apps/CalPolyAnalyst/SHUAPP" push')
  
  message("Done — cache saved and pushed to GitHub")
} else {
  stop("No CA prospects found.")
}