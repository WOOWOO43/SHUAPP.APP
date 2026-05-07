# ── scrape_job.R ─────────────────────────────────────────────
# Called by GitHub Actions. No Shiny dependency.
# Scrapes all three schools, filters CA, saves cache CSV.
# ─────────────────────────────────────────────────────────────

library(httr)
library(rvest)
library(dplyr)
library(stringr)

SEASON     <- 2026
CACHE_FILE <- "ca_offers_cache.csv"
STAMP_FILE <- "ca_offers_cache_timestamp.txt"

SCHOOLS <- list(
  "UC Davis"  = list(slug = "uc-davis"),
  "Air Force" = list(slug = "air-force"),
  "Army"      = list(slug = "army")
)

save_cache <- function(df) {
  write.csv(df, CACHE_FILE, row.names = FALSE)
  writeLines(format(Sys.time(), "%b %d, %Y %I:%M %p"), STAMP_FILE)
  message("Cache saved: ", nrow(df), " CA prospects")
}

# ── run ───────────────────────────────────────────────────────
all_ca <- NULL

for (nm in names(SCHOOLS)) {
  message("Scraping ", nm, "...")
  df <- scrape_offers(SCHOOLS[[nm]]$slug, nm)
  if (!is.null(df) && nrow(df) > 0) {
    ca <- df |> filter(toupper(trimws(State)) == "CA")
    message("  → ", nrow(ca), " CA prospects")
    all_ca <- bind_rows(all_ca, ca)
  }
  Sys.sleep(runif(1, 1.5, 3))
}

if (!is.null(all_ca) && nrow(all_ca) > 0) {
  save_cache(all_ca)
} else {
  stop("Scrape returned no CA prospects — cache not updated.")
}