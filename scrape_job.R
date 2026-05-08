# ── scrape_job.R ─────────────────────────────────────────────
# Called by GitHub Actions. No Shiny dependency.
# ─────────────────────────────────────────────────────────────

source("scraper_functions.R")

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