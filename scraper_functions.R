# ── scraper_functions.R ──────────────────────────────────────
# All scraping logic with zero Shiny dependency.
# Sourced by both app.R and scrape_job.R
# ─────────────────────────────────────────────────────────────

library(httr)
library(rvest)
library(dplyr)
library(stringr)

SEASON <- 2027

SCHOOLS <- list(
  "UC Davis"   = list(slug = "uc-davis",   color = "#002855", accent = "#B8922A"),
  "Air Force"  = list(slug = "air-force",  color = "#003087", accent = "#8A9BA8"),
  "Army"       = list(slug = "army",       color = "#1C3F26", accent = "#B5A165")
)

CACHE_FILE <- "ca_offers_cache.csv"
STAMP_FILE <- "ca_offers_cache_timestamp.txt"

# ── CSS selectors ─────────────────────────────────────────────
SELECTORS <- list(
  row    = c(".ri-page__list-item",
             "li.ri-page__list-item",
             "[class*='ri-page__list-item']"),
  name   = c(".ri-page__name-link",
             "a.name",
             ".name a",
             ".recruit a",
             "[class*='name-link']"),
  loc    = c(".recruit",
             "[class*='recruit']"),
  pos    = c(".pos",
             ".position",
             "[class*='pos']"),
  hw     = c(".metrics",
             ".hw",
             "[class*='metrics']",
             "[class*='hw']"),
  stars  = c(".ri-page__star-and-score",
             ".rating",
             "[class*='star-and-score']",
             "[class*='rating']"),
  score  = c(".score",
             "[class*='score']"),
  commit = c(".status img",
             ".temp-status img",
             "[class*='status'] img",
             ".checkmark img")
)

# ── cache helpers ─────────────────────────────────────────────
save_cache <- function(df) {
  tryCatch({
    write.csv(df, CACHE_FILE, row.names = FALSE)
    writeLines(format(Sys.time(), "%b %d, %Y %I:%M %p"), STAMP_FILE)
    message("Cache saved → ", CACHE_FILE)
  }, error = function(e) message("Cache write failed: ", e$message))
}

load_cache <- function() {
  # Try local file first (dev), then raw GitHub URL (shinyapps.io)
  if (file.exists(CACHE_FILE)) {
    return(tryCatch(read.csv(CACHE_FILE, stringsAsFactors = FALSE),
                    error = function(e) NULL))
  }
  raw_url <- "https://raw.githubusercontent.com/WOOWOO43/SHUAPP.APP/main/ca_offers_cache.csv"
  tryCatch(read.csv(raw_url, stringsAsFactors = FALSE),
           error = function(e) NULL)
}

load_cache_timestamp <- function() {
  if (file.exists(STAMP_FILE))
    return(tryCatch(readLines(STAMP_FILE)[1], error = function(e) NULL))
  NULL
}

# ── fetch helper ──────────────────────────────────────────────
fetch_247 <- function(url) {
  tryCatch(
    GET(url,
        add_headers(
          "User-Agent"      = paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
                                     "AppleWebKit/537.36 (KHTML, like Gecko) ",
                                     "Chrome/124.0.0.0 Safari/537.36"),
          "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          "Accept-Language" = "en-US,en;q=0.9",
          "Accept-Encoding" = "gzip, deflate, br",
          "Referer"         = "https://247sports.com/",
          "Sec-Fetch-Dest"  = "document",
          "Sec-Fetch-Mode"  = "navigate",
          "Sec-Fetch-Site"  = "same-origin",
          "Upgrade-Insecure-Requests" = "1"
        ),
        timeout(30)),
    error = function(e) { message("Fetch error: ", e$message); NULL }
  )
}

# ── try_node ──────────────────────────────────────────────────
try_node <- function(parent, selectors, multi = FALSE) {
  for (sel in selectors) {
    result <- tryCatch(
      if (multi) html_nodes(parent, sel) else html_node(parent, sel),
      error = function(e) NULL
    )
    if (!is.null(result) && length(result) > 0) return(result)
  }
  NULL
}

# ── debug helper ──────────────────────────────────────────────
debug_page <- function(school_slug, season = SEASON) {
  url  <- sprintf("https://247sports.com/college/%s/season/%d-football/offers/",
                  school_slug, season)
  resp <- fetch_247(url)
  if (is.null(resp) || status_code(resp) != 200) {
    cat(sprintf("HTTP %s\n", if (is.null(resp)) "ERROR" else status_code(resp)))
    return(invisible(NULL))
  }
  html <- content(resp, "text", encoding = "UTF-8") |> read_html()
  all_classes <- html_nodes(html, "[class]") |> html_attr("class") |>
    strsplit("\\s+") |> unlist() |> unique() |> sort()
  cat("=== Classes ===\n"); cat(paste(all_classes, collapse = "\n"), "\n\n")
  cat("=== Row selector hits ===\n")
  for (sel in SELECTORS$row)
    cat(sprintf("  %-45s → %d\n", sel, length(html_nodes(html, sel))))
  invisible(html)
}

# ── parse location ────────────────────────────────────────────
parse_location <- function(loc_raw) {
  if (is.null(loc_raw) || is.na(loc_raw) || nchar(trimws(loc_raw)) == 0)
    return(list(hs = NA_character_, city = NA_character_, state = NA_character_))
  loc_raw <- trimws(loc_raw)
  m1 <- regexpr("\\(([^,]+),\\s*([A-Z]{2})\\)", loc_raw)
  if (m1 > 0) {
    bracket <- regmatches(loc_raw, m1)
    inner   <- gsub("[()]", "", bracket)
    parts   <- strsplit(inner, ",\\s*")[[1]]
    return(list(hs = trimws(sub(bracket, "", loc_raw, fixed = TRUE)),
                city = trimws(parts[1]), state = trimws(parts[2])))
  }
  m2 <- regexpr("([A-Za-z ]+),\\s*([A-Z]{2})$", loc_raw)
  if (m2 > 0) {
    parts <- strsplit(trimws(regmatches(loc_raw, m2)), ",\\s*")[[1]]
    return(list(hs = NA_character_, city = trimws(parts[1]), state = trimws(parts[2])))
  }
  m3 <- regexpr("\\b([A-Z]{2})\\b", loc_raw)
  list(hs = loc_raw, city = NA_character_,
       state = if (m3 > 0) regmatches(loc_raw, m3) else NA_character_)
}

# ── main scraper ──────────────────────────────────────────────
scrape_offers <- function(school_slug, school_name) {
  url  <- sprintf("https://247sports.com/college/%s/season/%d-football/offers/",
                  school_slug, SEASON)
  resp <- fetch_247(url)
  if (is.null(resp) || status_code(resp) != 200) {
    warning(sprintf("[%s] HTTP %s – skipping", school_name,
                    if (is.null(resp)) "ERROR" else status_code(resp)))
    return(NULL)
  }
  html  <- content(resp, "text", encoding = "UTF-8") |> read_html()
  nodes <- try_node(html, SELECTORS$row, multi = TRUE)
  if (is.null(nodes) || length(nodes) == 0) {
    warning(sprintf("[%s] No recruit rows found. Run debug_page('%s').",
                    school_name, school_slug))
    return(NULL)
  }
  message(sprintf("[%s] Found %d nodes.", school_name, length(nodes)))
  
  parse_one <- function(node) {
    name_node <- try_node(node, SELECTORS$name)
    name <- if (!is.null(name_node)) html_text(name_node, trim = TRUE) else NA_character_
    
    recruit_node <- try_node(node, SELECTORS$loc)
    loc_raw <- NA_character_
    if (!is.null(recruit_node)) {
      full_text <- html_text(recruit_node, trim = TRUE)
      loc_raw <- if (!is.na(name) && nchar(name) > 0)
        trimws(sub(name, "", full_text, fixed = TRUE)) else full_text
      loc_raw <- trimws(gsub("\\s+", " ", loc_raw))
    }
    loc <- parse_location(loc_raw)
    
    pos_node <- try_node(node, SELECTORS$pos)
    hw_node  <- try_node(node, SELECTORS$hw)
    
    stars_node <- try_node(node, SELECTORS$stars)
    stars <- if (!is.null(stars_node)) {
      n <- length(html_nodes(stars_node, ".icon-starsolid.yellow"))
      if (n == 0) n <- length(html_nodes(stars_node, ".yellow"))
      if (n == 0) NA_integer_ else as.integer(n)
    } else NA_integer_
    
    score_node <- try_node(node, SELECTORS$score)
    commit_node <- try_node(node, SELECTORS$commit)
    
    tibble(
      School    = school_name,
      Name      = name,
      Pos       = if (!is.null(pos_node)) html_text(pos_node, trim = TRUE) else NA_character_,
      HS        = loc$hs,
      City      = loc$city,
      State     = loc$state,
      `Ht/Wt`   = if (!is.null(hw_node)) html_text(hw_node, trim = TRUE) else NA_character_,
      Stars     = stars,
      Score     = if (!is.null(score_node))
        suppressWarnings(as.integer(html_text(score_node, trim = TRUE)))
      else NA_integer_,
      Committed = if (!is.null(commit_node)) html_attr(commit_node, "alt") else NA_character_
    )
  }
  
  bind_rows(lapply(nodes, parse_one))
}