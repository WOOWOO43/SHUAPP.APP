## ============================================================
##  CA Offer Tracker — Army / Air Force / UC Davis
##  247sports scraper, California prospects only
##  Run locally: shiny::runApp("app.R")
##
##  If selectors break, run debug_page("uc-davis") in the
##  R console FIRST to identify current class names, then
##  update SELECTORS below accordingly.
## ============================================================

library(shiny)
library(httr)
library(rvest)
library(dplyr)
library(DT)
library(stringr)
source("scraper_functions.R")

# ── school config ────────────────────────────────────────────
SCHOOLS <- list(
  "UC Davis"   = list(slug = "uc-davis",   color = "#002855", accent = "#B8922A"),
  "Air Force"  = list(slug = "air-force",  color = "#003087", accent = "#8A9BA8"),
  "Army"       = list(slug = "army",       color = "#1C3F26", accent = "#B5A165")
)

SEASON <- 2027

# ── CSS selector candidates (tried in order) ─────────────────
# Last verified against live page: 2026 season
# Re-run debug_page("uc-davis") if selectors break again
SELECTORS <- list(
  # Each recruit row lives in .ri-page__list-item
  row    = c(".ri-page__list-item",
             "li.ri-page__list-item",
             "[class*='ri-page__list-item']"),
  
  # Player name link
  name   = c(".ri-page__name-link",
             "a.name",
             ".name a",
             ".recruit a",
             "[class*='name-link']"),
  
  # High school + "(City, ST)" — sits inside .recruit as text
  # 247sports puts this as plain text after the name; we pull from .recruit
  loc    = c(".recruit",
             "[class*='recruit']"),
  
  # Position abbreviation
  pos    = c(".pos",
             ".position",
             "[class*='pos']"),
  
  # Height / Weight  e.g. "6-2 / 205"
  hw     = c(".metrics",
             ".hw",
             "[class*='metrics']",
             "[class*='hw']"),
  
  # Star rating container — filled stars have class "icon-starsolid yellow"
  stars  = c(".ri-page__star-and-score",
             ".rating",
             "[class*='star-and-score']",
             "[class*='rating']"),
  
  # Composite score number
  score  = c(".score",
             "[class*='score']"),
  
  # Committed school — image alt text
  commit = c(".status img",
             ".temp-status img",
             "[class*='status'] img",
             ".checkmark img")
)

# ── shared fetch helper ───────────────────────────────────────
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

# ── try_node: first matching selector wins ────────────────────
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

# ─────────────────────────────────────────────────────────────
#  debug_page()  — run this from the R console when selectors
#  break.  Prints all unique class names found on the page so
#  you can identify the new structure.
#
#  Usage:   debug_page("uc-davis")
# ─────────────────────────────────────────────────────────────
debug_page <- function(school_slug, season = SEASON) {
  url  <- sprintf("https://247sports.com/college/%s/season/%d-football/offers/",
                  school_slug, season)
  resp <- fetch_247(url)
  
  if (is.null(resp) || status_code(resp) != 200) {
    cat(sprintf("HTTP %s – could not fetch page.\n",
                if (is.null(resp)) "ERROR" else status_code(resp)))
    return(invisible(NULL))
  }
  
  html <- content(resp, "text", encoding = "UTF-8") |> read_html()
  
  # ── all unique class strings ──────────────────────────────
  all_classes <- html_nodes(html, "[class]") |>
    html_attr("class") |>
    strsplit("\\s+") |>
    unlist() |>
    unique() |>
    sort()
  
  cat("=== All unique class names on the page ===\n")
  cat(paste(all_classes, collapse = "\n"), "\n\n")
  
  # ── check current row selectors ──────────────────────────
  cat("=== Row selector hits ===\n")
  for (sel in SELECTORS$row) {
    n <- length(html_nodes(html, sel))
    cat(sprintf("  %-45s  → %d nodes\n", sel, n))
  }
  
  # ── sample first 500 chars of <body> ────────────────────
  body_text <- html |> html_node("body") |> as.character()
  cat("\n=== First 600 chars of <body> ===\n")
  cat(substr(body_text, 1, 600), "\n")
  
  invisible(html)
}

# ── main scraper ─────────────────────────────────────────────
scrape_offers <- function(school_slug, school_name) {
  url  <- sprintf("https://247sports.com/college/%s/season/%d-football/offers/",
                  school_slug, SEASON)
  resp <- fetch_247(url)
  
  if (is.null(resp) || status_code(resp) != 200) {
    warning(sprintf("[%s] HTTP %s – skipping",
                    school_name,
                    if (is.null(resp)) "ERROR" else status_code(resp)))
    return(NULL)
  }
  
  html <- content(resp, "text", encoding = "UTF-8") |> read_html()
  
  # ── find row nodes using first working selector ────────────
  nodes <- try_node(html, SELECTORS$row, multi = TRUE)
  
  if (is.null(nodes) || length(nodes) == 0) {
    warning(sprintf(paste0(
      "[%s] No recruit rows found.\n",
      "  → Run debug_page('%s') in your R console to inspect\n",
      "    class names and update SELECTORS$row at the top of app.R."
    ), school_name, school_slug))
    return(NULL)
  }
  
  message(sprintf("[%s] Found %d recruit nodes.", school_name, length(nodes)))
  
  # ── parse location string ─────────────────────────────────
  parse_location <- function(loc_raw) {
    if (is.null(loc_raw) || is.na(loc_raw) || nchar(trimws(loc_raw)) == 0)
      return(list(hs = NA_character_, city = NA_character_, state = NA_character_))
    
    loc_raw <- trimws(loc_raw)
    
    # Pattern 1: "HS Name (City, ST)"
    m1 <- regexpr("\\(([^,]+),\\s*([A-Z]{2})\\)", loc_raw)
    if (m1 > 0) {
      bracket <- regmatches(loc_raw, m1)
      inner   <- gsub("[()]", "", bracket)
      parts   <- strsplit(inner, ",\\s*")[[1]]
      return(list(
        hs    = trimws(sub(bracket, "", loc_raw, fixed = TRUE)),
        city  = trimws(parts[1]),
        state = trimws(parts[2])
      ))
    }
    
    # Pattern 2: "City, ST" (no HS prefix)
    m2 <- regexpr("([A-Za-z ]+),\\s*([A-Z]{2})$", loc_raw)
    if (m2 > 0) {
      parts <- strsplit(trimws(regmatches(loc_raw, m2)), ",\\s*")[[1]]
      return(list(hs = NA_character_,
                  city  = trimws(parts[1]),
                  state = trimws(parts[2])))
    }
    
    # Pattern 3: state abbreviation anywhere
    m3 <- regexpr("\\b([A-Z]{2})\\b", loc_raw)
    st <- if (m3 > 0) regmatches(loc_raw, m3) else NA_character_
    
    list(hs = loc_raw, city = NA_character_, state = st)
  }
  
  # ── parse one recruit node ────────────────────────────────
  parse_one <- function(node) {
    # Name
    name_node <- try_node(node, SELECTORS$name)
    name  <- if (!is.null(name_node)) html_text(name_node, trim = TRUE) else NA_character_
    
    # Location: .recruit contains the name link + plain text for "HS (City, ST)"
    # Strip the name text to isolate the location line
    loc_raw <- NA_character_
    recruit_node <- try_node(node, SELECTORS$loc)
    if (!is.null(recruit_node)) {
      full_text <- html_text(recruit_node, trim = TRUE)
      # Remove the player name from the text to get just the HS/location
      if (!is.na(name) && nchar(name) > 0) {
        loc_raw <- trimws(sub(fixed(name), "", full_text, fixed = TRUE))
      } else {
        loc_raw <- full_text
      }
      # Also strip any leading/trailing whitespace/newlines
      loc_raw <- trimws(gsub("\\s+", " ", loc_raw))
    }
    loc <- parse_location(loc_raw)
    
    # Position
    pos_node  <- try_node(node, SELECTORS$pos)
    pos       <- if (!is.null(pos_node)) html_text(pos_node, trim = TRUE) else NA_character_
    
    # Height / Weight
    hw_node   <- try_node(node, SELECTORS$hw)
    hw        <- if (!is.null(hw_node)) html_text(hw_node, trim = TRUE) else NA_character_
    
    # Stars: count nodes with BOTH icon-starsolid AND yellow classes
    stars_node <- try_node(node, SELECTORS$stars)
    stars <- if (!is.null(stars_node)) {
      filled <- length(html_nodes(stars_node, ".icon-starsolid.yellow"))
      if (filled == 0L) {
        # fallback: any yellow icon
        filled <- length(html_nodes(stars_node, ".yellow"))
      }
      if (filled == 0L) NA_integer_ else as.integer(filled)
    } else NA_integer_
    
    # Composite score
    score_node <- try_node(node, SELECTORS$score)
    score <- if (!is.null(score_node)) {
      suppressWarnings(as.integer(html_text(score_node, trim = TRUE)))
    } else NA_integer_
    
    # Committed school
    commit_node <- try_node(node, SELECTORS$commit)
    committed   <- if (!is.null(commit_node)) html_attr(commit_node, "alt") else NA_character_
    
    tibble(
      School    = school_name,
      Name      = name,
      Pos       = pos,
      HS        = loc$hs,
      City      = loc$city,
      State     = loc$state,
      `Ht/Wt`   = hw,
      Stars     = stars,
      Score     = score,
      Committed = committed
    )
  }
  
  bind_rows(lapply(nodes, parse_one))
}


# ── star renderer ────────────────────────────────────────────
render_stars <- function(n) {
  if (is.na(n) || !is.numeric(n)) return("—")
  filled <- paste(rep("★", min(n, 5)), collapse = "")
  empty  <- paste(rep("☆", 5 - min(n, 5)), collapse = "")
  paste0('<span style="color:#f5c518;font-size:1.1em">', filled, '</span>',
         '<span style="color:#ccc;font-size:1.1em">',   empty,  '</span>')
}


# ── UI ───────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@400;600;700;900&family=Barlow:wght@300;400;500&display=swap",
              rel = "stylesheet"),
    tags$style(HTML("
      * { box-sizing: border-box; }

      body {
        background: #0d1117;
        color: #e6edf3;
        font-family: 'Barlow', sans-serif;
        font-size: 14px;
        margin: 0;
        padding: 0;
      }

      /* ── header ── */
      .app-header {
        background: linear-gradient(135deg, #0d1117 0%, #161b22 100%);
        border-bottom: 2px solid #21262d;
        padding: 20px 32px 16px;
        display: flex;
        align-items: center;
        gap: 16px;
      }

      .app-title {
        font-family: 'Barlow Condensed', sans-serif;
        font-size: 2rem;
        font-weight: 900;
        letter-spacing: 1px;
        color: #f0f6fc;
        margin: 0;
        text-transform: uppercase;
      }

      .app-subtitle {
        font-size: 0.85rem;
        color: #8b949e;
        letter-spacing: 0.5px;
        margin: 2px 0 0;
        font-weight: 300;
      }

      .ca-badge {
        background: linear-gradient(135deg, #1f6feb, #388bfd);
        color: #fff;
        font-family: 'Barlow Condensed', sans-serif;
        font-weight: 700;
        font-size: 0.75rem;
        letter-spacing: 1.5px;
        padding: 4px 10px;
        border-radius: 4px;
        text-transform: uppercase;
        margin-left: auto;
      }

      /* ── control panel ── */
      .control-panel {
        background: #161b22;
        border-bottom: 1px solid #21262d;
        padding: 14px 32px;
        display: flex;
        align-items: center;
        gap: 24px;
        flex-wrap: wrap;
      }

      .panel-label {
        font-size: 0.7rem;
        font-weight: 600;
        letter-spacing: 1px;
        text-transform: uppercase;
        color: #8b949e;
        margin-bottom: 4px;
      }

      .school-toggle {
        display: flex;
        gap: 8px;
      }

      .school-chip {
        cursor: pointer;
        border-radius: 20px;
        padding: 5px 14px;
        font-family: 'Barlow Condensed', sans-serif;
        font-weight: 600;
        font-size: 0.82rem;
        letter-spacing: 0.5px;
        text-transform: uppercase;
        transition: all 0.15s;
        border: 1.5px solid transparent;
        user-select: none;
      }

      .btn-scrape {
        background: linear-gradient(135deg, #238636, #2ea043);
        color: #fff !important;
        border: none !important;
        font-family: 'Barlow Condensed', sans-serif !important;
        font-weight: 700 !important;
        font-size: 0.9rem !important;
        letter-spacing: 1px !important;
        text-transform: uppercase;
        padding: 8px 22px !important;
        border-radius: 6px !important;
        transition: all 0.15s;
        margin-left: auto;
      }
      .btn-scrape:hover { background: linear-gradient(135deg, #2ea043, #3fb950) !important; }

      /* ── status bar ── */
      .status-bar {
        background: #0d1117;
        padding: 8px 32px;
        font-size: 0.78rem;
        color: #8b949e;
        border-bottom: 1px solid #21262d;
        display: flex;
        align-items: center;
        gap: 8px;
        min-height: 34px;
      }

      .status-dot {
        width: 7px; height: 7px;
        border-radius: 50%;
        background: #3fb950;
        flex-shrink: 0;
      }
      .status-dot.loading { background: #f5c518; animation: pulse 1s infinite; }
      .status-dot.error   { background: #f85149; }

      @keyframes pulse { 0%,100% { opacity:1; } 50% { opacity:0.3; } }

      /* ── main content ── */
      .main-content { padding: 20px 24px; }

      /* ── stat pills ── */
      .stat-row {
        display: flex;
        gap: 12px;
        margin-bottom: 18px;
        flex-wrap: wrap;
      }

      .stat-pill {
        background: #161b22;
        border: 1px solid #21262d;
        border-radius: 8px;
        padding: 10px 20px;
        text-align: center;
        min-width: 100px;
      }

      .stat-pill .val {
        font-family: 'Barlow Condensed', sans-serif;
        font-size: 1.8rem;
        font-weight: 900;
        color: #388bfd;
        line-height: 1;
      }

      .stat-pill .lbl {
        font-size: 0.67rem;
        letter-spacing: 1px;
        text-transform: uppercase;
        color: #8b949e;
        margin-top: 2px;
        font-weight: 600;
      }

      /* ── DataTable overrides ── */
      .dataTables_wrapper { color: #e6edf3 !important; }

      table.dataTable {
        background: #0d1117 !important;
        color: #e6edf3 !important;
        border: 1px solid #21262d !important;
        border-radius: 8px;
        overflow: hidden;
      }

      table.dataTable thead th {
        background: #161b22 !important;
        color: #8b949e !important;
        font-family: 'Barlow Condensed', sans-serif !important;
        font-size: 0.72rem !important;
        letter-spacing: 1.2px !important;
        text-transform: uppercase !important;
        border-bottom: 1px solid #30363d !important;
        font-weight: 600 !important;
        padding: 10px 12px !important;
      }

      table.dataTable tbody tr {
        border-bottom: 1px solid #21262d !important;
        transition: background 0.1s;
      }

      table.dataTable tbody tr:hover td {
        background: #161b22 !important;
      }

      table.dataTable tbody td {
        background: #0d1117 !important;
        border: none !important;
        padding: 10px 12px !important;
        vertical-align: middle;
      }

      .dataTables_filter input,
      .dataTables_length select {
        background: #161b22 !important;
        border: 1px solid #30363d !important;
        color: #e6edf3 !important;
        border-radius: 4px !important;
        padding: 4px 8px !important;
      }

      .dataTables_info, .dataTables_paginate {
        color: #8b949e !important;
        font-size: 0.78rem !important;
      }

      .paginate_button { color: #8b949e !important; }
      .paginate_button.current { background: #21262d !important; color: #e6edf3 !important; border-radius: 4px; }

      /* ── school badges in table ── */
      .badge-ucd  { background: #002855; color: #B8922A; }
      .badge-af   { background: #003087; color: #8A9BA8; }
      .badge-army { background: #1C3F26; color: #B5A165; }

      .school-badge {
        display: inline-block;
        font-family: 'Barlow Condensed', sans-serif;
        font-weight: 700;
        font-size: 0.72rem;
        letter-spacing: 0.8px;
        text-transform: uppercase;
        padding: 2px 8px;
        border-radius: 4px;
      }

      /* position badge */
      .pos-badge {
        display: inline-block;
        background: #21262d;
        color: #f0f6fc;
        font-family: 'Barlow Condensed', sans-serif;
        font-weight: 700;
        font-size: 0.78rem;
        letter-spacing: 0.5px;
        padding: 2px 8px;
        border-radius: 4px;
        min-width: 36px;
        text-align: center;
      }

      /* name styling */
      .player-name-cell {
        font-weight: 600;
        font-size: 0.95rem;
        color: #f0f6fc;
      }

      .player-hs {
        font-size: 0.75rem;
        color: #8b949e;
        margin-top: 2px;
      }

      /* score badge */
      .score-badge {
        display: inline-block;
        background: #21262d;
        color: #f5c518;
        font-family: 'Barlow Condensed', sans-serif;
        font-weight: 700;
        font-size: 1rem;
        padding: 2px 8px;
        border-radius: 4px;
        min-width: 36px;
        text-align: center;
      }

      /* alert message */
      .alert-info {
        background: #0c2d6b33;
        border: 1px solid #1f6feb;
        border-radius: 8px;
        padding: 16px 20px;
        color: #79c0ff;
        font-size: 0.85rem;
        line-height: 1.6;
        margin-bottom: 16px;
      }

      .alert-warn {
        background: #3d1f0033;
        border: 1px solid #f0883e;
        border-radius: 8px;
        padding: 14px 18px;
        color: #f0883e;
        font-size: 0.82rem;
        margin-bottom: 12px;
      }
    "))
  ),
  
  # ── header ──
  div(class = "app-header",
      div(
        tags$p(class = "app-title", "CA Offer Tracker"),
        tags$p(class = "app-subtitle",
               sprintf("Army · Air Force · UC Davis  ·  %d Season", SEASON))
      ),
      div(class = "ca-badge", "🌴 CA Only")
  ),
  
  # ── controls ──
  div(class = "control-panel",
      div(
        div(class = "panel-label", "Schools"),
        checkboxGroupInput(
          "schools",
          label    = NULL,
          choices  = names(SCHOOLS),
          selected = names(SCHOOLS),
          inline   = TRUE
        )
      ),
      div(style = "margin-left:auto;",
          actionButton("scrape", "⟳  Refresh Data", class = "btn-scrape")
      )
  ),
  
  # ── status ──
  uiOutput("status_bar"),
  
  # ── body ──
  div(class = "main-content",
      
      # stat pills
      uiOutput("stat_pills"),
      
      # table
      uiOutput("table_or_msg")
  )
)


# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data   = NULL,
    status = "idle",    # idle | loading | done | error
    msg    = "Click 'Refresh Data' to fetch current offers.",
    n_raw  = 0
  )
  
  # ── scrape on button ─────────────────────────────────────
  observeEvent(input$scrape, {
    req(length(input$schools) > 0)
    
    rv$status <- "loading"
    rv$msg    <- "Fetching offers…"
    rv$data   <- NULL
    
    selected <- SCHOOLS[input$schools]
    
    withProgress(message = "Scraping 247sports…", {
      all_ca  <- NULL
      n_total <- 0
      
      for (i in seq_along(selected)) {
        nm   <- names(selected)[i]
        slug <- selected[[i]]$slug
        incProgress(1 / length(selected),
                    detail = sprintf("Loading %s…", nm))
        df <- scrape_offers(slug, nm)
        if (!is.null(df) && nrow(df) > 0) {
          n_total <- n_total + nrow(df)
          ca_df   <- df |> filter(toupper(trimws(State)) == "CA")
          all_ca  <- bind_rows(all_ca, ca_df)
        }
        Sys.sleep(runif(1, 1.2, 2.2))   # polite delay
      }
      
      if (!is.null(all_ca) && nrow(all_ca) > 0) {
        rv$data   <- all_ca |> arrange(desc(Score), Name)
        rv$n_raw  <- n_total
        rv$status <- "done"
        rv$msg    <- sprintf(
          "Found %d CA prospect%s out of %d total offer%s across %d school%s  ·  %s",
          nrow(rv$data), if (nrow(rv$data) != 1) "s" else "",
          n_total,       if (n_total != 1) "s" else "",
          length(selected), if (length(selected) != 1) "s" else "",
          format(Sys.time(), "%b %d, %Y %I:%M %p")
        )
      } else {
        rv$status <- "error"
        rv$msg    <- paste0(
          "No California prospects found or scrape failed. ",
          "Check console for warnings. Page structure may have changed."
        )
      }
    })
  })
  
  # ── status bar ──────────────────────────────────────────
  output$status_bar <- renderUI({
    dot_class <- switch(rv$status,
                        loading = "status-dot loading",
                        error   = "status-dot error",
                        "status-dot"
    )
    div(class = "status-bar",
        div(class = dot_class),
        span(rv$msg)
    )
  })
  
  # ── stat pills ──────────────────────────────────────────
  output$stat_pills <- renderUI({
    df <- rv$data
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    counts <- df |> count(School)
    pills  <- lapply(seq_len(nrow(counts)), function(i) {
      div(class = "stat-pill",
          div(class = "val", counts$n[i]),
          div(class = "lbl", counts$School[i])
      )
    })
    
    pills <- c(
      list(div(class = "stat-pill",
               div(class = "val", nrow(df)),
               div(class = "lbl", "Total CA Prospects")
      )),
      pills,
      list(div(class = "stat-pill",
               div(class = "val",
                   if (sum(!is.na(df$Stars)) > 0)
                     sprintf("%.1f★", mean(df$Stars, na.rm = TRUE))
                   else "—"
               ),
               div(class = "lbl", "Avg Stars")
      ))
    )
    
    do.call(div, c(list(class = "stat-row"), pills))
  })
  
  # ── table ────────────────────────────────────────────────
  output$table_or_msg <- renderUI({
    if (rv$status == "idle") {
      return(div(class = "alert-info",
                 tags$b("ℹ️  How to use:"), " Select the schools you want, then click ",
                 tags$b("Refresh Data"), ". Only prospects with a California high school will appear.",
                 tags$br(), tags$br(),
                 tags$b("Note:"), " 247sports may require a residential IP address. If you get errors, ",
                 "try running the app with a VPN off, or verify the selectors haven't changed via DevTools."
      ))
    }
    
    if (rv$status == "error") {
      return(div(class = "alert-warn",
                 tags$b("⚠ Scrape issue: "), rv$msg
      ))
    }
    
    if (is.null(rv$data) || nrow(rv$data) == 0) {
      return(div(class = "alert-info", "No data yet."))
    }
    
    DTOutput("prospects_table")
  })
  
  output$prospects_table <- renderDT({
    df <- rv$data
    req(!is.null(df), nrow(df) > 0)
    
    # ── badge helpers ──
    school_badge <- function(s) {
      cls <- switch(s,
                    "UC Davis"  = "badge-ucd",
                    "Air Force" = "badge-af",
                    "Army"      = "badge-army",
                    ""
      )
      sprintf('<span class="school-badge %s">%s</span>', cls, s)
    }
    
    name_cell <- function(name, hs, city) {
      loc <- if (!is.na(city)) paste0(city, ", CA") else "CA"
      hs_clean <- if (!is.na(hs) && nchar(trimws(hs)) > 0) trimws(hs) else "—"
      sprintf(
        '<div class="player-name-cell">%s</div><div class="player-hs">%s &middot; %s</div>',
        name, hs_clean, loc
      )
    }
    
    pos_cell <- function(p) {
      if (is.na(p) || nchar(trimws(p)) == 0) return("—")
      sprintf('<span class="pos-badge">%s</span>', trimws(p))
    }
    
    score_cell <- function(sc) {
      if (is.na(sc)) return("—")
      sprintf('<span class="score-badge">%s</span>', sc)
    }
    
    display <- df |>
      mutate(
        School_html  = sapply(School, school_badge),
        Name_html    = mapply(name_cell, Name, HS, City),
        Pos_html     = sapply(Pos, pos_cell),
        Stars_html   = sapply(Stars, render_stars),
        Score_html   = sapply(Score, score_cell),
        Committed_disp = if_else(is.na(Committed), "—", as.character(Committed))
      ) |>
      select(School_html, Name_html, Pos_html, `Ht/Wt`, Stars_html, Score_html, Committed_disp)
    
    colnames(display) <- c("School", "Prospect", "Pos", "Ht/Wt", "Stars", "Score", "Committed")
    
    datatable(
      display,
      escape      = FALSE,
      rownames    = FALSE,
      selection   = "none",
      class       = "cell-border",
      options     = list(
        pageLength  = 25,
        dom         = "ftip",
        order       = list(list(5, "desc")),
        columnDefs  = list(
          list(className = "dt-center", targets = c(2, 3, 4, 5, 6)),
          list(width = "110px", targets = 0),
          list(width = "250px", targets = 1),
          list(width = "60px",  targets = 2),
          list(width = "90px",  targets = 3)
        ),
        language    = list(
          search      = "",
          searchPlaceholder = "Search prospects…"
        ),
        initComplete = JS("
          function(settings, json) {
            $(this.api().table().container())
              .css({'background-color': '#0d1117', 'color': '#e6edf3'});
          }
        ")
      )
    )
  }, server = FALSE)
  
}

shinyApp(ui, server)