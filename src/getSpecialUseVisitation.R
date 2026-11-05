#' Scrape monthly NPS special use visitation from IRMA Stats
#'
#' @param park Four-letter park unit code, e.g. "ZION".
#' @param output_dir Folder for the saved CSV.
#' @param save_csv Whether to save `<park>.csv`.
#' @param retries Retry attempts per request.
#' @param quiet Whether to suppress progress messages.
#' @param workers Parallel workers used to fetch months.
#' @return Tibble with one row per month and metric. Attributes: `park`,
#'   `failed_months`, and `output_csv` (if saved).
getSpecialUseVisitation <- function(park,
                                    output_dir = "data/all/special_use_visitation/",
                                    save_csv   = TRUE,
                                    retries    = 6,
                                    quiet      = FALSE,
                                    workers    = 2L) {
  
  park <- toupper(trimws(park))
  if (!nzchar(park)) stop("park is required.")
  
  landing_url <- sprintf(
    "https://irma.nps.gov/Stats/SSRSReports/Park%%20Specific%%20Reports/Monthly%%20Public%%20Use?Park=%s",
    utils::URLencode(park, reserved = TRUE)
  )
  
  # ── helpers ────────────────────────────────────────────────────────────────
  make_handle <- function(cookie_file) {
    h <- curl::new_handle()
    curl::handle_setheaders(h, "User-Agent" = "Mozilla/5.0-R-script-curl")
    curl::handle_setopt(
      h,
      followlocation = 1L,
      cookiefile = cookie_file,
      cookiejar = cookie_file
    )
    h
  }
  
  fetch_text <- function(url, h, retries) {
    for (i in seq_len(retries)) {
      res <- try(curl::curl_fetch_memory(url, handle = h), silent = TRUE)
      
      if (!inherits(res, "try-error") &&
          identical(res$status_code, 200L) &&
          length(res$content) > 0) {
        
        txt <- rawToChar(res$content)
        
        if (!grepl(
          "Service unavailable|MvcReportViewer Error",
          txt,
          ignore.case = TRUE
        )) {
          return(txt)
        }
      }
      
      Sys.sleep(min(2 * i, 10))
    }
    
    stop("Failed to fetch: ", url)
  }
  
  fetch_raw <- function(url, h, retries) {
    for (i in seq_len(retries)) {
      res <- try(curl::curl_fetch_memory(url, handle = h), silent = TRUE)
      
      if (!inherits(res, "try-error") &&
          identical(res$status_code, 200L) &&
          length(res$content) > 0) {
        return(res$content)
      }
      
      Sys.sleep(min(2 * i, 10))
    }
    
    stop("Failed to fetch raw: ", url)
  }
  
  add_or_replace_query_param <- function(url, param, value) {
    enc <- utils::URLencode(value, reserved = TRUE)
    
    pattern <- paste0("([?&])", param, "=[^&]*")
    replacement <- paste0("\\1", param, "=", enc)
    
    if (grepl(pattern, url)) {
      sub(pattern, replacement, url)
    } else {
      sep <- if (grepl("\\?", url)) "&" else "?"
      paste0(url, sep, param, "=", enc)
    }
  }
  
  set_rpt_date <- function(url, month_label) {
    add_or_replace_query_param(url, "RptDate", month_label)
  }
  
  extract_export_xml_url <- function(viewer_html) {
    rel <- stringr::str_match(
      viewer_html,
      "\"ExportUrlBase\":\"([^\"]+Format=)\""
    )[, 2]
    
    if (is.na(rel)) stop("Could not find ExportUrlBase.")
    
    rel <- stringr::str_replace_all(rel, "\\\\u0026", "&")
    rel <- stringr::str_replace_all(rel, "\\\\/", "/")
    
    if (startsWith(rel, "http")) {
      paste0(rel, "XML")
    } else {
      paste0("https://irma.nps.gov", rel, "XML")
    }
  }
  
  parse_special_use_xml <- function(raw_xml, rpt_date_label, source_xml_url) {
    doc <- xml2::read_xml(raw_xml)
    xml2::xml_ns_strip(doc)
    
    hdr  <- xml2::xml_find_first(doc, ".//Details")
    rows <- xml2::xml_find_all(doc, ".//Details1")
    
    if (length(rows) == 0) {
      return(tibble::tibble())
    }
    
    month_label <- xml2::xml_attr(hdr, "Textbox5")
    
    tibble::tibble(
      park_name      = xml2::xml_attr(hdr, "ParkName"),
      park_code      = xml2::xml_attr(hdr, "ParkCode"),
      month_label    = month_label,
      month_mm_yyyy  = xml2::xml_attr(hdr, "Textbox33"),
      month_start    = as.Date(
        paste0("01 ", month_label),
        format = "%d %B %Y"
      ),
      rpt_date_param = rpt_date_label,
      metric         = xml2::xml_attr(rows, "Field38"),
      this_month     = suppressWarnings(as.numeric(xml2::xml_attr(rows, "Field39"))),
      year_to_date   = suppressWarnings(as.numeric(xml2::xml_attr(rows, "Field40"))),
      source_xml_url = source_xml_url
    )
  }
  
  # ── 1. Landing page → viewer URL ───────────────────────────────────────────
  cookie_file <- tempfile(fileext = ".cookies")
  h <- make_handle(cookie_file)
  
  landing_html <- fetch_text(landing_url, h, retries)
  
  iframe_src <- landing_html |>
    xml2::read_html() |>
    rvest::html_element("iframe") |>
    rvest::html_attr("src")
  
  if (is.na(iframe_src) || !nzchar(iframe_src)) {
    stop("Could not find iframe src.")
  }
  
  viewer_url <- if (startsWith(iframe_src, "http")) {
    iframe_src
  } else {
    paste0("https://irma.nps.gov", iframe_src)
  }
  
  viewer_url <- gsub("&amp;", "&", viewer_url, fixed = TRUE)
  
  if (!grepl("([?&])Park=", viewer_url)) {
    viewer_url <- add_or_replace_query_param(viewer_url, "Park", park)
  }
  
  # ── 2. Discover available months ───────────────────────────────────────────
  first_html <- fetch_text(viewer_url, h, retries)
  
  rpt_dates <- first_html |>
    xml2::read_html() |>
    rvest::html_elements("select#ReportViewer_ctl04_ctl05_ddValue option") |>
    rvest::html_text2() |>
    stringr::str_squish()
  
  rpt_dates <- rpt_dates[!is.na(rpt_dates)]
  rpt_dates <- rpt_dates[nzchar(rpt_dates)]
  rpt_dates <- rpt_dates[!grepl("^<Select", rpt_dates, ignore.case = TRUE)]
  
  if (length(rpt_dates) == 0) {
    stop("No RptDate options found.")
  }
  
  # ── 3. Fetch each month in parallel ────────────────────────────────────────
  old_plan <- future::plan()
  future::plan(future::multisession, workers = workers)
  on.exit(future::plan(old_plan), add = TRUE)
  
  monthly_results <- furrr::future_map(
    rpt_dates,
    function(m) {
      cf <- tempfile(fileext = ".cookies")
      wh <- make_handle(cf)
      
      tryCatch({
        if (!quiet) message("Fetching: ", m)
        
        month_url  <- set_rpt_date(viewer_url, m)
        month_html <- fetch_text(month_url, wh, retries)
        xml_url    <- extract_export_xml_url(month_html)
        xml_raw    <- fetch_raw(xml_url, wh, retries)
        
        list(
          month = m,
          ok = TRUE,
          error = NA_character_,
          data = parse_special_use_xml(xml_raw, m, xml_url)
        )
      }, error = function(e) {
        if (!quiet) message("  Failed ", m, ": ", conditionMessage(e))
        
        list(
          month = m,
          ok = FALSE,
          error = conditionMessage(e),
          data = tibble::tibble()
        )
      })
    },
    .options = furrr::furrr_options(seed = NULL)
  )
  
  failed_months <- vapply(
    monthly_results[!vapply(monthly_results, `[[`, logical(1), "ok")],
    `[[`,
    character(1),
    "month"
  )
  
  out <- dplyr::bind_rows(lapply(monthly_results, `[[`, "data"))
  
  # ── 4. Post-process ────────────────────────────────────────────────────────
  if (nrow(out) > 0) {
    out <- out |>
      dplyr::arrange(month_start, metric) |>
      dplyr::mutate(park_unit = park)
  } else {
    out <- tibble::tibble(
      park_name      = character(),
      park_code      = character(),
      month_label    = character(),
      month_mm_yyyy  = character(),
      month_start    = as.Date(character()),
      rpt_date_param = character(),
      metric         = character(),
      this_month     = numeric(),
      year_to_date   = numeric(),
      source_xml_url = character(),
      park_unit      = character()
    )
  }
  
  if (save_csv) {
    if (is.null(output_dir)) {
      output_dir <- paste0(tolower(park), "_special_use_downloads")
    }
    
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    
    out_csv <- file.path(output_dir, paste0(park, ".csv"))
    readr::write_csv(out, out_csv)
    
    attr(out, "output_csv") <- out_csv
  }
  
  attr(out, "park") <- park
  attr(out, "failed_months") <- failed_months
  
  out
}