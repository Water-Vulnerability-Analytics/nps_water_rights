download_raw_loca2 <- function(output_dir = "data/all/loca2_mwbm/",
                               var = c("pr", "tasmin", "tasmax"),
                               overwrite = FALSE,
                               verbose = TRUE,
                               retries = 5L,
                               max_depth = 10L) {
  if (!requireNamespace("curl", quietly = TRUE)) stop("Install package 'curl'.")
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Install package 'xml2'.")
  
  base_url <- "https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/"
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  var <- unique(trimws(var))
  var <- var[nzchar(var)]
  if (length(var) == 0) stop("Provide at least one variable in `var`.")
  
  MWBM_MODELS <- c(
    "ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CESM2-LENS",
    "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "CanESM5", "EC-Earth3",
    "EC-Earth3-Veg", "FGOALS-g3", "GFDL-CM4", "GFDL-ESM4", "HadGEM3-GC31-LL",
    "HadGEM3-GC31-MM", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "KACE-1-0-G",
    "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "NorESM2-LM",
    "NorESM2-MM", "TaiESM1"
  )
  
  h <- curl::new_handle()
  curl::handle_setheaders(h, "User-Agent" = "Mozilla/5.0-R-loca2-downloader")
  curl::handle_setopt(h, followlocation = 1L)
  
  fetch_html <- function(url, tries = 4L) {
    for (i in seq_len(tries)) {
      res <- try(curl::curl_fetch_memory(url, handle = h), silent = TRUE)
      if (!inherits(res, "try-error") && res$status_code < 400 && length(res$content) > 0) {
        return(rawToChar(res$content))
      }
      Sys.sleep(min(i, 3))
    }
    NULL
  }
  
  crawl_loca2_monthly_urls <- function(base, var, max_depth = 10L, verbose = TRUE) {
    if (!grepl("/$", base)) base <- paste0(base, "/")
    
    q <- data.frame(url = base, depth = 0L, stringsAsFactors = FALSE)
    visited <- character()
    files <- character()
    
    while (nrow(q) > 0) {
      cur <- q[1, , drop = FALSE]
      q <- q[-1, , drop = FALSE]
      u <- cur$url[[1]]
      d <- cur$depth[[1]]
      
      if (u %in% visited) next
      visited <- c(visited, u)
      
      if (verbose && length(visited) %% 50L == 0L) {
        message("Crawled dirs: ", length(visited))
      }
      
      html <- fetch_html(u)
      if (is.null(html)) next
      
      doc <- try(xml2::read_html(html), silent = TRUE)
      if (inherits(doc, "try-error")) next
      
      hrefs <- xml2::xml_attr(xml2::xml_find_all(doc, ".//a"), "href")
      hrefs <- hrefs[!is.na(hrefs) & nzchar(hrefs)]
      hrefs <- hrefs[!hrefs %in% c("../", "./")]
      hrefs <- hrefs[!grepl("^#|^\\?|^mailto:|^javascript:", hrefs)]
      
      links <- xml2::url_absolute(hrefs, u)
      links <- sub("#.*$", "", links)
      links <- sub("\\?.*$", "", links)
      links <- unique(links[grepl("^https?://", links)])
      links <- links[startsWith(links, base)]
      
      dirs <- links[grepl("/$", links)]
      if (length(dirs) > 0 && d < max_depth) {
        add <- setdiff(dirs, visited)
        if (length(add) > 0) {
          q <- rbind(q, data.frame(url = add, depth = d + 1L, stringsAsFactors = FALSE))
        }
      }
      
      var_path_ok <- Reduce(`|`, lapply(var, function(v) grepl(paste0("/", v, "/"), links, fixed = TRUE)))
      var_file_ok <- Reduce(`|`, lapply(var, function(v) startsWith(basename(links), paste0(v, "."))))
      monthly_ok <- grepl("\\.monthly\\.nc$", basename(links), ignore.case = TRUE)
      
      var_nc <- links[var_path_ok & var_file_ok & monthly_ok]
      if (length(var_nc) > 0) files <- c(files, var_nc)
    }
    
    sort(unique(files))
  }
  
  parse_loca2_file_meta <- function(urls) {
    files <- basename(urls)
    parts <- strsplit(files, ".", fixed = TRUE)
    get_part <- function(i) vapply(parts, function(x) if (length(x) >= i) x[i] else NA_character_, character(1))
    
    data.frame(
      url = urls,
      file = files,
      variable = get_part(1),
      model = get_part(2),
      scenario = get_part(3),
      run = get_part(4),
      period = get_part(5),
      stringsAsFactors = FALSE
    )
  }
  
  select_mwbm_loca2_urls <- function(urls,
                                     var,
                                     models = MWBM_MODELS,
                                     scenarios = c("historical", "ssp245", "ssp370", "ssp585"),
                                     preferred_runs = c("r1i1p1f1", "r1i1p1f2", "r1i1p1f3", "r1i1p1f4"),
                                     verbose = TRUE) {
    meta <- parse_loca2_file_meta(urls)
    meta <- meta[
      meta$variable %in% var &
        meta$model %in% models &
        meta$scenario %in% scenarios,
      ,
      drop = FALSE
    ]
    
    if (nrow(meta) == 0) stop("No URLs matched MWBM model/scenario filters.")
    
    meta$key <- sub("_v[0-9]{8}(?=\\.monthly\\.nc$)", "", meta$file, perl = TRUE)
    vtag <- regmatches(meta$file, regexpr("v[0-9]{8}", meta$file))
    meta$version_num <- suppressWarnings(as.integer(sub("^v", "", vtag)))
    meta$version_num[is.na(meta$version_num)] <- -1L
    meta <- meta[order(meta$key, -meta$version_num, meta$file), , drop = FALSE]
    meta <- meta[!duplicated(meta$key), , drop = FALSE]
    
    fut <- meta$scenario %in% c("ssp245", "ssp370", "ssp585")
    run_pool <- unique(meta[fut, c("model", "run"), drop = FALSE])
    if (nrow(run_pool) == 0) run_pool <- unique(meta[, c("model", "run"), drop = FALSE])
    
    run_pool$n_ssp <- mapply(function(m, r) {
      length(unique(meta$scenario[meta$model == m & meta$run == r & meta$scenario %in% c("ssp245", "ssp370", "ssp585")]))
    }, run_pool$model, run_pool$run)
    
    run_pool$n_files <- mapply(function(m, r) {
      sum(meta$model == m & meta$run == r)
    }, run_pool$model, run_pool$run)
    
    run_pool$pref_rank <- match(run_pool$run, preferred_runs)
    run_pool$pref_rank[is.na(run_pool$pref_rank)] <- length(preferred_runs) + 1L
    
    run_pool <- run_pool[order(run_pool$model, -run_pool$n_ssp, -run_pool$n_files, run_pool$pref_rank, run_pool$run), , drop = FALSE]
    selected_runs <- run_pool[!duplicated(run_pool$model), c("model", "run"), drop = FALSE]
    
    selected <- merge(meta, selected_runs, by = c("model", "run"), all = FALSE)
    selected <- selected[order(selected$variable, selected$model, selected$scenario, selected$period), , drop = FALSE]
    
    if (verbose) {
      message("Variables requested: ", paste(var, collapse = ", "))
      message("MWBM models found: ", length(unique(selected$model)), " / ", length(models))
      message("Selected one realization per model: ", nrow(selected_runs), " runs")
      message("Files after MWBM filtering: ", nrow(selected))
    }
    
    selected$url[order(selected$url)]
  }
  
  download_urls_one_by_one <- function(urls, output_dir, retries = 5L, overwrite = FALSE, verbose = TRUE) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    dest <- file.path(output_dir, basename(urls))
    if (!overwrite) {
      keep <- !file.exists(dest)
      urls <- urls[keep]
      dest <- dest[keep]
    }
    
    if (length(urls) == 0) {
      if (verbose) message("No missing files to download.")
      return(data.frame())
    }
    
    if (anyDuplicated(dest)) stop("Basename collisions found in URLs.")
    
    hdl <- curl::new_handle()
    curl::handle_setheaders(hdl, "User-Agent" = "Mozilla/5.0-R-loca2-downloader")
    curl::handle_setopt(hdl, followlocation = 1L)
    
    out <- vector("list", length(urls))
    
    for (i in seq_along(urls)) {
      u <- urls[i]
      d <- dest[i]
      ok <- FALSE
      status_code <- NA_integer_
      err <- ""
      
      for (a in seq_len(retries)) {
        res <- try(curl::curl_fetch_disk(u, d, handle = hdl), silent = TRUE)
        if (!inherits(res, "try-error")) {
          status_code <- res$status_code
          if (status_code %in% c(200L, 206L, 416L)) {
            ok <- TRUE
            break
          } else {
            err <- paste0("HTTP ", status_code)
            if (file.exists(d)) file.remove(d)
          }
        } else {
          err <- as.character(res)
          if (file.exists(d)) file.remove(d)
        }
        Sys.sleep(min(2 * a, 10))
      }
      
      out[[i]] <- data.frame(ok = ok, status_code = status_code, url = u, destfile = d, error = err, stringsAsFactors = FALSE)
      
      if (verbose && (i %% 20L == 0L || i == length(urls))) {
        n_ok <- sum(vapply(out[1:i], function(x) x$ok, logical(1)))
        message(sprintf("[%d/%d] complete | ok so far: %d", i, length(urls), n_ok))
      }
      
      Sys.sleep(0.15)
    }
    
    do.call(rbind, out)
  }
  
  urls_all <- crawl_loca2_monthly_urls(base_url, var = var, max_depth = max_depth, verbose = verbose)
  if (verbose) message("Discovered monthly files for [", paste(var, collapse = ", "), "]: ", length(urls_all))
  
  urls_needed <- select_mwbm_loca2_urls(urls_all, var = var, verbose = verbose)
  res <- download_urls_one_by_one(urls_needed, output_dir = output_dir, retries = retries, overwrite = overwrite, verbose = verbose)
  
  invisible(res)
}


# download_raw_loca2(var = c("pr"))
# download_raw_loca2(var = c("pr", "tasmin", "tasmax"))
