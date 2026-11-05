get_traffic_counts <- function(park_unit) {
  
  timeout_sec <- 60
  url <- paste0("https://irma.nps.gov/Stats/SSRSReports/Park%20Specific%20Reports/Traffic%20Counts?Park=", park_unit)
  ua <- user_agent("Mozilla/5.0 (R; httr)")
  
  # find the wrapper to the iframe (if present)
  wrapper_html <- GET(url, ua, timeout(timeout_sec)) %>%
    stop_for_status() %>%
    content(as = "text", encoding = "UTF-8") %>%
    read_html()
  
  iframe <- wrapper_html %>% 
    html_element("iframe") %>% 
    html_attr("src")
  
  doc <- if (is.na(iframe)) {
    wrapper_html
  } else {
    GET(url_absolute(iframe, url), ua, timeout(timeout_sec)) %>%
      stop_for_status() %>%
      content(as = "text", encoding = "UTF-8") %>%
      read_html()
  }
  
  # extract largest (should also be the only) table
  raw_data <- doc %>%
    html_elements("table") %>%
    map(html_table, fill = TRUE) %>%
    {
      if (length(.) == 0) stop("No tables found on the report page.")
      .[[which.max(map_int(., nrow))]]
    }
  
  # organize table
  parse_year_row <- function(s) {
    yr <- str_extract(s, "^\\d{4}")
    toks <- str_extract_all(s, "\\-?\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?%?|\\-?\\d+(?:\\.\\d+)?%?")[[1]]
    
    if (length(toks) < 14 || is.na(yr)) return(NULL)
    
    tibble(year = as.integer(yr),
           jan = parse_number(toks[2]),
           feb = parse_number(toks[3]),
           mar = parse_number(toks[4]),
           apr = parse_number(toks[5]),
           may = parse_number(toks[6]),
           jun = parse_number(toks[7]),
           jul = parse_number(toks[8]),
           aug = parse_number(toks[9]),
           sep = parse_number(toks[10]),
           oct = parse_number(toks[11]),
           nov = parse_number(toks[12]),
           dec = parse_number(toks[13]),
           annual_total = parse_number(toks[14]),
           change_from_prev_year_pct = suppressWarnings(as.numeric(str_replace(toks[15], "%", ""))))
  }
  
  # collapse rows into text lines and clean
  table_wide <- raw_data %>%
    mutate(across(everything(), ~na_if(str_squish(as.character(.x)), ""))) %>%
    janitor::remove_empty(which = c("rows", "cols")) %>%
    mutate(line = pmap_chr(across(everything()), ~{
      c(...) %>%
        discard(is.na) %>%
        discard(~.x == "") %>%
        paste(collapse = " ") %>%
        str_squish()
    })) %>%
    filter(line != "",
           !str_detect(line, regex("Find\\s*\\||Word\\s+Excel|Accessible\\s+PDF|Add\\s+Color", ignore_case = TRUE))) %>%
    # filter for traffic count lines and clean
    filter(str_detect(line, regex("TRAFFIC COUNT AT|JAN\\s*FEB|Traffic CountsPage|Change From|Annual Total", ignore_case = TRUE)) |
             str_detect(line, "^\\d{4}")) %>%
    mutate(line = line %>%
             str_replace_all("(?<=[A-Za-z])(?=\\d)", " ") %>%
             str_replace_all("(?<=\\d)(?=[A-Za-z])", " ") %>%
             str_squish(),
           station = if_else(str_detect(line, regex("TRAFFIC COUNT AT", ignore_case = TRUE)),
                             str_trim(str_replace(line, regex(".*?(TRAFFIC COUNT AT.*)$", ignore_case = TRUE), "\\1")),
                             NA_character_)) %>%
    fill(station, .direction = "down") %>%
    filter(str_detect(line, "^\\d{4}")) %>%
    mutate(parsed = map(line, parse_year_row)) %>%
    filter(!map_lgl(parsed, is.null)) %>%
    select(station, parsed) %>%
    unnest(parsed) %>%
    mutate(station = str_squish(station)) 
  
  table_long <- table_wide %>%
    pivot_longer(cols = jan:dec, names_to = "month", values_to = "count") %>%
    mutate(month = factor(month, levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")),
           month_num = as.integer(month),
           ym = as.Date(sprintf("%d-%02d-01", year, month_num)),
           park_unit = park_unit) %>%
    arrange(station, year, month) %>%
    select(park_unit, station, ym, count)
  
  return(table_long)
}

# test_1 <- get_traffic_counts("ROMO") 
# test_2 <- get_traffic_counts("SHEN")
# test_3 <- get_traffic_counts("GLAC")
