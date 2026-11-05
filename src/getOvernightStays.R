#' Get monthly visitor use and overnight-stay components for NPS unit(s)
#'
#' Reads locally stored NPS visitor use data files (from Data Store exports),
#' filters to one or more NPS unit codes, reshapes to compute derived day-use and
#' overnight-use metrics, and returns a tidy monthly time series.
#' @param parks Character vector of NPS unit code(s)
#'
#' @return A tibble with one row per unit code per month of visitation data.
#' 
getOvernightStays <- function(parks){
  # vars we care about - 
  visit_vars <- c("Backcountry", "ConcessionerCamping", "ConcessionerLodging",
                  "MiscellaneousOvernightStays", "NonRecreationOvernightStays",
                  "NonRecreationVisits", "RecreationVisits",
                  "RVCampers", "TentCampers")
  
  all_parks <- list.files("data/all/nps_visitor_use_data/", full.names = TRUE) %>%
    purrr::map(~data.table::fread(.) %>% mutate(across(everything(), as.character))) %>%
    dplyr::bind_rows() %>%
    mutate(UnitCode = case_when(UnitCode == "JOFK" ~ "JOFI",
                                UnitCode == "KLSE" ~ "KLGO",
                                UnitCode == "MAMC" ~ "MABE", 
                                UnitCode == "NCPC" ~ "NACA",
                                UnitCode == "NCPE" ~ "NACA",
                                UnitCode == "NAVC" ~ "NACA",
                                UnitCode == "OKCI" ~ "OKCI",
                                UnitCode == "PRPA" ~ "NACA",
                                .default = UnitCode))
  
  park_select <- all_parks %>%
    dplyr::filter(UnitCode %in% parks) %>%
    select(UnitCode, Month, Year, Backcountry, ConcessionerCamping, ConcessionerLodging,
           MiscellaneousOvernightStays, NonRecreationOvernightStays, NonRecreationVisits, RecreationVisits, RVCampers, TentCampers) %>%
    mutate(ym = lubridate::ym(paste0(Year, "-", Month))) %>%
    pivot_longer(cols = any_of(visit_vars), names_to = "var", values_to = "visits") %>%
    mutate(visits = as.numeric(gsub(",|-", "", visits))) %>%
    distinct()
  
  park_select_wide <- park_select %>% pivot_wider(names_from = var, values_from = visits, values_fn = mean) %>%
    mutate(RecreationDayUse = RecreationVisits - ConcessionerLodging - ConcessionerCamping - TentCampers - RVCampers - Backcountry - MiscellaneousOvernightStays,
           NonRecreationDayUse = NonRecreationVisits - NonRecreationOvernightStays,
           RecreationOvernightUse = ConcessionerLodging + ConcessionerCamping + TentCampers + RVCampers + Backcountry + MiscellaneousOvernightStays,
           NonRecreationOvernightUse = NonRecreationOvernightStays,
           TotalDayUse = NonRecreationDayUse + RecreationDayUse,
           TotalOvernightUse = NonRecreationOvernightUse + RecreationOvernightUse,
           TotalPeople = RecreationDayUse + RecreationOvernightUse + NonRecreationDayUse + NonRecreationOvernightUse) %>%
    select(-c(Month, Year, NonRecreationOvernightStays)) 
  
  return(park_select_wide)
  
}

#   suppressMessages(suppressWarnings({
#   
#   raw_table <- rvest::read_html(paste0('https://irma.nps.gov/Stats/MvcReportViewer.aspx?_id=5f032a4d-01f9-4078-94b4-6c26358cc826&_m=Remote&_r=%2fNPS.Stats.Reports%2fPark+Specific+Reports%2fOvernight+Stays+(1979+-+Last+Calendar+Year)&_15=True&_16=True&_18=True&_19=True&_34=False&_35=False&_39=880px&Park=', park)) %>%
#     rvest::html_table() %>%
#     .[[34]]
#   
#   column_names <- raw_table[2, ]
#   
#   # Create a new data frame with the correct column names
#   fixed_table <- raw_table %>%
#     # Remove the empty rows and the one that contains the column names
#     slice(c(-1, -2)) %>%  
#     # Set the column names to the extracted values
#     set_names(column_names) %>% 
#     # Remove commas, convert all columns to numeric
#     mutate(across(everything(), ~ str_replace_all(., ",", ""))) %>%
#     mutate(across(everything(), as.numeric)) %>%
#     # Add park name as column
#     mutate(park = park)
#   
#   }))
#   
#   return(fixed_table)
#   
# }
# # Code to grab for all parks:
#  parks <- getParkSystem() %>%
#    .$UNIT_CODE %>%
#    map(~try(getOvernightStays(park = .), silent = TRUE)) %>%
#    keep(., is.data.frame) %>%
#    bind_rows()
# 
# # Parks with this info:
# sort(unique(parks$park))
# #
# #
# #
# # # Parks without this info:
# no_data <- getParkSystem() %>%
#   sf::st_drop_geometry() %>%
#   dplyr::filter(!UNIT_CODE %in% parks$park)
# 
# getUnitVisitationSub <- function(park){
#   
#   suppressMessages(suppressWarnings({
#     
#     raw_table <- rvest::read_html(paste0("https://irma.nps.gov/Stats/MvcReportViewer.aspx?_id=d09e9a98-bfc0-4186-8d0c-44dd2baee357&_m=Remote&_r=%2fNPS.Stats.Reports%2fPark+Specific+Reports%2fSummary+of+Visitor+Use+By+Month+and+Year+(1979+-+Last+Calendar+Year)&_15=True&_16=True&_18=True&_19=True&_34=False&_35=False&_39=880px&Park=", park)) %>%
#       rvest::html_table(., trim = TRUE, header = TRUE, "/html/body/form/div[3]/span/div/table/tbody/tr[5]/td[3]/div/div[1]/div/table/tbody/tr/td/table/tbody/tr[1]/td/table/tbody/tr/td/table/tbody/tr[4]/td[3]/table/tbody/tr[2]") %>%
#       rvest::html_table() 
#     
#     table_data <- xml2::xml_attr(raw_table)
#     
#     column_names <- raw_table[2, ]
#     
#     # Create a new data frame with the correct column names
#     fixed_table <- raw_table %>%
#       # Remove the empty rows and the one that contains the column names
#       slice(c(-1, -2)) %>%  
#       # Set the column names to the extracted values
#       set_names(column_names) %>% 
#       # Remove commas, convert all columns to numeric
#       mutate(across(everything(), ~ str_replace_all(., ",", ""))) %>%
#       mutate(across(everything(), as.numeric)) %>%
#       # Add park name as column
#       mutate(park = park)
#     
#   }))
#   
#   return(fixed_table)
#   
# }
# How to clean the excel version of the output:
# raw =  readxl::read_excel("data/misc/BRCA_visitor_sub_raw.xlsx", skip = 2, col_names = TRUE)
# 
# #test <-  raw %>% filter(!is.na(...1) & !grepl("Totals", ...1)) %>% filter(is.na(`Recreation Visitors`))
# 
# raw_new <- raw %>% 
#   filter(!is.na(...1) & !grepl("Totals", ...1)) %>% 
#   mutate(year = ifelse(is.na(`Recreation Visitors`), ...1, NA)) %>%
#   fill(year, .direction = "down") %>%
#   filter(!is.na(`Recreation Visitors`)) %>%
#   mutate(ym = ym(paste(year, "-", month(parse_date_time(...1, orders = "b")))),
#          UNIT_CODE = "BRCA") %>%
#   select(-...1) %>%
#   select(UNIT_CODE, year, ym, everything()) %>%
#   janitor::clean_names() %>%
#   write_csv("data/park/BRCA/visitors_hr.csv")
