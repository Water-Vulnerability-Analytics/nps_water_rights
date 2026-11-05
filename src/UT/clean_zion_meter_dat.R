clean_zion_meter_dat <- function() {
  
  paths <- list.files("data/park/ZION/meter_data/", full.names = TRUE, pattern = ".xlsx") 
  
  clean_meter_dat <- function(path) {
    
    #path <- meter_fils[9]
    year <- basename(path) %>% substr(1,4) %>% as.numeric()
    
    cleaned_dat <- readxl::read_xlsx(path) %>%
      clean_names() %>%
      select(!ends_with("_used")) %>%
      rename_with(~ str_remove(., "_read"), ends_with("_read")) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_longer(cols = any_of(c("jan","feb","mar","apr","may","jun","june",
                                   "jul","july","aug","sept","sep","oct","nov",
                                   "dec")), names_to = "month", values_to = "monthly_read") %>%
      dplyr::mutate(year = year,
                    ym = ymd(paste(year,month,"01")),
                    monthly_read = as.numeric(monthly_read)) %>%
      dplyr::select(ym, meter_location_service, monthly_read)
    
  }
  
  # Read in meter dat - note, this is monthly totalizer reading
  meter_dat <- paths %>%
    purrr::map_dfr(clean_meter_dat) %>%
    drop_na(meter_location_service) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = year(ym))
  
  # Make sure all months are included for all sites  
  complete_dates <- expand_grid(ym = unique(meter_dat$ym),
                                meter_location_service = unique(meter_dat$meter_location_service))
  meter_dat <- meter_dat %>%
    full_join(complete_dates, by = c("ym", "meter_location_service")) %>%
    arrange(ym) %>%
    dplyr::group_by(meter_location_service) %>%
    dplyr::mutate(monthly_read_interp = na.approx(monthly_read, na.rm = FALSE)) %>%
    dplyr::mutate(monthly_diff = c(NA, diff(monthly_read_interp))) %>%
    dplyr::mutate(monthly_diff = ifelse(monthly_diff < 0, NA, monthly_diff)) %>%
    dplyr::mutate(monthly_diff = na.approx(monthly_diff, na.rm = FALSE)) %>%
    dplyr::mutate(monthly_diff = ifelse(monthly_diff > 3.0E07, NA, monthly_diff))
  
  #ggplotly(
  #  ggplot(meter_dat, #%>% dplyr::filter(meter_location_service == "L. Grotto Sandtrap/Spring"), 
  #         aes(x = ym, y = monthly_diff, color = meter_location_service)) + geom_line() + theme_bw()
  #)
}
