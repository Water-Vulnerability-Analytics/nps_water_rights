#' Function to read in USGS Groundwater Climate REsponse Network (CRN) .xls file 
#' containing state, site identification number, site name, well depth (ft), 
#' national aquifer name, and funding source for 

#' https://rconnect.usgs.gov/crn-map/CRNmap.html
#' Predownloaded for now because xls file is a blob:: link that is hard to
#' download directly into R. TO UPDATE IN FUTURE!!


get_crn <- function() {
  
 CRN <- read_xlsx("data/all/CRN/CRN_Info_table.xlsx",
                  col_types = c("text","text","text","numeric","text","text")) %>%
   clean_names()
  
 
 data_type_priority <- c("dv", "gw", "uv")
 code_priority <- c("72019", "62610", "62611", "72150", "72229", 
                    "30210", "99019")
 
 CRN_dat <- listNWIS(sites = CRN$site_identification_number) %>%
   dplyr::filter(site_type == "Well",
                 code %in% code_priority) %>%
   # many sites have multiple data types available (daily, continuous, and 
   # measurements; depth, elevation, datums). Here, we make some rules and
   # prioritize selection based on the max timespan of data availability,
   dplyr::mutate(tot_time = time_length(interval(begin_date,end_date),"year")) %>%
   dplyr::group_by(site_no) %>%
   dplyr::filter(tot_time == max(tot_time)) %>%
   # Then the first in the priority list of parameter codes
   dplyr::filter(code == first(code_priority[code_priority %in% code])) %>%
   # Then, select the first available in the parameter priority codes
   dplyr::filter(data_type_cd == first(data_type_priority[data_type_priority %in% data_type_cd])) %>%
   # still returning duplicates so select row with max # obs
   dplyr::filter(n_obs == max(n_obs)) %>%
   ungroup() %>%
   # Still duplicates with no discernable difference between rows so just keep one
   distinct(site_no, .keep_all = TRUE) %>%
    add_gw_meta() 
   
  return(CRN_dat)
  
}
