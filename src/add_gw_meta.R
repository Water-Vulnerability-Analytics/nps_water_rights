#' Adds well and aquifer metadata to nwis_groundwater dataframe based on USGS site_no 
#' @param nwis_groundwater a list of nwis groundwater sites returned and filtered from function 'listNWIS'
#'              

add_gw_meta <- function(nwis_groundwater) {

# Define function to get well metadata
get_well_meta <- function(gw_list) {
  
  # First, get well metadata using dataRetrieval function 'readNWISsite'
  nwis_site_dat <- dataRetrieval::readNWISsite(gw_list$site_no) %>% 
    dplyr::select(c("site_no",
                    "state_cd",
                    "huc_cd", 
                    "basin_cd", 
                    "topo_cd", 
                    "drain_area_va", 
                    "contrib_drain_area_va",
                    "nat_aqfr_cd", 
                    "aqfr_cd", 
                    "aqfr_type_cd", 
                    "well_depth_va"))
 
  nwis_groundwater_meta <- gw_list %>%
    dplyr::left_join(., nwis_site_dat, by = "site_no")  
  
  return(nwis_groundwater_meta)
  
}


if (nrow(nwis_groundwater) > 35000) {
  
  # Split the data frame into chunks of desired size (here, 35000 rows)

    df_list <- split(nwis_groundwater, 
                   ceiling(seq_len(nrow(nwis_groundwater)) / 35000))

  # Process each chunk and combine results
  nwis_groundwater_meta <- map_dfr(df_list, get_well_meta)
  
} else {
  nwis_groundwater_meta <- get_well_meta(gw_list = nwis_groundwater)
}
 
# Next get aquifer metadata 
aquifer_table <- read_waterdata_metadata("aquifer-codes")
national_table <- read_waterdata_metadata("national-aquifer-codes")
aquifer_types <- read_waterdata_metadata("aquifer-types")
#hucs <- read_waterdata_metadata("hydrologic-unit-codes")

nwis_groundwater_meta <- nwis_groundwater_meta %>%
  dplyr::left_join(., aquifer_table, 
             by = c("aqfr_cd" = "aquifer_code")) %>%
  dplyr::left_join(., national_table,
                   by = c("nat_aqfr_cd" = "national_aquifer_code")) %>%
  dplyr::left_join(aquifer_types, 
                   by = c("aqfr_type_cd" = "aquifer_type"))



# Return updated dataframe with metadata columns
return(nwis_groundwater_meta)

}
