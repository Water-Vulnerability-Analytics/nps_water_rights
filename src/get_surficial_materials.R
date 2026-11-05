#' Read USGS Surficial Geology and Filter by AOI
#'
#' Dataset Citation: Soller, D.R., Reheis, M.C., Garrity, C.P., and Van Sistine, D.R., 2009, Map database for surficial materials in the conterminous United States: U.S. Geological Survey Data Series 425, scale 1:5,000,000 [https://pubs.usgs.gov/ds/425/].
#'
#' @param url The download link to the ZIP file
#' @param aoi An sf object representing the Area of Interest (polygon or pt)
#' @return An sf object containing polygons that intersect the AOI
get_surficial_materials <- function(aoi) {
  
  url <- "https://pubs.usgs.gov/ds/425/USGS_DS_425_SHAPES.zip"
  
  # 1. Create a temporary file and directory
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempdir()
  
  # 2. Download the file
  message("Downloading data...")
  download.file(url, temp_zip, mode = "wb", quiet = TRUE)
  
  # 3. Unzip the file
  unzip(temp_zip, exdir = temp_dir)
  
  # 4. Identify the shapefile (.shp) within the unzipped folder
  # This specific USGS dataset contains several files; we look for the main .shp
  shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  
  # If multiple shp files exist, we target the one likely containing the materials
  # In DS_425, 'surficial_materials.shp' is the primary target
  shp_path <- shp_file[grep("Surficial_materials", shp_file)]
  
  if (length(shp_path) == 0) shp_path <- shp_file[1] # Fallback to first shp found
  
  # 5. Read the shapefile
  message("Reading shapefile...")
  geo_data <- st_read(shp_path, quiet = TRUE)
  
  # 6. Ensure Coordinate Reference Systems (CRS) match
  if (st_crs(geo_data) != st_crs(aoi)) {
    message("Transforming AOI CRS to match data...")
    aoi <- st_transform(aoi, st_crs(geo_data))
  }
  
  # 7. Perform spatial filter (returns polygons overlapping the AOI)
  message("Filtering by AOI...")
  result <- geo_data[aoi, ]
  
  # Clean up: delete temp file
  unlink(temp_zip)
  
  return(result)
}

# test
#test <- get_usgs_geology(aoi = select_system)
#mapview(test, zcol = "UNIT_NAME")
