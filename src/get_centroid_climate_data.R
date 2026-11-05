get_centroid_climate_data <- function(park,
                                      save = FALSE,
                                      path = "data/park") {
  
  
  # This file has been changed....
  # call <-
  #   "https://parkfutures.s3.us-west-2.amazonaws.com/maca-tprh-data/0Climate_Zones_by_Unit_cleaned.csv"
  # 
  
  # function to get all park folder names
  extract_folders <- function(html_path, park) {
    # Read the HTML file
    html_content <- read_html(html_path)
    
    # Extract all folder names
    folder_elements <- html_content %>% 
      html_nodes(".folder, a") %>%
      html_text()
    
    # Filter folder names containing "YELL"
    folders <- folder_elements[str_detect(folder_elements, park)]
    
    # Return unique matches
    return(unique(folders))
  }
  
  if (save == FALSE) {
    if (!dir.exists("temp_data/")) {
      dir.create("temp_data")
    }
    
    if (!dir.exists("temp_data/files")) {
      dir.create("temp_data/files")
    }
    

    park_folders <- extract_folders("https://parkfutures.s3.us-west-2.amazonaws.com/maca-tprh-data/index.html", park)
    
    
    walk(park_folders, ~ download.file(
      paste0(
        "https://parkfutures.s3.us-west-2.amazonaws.com/maca-tprh-data/",
        .x
      ),
      destfile = paste0("temp_data/files/", .x)
    )
    
    )
    
    # read files into environment
    walk(list.files("temp_data/files", full.names = TRUE, recursive = TRUE), function(x) {
                     tmp <- read_csv(x)
                     # hacky way to pull filename to assign to env object
                     name <-  str_sub(x, 17, -5)
                     assign(name, tmp, envir = .GlobalEnv)
                   })
    
    unlink("temp_data/", recursive = TRUE)
    
  } else {
    if (!dir.exists(paste0(getwd(), "/templeod_dataall/"))) {
      dir.create(file.path(getwd(), "/templeod_dataall/"), showWarnings = FALSE)
    }
    
    if (!dir.exists(paste0(getwd(), "/", path, "/", park))) {
      dir.create(file.path(paste0(getwd(), "/", path, "/", park)), showWarnings = FALSE)
    }
    
    if (!dir.exists(paste0(getwd(), "/", path, "/", park, "/centroid/"))) {
      dir.create(file.path(paste0(
        getwd(), "/", path, "/", park, "/centroid/"
      )), showWarnings = FALSE)
    }
    
    if (!dir.exists(paste0(getwd(), "/", path, "/", park, "/centroid/climate/"))) {
      dir.create(file.path(
        paste0(getwd(), "/", path, "/", park, "/centroid/climate/")
      ), showWarnings = FALSE)
    }

    park_folders <- extract_folders("https://parkfutures.s3.us-west-2.amazonaws.com/maca-tprh-data/index.html", park)
    
    
    walk(park_folders, ~ download.file(
      paste0(
        "https://parkfutures.s3.us-west-2.amazonaws.com/maca-tprh-data/",
        .x
      ),
      destfile = paste0(path, "/", park, "/centroid/climate/", .x)
    )
    
  )

    
    unlink("templeod_dataall", recursive = TRUE)
  }
  
}
