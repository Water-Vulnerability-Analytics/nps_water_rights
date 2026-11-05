#' Delineate a watershed from a DEM using Whitebox
#'
#' Breaches depressions, computes D8 flow direction/accumulation, and
#' delineates the watershed upstream of a pour point using a DEM. 
#' The pour point is snapped interactively by default: flow
#' accumulation is cropped to a buffer around the pour point, plotted, and
#' the user clicks the cell to snap to (the automated `wbt` snap tools
#' don't perform well on high-resolution DEMs).
#'
#' @param dem A SpatRaster DEM. Projected to `crs_out` before flow routing
#'   if not already in that CRS.
#' @param pour_point An sf or SpatVector point representing the outlet to
#'   delineate the watershed.
#' @param output_dir Directory to write intermediate and final files to.
#' @param file_prefix Prefix used for all files written to `output_dir`
#'   (e.g. "paradise", "paradise_outlet").
#' @param breach_dist Max search distance (grid cells) passed to
#'   `wbt_breach_depressions_least_cost()`. The algorithm will search 
#'   outward from a depression/pit to find the lowest-cost 
#'   path to breach a channel through it. Default 10.
#' @param snap_buffer_dist Radius (in the DEM's projected units, e.g.
#'   meters for EPSG:5070) of the flow-accumulation crop shown for
#'   snapping. Default 50.
#' @param interactive_snap If TRUE (default), plot flow accumulation
#'   around the pour point and prompt a click to snap to. If FALSE, the
#'   pour point is used as provided with no snapping.
#' @param write_gpkg If TRUE (default), also save the watershed polygon as
#'   a GeoPackage.
#' @param overwrite Overwrite existing intermediate/output files. Default
#'   TRUE.
#' @param crs_out CRS to project the returned watershed polygon to.
#'   Default `"EPSG:5070"` (CONUS Albers).
#' @return An sf polygon of the delineated watershed, projected to
#'   `crs_out`.

delineate_watershed <- function(dem,
                                pour_point,
                                output_dir,
                                file_prefix,
                                breach_dist = 10,
                                snap_buffer_dist = 50,
                                interactive_snap = TRUE,
                                write_gpkg = TRUE,
                                overwrite = TRUE,
                                crs_out = "EPSG:5070") {
  

  # Breaching/flow routing needs a projected CRS, so project the DEM up front
  # (skip if it's already there to avoid needless resampling)
  if (!terra::same.crs(dem, crs_out)) {
    dem <- terra::project(dem, crs_out)
  }

  dem_path <- file.path(output_dir, paste0(file_prefix, "_dem.tif"))
  breached_path <- file.path(output_dir, paste0(file_prefix, "_breached.tif"))
  flow_dir_path <- file.path(output_dir, paste0(file_prefix, "_flow_dir.tif"))
  flow_accum_path <- file.path(output_dir, paste0(file_prefix, "_flow_accum.tif"))
  pour_pt_path <- file.path(output_dir, paste0(file_prefix, "_pour_pt.shp"))
  snapped_pt_path <- file.path(output_dir, paste0(file_prefix, "_snapped_pour_point.shp"))
  watershed_path <- file.path(output_dir, paste0(file_prefix, "_watershed.tif"))

  # whitebox is a CLI wrapper, so it needs files on disk rather than R objects
  terra::writeRaster(dem, dem_path, overwrite = overwrite)

  # 1. Breach depressions (fill)
  whitebox::wbt_breach_depressions_least_cost(dem_path, breached_path, dist = breach_dist)

  # 2. Flow direction & accumulation
  whitebox::wbt_d8_pointer(breached_path, flow_dir_path)
  whitebox::wbt_d8_flow_accumulation(breached_path, flow_accum_path)

  # 3. Prep pour point
  pour_point_v <- terra::vect(pour_point) %>%
    terra::project(., terra::crs(dem))

  terra::writeVector(pour_point_v, pour_pt_path, overwrite = overwrite)

  # Buffer around the pour point and then crop flow accumulation to that buffer
  # The snapping tool for pour points is often wonky so it's easier to do
  # manually at times
  flow_accum <- terra::rast(flow_accum_path)
  buf <- terra::buffer(pour_point_v, width = snap_buffer_dist)
  flow_accum_clip <- terra::crop(flow_accum, buf)

  if (interactive_snap) {
    # Automated snap tools don't perform well on high-res DEMs, so click
    # the pour point cell manually along the flow accumulation lines.
    # click() needs a real OS window - RStudio's Plots pane doesn't
    # reliably capture mouse events for it.
    grDevices::quartz()
    terra::plot(flow_accum_clip)
    terra::plot(pour_point_v, add = TRUE)

    message("Click the pour point location in the Quartz plot window now.")
    pt_click <- terra::click(flow_accum_clip, n = 1, xy = TRUE)
    grDevices::dev.off()
    snapped_point <- terra::vect(pt_click, geom = c("x", "y"), crs = terra::crs(dem))
  } else {
    snapped_point <- pour_point_v
  }

  terra::writeVector(snapped_point, snapped_pt_path, overwrite = overwrite)

  # 4. Delineate watershed
  whitebox::wbt_watershed(flow_dir_path, snapped_pt_path, watershed_path)

  watershed <- terra::rast(watershed_path) %>%
    terra::as.polygons(dissolve = TRUE) %>%
    sf::st_as_sf() %>%
    sf::st_transform(crs_out)

  if (write_gpkg) {
    sf::st_write(watershed, file.path(output_dir, paste0(file_prefix, "_watershed.gpkg")),
                 delete_dsn = overwrite, quiet = TRUE)
  }

  watershed
}
