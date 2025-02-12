## Modification of flex.erase() written by Nelson Stauffer

## clip.arcpy() Clips spatial polygon data frames using ARCPY
# spdf A Spatial Point or Spatial Polygons Data Frame to be clipped
# spdf.clip A Spatial Polygons Data Frame to clip spdf by
# temp.path Optional character string. A pre-existing filepath to use as a temporary folder to write files to. Defaults to a temporary directory from \code{tempdir()}.
# python.search.path Optional character string. The filepath to search for pythonw.exe in. Defaults to "C:/Python27".
clip.arcpy <- function(spdf,
                       spdf.clip,
                       temp.path = NULL,
                       python.search.path = "C:/Python27"
){
  if (!(class(spdf) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame"))) {
    stop("spdf must be a valid Spatial Polygons or Spatial Points Data Frame")
  }
  if (class(spdf.clip) != "SpatialPolygonsDataFrame") {
    stop("spdf.clip must be a valid Spatial Polygons Data Frame")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (is.null(temp.path)) {
    temp.path <- tempdir()
  } else if (!file.exists(temp.path)) {
    stop("If providing a value for temp.path, it must be a valid, pre-existing filepath.")
  }
  
  # Conform the clipping frame to the SPDF to be clipped
  if (spdf@proj4string@projargs != spdf.clip@proj4string@projargs) {
    spdf.clip <- sp::spTransform(spdf.clip, CRSobj = spdf@proj4string)
  }
  
  ## Write out the two current frames
  rgdal::writeOGR(obj = spdf,
                  dsn = temp.path,
                  layer = "inshape",
                  driver = "ESRI Shapefile",
                  overwrite_layer = TRUE)
  rgdal::writeOGR(obj = spdf.clip,
                  dsn = temp.directory,
                  layer = "clipshape",
                  driver = "ESRI Shapefile",
                  overwrite_layer = TRUE)
  
  ## Construct a quick python script to clip spdf by spdf.clip
  arcpy.script <- c("import arcpy",
                    "from arcpy import env",
                    paste0("env.workspace = '", temp.path, "'"),
                    "in_features = 'inshape.shp'",
                    "clip_features = 'clipshape.shp'",
                    "out_feature_class = 'clipresults.shp'",
                    "xy_tolerance = ''",
                    "arcpy.Clip_analysis(in_features, clip_features, out_feature_class,xy_tolerance)"
  )

  
  ## Write the constructed script out
  cat(arcpy.script, file = paste0(temp.path, "/clip.py"), sep = "\n", append = FALSE)
  
  ## Find the local machine's copy of pythonw.exe in python.search.path. There are no failsafes for if this isn't where to find it
  python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
  if (length(python.path) < 1) {
    stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
  } else {
    python.path <- python.path[1]
  }
  
  ## Execute the Python script
  system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/clip.py"), pattern = "/", replacement = "\\\\")))
  
  ## Read in the results and rename the attributes because rgdal::writeOGR() almost certainly truncated them
  clip.results <- rgdal::readOGR(dsn = temp.directory, layer = "clipresults", stringsAsFactors = FALSE)
  names(clip.results@data) <- names(spdf@data)
  
  ## Make sure that the results are in the same projection as the original source SPDF
  if (clip.results@proj4string@projargs != spdf@proj4string@projargs) {
    output <- spTransform(clip.results, CRSobj = spdf@proj4string)
  } else {
    output <- clip.results
  }
  
  return(output)
}