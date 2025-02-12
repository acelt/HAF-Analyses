## Modification of flex.erase() written by Nelson Stauffer

## flex.clip() - clips spatial polygon data frames using ARCPY.  see flex.erase() to erase spatial polygon data frames.


flex.clip <- function(spdf,
                       spdf.clip,
                       method = "arcpy",
                       temp.path = "",
                       python.search.path = "C:/Python27"
){
  if (class(spdf) != "SpatialPolygonsDataFrame") {
    stop("spdf must be a valid Spatial Polygons Data Frame")
  }
  if (class(spdf.clip) != "SpatialPolygonsDataFrame") {
    stop("spdf.clip) must be a valid Spatial Polygons Data Frame")
  }
  if (!(stringr::str_to_upper(method) %in% c("ARCPY"))) {
    stop("method must be 'arcpy'.")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (!file.exists(temp.path)) {
    stop("temp.path must be a valid, pre-existing filepath.")
  }

  if (spdf@proj4string@projargs != spdf.clip@proj4string@projargs) {
    spdf.clip <- sp::spTransform(spdf.clip, CRSobj = spdf@proj4string)
  }
           ## Create a temp directory
           temp.directory <- paste0(temp.path, "/arcpy_temp")
           dir.create(temp.directory, showWarnings = FALSE)

           ## Write out the two current frames
           rgdal::writeOGR(obj = spdf, dsn = temp.directory, layer = "inshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)
           rgdal::writeOGR(obj = spdf.clip, dsn = temp.directory, layer = "clipshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)

           ## Construct a quick python script to clip spdf by spdf.clip
           arcpy.script <- c("import arcpy",
                             "from arcpy import env",
                             paste0("env.workspace = '", temp.directory, "'"),
                             "in_features = 'inshape.shp'",
                             "clip_features = 'clipshape.shp'",
                             "out_feature_class = 'clipresults.shp'",
                             "xy_tolerance = ''",
                             "arcpy.Clip_analysis(in_features, clip_features, out_feature_class,xy_tolerance)"
           )
           ## Write the constructed script out
           cat(arcpy.script, file = paste0(temp.directory, "/clip.py"), sep = "\n", append = F)

           ## Find the local machine's copy of pythonw.exe in C:/Python27. There are no failsafes for if this isn't where to find it
           python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
           if (length(python.path) < 1) {
             stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
           } else {
             python.path <- python.path[1]
           }

           ## Execute the Python script
           system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/clip.py"), pattern = "/", replacement = "\\\\")))

           ## Read in the results and rename the attributes because rgdal::writeOGR() truncated them
           clip.results <- rgdal::readOGR(dsn = temp.directory, layer = "clipresults", stringsAsFactors = FALSE)
           names(clip.results@data) <- names(spdf@data)

           if (clip.results@proj4string@projargs != spdf@proj4string@projargs) {
             output <- spTransform(clip.results, CRSobj = spdf@proj4string)
           } else {
             output <- clip.results
           }
           ## Remove the temp folder and files
           if (grepl(method, pattern = "arcpy", ignore.case = TRUE)) {
             system(paste("cmd /c rmdir", stringr::str_replace_all(temp.directory, pattern = "/", replacement = "\\\\"), "/s /q"))
           }

  return(output)
}


#####################################################################  Clip pts to polygon

## flex.clip.pts() - clips spatial point data frames using ARCPY.  
flex.clip.pts <- function(spdf,
                       spdf.clip,
                       method = "arcpy",
                       temp.path = "",
                       python.search.path = "C:/Python27"
){
  if (class(spdf) != "SpatialPointsDataFrame") {    
    stop("spdf must be a valid Spatial Points Data Frame")
  }
  if (class(spdf.clip) != "SpatialPolygonsDataFrame") {
    stop("spdf.clip) must be a valid Spatial Polygons Data Frame")
  }
  if (!(stringr::str_to_upper(method) %in% c("ARCPY"))) {
    stop("method must be 'arcpy'.")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (!file.exists(temp.path)) {
    stop("temp.path must be a valid, pre-existing filepath.")
  }

  if (spdf@proj4string@projargs != spdf.clip@proj4string@projargs) {
    spdf.clip <- sp::spTransform(spdf.clip, CRSobj = spdf@proj4string)
  }
           ## Create a temp directory
           temp.directory <- paste0(temp.path, "/arcpy_temp")
           dir.create(temp.directory, showWarnings = FALSE)

           ## Write out the two current frames
           rgdal::writeOGR(obj = spdf, dsn = temp.directory, layer = "inshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)
           rgdal::writeOGR(obj = spdf.clip, dsn = temp.directory, layer = "clipshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)

           ## Construct a quick python script to clip spdf by spdf.clip
           arcpy.script <- c("import arcpy",
                             "from arcpy import env",
                             paste0("env.workspace = '", temp.directory, "'"),
                             "in_features = 'inshape.shp'",
                             "clip_features = 'clipshape.shp'",
                             "out_feature_class = 'clipresults.shp'",
                             "xy_tolerance = ''",
                             "arcpy.Clip_analysis(in_features, clip_features, out_feature_class,xy_tolerance)"
           )
           ## Write the constructed script out
           cat(arcpy.script, file = paste0(temp.directory, "/clip.py"), sep = "\n", append = F)

           ## Find the local machine's copy of pythonw.exe in C:/Python27. There are no failsafes for if this isn't where to find it
           python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
           if (length(python.path) < 1) {
             stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
           } else {
             python.path <- python.path[1]
           }

           ## Execute the Python script
           system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/clip.py"), pattern = "/", replacement = "\\\\")))

           ## Read in the results and rename the attributes because rgdal::writeOGR() truncated them
           clip.results <- rgdal::readOGR(dsn = temp.directory, layer = "clipresults", stringsAsFactors = FALSE)
           names(clip.results@data) <- names(spdf@data)

           if (clip.results@proj4string@projargs != spdf@proj4string@projargs) {
             output <- spTransform(clip.results, CRSobj = spdf@proj4string)
           } else {
             output <- clip.results
           }
           ## Remove the temp folder and files
           if (grepl(method, pattern = "arcpy", ignore.case = TRUE)) {
             system(paste("cmd /c rmdir", stringr::str_replace_all(temp.directory, pattern = "/", replacement = "\\\\"), "/s /q"))
           }

  return(output)
}

##################################################################################################
## Written by Nelson Stauffer

#' Erase a Spatial Polygons Data Frame from another, either with rgeos or ArcPy
#' @description Spatial manipulations in R can get, for lack of a better term, squirrelly. Even with multiple failsafes in place to try to compensate for sliver geometry, \code{rgeos::gDifference()} still isn't as robust as one might hope. This wrapper removes the geometry of one SPDF from another via a system call to \code{Python} using the library \code{ArcPy} or via \code{rgeos::gDifference()}.
#' @param spdf A Spatial Polygons Data Frame to remove FROM.
#' @param spdf.erase  A Spatial Polygons Data Frame to remove geometry from \code{spdf} WITH.
#' @param method Character string. This must either be \code{"arcpy"} or \code{"rgeos"} and determines which approach will be used to frames from one another if \code{combine} is \code{TRUE}. If \code{"arcpy"} is used, then R must have write permissions to the folder \code{temp.path} and a valid install of ArcPy. This is preferable to \code{"rgeos"} because the functions involved tend to crash at random when handling very small remainder geometries. Case insensitive. Defaults to \code{"arcpy"}.
#' @param temp.path Optional character string. If \code{erase} is \code{"arcpy"} this must be the path to a folder that R has write permissions to so that a subfolder called arcpy_temp can be created and used for ArcPy erasure steps.
#' @param python.search.path Character string. The filepath for the folder containing \code{pythonw.exe}. Defaults to \code{"C:/Python27"}.
#' @param sliverdrop Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_dropSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverwarn Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_warnSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverdrop Optional numeric value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_polyThreshold()} to temporarily set the environment during the erasure attempt. Defaults to \code{0.01}.
#' @return The remaining geometry and data in \code{spdf} after \code{spdf.erase} has been removed from it.
#' @export



flex.erase <- function(spdf,
                       spdf.erase,
                       method = "arcpy",
                       temp.path = "",
                       python.search.path = "C:/Python27",
                       sliverdrop = T,
                       sliverwarn = T,
                       sliverthreshold = 0.01
){
  if (class(spdf) != "SpatialPolygonsDataFrame") {
    stop("spdf must be a valid Spatial Polygons Data Frame")
  }
  if (class(spdf.erase) != "SpatialPolygonsDataFrame") {
    stop("spdf.erase must be a valid Spatial Polygons Data Frame")
  }
  if (!(stringr::str_to_upper(method) %in% c("ARCPY", "RGEOS"))) {
    stop("method must be either 'arcpy' or 'rgeos'.")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (!file.exists(temp.path)) {
    stop("temp.path must be a valid, pre-existing filepath.")
  }

  if (spdf@proj4string@projargs != spdf.erase@proj4string@projargs) {
    spdf.erase <- sp::spTransform(spdf.erase, CRSobj = spdf@proj4string)
  }
  switch(stringr::str_to_upper(method),
         "ARCPY" = {
           ## Create a temp directory
           temp.directory <- paste0(temp.path, "/arcpy_temp")
           dir.create(temp.directory, showWarnings = FALSE)

           ## Write out the two current frames
           rgdal::writeOGR(obj = spdf, dsn = temp.directory, layer = "inshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)
           rgdal::writeOGR(obj = spdf.erase, dsn = temp.directory, layer = "eraseshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)

           ## Construct a quick python script to erase frame.spdf from frame.spdf.temp
           arcpy.script <- c("import arcpy",
                             "from arcpy import env",
                             paste0("env.workspace = '", temp.directory, "'"),
                             "in_features = 'inshape.shp'",
                             "erase_features = 'eraseshape.shp'",
                             "out_feature_class = 'eraseresults.shp'",
                             "xy_tolerance = ''",
                             "arcpy.Erase_analysis(in_features, erase_features, out_feature_class)"
           )
           ## Write the constructed script out
           cat(arcpy.script, file = paste0(temp.directory, "/erase.py"), sep = "\n", append = F)

           ## Find the local machine's copy of pythonw.exe in C:/Python27. There are no failsafes for if this isn't where to find it
           python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
           if (length(python.path) < 1) {
             stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
           } else {
             python.path <- python.path[1]
           }

           ## Execute the Python script
           system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/erase.py"), pattern = "/", replacement = "\\\\")))

           ## Read in the results and rename the attributes because rgdal::writeOGR() truncated them
           erase.results <- rgdal::readOGR(dsn = temp.directory, layer = "eraseresults", stringsAsFactors = FALSE)
           names(erase.results@data) <- names(spdf@data)    ## double check the use of names(spdf...)

           if (erase.results@proj4string@projargs != spdf@proj4string@projargs) {
             output <- spTransform(erase.results, CRSobj = spdf@proj4string)
           } else {
             output <- erase.results
           }
           ## Remove the temp folder and files
           if (grepl(method, pattern = "arcpy", ignore.case = TRUE)) {
             system(paste("cmd /c rmdir", stringr::str_replace_all(temp.directory, pattern = "/", replacement = "\\\\"), "/s /q"))
           }
         }, "RGEOS" = {
           ## This lets rgeos deal with tiny fragments of polygons without crashing
           ## This and the following tryCatch() may be unnecessary since the argument drop_lower_td = TRUE was added, but it works so I'm leaving it
           current.drop <- rgeos::get_RGEOS_dropSlivers()
           current.warn <- rgeos::get_RGEOS_warnSlivers()
           current.tol <- rgeos::get_RGEOS_polyThreshold()

           rgeos::set_RGEOS_dropSlivers(sliverdrop)
           rgeos::set_RGEOS_warnSlivers(sliverwarn)
           rgeos::set_RGEOS_polyThreshold(sliverthreshold)
           message(paste0("Attempting using rgeos::set_RGEOS_dropslivers(", sliverdrop, ") and rgeos::set_RGEOS_warnslivers(", sliverwarn, ") and set_REGOS_polyThreshold(", sliverthreshold, ")"))
           ## Making this Albers for right now for gBuffer()
           ## The gbuffer() is a common hack to deal with ring self-intersections, which it seems to do just fine here?
           sp.temp <- rgeos::gDifference(spgeom1 = rgeos::gBuffer(sp::spTransform(spdf, CRS("+proj=aea")),
                                                                  byid = TRUE,
                                                                  width = 0.1),
                                         			  spgeom2 = rgeos::gBuffer(sp::spTransform(spdf.erase,
                                                                  CRS("+proj=aea")),
                                                                  byid = TRUE,
                                                                  width = 0.1),
                                         			  drop_lower_td = TRUE)
           if (!is.null(frame.sp.temp)) {
             output <- sp::spTransform(sp::SpatialPolygonsDataFrame(sp.temp,
                                       data = spdf@data[1:length(sp.temp@polygons),]),
                                       CRSobj = spdf@proj4string)
           } else {
             output <- NULL
           }

           rgeos::set_RGEOS_dropSlivers(current.drop)
           rgeos::set_RGEOS_warnSlivers(current.warn)
           rgeos::set_RGEOS_polyThreshold(current.tol)

         }
  )
  return(output)
}


################################################################################################
## Modification of flex.erase() written by Nelson Stauffer

## flex.dissolve() - dissolve spatial polygon data frames using ARCPY.  Fixing slivers sometimes creates an additional stratum,ru,inference combination.
##                   This function ensures a minimimal polygonal version of the final map, which then can be populated with aerial fields.

## THIS version works only to dissolve a stratum file; i.e.,  after dissolving, STRATUM field name is reset to DMNNT_STRTM

flex.dissolve <- function(spdf,
                       method = "arcpy",
                       temp.path = "",
                       python.search.path = "C:/Python27"
){
  if (class(spdf) != "SpatialPolygonsDataFrame") {
    stop("spdf must be a valid Spatial Polygons Data Frame")
  }

  if (!(stringr::str_to_upper(method) %in% c("ARCPY"))) {
    stop("method must be 'arcpy'.")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (!file.exists(temp.path)) {
    stop("temp.path must be a valid, pre-existing filepath.")
  }
  
  spdf$SHAPE_LENG<-NULL
  spdf$SHAPE_AREA<-NULL

           ## Create a temp directory
           temp.directory <- paste0(temp.path, "/arcpy_temp")
           dir.create(temp.directory, showWarnings = FALSE)

           ## Write out the two current frames
           rgdal::writeOGR(obj = spdf, dsn = temp.directory, layer = "inshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)



           ## Dissolve_management (in_features, out_feature_class, {dissolve_field}, {statistics_fields}, {multi_part}, {unsplit_lines})


           ## Construct a quick python script to dissolve spdf
                             
##  For future reference  "arcpy.Dissolve_management(in_features, out_feature_class,[\"DMNNT_S\", \"RU\",\"infernc\"])"

           arcpy.script <- c("import arcpy",
                             "from arcpy import env",
                             paste0("env.workspace = '", temp.directory, "'"),
                             "in_features = 'inshape.shp'",
                             "out_feature_class = 'outshape.shp'",
                             "arcpy.Dissolve_management(in_features, out_feature_class,[\"DMNNT_S\"])"
           )
           ## Write the constructed script out
           cat(arcpy.script, file = paste0(temp.directory, "/dissolve.py"), sep = "\n", append = F)
        
           ##system("copy arcpy_temp\dissolve.py+dend.txt arcpy_temp\dissolve.py")           
           ##system("dend.bat")

           ## Find the local machine's copy of pythonw.exe in C:/Python27. There are no failsafes for if this isn't where to find it
           python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
           if (length(python.path) < 1) {
             stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
           } else {
             python.path <- python.path[1]
           }

           ## Execute the Python script
           system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/dissolve.py"), pattern = "/", replacement = "\\\\")))

           ## Read in the results and rename the attributes because rgdal::writeOGR() truncated them
           out.results <- rgdal::readOGR(dsn = temp.directory, layer = "outshape", stringsAsFactors = FALSE)
  spdf$SHAPE_LENG<-NULL
  spdf$SHAPE_AREA<-NULL
  spdf$OBJECTID<-NULL

        ## We seem to truncate attributes so the following doesn't function properly   names(out.results@data) <- names(spdf@data)
  
           ## Here is a good place to reset the stratum name
            names(out.results)[names(out.results) == "DMNNT_S"] <- "DMNNT_STRTM"

           if (out.results@proj4string@projargs != spdf@proj4string@projargs) {
             output <- spTransform(out.results, CRSobj = spdf@proj4string)
           } else {
             output <- out.results
           }
           ## Remove the temp folder and files
           if (grepl(method, pattern = "arcpy", ignore.case = TRUE)) {
             system(paste("cmd /c rmdir", stringr::str_replace_all(temp.directory, pattern = "/", replacement = "\\\\"), "/s /q"))
           }

  return(output)
}
