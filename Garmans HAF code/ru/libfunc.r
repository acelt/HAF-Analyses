## 11/14/2017
##  Functions for ru.r
##########################################################
#### GLOBAL VARIABLES ####
##########################################################
library(raster)
library(tidyverse)
library(stringr)
library(xlsx)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(arcgisbinding)
library(digest)
library(spsurvey)
arc.check_product()




################## Key functions used in MAIN 
## A function that evaluates a parsed text string like the ones in $eval.string.upper and lower. Used in a lapply() later. Probably replaceable with parse() %>% eval() there though
safe.parser <- function(string){
  output <- safely(eval(parse(text = string)))
  return(output[[1]])
}

## For those strange occasions when the safe version gives you errors?
parser <- function(string){
  output <- eval(parse(text = string))
  return(output)
}

## A function to make sure that input strings are correctly formatted for filepaths, .gdb filenames, .xlsx filenames, .csv filenames, and .shp filenames
sanitizer <- function(string, type){
  switch(type,
         filepath = {
           if (!grepl(x = string, pattern = "/$") & !grepl(x = string, pattern = "\\\\$")) {
             string <- paste0(string, "/")
           }
         },
         gdb = {
           if (!grepl(x = string, pattern = "\\.[Gg][Dd][Bb]$")) {
             string <- paste0(string, ".gdb")
           }
         },
         xlsx = {
           if (!grepl(x = string, pattern = "\\.[Xx][Ll][Ss][Xx]$")) {
             string <- paste0(string, ".xlsx")
           }
         },
         csv = {
           if (!grepl(x = string, pattern = "\\.[Cc][Ss][Vv]$")) {
             string <- paste0(string, ".csv")
           }
         },
         shp = {
           if (!grepl(x = string, pattern = "\\.[Ss][Hh][Pp]$")) {
             string <- paste0(string, ".shp")
           }
         }
  )
  return(string)
}

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

## THIS version works only to dissolve a reporting unit file; dissolves on RU

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
                             "arcpy.Dissolve_management(in_features, out_feature_class,[\"RU\"])"
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


        ## We seem to truncate attributes so the following doesn't function properly   names(out.results@data) <- names(spdf@data)
  
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
## Adds areas in hectares and/or square kilometers, by polygon ID
area.add <- function(spdf, ## SpatialPolygonsDataFrame to add area values to
                     area.ha = T, ## Add area in hectares?
                     area.sqkm = T, ## Add area in square kilometers?
                     byid = T ## Do it for the whole SPDF or on a per-polygon basis? Generally don't want to toggle this
                     ){
  ## Make sure the SPDF is in Albers equal area projection
  spdf <- spTransform(x = spdf, CRSobj = CRS("+proj=aea"))
  
  ## Add the area in hectares, stripping the IDs from gArea() output
  spdf@data$AREA.HA <- gArea(spdf, byid = byid) * 0.0001 %>% unname()
  ## Add the area in square kilometers, converting from hectares
  spdf@data$AREA.SQKM <- spdf@data$AREA.HA * 0.01
  
  ## Remove the areas that weren't requested. It's more straightforward and computationally cheaper to do it this way than run gArea() more than once
  if (!(area.ha)) {
    spdf@data$AREA.HA <- NULL
  }
  if (!(area.sqkm)) {
    spdf@data$AREA.SQKM <- NULL
  }
  return(spdf)
}

## CleanUp() - clean-up reporting units 

CleanUp<-function(cleanup)
{
    projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83

    for(nam in cleanup) {
  
       ## Creates an SPDF 
    #     assign(x = "clean",
    #     value = nam %>% arc.open() %>% arc.select %>%
    #     SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .)%>%spTransform(projection)) 

       clean<-readOGR(dsn=getwd(),layer = nam,stringsAsFactors=FALSE)    
       if(nrow(clean)>1)clean<-flex.dissolve(clean,method="arcpy",temp.path=getwd())

       if(is.null(clean$TERRA))clean$TERRA<-"NA"

       temp<-clean[, c("TERRA","RU")]
       temp<- area.add(spdf = temp,T,T)
       temp<-spTransform(temp,projection)
       rgdal::writeOGR(obj =temp, dsn = getwd(), layer = nam, driver = "ESRI Shapefile", overwrite_layer = TRUE)
    }
    return(1)		## For fun
}

## Clip() - clip habitat by specified frame.
         ##Clip(AOI, lmfstrata, the 2 clip lists, index of clip lists to use, list of RUs and ru codes, begin & end sequence of clip.RU and ru that will be created)
         ## E.g., Clip(habitat,lmfstrata,clip.path.clip.layer,1,clip.RU,ru,1,2,framename) - use the first SDD in clip.path, clip habitat by that frame, then
		  								  ## clip to LMF I and then LMF II to create r1ru.shp, r2ru.shp which
										  ## will be attributed as RU1 and RU2.  Return sum area of all
										  ## generated reporting units.
										  ## framename is either NA to use everything, OR the name of the
										  ## sample frame ID to use.
				

Clip<-function(habitat,lmfstrata,clip.path,clip.layer,clip.code,clip.RU,ru,begin,end,framename)
{
         projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
         totalarea<-0

         if(clip.code==0) {								
            RUCLIP<-habitat
            a<-RUCLIP            
         }else {
         	s<-clip.path[clip.code]
         	layer<-clip.layer[clip.code]

         	frame<- readOGR(dsn=s,layer = layer,stringsAsFactors=FALSE)    
                if(!is.na(framename)) {
                   frame<-frame[frame$TERRA_SAMPLE_FRAME_ID %in% framename ,]                        
                }
         	RUCLIP<-flex.clip(frame,habitat,method="arcpy",temp.path=getwd())		## clip frame by habitat
         	RUCLIP<-spTransform(RUCLIP,projection)  
         	a<-RUCLIP

  	 }									## In case lmfstrata==NULL

         if(!is.null(lmfstrata)) {
         	lmf<-lmfstrata[lmfstrata$STRATUM==1 ,]	## TYPE I LMF area
         	a<-flex.clip(lmf,RUCLIP,method="arcpy",temp.path=getwd())		## Clip to TYPE I
    	 	a <- area.add(spdf = a,T,T)
         	totalarea<-totalarea+sum(a$AREA.HA)
    	 	a<-spTransform(a,projection)  
         	index<-begin
    	 	a$RU<-ru[index]
         	rgdal::writeOGR(obj =a, dsn = getwd(), layer = clip.RU[index], driver = "ESRI Shapefile", overwrite_layer = TRUE)
    
         	lmf<-lmfstrata[lmfstrata$STRATUM==2 ,]	## TYPE 2 LMF area
         	a<-flex.clip(lmf,RUCLIP,method="arcpy",temp.path=getwd())
    	 	a <- area.add(spdf = a,T,T)
         	totalarea<-totalarea+sum(a$AREA.HA)
    	 	a<-spTransform(a,projection)  
         	index<-end
    	 	a$RU<-ru[index]
         	rgdal::writeOGR(obj =a, dsn = getwd(), layer = clip.RU[index], driver = "ESRI Shapefile", overwrite_layer = TRUE)
	  }else {
                a <- area.add(spdf = a,T,T)			## THIS is frame clipped by habitat only (RUCLIP)
         	totalarea<-totalarea+sum(a$AREA.HA)
    	 	a<-spTransform(a,projection)  
         	index<-begin
    	 	a$RU<-ru[index]
         	rgdal::writeOGR(obj =a, dsn = getwd(), layer = clip.RU[index], driver = "ESRI Shapefile", overwrite_layer = TRUE)
          }
          return(totalarea)
}## Erase() - erase reporting units from the AOI

Erase<-function(habitat,erase,begin,end)
{
## This is where we erase previous reporting units to derive reporting units that fall outside of
##      the project areas - when habitat is larger than the aggregated project areas.

    projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83

    RU.spdf<-habitat		## Original habitat
    for(i in begin:end) {
       nam<-erase[i]
       ## Creates an SPDF 
       assign(x = "erase.spdf",
       value = nam %>% arc.open() %>% arc.select %>%
       SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .)%>%spTransform(projection))

       RU.spdf<-flex.erase(RU.spdf,erase.spdf,method="arcpy",temp.path=getwd())
       RU.spdf<-spTransform(RU.spdf,projection)  
    }
    return(RU.spdf)
}

## IngestLMF() - create the LMF strata composite

IngestLMF<-function(lmf.src,data.src) 
{

############# Ingest and concatenate all of the LMF strata necessary for the AOI. THESE MUST BE already clipped to SMA
           projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
           if(!is.null(lmf.src)) {
           	lmfstrata<-SpatialPolygonsDataFrame
           	index<-0
          	 ## Looped so that it can execute across all the lmfs in the vector (if there are more than one)
          	 for (s in lmf.src) {
             		index<-index+1

             		lmf<- readOGR(dsn=data.src[index],
                                layer = paste(s,"_strataSMA",sep="") ,
                                stringsAsFactors = F) ## The [[]] is to get the SPDF (or NULL) out of the list returned by the safely()
             		# The spTransform() is just to be safe
             		if (!is.null(lmf)) {
              	 		lmf <- spTransform(lmf, projection)
               	 		names(lmf@data) <- str_to_upper(names(lmf@data)) 
    		 		a<- lmf[, c("STRATUM")]
                		if(index==1) {
                    			lmfstrata<-a
                		}else {
                    			lmfstrata<-rbind(lmfstrata,a)
                		}
			}
             	 }
	    }
            return(lmfstrata)
}
## ReadShapefile - ingest a shapefile assuming it resides in getwd()

ReadShapefile<-function(theshape)
{

      	 projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83

         ## Creates an SPDF 
         assign(x = "shape",
         value = theshape %>% arc.open() %>% arc.select %>%
         SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .))
	 shape<-spTransform(shape,projection)
         return(shape)
}

## SaveShapefile - save a shapefile

## SaveShapefile(the spdf, clip.RU, ru, filename entry [clip.RU], RU name entry [ru])

SaveShapefile<-function(a,clip.RU,ru,id1,id2)
{

   	projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
        totalarea<-0

              a <- area.add(spdf = a,T,T)			##  
         	totalarea<-totalarea+sum(a$AREA.HA)
    	 	a<-spTransform(a,projection)  
                a$RU<-ru[id2]
         	rgdal::writeOGR(obj =a, dsn = getwd(), layer = clip.RU[id1], driver = "ESRI Shapefile", overwrite_layer = TRUE)
	  return(totalarea)
}