## Shapefile attribute extraction function where the shapefile attribute table contains the values to assign
attribute.shapefile <- function(shape1,
                                # data.path = "", ## If the shape is in a .gdb feature class then this should be the full path, including the file extension .gdb. If the SPDF is already made, do not specify this argument
                                shape2, ## The name of the shapefile or feature class !!!OR!!! an SPDF
                                attributefield = "", ## Name of the field in the shape that specifies the attribute to assign to the points
                                newfield = "Evaluation.Stratum", ## Name of the new field in the output to assign the values from attributefield to
                                projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"),
                                sliverwarn = F,
                                sliverdrop = F,
                                sliverthreshold = 0.01
                                ){
  set_RGEOS_dropSlivers(sliverdrop)
  set_RGEOS_warnSlivers(sliverwarn)
  set_RGEOS_polyThreshold(sliverthreshold)
  ## Strip the file extension from shape, just in case it was there
  # if (is.character(shape2)) {
  #   shape2 <- str_replace(shape2, pattern = "\\.[Ss][Hh][Pp]$", replacement = "")
  # }
  # ## If this is coming from a geodatabase, extract the shapefile appropriately. Otherwise read in the .shp
  # if (grepl(x = data.path, pattern = "\\.[Gg][Dd][Bb]$")) {
  #   shape2.spdf <- readOGR(dsn = data.path, layer = shape2, stringsAsFactors = F) %>% spTransform(projection)
  # } else if (data.path != "") {
  #   shape2.spdf <- readOGR(dsn = paste0(data.path, "/", shape2, ".shp"), layer = shape2, stringsAsFactors = F) %>% spTransform(projection)
  # } else if (class(shape2)[1] == "SpatialPointsDataFrame" | class(shape2)[1] == "SpatialPolygonsDataFrame") {
  #   shape2.spdf <- shape2 %>% spTransform(projection)
  # }
  if (paste(shape1@proj4string) != paste(shape2@proj4string)) {
    ## Make sure that the points also adhere to the same projection
    shape1 <- shape1 %>% spTransform(projection)
    shape2 <- shape2 %>% spTransform(projection)
  }


  ## Because there might be overlap between polygons with different evaluation stratum identities, we'll check each eval stratum independently
  for (n in unique(shape2@data[, attributefield])) {
    ## Create a copy of the points to work with on this loop
    current.shape1 <- shape1
    ## Get the data frame from checking the points against the current subset of the polygons
    over.result <- over(current.shape1, shape2[shape2@data[, attributefield] == n,])
    ## Add the values to the newfield column
    current.shape1@data[, newfield] <- over.result[, attributefield]
    ## Make sure that the polygons have unique IDs
    if (class(current.shape1) == "SpatialPolygonsDataFrame") {
      current.shape1 <- spChFIDs(current.shape1, paste(runif(n = 1, min = 0, max = 666666666), row.names(current.shape1), sep = "."))
    }
    ## Store the results from this loop using the naming scheme "over__[current value of n]" with spaces replaced with underscores to prevent parsing errors later
    ## But only if the number of coordinates is greater than 0!
    print(nrow(current.shape1[!is.na(current.shape1@data[, newfield]),]))
    if (nrow(current.shape1[!is.na(current.shape1@data[, newfield]),]) > 0) {
      assign(x = str_replace(paste0("over__", n), " ", "_"),
             ## Only keep the ones that actually took on an attribute
             value = current.shape1[!is.na(current.shape1@data[, newfield]),])
    }
  }
  ## List all the objects in the working environment that start with "over__" and rbind them into a single SPDF
  attributed.spdfs <- ls()[grepl(x = ls(), pattern = "^over__")]
  ## Handle all the situations where there might not be intersections, there's only one attributed SPDF, or we get the expected results
  if (length(attributed.spdfs) > 0) {
    if (length(attributed.spdfs) == 1) {
      output <- get(attributed.spdfs[1])
    } else {
      output <- eval(parse(text = paste0("rbind(`", paste(attributed.spdfs, collapse = "`,`") ,"`)")))
    }
  } else {
    output <- NULL
  }
  return(output)
}
