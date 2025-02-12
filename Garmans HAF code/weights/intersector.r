## 4/17/2017
## THis version uses gClip() to clip polygons instead of raster::intersect


## Creates a SpatialPolygonsDataFrame from the intersection of two SpatialPolygonsDataFrames
## Basically a wrapping of raster::intersect() now, just with additional opportunity to call area.add() and automatically added unique identifiers
intersector <- function(spdf1, ## A SpatialPolygonsShapefile
                        spdf1.attributefieldname, ## Name of the field in SPDF1 unique to the unit groups or units to take values from
                        spdf1.attributefieldname.output = NULL, ## Optional name of the field in the output SPDF to duplicate values from spdf1.attributefieldname in
                        spdf2, ## A SpatialPolygonsShapefile
                        spdf2.attributefieldname, ## Name of the field in SPDF2 unique to the unit groups or units to take values from
                        spdf2.attributefieldname.output = NULL,  ## Optional name of the field in the output SPDF to duplicate values from spdf2.attributefieldname in
                        area.ha = T, ## Add fields for area in hectares for individual polygons and the sum of those within unique combinations of the input attribute fields
                        area.sqkm = T, ## Add fields for area in square kilometers for individual polygons and the sum of those within unique combinations of the input attribute fields
                        projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") ## Standard NAD83
){
  ## We'll need Alber's equal area projection for area calculations
  projectionAL <- CRS("+proj=aea")
  ## Sanitization
  spdf1 <- spdf1 %>% spTransform(projection)
  spdf2 <- spdf2 %>% spTransform(projection)
  names(spdf1) <- str_to_upper(names(spdf1))
  names(spdf2) <- str_to_upper(names(spdf2))
  spdf1.attributefieldname <- str_to_upper(spdf1.attributefieldname)
  spdf2.attributefieldname <- str_to_upper(spdf2.attributefieldname)
  
  ## Find the intersection of the two SPDFs
  ## intersect.spdf.attribute <- raster::intersect(x = spdf1, y = spdf2)
     intersect.spdf.attribute <-gClip(spdf1,spdf2)
  
  ## Create a single field to serve as a unique identifier to dissolve the polygons by. This concatenates with a known nonsense string so we can split them later
  ## for (n in seq_along(intersect.spdf.attribute@data)) {
  for( n in 1:nrow(intersect.spdf.attribute@data)) {
    intersect.spdf.attribute@data$UNIQUE.IDENTIFIER[n] <- sha1(x = paste0(intersect.spdf.attribute@data[n, spdf1.attributefieldname],
                                                                       intersect.spdf.attribute@data[n, spdf2.attributefieldname]),
                                                            digits = 14)
  }
  
  
  ## If we're adding areas then:
  if (area.ha | area.sqkm) {
    ## Add the areas in hectares and square kilometers for each as called for
    intersect.spdf.attribute <- area.add(spdf = intersect.spdf.attribute,
                                         area.ha = area.ha,
                                         area.sqkm = area.sqkm)

    ## Now we summarize() the areas by unique identifier and then merge that with the original data frame and use it to overwrite the original data frame
    ## The arguments to summarize() are specific to what columns exist and what columns are therefore being added, so there are three alternatives
    if (area.ha & area.sqkm) {
      ## When there are both units represented
      ## group_by_() is used instead of group_by() so that we can provide strings as arguments to let us programmatically use the attributefieldname.output values
      intersect.spdf.attribute@data <- group_by(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.HA.UNIT.SUM = sum(AREA.HA), AREA.SQKM.UNIT.SUM = sum(AREA.SQKM)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    } else if (!(area.ha) & area.sqkm) {
      ## When there's no area.ha
      intersect.spdf.attribute@data <- group_by_(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.SQKM.UNIT.SUM = sum(AREA.SQKM)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    } else if (area.ha & !(area.sqkm)) {
      ## When there's no area.sqkm
      intersect.spdf.attribute@data <- group_by_(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.HA.UNIT.SUM = sum(AREA.HA)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    }
  }
  
  ## Create the fields requested if they' exist're specified
  if (!is.null(spdf1.attributefieldname.output)) {
    intersect.spdf.attribute@data[, spdf1.attributefieldname.output] <- intersect.spdf.attribute@data[, spdf1.attributefieldname]
  }
  if (!is.null(spdf2.attributefieldname.output)) {
    intersect.spdf.attribute@data[, spdf2.attributefieldname.output] <- intersect.spdf.attribute@data[, spdf2.attributefieldname]
  }
  
  ## Return the final SPDF, making sure to project it into NAD83 (or whatever projection was provided to override the default)
  return(intersect.spdf.attribute %>% spTransform(projection))
}
