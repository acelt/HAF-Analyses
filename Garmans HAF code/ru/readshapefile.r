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

