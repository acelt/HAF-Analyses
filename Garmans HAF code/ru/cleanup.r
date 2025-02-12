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

