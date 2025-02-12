## Erase() - erase reporting units from the AOI

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

