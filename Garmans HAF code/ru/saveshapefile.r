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
}