## StrataRU() - transforms the strata file into strata by reporting unit 
## StrataRU(clip,indexb,indexe,list of RU numbers)

StrataRU<-function(strata.spdf,clip,indexb,indexe,RU)
{
    strata.spdf$RU<-0
    index<-0

    for(nam in clip) {
       index<-index+1
       if(index>=indexb & index <=indexe) {
          if(!is.na(nam)) {
       		## Creates an SPDF 
       		assign(x = "clip.spdf",
       		value = nam %>% arc.open() %>% arc.select %>%
       		SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .)%>%spTransform(projection))

      		RU.spdf<-flex.clip(strata.spdf,clip.spdf,method="arcpy",temp.path=getwd())
       		RU.spdf$RU<-RU[index]
       		strata.spdf<-flex.erase(strata.spdf,clip.spdf,method="arcpy",temp.path=getwd())

       		uid<-1
       		n <- length(slot(RU.spdf, "polygons"))
       		poly.data <- spChFIDs(RU.spdf, as.character(uid:(uid+n-1)))
       		uid <- uid + n

       		n <- length(slot(strata.spdf, "polygons"))
       		temp.data <- spChFIDs(strata.spdf, as.character(uid:(uid+n-1)))
       		uid <- uid + n
       		strata.spdf <- spRbind(temp.data,poly.data)
	  }
       }
    }

    ## Small slivers can end up as RU==0; clean-up here.
    strata.spdf<-strata.spdf[strata.spdf$RU>0 ,]
    return(strata.spdf)
}
