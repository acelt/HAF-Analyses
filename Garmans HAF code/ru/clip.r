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
}