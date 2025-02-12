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
