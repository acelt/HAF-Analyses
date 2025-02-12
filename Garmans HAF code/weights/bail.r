## bail.r 
## When weighter() detects a NULL pts.spdf, bail() is called to summarize info about the frame.spdf
## which could be either the sample frame or the stratum file.  The summary info along with
## the frame shapefile is output so that the area lacking points can be accounted for in deriving
## the true sampled area.
 
 bail<-function(s,		## SDD name in sdd.src
                fileprefix,	## user specified prefix to add before s when naming output files
                frame.spdf,	## either the sample frame or the stratum file
                framecode	## ==1 if frame.spdf is a stratum file, else it is a sample frame
 ){
 
 
        ## figure out the filename to output frame info
 	t<-s
    	if(!is.null(fileprefix))t<-paste0(fileprefix,t)
    	
        if(framecode) {			## if true then frame.spdf is a stratum file
           final.df<-NULL
           for(i in 1:nrow(frame.spdf)) {
              temp.df<-data.frame(Strata=frame.spdf$DMNNT_STRTM[i],Area.ha=frame.spdf$AREA.HA[i])
              final.df<-rbind(final.df,temp.df)            
           }
           ## write.csv(final.df, file = paste0(t, "NOPOINTS", Sys.Date()),row.names=F) 
	   if(!is.null(fileprefix)) {
              write.csv(final.df, file = paste0(fileprefix,"NOPOINTS"),row.names=F,quote=F)
           }else {
              write.csv(final.df, file = "NOPOINTS",row.names=F,quote=F)
	   }
        }else {	                                ## else frame.spdf is the sample frame 	
           final.df<-NULL
           for(i in 1:nrow(frame.spdf)) {
          	temp.df<-data.frame(Strata=frame.spdf$SAMPLE_FRAME_GROUP[i],Area.ha=frame.spdf$AREA.HA[i])
          	final.df<-rbind(final.df,temp.df)
           }
           ##write.csv(final.df, file = paste0(t, "NOPOINTS", Sys.Date()),row.names=F)
	   if(!is.null(fileprefix)) {
              write.csv(final.df, file = paste0(fileprefix,"NOPOINTS"),row.names=F,quote=F)
           }else {
              write.csv(final.df, file = "NOPOINTS",row.names=F,quote=F)
	   }
        }
        
        
        
        ## output the frame shapefle
        
            ## If s ends in .gdb, then eliminate this suffix  
            nam<-s
	    a<-substr(s,nchar(s)-3,nchar(s))
            if(a==".gdb") nam<-substr(s,1,nchar(s)-4)


	    src<-getwd() %>% sanitizer(type = "filepath")
            t<-"NOPOINTS"
            if(!is.null(fileprefix))t<-paste0(fileprefix,t)
            a<-paste(t,nam,sep="_")
	    frame.spdf %>% arc.write (paste(src,a), data = .)      
	    
	 dummy<-1   
	 return(dummy)
}	 
        
        