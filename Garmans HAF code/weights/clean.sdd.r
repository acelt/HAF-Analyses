## clean.sdd.r - Function to eliminate points not matching specified years and OVERDRAW pts that have no evidence of being used.
## Operates on, and returns the pts file(s) based on the specified begin and end sequence in workinglist.



clean.sdd <- function(sdd.list,
                      valid.fates = c("Target Sampled","TS","Non-Target","NT","Inaccessible","IA"),
                      yearlist,
                      begin,
                      end) 
{
  
  ###################################################################################### Loop thru SDD$pts
  for(index in begin:end) {
    
    s <- names(sdd.list$pts[index])
    pts <- sdd.list$pts[[s]]
    
    if(!is.null(pts)) {		## only if we have SDD pts
      
      ######################## Select pts for the specified years; assumes yr of earliest AIM data is 2011
      for(i in 1:30) {
        pts$theyr[grepl(pts$PANEL,pattern=(2010+i))==TRUE]<-2010+i
        pts$theyrD[grepl(pts$DT_VST,pattern=(2010+i))==TRUE]<-2010+i
      }    
      
      ## Must be an easier way to do the following, but couldn't figure it out!!!!
      pts$a<-1  				## default to 1
      pts$a[pts$theyr %in% yearlist] <-2    	## set to 2 if theyr is in yearlist (user-specified year of interest)
      pts$a[pts$theyrD %in% yearlist] <-2    	## set to 2 if theyrD is in yearlist (user-specified year of interest)
      pts<-pts[pts$a==2,]  		    	## render pts to those years specified by the user
      
      if(nrow(pts)==0) {
        pts<-NULL			    	## no pts satisfied the specified time period
      } else {
        pts$theyr<-NULL       		## clean up
        pts$theyrD<-NULL
        pts$a<-NULL
      }
      
      ###################### if !is.null(pts), then eliminate OVERSAMPLE pts that lack evidence of being accessed
      if(!is.null(pts)) {
        
        pts$a<-0
        pts$b<-0
        pts$a[grepl(pts$PANEL,pattern="OverSample")==TRUE]<-1
        pts$a[grepl(pts$PANEL,pattern="Oversample")==TRUE]<-1		## Spelling issue
        pts$b[pts$FINAL_DESIG %in% unknown.fates] <-1
        pts<-pts[pts$a!=1 | pts$b!=1,]   ## pts$a==1 & pts$b==1 are OverSamples with fates in unknown.fates
      }
      
      if(!is.null(pts)) {
        pts$a<-NULL
        pts$b<-NULL
      }
      
      
    } ## !is.null
    
    sdd.list$pts[[s]]<-pts
    
  }  ## for index
  
  return(sdd.list)
  
} ## end of function  

