## filter.date.r - Function to eliminate points not matching specified dates.
## Updates the N pts file in workinglist and returns the named list (workinglist).
## This should be performed before clipping to the RU.  

## Points with NA dates are retained,
## else points with visit dates >= target[] <= are retained,
## else, points are removed.


filter.date <- function(importSDD,target,N) 
{
	s<-names(importSDD$pts[N])
	pts<-importSDD$pts[[s]]

	pts$season<-substr(pts$DT_VST,6,10)		## Assumes a consistent format!!!!!!!!
	pts$season<-gsub("-","",pts$season)

	for(i in 1:nrow(pts)) {
	  	pts$keep[i]<-0
  		if(is.na(pts$season[i])) {
     	 		pts$keep[i]<-1
                }else if(pts$season[i]=="") {
                        pts$keep[i]<-1
  		}else {
     			if(as.numeric(pts$season[i])>= target[1] & as.numeric(pts$season[i])<=target[2]){pts$keep[i]<-1}
  		}
	}
	pts<-pts[pts$keep==1,] 

        if(!is.null(pts)) {
           pts$season<-NULL
           pts$keep<-NULL
        }

        importSDD$pts[[s]]<-pts
   	return(importSDD)
} ## end of function  
