## filter.dateCLIP.r - Function to eliminate points not matching specified dates AND clips to the specified AOI.
## Operates on, and returns the N pts file in workinglist.

## Points with NA dates are retained,
## else points with visit dates >= target[] <= are first retained, 
## else, points are removed,
## then pts are clipped to the specified AOI.


filter.dateCLIP <- function(importSDD,target,clipper,N) 
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

        pts<-flex.clip.pts(pts,clipper,method = "arcpy",temp.path = getwd(),
                           python.search.path = "C:/Python27")
   	return(pts)
} ## end of function  
