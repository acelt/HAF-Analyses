##  CheckFixStrata.r - Checks and fixes the strataum of pts based on overlaying pts with stratum file.
##  Uses the strata file as the MASTER to check & fix pt strata.
##  importSDD is workinglist; entry is the numeric sequence of the pts file in workinglist$pts.


CheckFixStrata <- function(importSDD,entry) 
{


   projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")



     s<-names(importSDD$strata[entry])
     if(is.null(importSDD$strata[[s]])) {
        print("There is no strata for CheckFixStrata")
        return(importSDD)
     }
     strata<-importSDD$strata[[s]]
     
     s<-names(importSDD$pts[entry])
     if(is.null(importSDD$pts[[s]])) {
        print("There is no pts file for CheckFixStrata")
        return(importSDD)
     }
     pts<-importSDD$pts[[s]]

     ## project to ensure over() functions properly.
     strata<- spTransform(strata, projection)
     pts<- spTransform(pts, projection)

     temp<-over(pts,strata)
     pts$STRATUM<-temp$DMNNT_STRTM
     pts$ERR<-0
     pts$ERR[pts$DSGN_STRTM_NM != pts$STRATUM]<-1
     zz<-grep(pts$ERR,pattern="1")
     if(length(zz)>0) {
       for(i in zz) {
        print(paste("STRATUM ERROR",i,pts$PLOT_NM[i],pts$DSGN_STRTM_NM[i],pts$STRATUM[i],sep="/"))
        pts$DSGN_STRTM_NM[i]<-pts$STRATUM[i]				## Replace
       }
     }else {
       print("No stratum errors")
     }
     pts$STRATUM<-NULL
     pts$ERR<-NULL
     remove(zz)

     importSDD$pts[[s]]<-pts		## Put original OR modified pts back into workinglist
     return(importSDD)
}   






   