## DeleteFP.r - Function to eliminate points matching the specified sample frame.
## Operates on, and returns the first pts file in workinglist which is passed as importSDD.
   


DeleteFP <- function(importSDD,framename) 
{

     s<-names(importSDD$pts[1])
     pts<-importSDD$pts[[s]]
     pts<-pts[pts$TERRA_SAMPLE_FRAME_ID!=framename,]
     importSDD$pts[[s]]<-pts
     return(importSDD)
} ## end of function  

   