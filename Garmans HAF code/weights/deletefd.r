## DeleteFD.r - Function to eliminate points matching the specified FINAL_DESIG types.
##  DeleteFD(workinglist, list of FINAL_DESIG types to eliminate, affected beginning entry, affected ending entry in named list)

DeleteFD <- function(import,target,begin,end) 
{
  for(index in begin:end) {
     s<-names(import$pts[index])
     pts<-import$pts[[s]]
     pts$MEUS<-0
     pts$MEUS[pts$FINAL_DESIG %in% target]<-1
     pts<-pts[pts$MEUS==0 ,]
     pts$MEUS<-NULL
     import$pts[[s]]<-pts
  }
     return(import)
} 
  
 
