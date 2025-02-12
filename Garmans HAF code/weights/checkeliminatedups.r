##  CheckEliminateDUPS() - After filter.dateCLIP operations, the aggregated pts file may contain duplicate pts; specifically,
##                         non Target Sampled pts.  This function is specifically designed to eliminate these Dups, if necessary,
##                         and update the first pts file in the named list (workinglist).

  CheckEliminateDUPS<- function(workinglist,pts)
{ 
   ## Check for DUPS - need to separate by AIM, then by LMF
   pts$keeper<-1

   a<-pts[pts$AIMLMF==1 ,]
   b<-unique(a$PLOT_NM)
   

   if(nrow(a)!=length(b)) {	## If true, then we have dups
       if(length(b) >0) {	## Just to be sure
          for(i in b) {
            c<-grep(pts$PLOT_NM, pattern=i)
            if(length(c)>1) {			# if c>1, then we have dups.  Keep the first 1, delete all others
               for(j in 2:length(c)) {
                   pts$keeper[c[j]]<-0
               }
            }
          }
       }
   }


   a<-pts[pts$AIMLMF==2 ,]
   b<-unique(a$PLOT_KEY)
   if(nrow(a)!=length(b)) {	## If true, then we have dups
       if(length(b) >0) {	## Just to be sure
          for(i in b) {
            c<-grep(pts$PLOT_KEY, pattern=i)
            if(length(c)>1) {			# if c>1, then we have dups.  Keep the first 1, delete all others
               for(j in 2:length(c)) {
                   pts$keeper[c[j]]<-0
               }
            }
          }
       }
   }
   pts<-pts[pts$keeper==1 ,]  
   pts$keeper<-NULL

   s<-names(workinglist$pts[1])
   workinglist$pts[[s]]<-pts

   return(workinglist)
}


