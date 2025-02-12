## CheckFD_DT - Function to list FINAL_DESIG OR DATE of pts in workinglist.
## entry is the seqeuntial entry in workinglist (passed as import), and
## keyword is FD for FINAL_DESIG or DT for DATE.   


CheckFD_DT<- function(import,entry,keyword) 
{

     s<-names(import$pts[entry])
     pts<-import$pts[[s]]
     if(!is.null(pts)) {		## only if we have pts
         for(i in 1:nrow(pts)) {
             if(keyword=="FD")print(paste(i,pts$PLOT_KEY[i],pts$FINAL_DESIG[i],pts$AIMLMF[i],sep=";"))
             if(keyword=="DT")print(paste(i,pts$PLOT_KEY[i],pts$DT_VST[i],pts$AIMLMF[i],sep=";"))
          }
     }
}  

   