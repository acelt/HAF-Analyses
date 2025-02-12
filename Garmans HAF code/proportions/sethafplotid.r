##  SetHAFPlotID() - ensures that HAF scores are using PLOTKEY as the plotID and the field is called
##                   Plot.Identifier 
##   SetHafPlotID(wgts.df is the weights info, score is the Excel spreadsheet scores)

SetHAFPlotID<-function(wgts.df,score,option)
{
    ## Given the tendency for HAF score PLOTIDs to not match SDD names, convert them to generally what SDDs use.
    score$PLOTIDC<-0

## Convert existing PlotID to syntax generally used in the SDDs.  This will not work if LMF keys are included as PlotID
      for(i in 1:nrow(score)) {
         a<-as.character(score$PLOTID[i])
	 ## This will back-off from the RHS until a _ is found, then change it to a - and glue the filename back together using
	 ## the -.  PlotIDs tend to differ between the field and SDDs by this - before a (plot) number.  e.g., LA_INTS_10 vs. LA_INTS-10.
         b<-a
	 size<-nchar(a)
	 j<-size+1
	 for(k in 1:size) {
  		j<-j-1
  		cc<-substr(a,j,j)
  		dash<-grepl(cc,pattern="_")
  		if(dash) {
                        if(size-j <6) {		## Sometimes, the left-most "-" is correct, so this will prevent changing a correct "_" to a "-" 
     				b<-paste(substr(a,1,j-1),"-",substr(a,j+1,nchar(a)),sep="")
     				break
                        }
  		}
	 }
         score$PLOTIDC[i]<-b
      }




## Now extract the PLOTKEYs from wgts.df by cross-walking score$PLOTIDC with wgts.df$PLOTID.SDD
      score$Plot.Identifier<-"NA"

      if(nrow(score)>0) {     ## Just to be sure
        for(i in 1:nrow(score)) {
            a<-grep(wgts.df$PLOTID.SDD,pattern=score$PLOTIDC[i])
            if(length(a)!=1) {
              print(paste("ERROR, cant find HAF score",score$PLOTIDC[i],sep="-"))
            }else {
               score$Plot.Identifier[i]<-as.character(wgts.df$PLOTKEY[a])
            }
        }
      }

      if(option==1) {		## only retain the matching pts -e.g., when dealing with allotments
	  score<-score[score$Plot.Identifier!="NA" ,]
      }
      
      b<-score[score$Plot.Identifier=="NA" ,]
      if(nrow(b)>0) {
         print("ERROR, not all score$Plot.Identifier are set")
         print(b)
      }
      return(score)
}	
