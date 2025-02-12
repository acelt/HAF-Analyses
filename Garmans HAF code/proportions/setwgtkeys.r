## SetWgtKeys() - initiate fake plotkeys for TS pts not yet entered into the SDD and TerrADat.
##                 After the call to this function, call SetPtKeys() to set pts.spdf with the
##                  info erived here.

SetWgtKeys<-function(wgts.df)
{

  target<-c("Target Sampled","TS","TARGET SAMPLED")

  counter<-99123456
  
   for(i in 1:nrow(wgts.df)) {
      if(wgts.df$FINAL_DESIG[i] %in% target) {
          if(is.na(wgts.df$PLOTKEY[i])) {
              counter<-counter+1
              wgts.df$PLOTKEY[i]<-as.numeric(counter)
          }
      }
   }
  return(wgts.df)
}




################################################################################################
## SetPtKeys() - sets the contrived plotkeys set in wgts.df (above) into the pts.spdf file.

SetPtKeys<-function(pts.spdf,wgts.df)
{

  ## Save actual plotkeys, then reset pts.spdf PLOT_KEY before saving PTS file at end of processing.
  pts.spdf$PLOT_KEY_SAVE<-pts.spdf$PLOT_KEY

  target<-c("Target Sampled","TS","TARGET SAMPLED")

   for(i in 1:nrow(pts.spdf)) {
      if(pts.spdf$FINAL_DESI[i] %in% target) {
          if(is.na(pts.spdf$PLOT_KEY[i])) {
              a<-wgts.df[wgts.df$PLOTID.SDD %in% pts.spdf$PLOT_NM[i] ,]
              pts.spdf$PLOT_KEY[i]<-a$PLOTKEY
          }
      }
   }
  return(pts.spdf)
}

#wgts.df$PLOTID.SDD
#wgts.df$PLOTKEY
#wgts.df$FINAL_DESIG

#pts.spdf$PLOT_NM
#pts.spdf$PLOT_KEY
#pts.spdf$FINAL_DESI


