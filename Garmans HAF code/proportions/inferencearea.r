## InferenceArea() - Set inference by reporting unit & aerial summaries into strata.spdf
## InferenceArea(strata file, aerial summaries, RU names,

InferenceArea<-function(strata.spdf,summary.df,RUname)
{
## At the end of summary is the total - don't include in the following looping (hence, inum-1).
   inum=nrow(summary.df)
   inum<-inum-1

## Transfer aerial extent info in summary.df to the shapefile
   strata.spdf$AREA.HA.Total<-0
   strata.spdf$AREA.HA.Sampled<-0
   strata.spdf$Proportion<-0

   for(i in 1:inum) {
       for(j in 1:nrow(strata.spdf)) {
         if(str_to_upper(strata.spdf$DMNNT_STRTM[j]) %in%summary.df$WEIGHT.ID[i] & strata.spdf$RU[j] %in%summary.df$REPORTING.UNIT[i]) {
             if(strata.spdf$Inference[j] ==1 && summary.df$AREA.HA.Sampled[i]>0) {
                strata.spdf$AREA.HA.Total[j]<-summary.df$AREA.HA.Total[i]
                strata.spdf$AREA.HA.Sampled[j]<-summary.df$AREA.HA.Sampled[i]
                strata.spdf$Proportion[j]<-summary.df$Proportion[i]
             }
             if(strata.spdf$Inference[j] ==0 && summary.df$AREA.HA.Sampled[i]==0) {
                strata.spdf$AREA.HA.Total[j]<-summary.df$AREA.HA.Total[i]
                strata.spdf$AREA.HA.Sampled[j]<-summary.df$AREA.HA.Sampled[i]
                strata.spdf$Proportion[j]<-summary.df$Proportion[i]
             }

	 }
       }
   }

     zz<-unique(strata.spdf$RU)
     for(i in zz) {
     	strata.spdf$RUname[strata.spdf$RU==i]<-RUname[i]
     }

     ## Can have AREA.HA.Total set to zero which means the stratum didn't actually occur in the reporting unit, so delete these cases here
     strata.spdf<-strata.spdf[strata.spdf$AREA.HA.Total>0 ,]
     return(strata.spdf)
}
