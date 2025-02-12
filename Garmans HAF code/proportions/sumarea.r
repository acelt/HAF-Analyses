## SumArea() - Summarize across strata totals, record in areasums.csv and _results.xlsx
## SumArea(adjusted strata area, the tab prefix, the tab counter,sheetname for _results.xlsx, name for area_sampled column,
##  output to _results.xlsx Y or N)

SumArea<-function(areas.df,tabprefix,tabcounter,sheetname,Option,verbose)
{
   strata<-unique(areas.df$WEIGHT.ID)
   totlarea<-0
   samparea<-0
   temp.df<-NULL
   areasum.df<-NULL
   for(i in 1:length(strata)) {
     total<-sum(areas.df$AREA.HA.Total[areas.df[, "WEIGHT.ID"] %in% strata[i]])
     sample<-sum(areas.df$AREA.HA.sample[areas.df[, "WEIGHT.ID"] %in% strata[i]])
     totlarea<-totlarea+total
     samparea<-samparea+sample
     prop=sample/total
     temp.df<-data.frame(Strata=as.character(strata[i]),AREA.HA.Total=total,AREA.HA.Sampled=sample,Proportion=prop)
     areasum.df<-rbind(areasum.df,temp.df)
   }
   temp.df<-data.frame(Strata="TOTALS",AREA.HA.Total=totlarea,AREA.HA.Sampled=samparea,Proportion=samparea/totlarea)
   areasum.df<-rbind(areasum.df,temp.df)
   names(areasum.df)[names(areasum.df)=="AREA.HA.Sampled"]<-Option

   fnam<-paste0(tabprefix[tabcounter],"areasums.csv")
   write.table(areasum.df,paste(src,fnam,sep="/"),row.names=F,quote=F,sep=",",append=T)
   write.table("",paste(src,fnam,sep="/"),row.names=F,quote=F,sep=",",append=T)

   if(verbose==0) {
   	fnam<-paste0(tabprefix[tabcounter],"_results.xlsx")
   	write.xlsx(areasum.df,fnam,sheetName=sheetname,col.names=T,row.names=F,append=T,showNA=F)
   }

   return(areasum.df)
}

