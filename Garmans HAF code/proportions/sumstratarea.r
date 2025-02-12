## SumStratArea() - summarize total area and sampled area by strata by reporting unit, then record to areasums.csv and _results.xlsx
## SumStratArea(adjusted area df, reporting unit names, the tab prefix, the tab counter,sheetname for _results.xlsx,APPend=F or T, name
## for the area_sampled column, output to _results.xlsx Y or N)
## APPend should be F in the first call (per tab) to this function, else T
## verbose = 0 means output results to _results.xlsx, =1 to suppress this output.

SumStratArea<-function(areas.df,RUname,tabprefix,tabcounter,sheetname,APPend,Option,verbose)
{
   ## areas.df is a summary of total area and sampled area by strata by reporting unit.  Summarize and include in areasums.csv
   summary.df<-data.frame
   summary.df<-areas.df

   ## Add reporting unit name to summary.df
   zz<-unique(summary.df$REPORTING.UNIT)
   for(i in zz) {
        summary.df$RUname[summary.df$REPORTING.UNIT==i]<-RUname[i]
   }
   b<-data.frame(WEIGHT.ID="Total",AREA.HA.Total=sum(summary.df$AREA.HA.Total),AREA.HA.sampled=sum(summary.df$AREA.HA.sampled),REPORTING.UNIT="All",RUname="NA")
   summary.df<-rbind(summary.df,b)
   summary.df$Proportion<-summary.df$AREA.HA.sampled/summary.df$AREA.HA.Total
   names(summary.df)[names(summary.df)=="AREA.HA.sampled"]<-Option

   fnam<-paste0(tabprefix[tabcounter],"areasums.csv")
   write.table(summary.df,fnam,row.names=F,quote=F,sep=",",append=APPend)
   write.table("",fnam,row.names=F,quote=F,sep=",",append=T)	## Add a space to the output file

   if(verbose==0) {
   	fnam<-paste0(tabprefix[tabcounter],"_results.xlsx")
   	write.xlsx(summary.df,fnam,sheetName=sheetname,col.names=T,row.names=F,append=T,showNA=F)
   }
   return(summary.df)
}
