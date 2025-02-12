## SumStratWgts() - sums the wgts by strata by reporting unit in ts.df and transfers it to areas.df
## SumStratWgts(the wgts,strata area,max possible no. of reporting units)

SumStratWgts<-function(tss.df,area.df,index)
{
        ## Clear AREA.HA.sampled 'cause those numbers may be wrong if some weighted plots were not in the HAF scores.
   	## Reset area sampled within areas.df using weights and reporting unit info in ts.df.
        ## Also, when working with non-target pts, the original AREA.HA.sampled doesn't include the non-target pts area (part of
        ## the schema implemented in AIMWEIGHTS).
   	## index should be the max number of reporting units (maxindex)
	
	## Translate REPORTING.UNIT (e.g., RU1) in tss.df to a NUMBER (e.g., 1)
        tss.df$REPORT.NUM<-0
        tss.df$REPORT.NUM<- tss.df$REPORTING.UNIT %>% str_extract(pattern="[0-9]{1,2}")

        ## Here we re-set the numbers using a potential subset of plots
        area.df$AREA.HA.sampled<-0
        d <- tss.df %>% dplyr::group_by(Wgt.Category,REPORT.NUM)  %>% dplyr::summarize(area = sum(WGT))
   	for(i in 1:index) {			## reporting unit numeric code - assumes we number them 1-n & index is the max
       		dd<-data.frame(d[grepl(d$REPORT.NUM,pattern=i)==TRUE ,])	## This will work for up to 2 digit RUs
                if(nrow(dd)>0) {
                    for(j in 1:nrow(dd)) {
                       area.df$AREA.HA.sampled[area.df$WEIGHT.ID %in% dd$Wgt.Category[j] & area.df$REPORTING.UNIT==i]<-dd$area[j]
                    }
                }
   	}
        return(area.df)
 }
