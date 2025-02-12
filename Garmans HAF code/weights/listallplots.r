## 4/17/2017
##  ListAllPlots.r
##  SDD and LMF plots are extracted from their respective databases based on user-specified years (i.e., the first yr the plots were observed).
##  This function extracts the annual replicates of these plots based on the PLOT_KEY field.  This works for AIM data, but doesn't for LMF data since
##  the LMF PLOT_KEY seems to have calendar year as the prefix of the PLOT_KEY.  THus, there will be no LMF replicates with the same PLOT_KEY.  Need to revisit
##  the current logic once we figure out exactly how LMF annual replicates are labeled.  


##  Within the s loop but at the end of the stratum loop in weighter.r, pointsweights.current contains the point info for the most recent s loop and is  
##  set to temppt.df which is used here to scruitinize TerraDat.spdf, and lmfmaster.spdf IFF lmf points are included. 
##  pointweights.df contains the entire list of first-time points for the AOI. It is used here to ensure that duplicate point info is not extracted in this function.
##  The results from this function, if any, are added to pointweights.df within weighter.r.  


## Available attributes:
##       temppt.df <- rbind(temppt.df, pointsweights.current[, c("FIELDOFFICE", "DATASRC", "DT_VST", "PLOT_KEY", "TERRA_TERRADAT_ID", "PLOT_NM","FINAL_DESIG", "REPORTING.UNIT", "WEIGHT.ID",  "WGT", "LONGITUDE", "LATITUDE", "ADJWGT", "XMETERS", "YMETERS")])


ListAllPlots <-function(input_temp.df=NULL,pointweights.df) {   ## input_temp.df is pointsweights.current in weighter.r, pointweights.df is a working list of plots to report 


        p.df<-NULL        
        interim.df<-NULL
       

        ## Sometimes the plot is listed >once with the same plot key, but only 1 of the records will have TERRA_TERRADAT_ID !=NULL.
        ## This happens when someone duplicates records in the SDD to indicate a plot is replicated (e.g., some NORCAL SDDs were like this) 
        ##  OR when future plots that haven't been sampled are listed (happens!!).

  	########################################## This gives us ONLY AIM points 
        interim.df<- input_temp.df[!is.na(input_temp.df$TERRA_TERRADAT_ID) ,] 
        interim.df<- interim.df[interim.df$TERRA_TERRADAT_ID !="LMF" ,] 		## We insert LMF into this field for lmf points

        if(nrow(interim.df)>0) {	## If AIM pts exist

	        ## Output data by year
	        interim.df <- merge(x = interim.df, y = terra.spdf[,c("DATEVISITE", "PRIMARYKEY","PLOTKEY")], by.x = "PLOT_KEY", by.y = "PLOTKEY", all = F)  
		if(nrow(interim.df)<=0) {
                   print("Plot_Key was not found in TerrADat in LISTALLPLOTS")
		   return(p.df)
                }

                ## Get the years from the visitation dates
  		interim.df$YEAR <- interim.df$DATEVISITE %>% str_extract(pattern = "2[0-9]{3}")

  		## Sort the data frame ascending by year
  		interim.df <- dplyr::arrange(interim.df, YEAR)

                interim.df$USE<-2
		interim.df$USE[interim.df$PRIMARYKEY %in% pointweights.df$TERRA_TERRADAT_ID]<-1
                interim.df<-interim.df[interim.df$USE==2 ,]
                if(nrow(interim.df)>0) {
  			fieldnames.available <- names(interim.df)[names(interim.df) %in% c("FIELDOFFICE",
                                                                     "DATASRC", 
                                                                     "PLOT_KEY",
                                                                     "PRIMARYKEY",
                                                                     "PLOT_NM",
                                                                     "FINAL_DESIG",
                                                                     "REPORTING.UNIT",
                                                                     "WEIGHT.ID",
                                                                     "WGT",
                                                                     "LONGITUDE",
                                                                     "LATITUDE",
								     "ADJWGT",
                                                                     "XMETERS",
                                                                     "YMETERS")]


                	p.df<-rbind(p.df,interim.df[,fieldnames.available])
                	names(p.df)[names(p.df)=="PRIMARYKEY"]<-"TERRA_TERRADAT_ID"		## Expecting returned dataframe to have TERRA_TERRADAT_ID not PrimaryKey
		} ## if nrow() >0

	}

        ######################################## Now process LMF points if they are used.  
        ##   NOTE: need to revisit once we figure out how PLOT_KEY of replicates are actually labeled.  Given the year prefix of PLOT_KEY, should never
        ##         find replicates using the current logic...  

        return(p.df)  ## for now don't even bother checking....

        interim.df<- input_temp.df[input_temp.df$DATASRC=="LMF",] 
        if(nrow(interim.df)==0) return(p.df)

        interim.df <- merge(x = interim.df, y = lmfmaster.spdf, by.x = "PLOT_KEY", by.y = "PLOTKEY", all = F) 
        interim.df$PrimaryKey<-interim.df$PLOT_KEY    ## to be consistent - PLOT_KEY is the 'primary key' for LMF data

        return(p.df)
}
