## 12/11/2017
## PlotTracking - uses the PlotTracking spreadsheet to populate the SDD.  Used whenever the SDD has not been updated
##                with the PlotTracking info.

## Checks to make sure that the PLOTID in the PlotTracking spreadsheet occurs in TerraDat (if FINAL_DESIG==TS, then PLOTID must occur in TerraDat), and that  all PLOTIDs of the spreadsheet  occurs in the SDD.
## If not, the errors are printed out (but processing continues). 


PlotTracking <- function(path.nam, ## Pathname to a spreadsheet
                         sheetname,  		## THe name of the sheet in the excel file to ingest
			 terra.spdf,  		## TerraDat
                         workinglist, 		## the SDD sample pts
                         DeleteOverDraw=T	## delete overdraw pts IFF they were not used
) {


	## Read the spreadsheet, standardize nomenclature, and store key attributes in retain

                s<-path.nam

		## may need to mod data before further processing, so using xcelIN as an initial frame for the plot tracking data
		xcelIN<-(read.xlsx(s,sheetName=sheetname,stringAsFactors=F))		## sheetname is typically PLOT TRACKING but not always!!
		print(paste("No. of rows in plot tracking = ",nrow(xcelIN)))		## just a quick check
		names(xcelIN) <- str_to_upper(names(xcelIN)) 
	        xcel<-xcelIN


		## Standardize data types and nomenclature
		names(xcel)[names(xcel) == "PLOT.ID"] <- "PLOTID"


		xcel$PLOTID<-as.character(xcel$PLOTID)		## deal with the data type 
		xcel$PANEL<-as.character(xcel$PANEL)
		xcel$PLOTSTATUS<-as.character(xcel$PLOT.STATUS)

									
		xcel$PANEL[xcel$PANEL=="OverSample"]<-"OverSample"		## This may need to be specified to each spreadsheet???
		xcel$PLOTSTATUS[xcel$PLOTSTATUS %in% c("Sampled")]<-"TS"
		xcel$PLOTSTATUS[xcel$PLOTSTATUS=="Rejected"]<-"IA"

		retain<-data.frame(xcel[, c("PLOTID","PANEL","PLOTSTATUS","DATE.VISITED")])		## store plotid, panel, and plotstatus



################  Locate the plotid in terradata then store the plot key, primary key, and date visited.  If plotid not in terradata, then
##                the plot was not sampled (but there are subsequent checks to make sure the plotid's aren't misspelled) but
##                frame data ID, PLKEY, PRKEY, and DV need to be set to NA. 

   		store.df<-NULL

   		for(i in 1:nrow(retain)) {
      			a<-grep(terra.spdf$PLOTID,pattern=retain$PLOTID[i])
        		if(length(a)>0) {
            			hit<-0
            			for(j in a) {
            				if(terra.spdf$PLOTID[j]== retain$PLOTID[i]) {
                   				temp.df<-data.frame(ID=terra.spdf$PLOTID[j],PLKEY=terra.spdf$PLOTKEY[j],PRKEY=terra.spdf$PRIMARYKEY[j],DV=terra.spdf$DATEVISITE[j])
                   				hit<-1	## 
            				}
	    			}
            			if(hit==0) {
           				temp.df<-data.frame(ID=retain$PLOTID[i],PLKEY="NA",PRKEY="NA",DV=retain$DATE.VISITED[i])
            			}
        		}else {
           				temp.df<-data.frame(ID=retain$PLOTID[i],PLKEY="NA",PRKEY="NA",DV=retain$DATE.VISITED[i])
        		}
        		store.df<-rbind(store.df,temp.df)
   		}


##################  We retained all the data so nrow of retain and store.df should be equal

   		if(nrow(retain) != nrow(store.df)) {
      			print(paste("nrow of retain and store.df !=  ",nrow(retain),nrow(store.df),sep=" "))

   		}
  		 store.df$ID<-as.character(store.df$ID)
  		 store.df$PLKEY<-as.character(store.df$PLKEY)
  		 store.df$PRKEY<-as.character(store.df$PRKEY)
  		 store.df$DV<-as.character(store.df$DV)


   		 retain<-cbind(retain,store.df)		## this will contain some dup info but that is for diagnostic purposes





####################### Need to check to see if the plot tracking spread sheet indicates a sampled plot YET we can't find it in TerraDat.  Typically, this occurs
##                      when plotID is coded incorrectly in the plot tracking sheet.
## 			Specifically, if PLOTSTATUS==TS, then we should have a plotkey, terradata ID, and date visited.


  		target<-c("Target Sampled","TS")

   		for(i in 1:nrow(retain)) {
      			if(retain$PLOTSTATUS[i] %in% target) {
         			if(retain$PLKEY[i]=="NA") {
            				print(paste("PlotID not in TerraDat ", retain$ID[i]))		## mismatch
         			}
      			}
   		}

###################### pick up the pts file of the corresponding SDD
       	        pts<-workinglist


####################### Compare plot tracking panel with SDD point draw. ?????
## 			Use Over to compare
   		t<-c("Year1")		## Year1	  ## probably need to make this more robust (e.g., what if we're dealing with Year 2 panels) OR needs to be passed to this function 
   		a<-grep(retain$PANEL,pattern="Over")
   		if(length(a)>0) {
     			for(i in a) {
         			b<-grep(pts$PLOT_NM,pattern=retain$PLOTID[i])
         				if(length(b)>0) {
            					for(j in b) {
	       						if(pts$PLOT_NM[j]==retain$PLOTID[i]) {
                                                                ## Essentially, if we have a plot labeled as an OverDraw and the SDD indicates other than an OverDraw, then a mismatch.  Print out the plotid and the
								##              SDD PT_Draw label and the PlotTracking Panel label.
	          						if(pts$PT_DRAW[j] %in% t)print(paste("ID, SDD, PlTrck ",retain$PLOTID[i],pts$PT_DRAW[j],retain$PANEL[i]))
	       						}
            					}
         				}
     			}
   		}   



######################## We can have plots with no final designation that were either skipped or that were overdraws.  The former
## 			should be noted as Unknowns, and the latter as NA and eventually eliminated from further analyses.   ????
 			
   		a<-grep(retain$PANEL,pattern="Year") 		
   		if(length(a)>0) {
     			for(i in a) {
        			if(is.na(retain$PLOTSTATUS[i]))retain$PLOTSTATUS[i]<-"Unknown"
     			}
   		}



####################### Loop thru retain, find PLOTID in the SDD, and set key attributes.  If PLOTID not in the SDD (hit==0), then print out the 'missing' PLOTID.
      		for(i in 1:nrow(retain)) {
         		a<-grep(pts$PLOT_NM,pattern=retain$PLOTID[i])
         		hit<-0
         		if(length(a)>0) {
            			for(j in a) {
               				if(pts$PLOT_NM[j]==retain$PLOTID[i]) {			## if retain contains other than NA, then set the attribute in pts.
  						hit<-1							
                  				if(retain$PLKEY[i]!="NA")pts$PLOT_KEY[j]<-retain$PLKEY[i]
                  				if(retain$PRKEY[i]!="NA")pts$TERRA_TERRADAT_ID[j]<-retain$PRKEY[i]
                  				pts$FINAL_DESIG[j]<-retain$PLOTSTATUS[i]
                  				if(!is.na(retain$DV[i]))pts$DT_VST[j]<-retain$DV[i]
                                         print(paste(pts$PLOT_NM[j],pts$PLOT_KEY[j],pts$TERRA_TERRADAT_ID[j],pts$FINAL_DESIG[j],pts$DT_VST[j],sep=" "))

               				}
            			}
         		}
         		if(hit==0) {
            			print(paste("Couldn't find the plot in the SDD ",retain$PLOTID[i],sep=" "))
         		} 
 		}


####################  If specified, delete overdraw pts that lack a final designation (i.e., not used).  This does not eliminate all NAs, just overdraw NAs..
                if(DeleteOverDraw) {
                   pts@data$delete<-1
		   a<-grep(pts$PANEL,pattern="Over")
                   if(length(a)>0){
			for(j in a) {
				if(is.na(pts$FINAL_DESIG[j]))pts$delete[j]<-pts$FINAL_DESIG[j]		## This sets $delete to NA
			}
			pts<-pts[!is.na(pts@data$delete),]			## Get rid of all records with $delete == NA
			pts$delete<-NULL
		   }
		}


#################### Check to see if FINAL_DESIG is TS, then there should be a date visited
                target<-c("Target Sampled","TS")
                a<-pts[pts$FINAL_DESIG %in% target ,]
                b<-a[!is.na(a$DT_VST) ,]
                print(paste("No. TS/DT_VST ",nrow(a),nrow(b),sep=" "))


	## Declutter and save space
        remove(store.df)
        remove(retain)
        remove(xcel)
        remove(xcelIN)
        remove(path.nam)


	## Return the modified pts file(s) contained in the named list
        return(pts)

} ## end of function

