## GenProportions() - generates proportional estimates and derives CIs for each TAB in a HAF spreadsheet.
##  main script initiates all the settings and inputs to this function.  

##  Outputs include proportions.csv, areasums.csv, _results.xlsx, finalmap.shp, and PTS.shp if
##   ptsfiles is !NULL.  All files will have the prefix specified in tabprefix.


GenProportions<-function(src,			## working directory
                         conf.level,		## integer confidence level
                         conf.levelF,		## conf.level as a fraction
                         RUname,		## user-specified names for each reporting unit
                         wgts.df,		## pts wgts generated in AIMWEIGHTS
                         indexb,		## beginning accession index into RUname - aka. beginning repoting unit 
                         indexe,		## ending accession index
                         scored,		## essentially the HAF spreadsheet (path and file name)
                         tabs,			## name of the tabs in the HAF spreadsheet
                         tabprefix,		## user-generated prefix for each tab - prefix of all output for a tab
                         STRATA,		## processed strata file
                         maxindex,		## max number of reporting units
                         SAVEareas.df,		## strata area summary
                         pts.spdf,		## NULL or the collection of pts files for this analysis generated in AIMWEIGHTS.R
                         VERBOSE,		## =0 to output observed and allpts results to _results.xlsx; else =1 to output only observed results
                         KeyOption,              ## 0=do nothing, 1 = generate correct HAF plot names and set plotkey, 2 = same as 1 but using 
						## contrived plotkeys (associated with using PlotTracking file).  The 2 is used to sanitize the PTS files.
		         retain			##  Retain = 1 to force SetHafPlotID() to retain only matching scored pts (allotment processing), else 0                         
                       )

{

   target<-c("Target Sampled","TS","TARGET SAMPLED")
   projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")



##  II.  Loop thru the tabs in the HAF spreadsheet.  Tabs can be different seasons or different indicators.
##############################################################
## III.  Derive proportional area stats and aerial summaries.

tabcounter<-0;
for(tabnam in tabs) {
   tabcounter<-tabcounter+1		## Accession index into tabprefix[]

   #Re-init strata.spdf using the saved strata file created prior to looping thru the tabs
   strata.spdf<-STRATA			

   ## Read suitability scores & check/get rid of NA Suitability		##MUTARE - make sure the score field is called Suitability
   score<-(read.xlsx(scored,sheetName=tabnam,stringAsFactors=F))
   score<-score[!is.na(score$Suitability) ,]


   if(KeyOption>0) {
	score$PLOTID<-score$Plot.Identifier
   	score<-SetHAFPlotID(wgts.df,score,retain)		## Create and set score$Plot.Identifier to PLOTKEY values
							## Assumes Plot.Identifier contains AIM & LMF plotkey
   }							## Assumes if PlotID used, then there are no LMF HAF scores.
							## The last argument is 1 if you only want to use the matching subset of
							## scored pts (e.g., with allotments), else 0.
							
									

################# 1. Derive proportions with observed pts only
## Eliminate non-TS points
   ts.df<-wgts.df[wgts.df$FINAL_DESIG %in% target,]
   print(paste("Total No. of TS pts=", nrow(ts.df),sep=" "))
   subset<-nrow(ts.df)

## Eliminate points that are not shared between the weight analysis and the HAF scored pts.							
   ts.df<-ts.df[ts.df$PLOTKEY %in% score$Plot.Identifier,]		
   print(paste("New Total No. of TS pts=", nrow(ts.df),sep=" "))
   if(nrow(ts.df)<=0) {
    stop("No points")
   }

## PropEstimates() - generates Normal, Binomial, Goodman proportional estimates and CIs, and outputs results to proportions.csv and _results.xlsx.
##  PropEstimates(list of pt wgts, HAF scores, confidence level for NORMAL, confidence level for BINOMIAL & Goodman,
##                tabprefix, the tab counter,sheetnam1,sheetnam2,append=F or T, Option, VERBOSE)
##  sheetnam1 - tab for storing NORMAL estimates, sheetnam2 - tab for storing BINOMIAL estimates
##  append should be F for the first call to this function (per tab), thereafter =T
##  Option should be "NT" when working with non-target pts (FINAL_DESIG is used as a condition class in PropEstimates), else
##                any other value.
##  VERBOSE is the code for outputing results to _results.xlsx.  We always want output for observed-pt analyses.
##  Returns mydata.cat, but this is not used for anything
 
   obs.ts.df<-ts.df			## Save; obs means only includes observed pts
   sheetname1<-c("OBS_NormalCI")
   sheetname2<-c("OBS_Binomial_Goodman_CI")
   Option<-c("Other")
    ## mydata.cat is returned, but ignore.  Use the mydata.cat in the subsequent call to PropEstimates (all pts) to populate the
    ## pts files at the very end of the loop.

   mydata.cat<-PropEstimates(ts.df,score,conf.level,conf.levelF,tabprefix,tabcounter,sheetname1,sheetname2,F,Option,0)						## MUTARE inside of PropEstimates()

################# 2. Derive proportions with non-target pts
   ts.df<-wgts.df					## re-init
   print(paste("Total No. of TS pts=", nrow(ts.df),sep=" "))
  
   tempts.df<-ts.df[ts.df$PLOTKEY %in% score$Plot.Identifier,]
   print(paste("Returned Total No. of TS pts=", nrow(tempts.df),sep=" "))

   nontarget<-c("Unknown","UNK",NA,"Non-Target","NT","Inaccessible","IA")	## shouldn't have "Not Needed","NN" at this point.
   temp<-ts.df[ts.df$FINAL_DESIG %in% nontarget,]
   if(nrow(temp)>0) {
      temp$PLOTKEY<-temp$PLOTID.SDD			## set PLOTKEY of non-target pts ONLY in this temp version of ts.df, cause
							## PropEstimates() needs NResponse plots to have a PLOTKEY.  ts.df
							## isn't used after the subsequent call to PropEstimates().
      ts.df<-rbind(tempts.df,temp)
   }else {
      ts.df<-tempts.df
   }
   print(paste("New Total No. of pts=", nrow(ts.df),sep=" "))
   if(nrow(ts.df)<=0) {
    stop("No points")
   }

   nt.ts.df<-ts.df					## save; nt refers to including non-target pts
   sheetname1<-c("ALL_NormalCI")
   sheetname2<-c("ALL_Binomial_Goodman_CI")
   Option<-c("NT")			## Indicate we are working with observed + non-target pts
   ## mydata.cat contains PLOTKEY as mysiteID and condition class as CatVar.  Use this one to populate pts files.
   mydata.cat<-PropEstimates(ts.df,score,conf.level,conf.levelF,tabprefix,tabcounter,sheetname1,sheetname2,T,Option,VERBOSE)

################# 3. Update/record total area and area sampled
   ########## observed pts only
   areas.df<-SAVEareas.df
   areas.df<-SumStratWgts(obs.ts.df,areas.df,maxindex)		## Adjust area sampled based on actual observed pts (maybe a subset of 
								## of the original wgts cause some pts maynot be in the HAF scores)
  
   Option<-c("AREA.HA.Sampled") 			## Name used for the area_sampled column
   ## Summarize total and sampled area, record in areasums.csv and _results.xlsx.  The last argument is VERBOSE code, but
   ## we always want to output observed-pt results.  SAME for next call to SumArea().
   summary.df<-SumStratArea(areas.df,RUname,tabprefix,tabcounter,"OBS_RUXStrata_HA",F,Option,0)		## F to open areasums.csv
   obs.summary.df<-summary.df					## Save for use below

   ## summarize across strata totals, record in areasums.csv and _results.xlsx
   areasum.df<-SumArea(areas.df,tabprefix,tabcounter,"OBS_Strata_HA",Option,0)


################# 4. Update/record total area and area sampled using observed + non-target pts
   areas.df<-SAVEareas.df
   areas.df<-SumStratWgts(nt.ts.df,areas.df,maxindex)		## Adjust area sampled based on actual observed pts (may be a subset of 
								## of the original wgts cause some pts maynot be in the HAF scores) PLUS
								## non-target pts.
   Option<-c("AREA.HA.AllPts")	 ## Name used for the area_sampled column
   ## Summarize total and sampled area, record in areasums.csv and _results.xlsx
   summary.df<-SumStratArea(areas.df,RUname,tabprefix,tabcounter,"ALL_RUXStrata_HA",T,Option,VERBOSE)		
   nt.summary.df<-summary.df					## Save for use below

   ## summarize across strata totals, record in areasums.csv and _results.xlsx
   areasum.df<-SumArea(areas.df,tabprefix,tabcounter,"ALL_Strata_HA",Option,VERBOSE)


   ## Print any warning messages
   warnprnt()


## IV.  Derive the shapefile showing the areas with no inference and those with inference
####################################################################### 
## Strata file read in Initialization, clipped into reporting unit pieces, then merged to create a strataXreporting unit spdf
## Re-init strata.spdf for the tab loop, create field Inference, and set all to 0 (no inference).  Then use
## summary.df to set inference by reporting unit & aerial summaries.

## An option (SetPts()) derives the no. of pts by condition class (incl. non-responses) by stratum by reporting unit.  There is no need
##    to repeat the following for int.summary.df given that SetPts output includes aerial extent and tallies of non-response types.

    strata.spdf<-STRATA		## re-init 
    summary.df<-obs.summary.df

    strata.spdf$Inference<-0
    for(i in indexb:indexe) {
      z<-summary.df$WEIGHT.ID[summary.df$REPORTING.UNIT %in% i & summary.df$AREA.HA.Sampled>0]
      strata.spdf$Inference[str_to_upper(strata.spdf$DMNNT_STRTM) %in% z & strata.spdf$RU %in% i]<-1
    }

## Need to dissolve polygons before transfer'n aerial extent info below
     strata.spdf<-flex.dissolve(strata.spdf,temp.path=getwd())

## InferenceArea() - Set inference by reporting unit & aerial summaries into strata.spdf
     strata.spdf<-InferenceArea(strata.spdf,summary.df,RUname)

     ########## Option to insert the tally of pts by reporting unit, by strata, by condition class into strata.spdf (the finalmap shapefile)
     strata.spdf<-SetPts(strata.spdf,wgts.df,score)

        ## Sort this version of strata.spdf by RU
        strata.spdf<- strata.spdf[order(strata.spdf$RU),]
     ##################### END of Option

     fnam<-paste0(tabprefix[tabcounter],"InferMap")			## Create finalmap shapefile
     strata.spdf %>% arc.write (paste(src,fnam,sep="/"), data = .) 

    ## Include the pt tally in the output spreadsheet - somewhat dups "OBS_RUXStrata_HA", but includes non-target pt summaries.
    ## But first we need to generate column totals in strata.spdf (these can not be inserted into the shapefile version - finalmap - so
    ## we do the totals here before writing to the spreadsheet).
    ## NOTE:  If you change the output names in SetPts() then you gotta change the names in SumStrataCols().
    ## Currently, we're set up to always do SetPts()!!!!!!!!!!!!!!!
 
     strata.df<-SumStrataCols(strata.spdf)	## converts spdf to df and sums columns.  Returns a df.

     fnam<-paste0(tabprefix[tabcounter],"_results.xlsx")
     write.xlsx(strata.df,fnam,sheetName="RUXStrata_HA_Npts",col.names=T,row.names=F,append=T,showNA=F)

## V.  Populate pts shapefile with condition class information using mydata.cat from PropEstimates() with all pts 
####################################################################### 
     if(!is.null(pts.spdf)) {
    	 pts<-pts.spdf				## re-init
						## Again, I can not get a short-hand method to work, thus looping approach!!!
     	mydata.cat$id<-as.character(mydata.cat$mysiteID)
     	if(nrow(pts)>0) {		##just to be sure
       		for(i in 1:nrow(pts)) {
         	       if(!is.na(pts$PLOT_KEY[i])) {		## Non-response plotkeys will be NA
           			a<-grep(mydata.cat$id,pattern=pts$PLOT_KEY[i])
           			if(length(a)==1) {
              				pts$CONDITION_CLASS[i]<-mydata.cat$CatVar[a]
           			}
         		}else {			## non-response pt
           			a<-grep(mydata.cat$id,pattern=pts$PLOT_NM[i]) ## pts.PLOT_NM is the SDD plotid; 
							 ## In PropEstimate(), mydata.cat$id(mysiteID) is set to PLOTKEY, 
							 ## BUT NT points contain plotid.
           			if(length(a)==1) {
	      				pts$CONDITION_CLASS[i]<-mydata.cat$CatVar[a]
           			}	
         		}## if then else  
       		 }## for  
     	}## if
 
     	a<-pts[is.na(pts$CONDITION_CLASS) ,]
     	if(nrow(a)>0) {
       	 print(paste("WARNING:  Not all pts have a CONDITION_CLASS. Nrows= ",nrow(a),sep=" "))
     	}

     	## Output pts shapefile.  First cleanup plotkey info if necessary
        if(KeyOption==2){
		pts$PLOT_KEY<-pts$PLOT_KEY_SAVE
 		pts$PLOT_KEY_SAVE<-NULL
	}
     	fnam<-paste0(tabprefix[tabcounter],"PTS")			## Create PTS shapefile
     	pts %>% arc.write (paste(src,fnam,sep="/"), data = .) 
     }  ## end of if null

} ## end of tabnam in tabs LOOP

  dummy<-1
  return(dummy)				## May be used to indicate proper execution, but for now means nothing!!
}

