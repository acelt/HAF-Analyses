## PROPORTION.R - DERIVES AERIAL PROPORTIONS using output from AIMWEIGHTS.
## THIS IS THE "MAIN" module that mediates processing.
## Modify a copy of this module for specific applications (MUTARE highlights where application-specific mods are required)

## Libraries and functions are included in PALL.R

## Examples are from CowLakes-PAC (spring&summer) -  Version 11/20/2017

## I.  INITIALIZATION
###########################################################   
## 1. Set working directory and standard projection

   target<-c("Target Sampled","TS","TARGET SAMPLED")
   projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")


## set working directory
   setwd("c:/projects/fohaf/or/cowlakes/pac")				## MUTARE

## set source
   src<-getwd()

## Set confidence interval						## MUTARE
   conf.level<-80			## For both Normal and Binomial
   conf.levelF<-conf.level/100		## Fractional representation

## 2. Set clipstrata for the finalmap shapefile. 

## Inference Area map Making:  Specify the final clip file (habitat area) when making a shapefile showing inference areas.
## Also add the path.
    clipstrata<-c("CowLakes_sprsum_sma")				## MUTARE
    clipstratapath<-c("c:/projects/fohaf/or/cowlakes/pac")		## MUTARE
    habitat<- readOGR(dsn=clipstratapath,layer=clipstrata,stringsAsFactors=FALSE)  
    names(habitat@data) <- str_to_upper(names(habitat@data)) 
    habitat<- spTransform(habitat, projection)  


##### Lists in #3-4 below need to be the same length.  			## MUTARE
## 3.  Specify/Set reporting unit codes, reporting units, and names of the reporting units (which are inserted into finalmap)
    RU<-c(1,2,3,4,5,6)				## reporting unit code added to a temp copy of strata 
    clip<-c("r1ru.shp","r2ru.shp","r3ru.shp","r4ru.shp","r5ru.shp","r6ru.shp")	## the full suite of reporting units to clip from and re-add to strata.  
    RUname<-c("PACI","PACII","OwyheeI","OwyheeII","WinterI","WinterII")  ## User-specified names for each reporting unit


									## MUTARE
## 4. Specify all _wgt_ and _ptstally_ files for the seasonal habitat being processed.
## Specify all wgt files.  Use NA where a WGT file doesn't exist - same for the following PTSTALLY files.
    wgts<-c("run1ORWA_PAC_2016-2020_SDD.gdb_wgt_2017-11-24",
            "run2ORWA_PAC_2016-2020_SDD.gdb_wgt_2017-11-24",
            "run3ID_OwyheeFieldOffice_2016_DD.gdb_wgt_2017-11-13",
            "run4ID_OwyheeFieldOffice_2016_DD.gdb_wgt_2017-11-13",
            "run5ID_OwyheeFieldOffice_2016_DD.gdb_wgt_2017-11-13",NA)


									## MUTARE
## Specify all PTSTALLY files - these are used to tally the total area by stratum and the total area actually sampled by stratum
   ptstally<-c("run1ORWA_PAC_2016-2020_SDD.gdb_ptstally_2017-11-24",
            "run2ORWA_PAC_2016-2020_SDD.gdb_ptstally_2017-11-24",
            "run3ID_OwyheeFieldOffice_2016_DD.gdb_ptstally_2017-11-13",
            "run4ID_OwyheeFieldOffice_2016_DD.gdb_ptstally_2017-11-13",
            "run5ID_OwyheeFieldOffice_2016_DD.gdb_ptstally_2017-11-13",NA)

									## MUTARE
## Specify the nopoints list - spelling counts!
   nopoints<-c(NA,NA,NA,NA,NA,"run6NOPOINTS")

									## MUTARE
## Specify resetting Wgt.Category (strata names) in WGTS file.  When a sample frame is used in AIMWEIGHTS, 
##         Wgt.Category is set to NA.  In such cases, set the strata to a seqquential number (e.g., reflecting the LMF strata - 1 or 2).
   Catwgt<-c(NA,NA,NA,NA,1,2)		# 1 - LMF Type I, 2- LMF TYPE II


## TO pullout specific combinations, set indexb, indexe (e.g., indexb=1 indexe=3 will use first 3 entries; indexb=2 indexe=2 will only use the second entry)
## indexb and indexe affects selection of RU, clip, RUname, wgts, ptstally, nopoints, and Catwgt (see above).  ADJUST strata file entries (see BELOW)
## to match the entries affected by indexb, indexe.

									## MUTARE
   indexb<-1
   indexe<-2



## 5.  Specify the excel spreadsheet containing scored HAF points (scored), the tabs in the excel to process (tabs),
##     and set the file prefix for proportions.csv, areasums.csv, final.shp, and _results.xlsx (tabprefix) corresponding to
##     the sequential order in tabs.
									## MUTARE
    scored<-c("c:/projects/fohaf/or/cowlakes/pac/CowLakesPAC_SpringSummer_DRAFT_NOC_HAF_Analysis_Request_v2.xlsx")
    tabs<-c("PAC_SprSum_SageCover","PAC_SprSum_HerbaceousCover","PAC_SprSum_HerbaceousHt","PAC_SprSum_ForbRichness",
             "PAC_SprSum_GrassRatio")
    tabprefix<-c("PACSagecover","PACHerb","PACHerbHt","PACForb","PACGrassR")



## 6.  Specify and ingest the strata file(s) if it exists, and/or the sample frame(s).  Create strata.spdf which results from
##     binding everything specified in stratpath/stratlayer.  stratpath, stratlayer, and stratcode must be the same length, and
##     sequential order is related among all 3.  

 								## MUTARE - path for r#ru's
   stratpath<-c("V:/ORWA/State/PAC/Data/Sample Design/2016","V:/ID/Owyhee_FO/Data/SampleDesign/2016/ID_OwyheeFieldOffice_2016_DD.gdb",
                 "c:/projects/fohaf/or/cowlakes/winter","c:/projects/fohaf/or/cowlakes/winter")
   stratlayer<-c("OR_PAC_Terrestrial_Strata","Terra_Strtfctn","r5ru","r6ru")
   stratcode<-c(NA,NA,1,2)	## Reset DMNNT_STRTM codes, especially when using reporting units created in AIMWEIGHTS 
			        ## (may lack a DMNNT_STRTM OR STRATUM field)


   stratpath<-c("V:/ORWA/State/PAC/Data/Sample Design/2016")
   stratlayer<-c("OR_PAC_Terrestrial_Strata")
   stratcode<-c(NA)	## DMNNT_STRTM codes, especially in case r6ru doesn't have a DMNNT_STRTM OR STRATUM field

   strata.spdf<-NULL
   for(i in 1:length(stratpath)) {
   	temp<- readOGR(dsn=stratpath[i],layer=stratlayer[i],stringsAsFactors=FALSE)  
        temp<- spTransform(temp, projection)  
   	names(temp@data) <- str_to_upper(names(temp@data)) 

   	d1=is.null(temp$DMNNT_STRTM)
   	d2=is.null(temp$STRATUM)
   	if(d1==TRUE) {			## IF DMNNT_STRTM doesn't already exists
        	if(d2==TRUE){		## And if STRATUM doesn't already exists then create DMNNT_STRTM field and set
			temp$DMNNT_STRTM<-stratcode[i]
                        print(paste("Adding DMNNT_STRTM field; stratum= ",stratcode[i],sep=" "))
                }else {
			## The stratum field must be called DMNNT_STRTM
   			names(temp)[names(temp) == "STRATUM"] <- "DMNNT_STRTM" 
		}
	}


## special fix for Owyhee
        if(i==2)temp$DMNNT_STRTM[1]<-"OTHER"

        a<-temp[, c("DMNNT_STRTM")]

## clip upfront to save time, but the r#ru files don't need to be clipped
        if(i<=2) {
        	a<-flex.clip(a,habitat,method="arcpy",temp.path=getwd())
        	a<- spTransform(a, projection)  
	}

## save/create strata.spdf
        if(i==1) {
          strata.spdf<-a
        }else {
          strata.spdf<-rbind(strata.spdf,a)
        }
   } ## length(stratpath)

     stratumfieldname = "DMNNT_STRTM"
     strata.spdf@data[, stratumfieldname] <- str_to_upper(strata.spdf@data[, stratumfieldname])

  # SAVE strata 
     STRATA<-strata.spdf    


##  7. Divide the strata file into strata by reporting unit
##     If there is no strata (e.g., a frame was used), then just add and set field $RU<-RU[index]
       strata.spdf<-StrataRU(STRATA,clip,indexb,indexe,RU)
       STRATA<-strata.spdf		## Save again for later use
 

##  8.  Ingest the wgt files and filter-out REPEAT>0 (i.e., only use first-time measures)
## Ingest all weight files
   wgts.df<-NULL
   temp.df<-NULL
   index<-0
   for(s in wgts) {
     index<-index+1
     if(index>= indexb & index<=indexe) {
        if(!is.na(s)) {			## Can have NA entries
     		infile<-read.csv(s,header=T)
              ##  infile$Wgt.Category<-paste0(infile$Wgt.Category,index)  ## Creates unique strata by reporting unit
      		temp.df <- infile[, c("DATASRC","PRIMARYKEY","PLOTID.SDD","FINAL_DESIG","Wgt.Category","WGT","REPORTING.UNIT","XMETERS","YMETERS","REPEAT")]
                ## Check to see if we need to reset Wgt.Category which can happen when working with LMF pts outside of an AIM project area.
                if(!is.na(Catwgt[index])) {
                    temp.df$Wgt.Category[is.na(temp.df$Wgt.Category)]<-as.character(Catwgt[index])
                }
      		wgts.df<-rbind(wgts.df,temp.df)
	}
     }
   }
   wgts.df<-wgts.df[wgts.df$REPEAT==0 ,]		## for now only use the initial measures

   wgts.df<-SetAIMPlotKey(wgts.df)			## As a convention, we'll use plotkey for the cross-walk between HAF scores and
						        ## the weighted points.  wgts files likely will only have PRIMARYKEY for AIM, so convert
							## PRIMARYKEY to PLOTKEY.  PRIMARYKEY of LMF plots are set; PLOTID of LMF pts is set to
							## LMF.  Non-target pts have a PLOTID (PLOTID.SDD) but NA for PRIMARYKEY.

							## SetAIMPlotKey() creates and sets wgts.df$PLOTKEY.  PLOTKEY will have the PLOTKEY for AIM
							## pts.  PRIMARYKEY is transferred to PLOTKEY and to PLOTID.SDD for LMF pts. 
							## Non-target pts should have NA for PRIMARYKEY and PLOTKEY.     


##  9.  Ingest ptstally and NOPOINTS (if any) files
   areas.df<-NULL
   temp.df<-NULL
   index<-0
   for(s in ptstally) {
      index<-index+1
      if(index>=indexb & index<=indexe) {
        if(!is.na(s)) {			## Can have NA entries
      		infile<-read.csv(s,header=T)
        	infile$REPORTING.UNIT<-index				## WHere we number the reporting unit of the ptstally file which lack the RU field, so by accession
      		temp.df <- infile[, c("WEIGHT.ID","AREA.HA.Total","AREA.HA.sampled","REPORTING.UNIT")]
                ## Check to see if we need to reset
                if(!is.na(Catwgt[index])) {
                    temp.df$WEIGHT.ID<-as.character((Catwgt[index]))
                } 
     		areas.df<-rbind(areas.df,temp.df)
	}
      }
   }
   maxindex<-index		## max. no. of reporting units


## If there are nopoints[] info (e.g., strata no. and area extent) to add to total
   for( index in indexb:indexe) {
      if(!is.na(nopoints[index])) {
      	infile<-read.csv(nopoints[index],header=T)
      	infile$REPORTING.UNIT<-index					## index equates to the reporting unit number 
      	temp.df<-infile[, c("Strata","Area.ha","REPORTING.UNIT") ]
      	names(temp.df) <- (names(temp.df)) %>% str_to_upper()
      	temp.df[, "STRATA"] <- str_to_upper(temp.df[, "STRATA"])
     	 b<-data.frame(WEIGHT.ID=temp.df$STRATA,AREA.HA.Total=temp.df$AREA.HA,AREA.HA.sampled=0,REPORTING.UNIT=temp.df$REPORTING.UNIT)
      	areas.df<-rbind(areas.df,b)
      }
   }
   SAVEareas.df<-areas.df		## Strata area - Used to re-init within the subsequent loop



##  10.  Ingest, concatenate all relevant pts files output by AIMWEIGHTS.  Condition class is added to each pt and output.  A pts file is generated
         ## for each tab in the HAF spreadsheet.  Assumes path is getwd().  NOTE:  shapefiles ending in _SDD can't be read using readOGR
         ## for some reason??  Need to make sure the _SDD is dropped in AIMEIGHTS.R which produces these input PTS files.
    pts.spdf<-NULL					## leave as NULL if you don't want to generate pts maps with condition_class
    ptsfiles<-c("run1PTS_ORWA_PAC_2016-2020","run2PTS_ORWA_PAC_2016-2020")		## MUTARE
    for(s in ptsfiles) {
      pts<- readOGR(dsn=getwd(),layer=s,stringsAsFactors=FALSE)  
      names(pts@data) <- str_to_upper(names(pts@data)) 
      pts<- spTransform(pts, projection)
      if(s==ptsfiles[1]) {
      	pts.spdf<-pts			## can't get rbind or spRbind to initially work....
      }else {
        pts.spdf<-rbind(pts.spdf,pts)
      }
    }
    pts.spdf$CONDITION_CLASS<-"NA"	## Init condition class.  This is the master copy, so this spdf is never modified.



##  11.  This version generates results for observed pts only, and for all pts combined.  There may be instances where
##       only observed-pt info should be saved in the _results.xlsx files, which tend to be the primary output sent to FOs
##       for their subsequent analyses.  Also, there may be instances where an analyst would like to see both observed and all pts
##       results.  Setting VERBOSE = 1 will output info only for observed-pt analyses.  VERBOSE=0 will output observed and observed
##       + non-response pt analyses.  NOTE:  AAAproportions.csv and AAAareasums.csv (AAA is the designated file prefix) files are not
##       affected by VERBOSE - they always will accumulate results for observed and observed+non-response assessments.  These
##       .csv files are currently just another way to store results, but are not the formal results sent to FOs.  

         VERBOSE<-0						## MUTARE




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
									## MUTARE - check on the following in HAF scores
   score<-SetHAFPlotID(wgts.df,score)				        ## Create and set score$Plot.Identifier to PLOTKEY values
									## Assumes Plot.Identifier contains AIM & LMF plotkey
									## Assumes if !Plot.Identifier, then PlotID is used.
									## Assumes if PlotID used, then there are no LMF HAF scores.
									## MUTARE - this function has not been checked out!!!!!!!!!!!!!

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

## PropEstimates() - generates Normal, Binomial, and Goodman proportional estimates and CIs, and outputs results to proportions.csv and _results.xlsx.
##  PropEstimates(list of pt wgts, HAF scores, confidence level for NORMAL, confidence level for BINOMIAL & Goodman,
##                tabprefix, the tab counter,sheetnam1,sheetnam2,append=F or T, Option, VERBOSE)
##  sheetnam1 - tab for storing NORMAL estimates, sheetnam2 - tab for storing BINOMIAL estimates
##  append should be F for the first call to this function (per tab), thereafter =T
##  Option should be "NT" when working with non-target pts (FINAL_DESIG is used as a condition class in PropEstimates), else
##         any other value.
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

     fnam<-paste0(tabprefix[tabcounter],"finalmap")			## Create finalmap shapefile
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

     	## Output pts shapefile
     	fnam<-paste0(tabprefix[tabcounter],"PTS")			## Create finalmap shapefile
     	pts %>% arc.write (paste(src,fnam,sep="/"), data = .) 
     }  ## end of if null

} ## end of tabnam in tabs LOOP


