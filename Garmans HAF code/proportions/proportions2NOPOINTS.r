## PROPORTION2NOPOINTS.R - DERIVES AERIAL PROPORTIONS using output from AIMWEIGHTS.
## THIS IS THE "MAIN" module that mediates processing.
## Modify a copy of this module for specific applications (MUTARE highlights where application-specific mods are required)

## Libraries and functions are included in libfunc2.r

## Warner, OR.  12/15/2017

## I.  INITIALIZATION
###########################################################   
## 1. Set working directory and standard projection

   target<-c("Target Sampled","TS","TARGET SAMPLED")
   projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")


## set working directory
   setwd("c:/projects/fohaf/or/warner/winterallot2")				## MUTARE

## set source
   src<-getwd()

## Set confidence interval						## MUTARE
   conf.level<-80			## For both Normal and Binomial
   conf.levelF<-conf.level/100		## Fractional representation


## 2. Set clipstrata for the finalmap shapefile. 

## Inference Area map Making:  Specify the final clip file (habitat area) when making a shapefile showing inference areas.
## Also add the path.
    clipstrata<-c("winterSMA")						## MUTARE
    clipstratapath<-c("c:/projects/fohaf/or/warner/haf")		## MUTARE
    habitat<- readOGR(dsn=clipstratapath,layer=clipstrata,stringsAsFactors=FALSE)  
    names(habitat@data) <- str_to_upper(names(habitat@data)) 
    habitat<- spTransform(habitat, projection)  


##### Lists in #3-4 below need to be the same length.  			## MUTARE
## 3.  Specify/Set reporting unit codes, reporting units, and names of the reporting units (which are inserted into finalmap)
    RU<-c(1,2,3,4)				## reporting unit code added to a temp copy of strata 
    clip<-c("r1ru.shp","r2ru.shp","r3ru.shp","r4ru.shp")	## the full suite of reporting units to clip from and re-add to strata.  
    RUname<-c("IntensiveAll","IntensiveLakeView","SFA_Lakeview","Lakeview")  ## User-specified names for each reporting unit


									## MUTARE
## 4. Specify all _wgt_ and _ptstally_ files for the seasonal habitat being processed.
## Specify all wgt files.  Use NA where a WGT file doesn't exist - same for the following PTSTALLY files.
    wgts<-c("run1OR_LakeviewDO_2016_2020_sdd.gdb_wgt_2017-12-17")
#,
 #            "run2OR_LakeviewDO_2016_2020_sdd.gdb_wgt_2017-12-16",
  #           "run3OR_LakeviewDO_2016_2020_sdd.gdb_wgt_2017-12-16",
#	     "run4OR_LakeviewDO_2016_2020_sdd.gdb_wgt_2017-12-16")
									## MUTARE
## Specify all PTSTALLY files - these are used to tally the total area by stratum and the total area actually sampled by stratum
   ptstally<-c("run1OR_LakeviewDO_2016_2020_sdd.gdb_ptstally_2017-12-17")
#,
 #            "run2OR_LakeviewDO_2016_2020_sdd.gdb_ptstally_2017-12-16",
  #           "run3OR_LakeviewDO_2016_2020_sdd.gdb_ptstally_2017-12-16",
   #          "run4OR_LakeviewDO_2016_2020_sdd.gdb_ptstally_2017-12-16")

									## MUTARE
## Specify the nopoints list - spelling counts!
   nopoints<-c(NA,NA,NA,NA)
   nopoints<-c(NA,"run2NOPOINTS")
									## MUTARE
## Specify resetting Wgt.Category (strata names) in WGTS file.  When a sample frame is used in AIMWEIGHTS, 
##         Wgt.Category is set to NA.  Else, set the strata to a sequential number (e.g., reflecting the LMF strata - 1 or 2).
   Catwgt<-c(NA,NA,NA,NA)		# 1 - LMF Type I, 2- LMF TYPE II


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
    scored<-c("c:/projects/fohaf/or/warner/haf/OR_2017_Warners_NOC_HAF_Analysis_Request_20171206_v2.xlsx")
    #tabs<-c("S-3 Nesting Early Brood Rearing")
    #tabs<-c("S-4 Upland Summer Late Brood")
    tabs<-c("S-6 Winter")
    #tabs<-c("S-4 Combined Grass_Forbs")
    tabprefix<-c("WinterAllotII")




## 6.  Specify and ingest the strata file(s) if it exists, and/or the sample frame(s).  Create strata.spdf which results from
##     binding everything specified in stratpath/stratlayer.  stratpath, stratlayer, and stratcode must be the same length, and
##     sequential order is related among all 3.  

 								## MUTARE - path for r#ru's
   #stratpath<-c("V:/ORWA/State/PAC/Data/Sample Design/2016","V:/ID/Owyhee_FO/Data/SampleDesign/2016/ID_OwyheeFieldOffice_2016_DD.gdb",
   #              "c:/projects/fohaf/or/cowlakes/winter","c:/projects/fohaf/or/cowlakes/winter")
   #stratlayer<-c("OR_PAC_Terrestrial_Strata","Terra_Strtfctn","r5ru","r6ru")
   #stratcode<-c(NA,NA,1,2)	## Reset DMNNT_STRTM codes, especially when using reporting units created in AIMWEIGHTS 
			        ## (may lack a DMNNT_STRTM OR STRATUM field)


   stratpath<-c("c:/projects/fohaf/or/warner/haf")
   stratlayer<-c("lv_strata_terse")		#_v2 for spring, else
   stratcode<-c(NA)	## DMNNT_STRTM codes, especially in case r6ru doesn't have a DMNNT_STRTM OR STRATUM field

   strata.spdf<-NULL
   for(i in 1:length(stratpath)) {
   	temp<- readOGR(dsn=stratpath[i],layer=stratlayer[i],stringsAsFactors=FALSE)  
        temp<- spTransform(temp, projection)  
   	names(temp@data) <- str_to_upper(names(temp@data)) 

        if(!is.null(temp$DMNNT_STRT))temp$DMNNT_STRTM<-temp$DMNNT_STRT
   	d1=is.null(temp$DMNNT_STRTM)
   	d2=is.null(temp$STRATUM)
   	if(d1==TRUE) {			## IF DMNNT_STRTM doesn't already exists
        	if(d2==TRUE){		## And if STRATUM doesn't already exists then create DMNNT_STRTM field and set
			temp$DMNNT_STRTM<-stratcode[i]
                        print(paste("Do you know we are adding DMNNT_STRTM field; stratum= ",stratcode[i],sep=" "))
                }else {
			## The stratum field must be called DMNNT_STRTM
   			names(temp)[names(temp) == "STRATUM"] <- "DMNNT_STRTM" 
		}
	}


## special fix for Owyhee
        if(i==2)temp$DMNNT_STRTM[1]<-"OTHER"

        a<-temp[, c("DMNNT_STRTM")]

## clip upfront to save time, but the r#ru files don't need to be clipped
        if(i==2) {
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

    ptsfiles<-c("run1PTS_OR_LakeviewDO_2016_2020","run2PTS_OR_LakeviewDO_2016_2020","run3PTS_OR_LakeviewDO_2016_2020",
                 "run4PTS_OR_LakeviewDO_2016_2020")	 			## MUTARE

    ptsfiles<-c("run1PTS_OR_LakeviewDO_2016_2020")
#,"run2PTS_OR_LakeviewDO_2016_2020")

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
    
    ## Need to sanitize names when using writeOGR
       if(!is.null(pts.spdf$PLOT_KE)) {
            names(pts.spdf)[names(pts.spdf) == "PLOT_KE"] <- "PLOT_KEY"
	    names(pts.spdf)[names(pts.spdf) == "FINAL_D"] <- "FINAL_DESI"
       }



##  11.  Here is where we can add PLOTKEY values WHEN we are dealing with TS pts that lack a primary and plot key.  This
##       occurs whenever we are working with a PlotTracking spreadsheet (the plot fates are not yet in the SDD or TerrADat
##       and so the keys also are not yet set).
##       See the decision tree below for the options here and what you should pass to GenProportions:

         KeyOption<-0
         KeyOption<-1
         KeyOption<-2

         if(KeyOption==2) {
         	wgts.df<-SetWgtKeys(wgts.df)		## Add fake plotkeys for pts not yet in SDD/TerrADat
                pts.spdf<-SetPtKeys(pts.spdf,wgts.df)   ## Add these fake keys to the SDD pts file. Function inside setwgtkeys.r file.
         }    
	
	

##  12.  GenProportions generates results for observed pts only, and for all pts combined.  There may be instances where
##       only observed-pt info should be saved in the _results.xlsx files, which tend to be the primary output sent to FOs
##       for their subsequent analyses.  Also, there may be instances where an analyst would like to see both observed and all pts
##       results.  Setting VERBOSE = 1 will output info only for observed-pt analyses.  VERBOSE=0 will output observed and observed
##       + non-response pt analyses.  NOTE:  AAAproportions.csv and AAAareasums.csv (AAA is the designated file prefix) files are not
##       affected by VERBOSE - they always will accumulate results for observed and observed+non-response assessments.  These
##       .csv files are currently just another way to store results, but are not the formal results sent to FOs.  

         VERBOSE<-1						## MUTARE
	 Retain<-1						## MUTARE

	 ## GenProportions loops thru each tab in the HAF spreadsheet, derives proportional estimates and CIs, and output results to
         ## proportions.csv, areasums.csv, _results, _InferMap.shp, and _PTS.shp (if ptsfiles is !NULL).  All files have the prefix specified
         ## in tabprefix.
         dummy<-GenProportions(src,		## working directory
                         conf.level,		## integer confidence level
                         conf.levelF,		## conf.level as a fraction
                         RUname,		## user-specified names for each reporting unit
                         wgts.df,		## pts wgts generated in AIMWEIGHTS
                         indexb,		## beginning accession index into RUnam - aka. beginning repoting unit 
                         indexe,		## ending accession index
                         scored,		## essentially the HAF spreadsheet (path and file name)
                         tabs,			## name of the tabs in the HAF spreadsheet
                         tabprefix,		## user-generated prefix for each tab - prefix of all output for a tab
                         STRATA,		## processed strata file
                         maxindex,		## max number of reporting units
                         SAVEareas.df,		## strata area summary
                         pts.spdf,		## NULL or the collection of pts files for this analysis generated in AIMWEIGHTS.R
                         VERBOSE,		## =0 to output observed and allpts results to _results.xlsx; else =1 to output only observed results
  			 KeyOption,		## =0 for do nothing, else see decision matrix below
			 Retain			## =1 to force retention of only score HAF pts that match wgts.df (allotment processing), else 0
						## when =1, you'll still get error messages re: no match between HAF pts and wgts.df pts, but ignore..
                         )



## Don't worry about non-target plot codes.  PlotID.SDDs (plot names) are shoved into plotkey fields where needed...  
##              SDD PLOTKEY	   HAF SpreadSheet Plot.Identifier	KeyOption - action occurrs in SetHAFPlotID(wgts.df,score) within GenProportions.
##              all plotkeys	      plotkey				  0 - do nothing
##		all plotkeys	      plotname				  1 - derive correct HAF plotname, derive & set plotkey into Plot.Identifier
##              only some plotkeys    plotname			          2  - same as 1, but assumes SetKeys() was used to ADD fake keys where needed
##								               to both wgts.df and pts.spdf before call to GenProportions.  Pts.spdf     	
##									       will need cosmetic changes before outputing the pts shapefile.
##                                                                        ?? May have a situation where Plot.Identifier is primarykey, SO a #3 to come?









