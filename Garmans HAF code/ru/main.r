## ru.r - quick and easy approach to deriving reporting units for a HAF assessment

## INITIALIZATION
#############################################################################

## 1. Set working directory et al.
   	setwd("c:/projects/fohaf/or/warner/summer")		## MUTARE spring, summer, winter

## 2.  Ingest and concatenate all of the LMF strata necessary for the AOI. THESE MUST BE already clipped to SMA.  Must be a field called stratum/STRATUM
   	lmf.src<-NULL				## IF LMF is not used, then leave lmf.scr set to NULL
        #lmf.src<-
        #data.src<-

## 3.  Ingest the Habitat file, clipped to SMA
        sf <-"c:/projects/fohaf/or/warner/summer/summerSMA.shp"  	##  MUTARE  springSMA.shp, summerSMA.shp, winterSMA.shp

         ## Creates an SPDF 
         assign(x = "habitat",
         value = sf %>% arc.open() %>% arc.select %>%
         SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .))
	 habitat<-spTransform(habitat,projection)
         SAVEhabitat<-habitat					## Unaltered habitat file


## 4.  Get the SDD frames  "Terra_Sample_Frame"

         clip.path<-c( "V:/ORWA/State/SFA/Data/Sample Design/2016/ORWA_SFA_SampleDesignDatabase_2016_2020.gdb",
                      "V:/ORWA/Lakeview_DO/Data/SampleDesign/2016/OR_LakeviewDO_2016_2020_SDD.gdb",
                      "c:/projects/fohaf/or/warner/haf","c:/projects/fohaf/or/warner/haf")
	 clip.layer<-c("Terra_Sample_Frame","Terra_Sample_Frame","allot1SMA","allot2SMA")

         clip.RU<-c("inter1","r1ru","r2ru","r3ru","r4ru","r5ru","r6ru")	## set reporting unit names and codes that are generated 
         ru<-c("RU1","RU1","RU2","RU3","RU4","RU5","RU6")

         erase<-c("r1ru.shp","r2ru.shp","r3ru.shp","r4ru.shp")			## set shapefiles names used to erase 
         cleanup<-c("r1ru","r2ru","r3ru","r4ru")				## Add on - cleans up reporting unit files to faciliate use in weighting.

############# Init
   	src<-getwd() %>% sanitizer(type = "filepath")
   	projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
  	## readOGR() wrapped in safely() so that it will return NULL instead of an error
  	safe.readOGR <- safely(readOGR, otherwise = NULL)
  	totalarea<-0		## records final area of all reporting units

        lmfstrata<-NULL
        if(!is.null(lmf.src))lmfstrata<-IngestLMF(lmf.src,data.src)


############### 

###############  Clip, erase, create reporting units      
         ##Clip(AOI, lmfstrata, the 2 clip lists, index of clip lists to use, list of RUs and ru codes, begin & end sequence of clip.RU and ru that will be created)
         ## E.g., Clip(habitat,lmfstrata,clip.path.clip.layer,clip.code=1,clip.RU,ru,begin=1,end=2,framename=NA or name) - clip.code means use the first frame in clip.path, 
									          ## clip habitat by that frame, then
		  								  ## clip to LMF I and then LMF II to create r1ru.shp, r2ru.shp which
										  ## will be attributed as RU1 and RU2.  
                                                                                  ## Generated Reporting units are written to disk within getwd() according
 										  ## to name in clip.RU.
										  ## set clip.code to 0 to use habitat as the reporting unit to clip or
									          ## as the reporting unit to save.
										  ## framename =NA to use the entire frame, else ID of the frame to use.
										  ## Clip does not return the affected habitat.
										  ## NOTE:  IF LMF is not used, then the LMF clip is not invoked.

	 ## For some applications, you'll need to pick up a non-reporting unit clipping then apply it in another clipping to get the
         ## actual reporting unit.  Set-up clip.RU[] and ru[] to generate interim clips and the final reporting unit names.



	## Clip habitat to SFA
        ## then clip to intensive = r1ru

         totalarea<-0
	 area<-Clip(habitat,lmfstrata,clip.path,clip.layer,1,clip.RU,ru,1,1,NA)	## return sum area of generated reporting units
         new<-ReadShapefile("inter1.shp")	
	 new<-spTransform(new,projection)
	 area<-Clip(new,lmfstrata,clip.path,clip.layer,2,clip.RU,ru,2,2,"ORWA_LakeviewDistrictOffice_SampleFrame_2")
         totalarea<-totalarea+area

        ## Clip habitat to LV
        ## then clip to intensive area
        ## then erase r1ru to create r2ru - intensive-LV

	 area<-Clip(habitat,lmfstrata,clip.path,clip.layer,2,clip.RU,ru,1,1,NA)	
         new<-ReadShapefile("inter1.shp")	
	 new<-spTransform(new,projection)
	 area<-Clip(new,lmfstrata,clip.path,clip.layer,2,clip.RU,ru,1,1,"ORWA_LakeviewDistrictOffice_SampleFrame_2")
         new<-ReadShapefile("inter1.shp")	
	 new<-spTransform(new,projection)
         new<-Erase(new,erase,1,1)
         area<-SaveShapefile(new,clip.RU,ru,3,3)	## use the 3rd entry in clip.ru and ru to set RU and filename to save
         totalarea<-totalarea+area

         ## Clip habitat to SFA
         ## then erase r1ru and r2ru to produce r3ru which is SFA/LV outside of intensive (since LV encompasses SFA, need to clip to LV)

	 area<-Clip(habitat,lmfstrata,clip.path,clip.layer,1,clip.RU,ru,1,1,NA)	## return sum area of generated reporting units
         new<-ReadShapefile("inter1.shp")	
	 new<-spTransform(new,projection)
         new<-Erase(new,erase,1,2)
         area<-SaveShapefile(new,clip.RU,ru,4,4)	## use the 4th entry in clip.ru and ru to set RU and filename to save
         totalarea<-totalarea+area

 
         ## Clip habitat to LV, frame 1
         ## erase r1ru, r2ru, r3ru to create r4ru which is LV outside of intensive

	 area<-Clip(habitat,lmfstrata,clip.path,clip.layer,2,clip.RU,ru,1,1,"ORWA_LakeviewDistrictOffice_SampleFrame_1")
         new<-ReadShapefile("inter1.shp")	
	 new<-spTransform(new,projection)
         new<-Erase(new,erase,1,3)		## erase using entries 1 thru 3 in erase
         area<-SaveShapefile(new,clip.RU,ru,5,5)	## use the 5th entry in clip.ru and ru to set RU and filename to save
         totalarea<-totalarea+area

        # area<-Clip(habitat,lmfstrata,NULL,NULL,0,clip.RU,ru,4,4)	# Another way to pass Clip a file to save



         ## Tally, compare acreages (ha) for situations where you consume the entire habitat area with reporting units
    	 SAVEhabitat <- area.add(spdf = SAVEhabitat,T,T)
         print(paste0("Habitat area= ",sum(SAVEhabitat$AREA.HA)))
         print(paste0("Sum area of RUs= ",totalarea))

         dummy<-CleanUp(cleanup)				## Will read & cleanup files (.shp) incl.dissolving, and re-store RUs as shapefiles



  	##############################################################
         ## Re-init habitat and generate reporting units for areas (e.g., allotments) that do not consume the entire habitat area.
 
     	 setwd("c:/projects/fohaf/or/warner/summerallot1")		## MUTARE spring, summer, winter

         habitat<-SAVEhabitat

         ## Clip habitat to allot1
         ## then clip to SFA
         ## then clip to intensive to create r1ru

         totalarea<-0
	 area<-Clip(habitat,lmfstrata,clip.path,clip.layer,3,clip.RU,ru,1,1,NA)		## overwrite inter1
         new<-ReadShapefile("inter1.shp")
	 new<-spTransform(new,projection)
         SAVENEW<-new				## Allotment clipped to habitat
	 area<-Clip(new,lmfstrata,clip.path,clip.layer,1,clip.RU,ru,1,1,NA)		## overwrite inter1
         new<-ReadShapefile("inter1.shp")
	 new<-spTransform(new,projection)
	 area<-Clip(new,lmfstrata,clip.path,clip.layer,2,clip.RU,ru,2,2,"ORWA_LakeviewDistrictOffice_SampleFrame_2")
         totalarea<-totalarea+area

         ## Use allotment clipped to habitat and erase r1ru to create r2ru

         new<-Erase(SAVENEW,erase,1,1)
	 area<-Clip(new,lmfstrata,clip.path,clip.layer,2,clip.RU,ru,3,3,"ORWA_LakeviewDistrictOffice_SampleFrame_2")
         totalarea<-totalarea+area

    	 SAVENEW <- area.add(spdf = SAVENEW,T,T)
         print(paste0("Habitat area= ",sum(SAVENEW$AREA.HA)))
         print(paste0("Sum area of RUs= ",totalarea))

         cleanup<-c("r1ru","r2ru")
         dummy<-CleanUp(cleanup)




         ## again for the other allotment

     	 setwd("c:/projects/fohaf/or/warner/summerallot2")		## MUTARE spring, summer, winter

         habitat<-SAVEhabitat

         ## Clip habitat to allot2
         ## then clip to SFA
         ## then clip to intensive to create r1ru

         totalarea<-0
	 area<-Clip(habitat,lmfstrata,clip.path,clip.layer,4,clip.RU,ru,1,1,NA)		## overwrite inter1
         new<-ReadShapefile("inter1.shp")
	 new<-spTransform(new,projection)
         SAVENEW<-new				## Allotment clipped to habitat
	 area<-Clip(new,lmfstrata,clip.path,clip.layer,1,clip.RU,ru,1,1,NA)		## overwrite inter1
         new<-ReadShapefile("inter1.shp")
	 new<-spTransform(new,projection)
	 area<-Clip(new,lmfstrata,clip.path,clip.layer,2,clip.RU,ru,2,2,"ORWA_LakeviewDistrictOffice_SampleFrame_2")
         totalarea<-totalarea+area

         ## Use allotment clipped to habitat and erase r1ru to create r2ru

         new<-Erase(SAVENEW,erase,1,1)
	 area<-Clip(new,lmfstrata,clip.path,clip.layer,2,clip.RU,ru,3,3,"ORWA_LakeviewDistrictOffice_SampleFrame_2")
         totalarea<-totalarea+area

    	 SAVENEW <- area.add(spdf = SAVENEW,T,T)
         print(paste0("Habitat area= ",sum(SAVENEW$AREA.HA)))
         print(paste0("Sum area of RUs= ",totalarea))

         cleanup<-c("r1ru","r2ru")
         dummy<-CleanUp(cleanup)


     

