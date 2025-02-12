## Performs QA/QC checks on HAF points sent to the NOC to derive weights and weighted estimates of HAF categories.
## Main (checkhaf.r)
## WARNER HAF 12/11/2017
##########################################################
##########################################################




   projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
######################## set working directory
   setwd("c:/projects/fohaf/or/warner")



###################################### ingest datasets
   terra.spdf <- readOGR(dsn="G:/aim/geo/terradat3",layer="terradat3",stringsAsFactors=FALSE)  ## Terradat file
     terra.spdf <- spTransform(terra.spdf, projection)
   names(terra.spdf@data) <- str_to_upper(names(terra.spdf@data)) 

   sfasdd.spdf <- readOGR(dsn="V:/ORWA/State/SFA/Data/Sample Design/2016/ORWA_SFA_SampleDesignDatabase_2016_2020.gdb",layer="Terra_Sample_Points",stringsAsFactors=FALSE)  ## PAC SDD
     sfasdd.spdf <- spTransform(sfasdd.spdf, projection)   
    names(sfasdd.spdf@data) <- str_to_upper(names(sfasdd.spdf@data)) 

   sfaframe.spdf <- readOGR(dsn="V:/ORWA/State/SFA/Data/Sample Design/2016/ORWA_SFA_SampleDesignDatabase_2016_2020.gdb",layer="Terra_Sample_Frame",stringsAsFactors=FALSE)  ## PAC SDD
     sfaframe.spdf <- spTransform(sfaframe.spdf, projection)   
    names(sfaframe.spdf@data) <- str_to_upper(names(sfaframe.spdf@data)) 

   sfastrata.spdf <- readOGR(dsn="V:/ORWA/State/SFA/Data/Sample Design/2016/ORWA_SFA_SampleDesignDatabase_2016_2020.gdb",layer="Terra_Strtfctn",stringsAsFactors=FALSE)  ## PAC SDD
     sfastrata.spdf<- spTransform(sfastrata.spdf, projection)  
   names(sfastrata.spdf@data) <- str_to_upper(names(sfastrata.spdf@data)) 

   lvsdd.spdf <- readOGR(dsn="V:/ORWA/Lakeview_DO/Data/SampleDesign/2016/OR_LakeviewDO_2016_2020_SDD.gdb",layer="Terra_Sample_Points",stringsAsFactors=FALSE)  ## PAC SDD
     lvsdd.spdf <- spTransform(lvsdd.spdf, projection)   
    names(lvsdd.spdf@data) <- str_to_upper(names(lvsdd.spdf@data)) 

   lvframe.spdf <- readOGR(dsn="V:/ORWA/Lakeview_DO/Data/SampleDesign/2016/OR_LakeviewDO_2016_2020_SDD.gdb",layer="Terra_Sample_Frame",stringsAsFactors=FALSE)  ## PAC SDD
     lvframe.spdf <- spTransform(lvframe.spdf, projection)   
    names(lvframe.spdf@data) <- str_to_upper(names(lvframe.spdf@data)) 

   lvstrata.spdf <- readOGR(dsn="V:/ORWA/Lakeview_DO/Data/SampleDesign/2016/OR_LakeviewDO_2016_2020_SDD.gdb",layer="Terra_Strtfctn",stringsAsFactors=FALSE)  ## PAC SDD
      lvstrata.spdf<- spTransform(lvstrata.spdf, projection)  
    names(lvstrata.spdf@data) <- str_to_upper(names(lvstrata.spdf@data)) 


   ####################### do the following if you need to update the SDDs with Plottracking info
      path.nam<-c("c:/projects/fohaf/or/warner/haf/LVDO_PLOTTRACKING_SLG.xlsx")
      sheetname<-c("Plot Tracking")
      lvsdd.spdf<-PlotTracking(path.nam,sheetname,terra.spdf,lvsdd.spdf,T)

      path.nam<-c("c:/projects/fohaf/or/warner/haf/SFA_PLOTTRACKING_SLG.xlsx")
      sfasdd.spdf<-PlotTracking(path.nam,sheetname,terra.spdf,sfasdd.spdf,T)


########################### Update the location of pts in the SDD using TerrADat coords (IFF the pt occurs in TerrADat)
    sfasdd.spdf<-UpdatePtLocations(terra.spdf,sfasdd.spdf)		  ## Updates the location of pts
    lvsdd.spdf<-UpdatePtLocations(terra.spdf,lvsdd.spdf)


########################### 
   ## SPRING HABITAT
   spring.spdf <- readOGR(dsn="C:/projects/fohaf/or/warner/haf",layer="springSMA",stringsAsFactors=FALSE)  
     spring.spdf <- spTransform(spring.spdf, projection)   
   names(spring.spdf@data) <- str_to_upper(names(spring.spdf@data)) 

   ## SUMMER
   summer.spdf <- readOGR(dsn="C:/projects/fohaf/or/warner/haf",layer="summerSMA",stringsAsFactors=FALSE)  
     summer.spdf <- spTransform(summer.spdf, projection)   
   names(summer.spdf@data) <- str_to_upper(names(summer.spdf@data))

   ## WINTER
   winter.spdf <- readOGR(dsn="C:/projects/fohaf/or/warner/haf",layer="winterSMA",stringsAsFactors=FALSE)  
     winter.spdf <- spTransform(winter.spdf, projection)   
   names(winter.spdf@data) <- str_to_upper(names(winter.spdf@data))

   index<-0
   for(index in 1:4) {

   print(paste("INDEX= ",index,sep=" "))
   ##  Set clip.spdf to HABITAT spdf
   if(index==1)clip.spdf<-spring.spdf
   if(index==2)clip.spdf<-summer.spdf
   if(index==3)clip.spdf<-winter.spdf
   if(index==4)clip.spdf<-summer.spdf		## Grass_forb stuff - summer habitat



################################# I. DO the plotkeys in the spreadsheets match plotkeys in TerrADat, SDDs, or LMF.
   if(index==1)xcel<-(read.xlsx("C:/projects/fohaf/or/warner/haf/OR_2017_Warners_NOC_HAF_Analysis_Request_20171206_v2.xlsx",sheetName="S-3 Nesting Early Brood Rearing",stringAsFactors=F))
   if(index==2)xcel<-(read.xlsx("C:/projects/fohaf/or/warner/haf/OR_2017_Warners_NOC_HAF_Analysis_Request_20171206_v2.xlsx",sheetName="S-4 Upland Summer Late Brood",stringAsFactors=F))
   if(index==3)xcel<-(read.xlsx("C:/projects/fohaf/or/warner/haf/OR_2017_Warners_NOC_HAF_Analysis_Request_20171206_v2.xlsx",sheetName="S-6 Winter",stringAsFactors=F))
   if(index==4)xcel<-(read.xlsx("C:/projects/fohaf/or/warner/haf/OR_2017_Warners_NOC_HAF_Analysis_Request_20171206_v2.xlsx",sheetName="S-4 Combined Grass_Forbs",stringAsFactors=F))

   xcel<-xcel[!is.na(xcel$AIM.or.LMF) ,]		## sometimes have to do this to get rid of junk following actual data rows!

   temp.df<-data.frame(DATA=xcel$AIM.or.LMF,DESIGN=xcel$Sample.Design,PLOTID=xcel$Plot.Identifier,DATE=xcel$Sample.Date)
   temp.df$PLOTKEY<-NA

   ######### Do the following if you need to convert plot name to proper name, and to pick up plotkey 
   temp.df<-SetHAFPlotID(temp.df)


  ##########  now we can continue.  Look at plotkey
   junk<-NULL
   a<-temp.df
   if(nrow(a)>0) {
      for(j in 1:nrow(a)) {
          tr<-0
          b<-grep(terra.spdf$PLOTKEY,pattern=a$PLOTID[j])
          if(length(b)==1) tr<-1
          b<-grep(sfasdd.spdf$PLOT_KEY,pattern=a$PLOTID[j])
          if(length(b)==1) tr<-1
          b<-grep(lvsdd.spdf$PLOT_KEY,pattern=a$PLOTID[j])
          if(length(b)==1) tr<-1

	##  IF tr==0, then no match
          if(tr==0)junk<-rbind(junk,a[j ,])
      }
   }
   if(!is.null(junk)) {
        print("No matches to spreadsheet pts based on plotkey")
        print(junk)
   }else {
     print("All spreadsheet plotkeys match SDDs & TerrADat")
   }


   ################### Look at plot names
   junk<-NULL
   a<-temp.df
   if(nrow(a)>0) {
      for(j in 1:nrow(a)) {
          tr<-0
          b<-grep(terra.spdf$PLOTID,pattern=a$PLOTIDC[j])
          if(length(b)==1) tr<-1
          b<-grep(sfasdd.spdf$PLOT_NM,pattern=a$PLOTIDC[j])
          if(length(b)==1) tr<-1
          b<-grep(lvsdd.spdf$PLOT_NM,pattern=a$PLOTIDC[j])
          if(length(b)==1) tr<-1

	##  IF tr==0, then no match
          if(tr==0)junk<-rbind(junk,a[j ,])
      }
   }
   if(!is.null(junk)) {
        print("No matches to spreadsheet pts based on plot name")
        print(junk)
   }else {
       print("All spreadsheet plot names match SDDs and TerrADat")
   }



################################# II. Clip SDD frames to SG habitat - currently not used.
  # newpacF<-flex.clip(pacframe.spdf,clip.spdf,method = "arcpy",temp.path = getwd(),
  #                     python.search.path = "C:/Python27")

 #  newowyF<-flex.clip(owyframe.spdf,clip.spdf,method = "arcpy",temp.path = getwd(),
   #                    python.search.path = "C:/Python27")


############################### III.  Clip SDD pts to SG habitat.
    newsfaS<-flex.clip.pts(sfasdd.spdf,clip.spdf,method = "arcpy",temp.path = getwd(),
                           python.search.path = "C:/Python27")
    newlvS<-flex.clip.pts(lvsdd.spdf,clip.spdf,method = "arcpy",temp.path = getwd(),
                           python.search.path = "C:/Python27")

    ## Record the clipped pts for future reference
    cpts.df<-data.frame(NAME=newsfaS$PLOT_NM,FRAME=newsfaS$TERRA_SAMPLE_FRAME_ID,PANEL=newsfaS$PANEL,PKEY=newsfaS$PLOT_KEY,FD=newsfaS$FINAL_DESIG,DATE=newsfaS$DT_VST)
    print("CLIPPED SFA POINTS")
    print(cpts.df)
    cpts.df<-data.frame(NAME=newlvS$PLOT_NM,FRAME=newlvS$TERRA_SAMPLE_FRAME_ID,PANEL=newlvS$PANEL,PKEY=newlvS$PLOT_KEY,FD=newlvS$FINAL_DESIG,DATE=newlvS$DT_VST)
    print("CLIPPED LV POINTS")
    print(cpts.df)
    cpts.df<-NULL

############################## IV.  Pull out target sampled pts - Check for year??
     target<-c("Target Sampled","TS","TARGET SAMPLED")

     newsfaST<-newsfaS[newsfaS$FINAL_DESIG %in% target,]
     newlvST<-newlvS[newlvS$FINAL_DESIG %in% target,]


############################## V.  Compare the plot IDs in the spreadsheet to make sure they are within SG habitat & within the sample frames & LMF.
##  Also, record tr >0 for pts that are used.  If not used for this HABITAT, tr=0.
##  This comparison may be repeated if ONLY plot names are being used (

   ## Init
   newsfaST$TR<-0
   newlvST$TR<-0
 

   junk<-NULL
   a<-temp.df				## The last EXCEL spreadsheet ingested
   if(nrow(a)>0) {
      for(j in 1:nrow(a)) {
          tr<-0
          b<-grep(newsfaST$PLOT_KEY,pattern=a$PLOTID[j])
          if(length(b)==1) {
             tr<-1
             newsfaST$TR[b]<-tr		## pts used for this season
          }
          b<-grep(newlvST$PLOT_KEY,pattern=a$PLOTID[j])
          if(length(b)==1){
              tr<-2
              newlvST$TR[b]<-tr	## Pts used for this season
          }   

	##  IF tr==0 then no match,
          if(tr==0)junk<-rbind(junk,a[j ,])
      }
   }
   if(!is.null(junk)) {
        print("No matches to spreadsheet pts based on clipped plot key")
        print(junk)
   }else {
       print("All spreadsheet plot keys match clipped SDDs and TerrADat")
   }


   ## OR THIS VERSION, based on plot names

   newsfaST$TR<-0
   newlvST$TR<-0
   junk<-NULL

   a<-temp.df				## The last EXCEL spreadsheet ingested
   if(nrow(a)>0) {
      for(j in 1:nrow(a)) {
          tr<-0
          b<-grep(newsfaST$PLOT_NM,pattern=a$PLOTIDC[j])
          if(length(b)==1) {
             tr<-1
             newsfaST$TR[b]<-tr		## pts used for this season
          }
          b<-grep(newlvST$PLOT_NM,pattern=a$PLOTIDC[j])
          if(length(b)==1){
              tr<-2
              newlvST$TR[b]<-tr	## Pts used for this season
          }   

	##  IF tr==0 then no match,
          if(tr==0)junk<-rbind(junk,a[j ,])
      }
   }
   if(!is.null(junk)) {
        print("No matches to spreadsheet pts based on clipped plot name")
        print(junk)
   }else {
       print("All spreadsheet plot names match clipped SDDs and TerrADat")
   }


############################## VI.  List the SDD/LMF pts not used for this season
   for(i in 1:nrow(newsfaST)) {
      if(newsfaST$TR[i]==0) print(paste("NOT USED",newsfaST$PLOT_NM[i],newsfaST$PLOT_KEY[i],newsfaST$DT_VST[i],sep=" "))
   }
   for(i in 1:nrow(newlvST)) {
      if(newlvST$TR[i]==0) print(paste("NOT USED",newlvST$PLOT_NM[i],newlvST$PLOT_KEY[i],newlvST$DT_VST[i],sep=" "))
   }


 
############################## VII.  Check the stratification of the AIM pts with appropriate stratum file.
##NOTE - Use all AIM points that fall within the HABITAT, not just those with FINAL_DESG == TS

  ## sfa strata
   newsfaS<- spTransform(newsfaS, projection)  	## To be sure and avoid any problems
   zz<-over(newsfaS,sfastrata.spdf)
   newsfaS$STRATUM<-zz$STRATUM
   newsfaS$ERR<-0
   newsfaS$ERR[newsfaS$DSGN_STRTM_NM != newsfaS$STRATUM]<-1
   zz<-grep(newsfaS$ERR,pattern="1")
   if(length(zz)>0) {
     for(i in zz) {
        print(paste("SFA STRATUM ERROR",newsfaS$DSGN_STRTM_NM[i],newsfaS$STRATUM[i],sep="/"))
     }
   }
   

## LV strata
   newlvS<- spTransform(newlvS, projection)  ## To be sure and avoid any problems
   zz<-over(newlvS,lvstrata.spdf)
   newlvS$STRATUM<-zz$DMNNT_STRTM
   newlvS$ERR<-0
   newlvS$ERR[newlvS$DSGN_STRTM_NM != newlvS$STRATUM]<-1
   zz<-grep(newlvS$ERR,pattern="1")
   if(length(zz)>0) {
     for(i in zz) {
        print(paste("LV STRATUM ERROR",newlvS$DSGN_STRTM_NM[i],newlvS$STRATUM[i],sep="/"))
     }
   }

}  ## end of for index
 q()






