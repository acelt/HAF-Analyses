## Performs QA/QC checks on HAF points sent to the NOC to derive weights and weighted estimates of HAF categories.
##
##########################################################
##########################################################
library(raster)
library(tidyverse)
library(stringr)
library(xlsx)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(arcgisbinding)
library(digest)
library(spsurvey)
arc.check_product()
## Modification of flex.erase() written by Nelson Stauffer

## flex.clip() - clips spatial polygon data frames using ARCPY.  see flex.erase() to erase spatial polygon data frames.


flex.clip <- function(spdf,
                       spdf.clip,
                       method = "arcpy",
                       temp.path = "",
                       python.search.path = "C:/Python27"
){
  if (class(spdf) != "SpatialPolygonsDataFrame") {
    stop("spdf must be a valid Spatial Polygons Data Frame")
  }
  if (class(spdf.clip) != "SpatialPolygonsDataFrame") {
    stop("spdf.clip) must be a valid Spatial Polygons Data Frame")
  }
  if (!(stringr::str_to_upper(method) %in% c("ARCPY"))) {
    stop("method must be 'arcpy'.")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (!file.exists(temp.path)) {
    stop("temp.path must be a valid, pre-existing filepath.")
  }

  if (spdf@proj4string@projargs != spdf.clip@proj4string@projargs) {
    spdf.clip <- sp::spTransform(spdf.clip, CRSobj = spdf@proj4string)
  }
           ## Create a temp directory
           temp.directory <- paste0(temp.path, "/arcpy_temp")
           dir.create(temp.directory, showWarnings = FALSE)

           ## Write out the two current frames
           rgdal::writeOGR(obj = spdf, dsn = temp.directory, layer = "inshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)
           rgdal::writeOGR(obj = spdf.clip, dsn = temp.directory, layer = "clipshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)

           ## Construct a quick python script to clip spdf by spdf.clip
           arcpy.script <- c("import arcpy",
                             "from arcpy import env",
                             paste0("env.workspace = '", temp.directory, "'"),
                             "in_features = 'inshape.shp'",
                             "clip_features = 'clipshape.shp'",
                             "out_feature_class = 'clipresults.shp'",
                             "xy_tolerance = ''",
                             "arcpy.Clip_analysis(in_features, clip_features, out_feature_class,xy_tolerance)"
           )
           ## Write the constructed script out
           cat(arcpy.script, file = paste0(temp.directory, "/clip.py"), sep = "\n", append = F)

           ## Find the local machine's copy of pythonw.exe in C:/Python27. There are no failsafes for if this isn't where to find it
           python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
           if (length(python.path) < 1) {
             stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
           } else {
             python.path <- python.path[1]
           }

           ## Execute the Python script
           system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/clip.py"), pattern = "/", replacement = "\\\\")))

           ## Read in the results and rename the attributes because rgdal::writeOGR() truncated them
           clip.results <- rgdal::readOGR(dsn = temp.directory, layer = "clipresults", stringsAsFactors = FALSE)
           names(clip.results@data) <- names(spdf@data)

           if (clip.results@proj4string@projargs != spdf@proj4string@projargs) {
             output <- spTransform(clip.results, CRSobj = spdf@proj4string)
           } else {
             output <- clip.results
           }
           ## Remove the temp folder and files
           if (grepl(method, pattern = "arcpy", ignore.case = TRUE)) {
             system(paste("cmd /c rmdir", stringr::str_replace_all(temp.directory, pattern = "/", replacement = "\\\\"), "/s /q"))
           }

  return(output)
}


##########################################################  Clip pts to polygon

## flex.clip.pts() - clips spatial point data frames using ARCPY.  
flex.clip.pts <- function(spdf,
                       spdf.clip,
                       method = "arcpy",
                       temp.path = "",
                       python.search.path = "C:/Python27"
){
  if (class(spdf) != "SpatialPointsDataFrame") {    
    stop("spdf must be a valid Spatial Points Data Frame")
  }
  if (class(spdf.clip) != "SpatialPolygonsDataFrame") {
    stop("spdf.clip) must be a valid Spatial Polygons Data Frame")
  }
  if (!(stringr::str_to_upper(method) %in% c("ARCPY"))) {
    stop("method must be 'arcpy'.")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (!file.exists(temp.path)) {
    stop("temp.path must be a valid, pre-existing filepath.")
  }

  if (spdf@proj4string@projargs != spdf.clip@proj4string@projargs) {
    spdf.clip <- sp::spTransform(spdf.clip, CRSobj = spdf@proj4string)
  }
           ## Create a temp directory
           temp.directory <- paste0(temp.path, "/arcpy_temp")
           dir.create(temp.directory, showWarnings = FALSE)

           ## Write out the two current frames
           rgdal::writeOGR(obj = spdf, dsn = temp.directory, layer = "inshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)
           rgdal::writeOGR(obj = spdf.clip, dsn = temp.directory, layer = "clipshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)

           ## Construct a quick python script to clip spdf by spdf.clip
           arcpy.script <- c("import arcpy",
                             "from arcpy import env",
                             paste0("env.workspace = '", temp.directory, "'"),
                             "in_features = 'inshape.shp'",
                             "clip_features = 'clipshape.shp'",
                             "out_feature_class = 'clipresults.shp'",
                             "xy_tolerance = ''",
                             "arcpy.Clip_analysis(in_features, clip_features, out_feature_class,xy_tolerance)"
           )
           ## Write the constructed script out
           cat(arcpy.script, file = paste0(temp.directory, "/clip.py"), sep = "\n", append = F)

           ## Find the local machine's copy of pythonw.exe in C:/Python27. There are no failsafes for if this isn't where to find it
           python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
           if (length(python.path) < 1) {
             stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
           } else {
             python.path <- python.path[1]
           }

           ## Execute the Python script
           system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/clip.py"), pattern = "/", replacement = "\\\\")))

           ## Read in the results and rename the attributes because rgdal::writeOGR() truncated them
           clip.results <- rgdal::readOGR(dsn = temp.directory, layer = "clipresults", stringsAsFactors = FALSE)
           names(clip.results@data) <- names(spdf@data)

           if (clip.results@proj4string@projargs != spdf@proj4string@projargs) {
             output <- spTransform(clip.results, CRSobj = spdf@proj4string)
           } else {
             output <- clip.results
           }
           ## Remove the temp folder and files
           if (grepl(method, pattern = "arcpy", ignore.case = TRUE)) {
             system(paste("cmd /c rmdir", stringr::str_replace_all(temp.directory, pattern = "/", replacement = "\\\\"), "/s /q"))
           }

  return(output)
}
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

##  SetHAFPlotID() - ensures that HAF scores have the correct plot_nm if this is what is being used. 
##   SetHafPlotID(score is the Excel spreadsheet scores)

SetHAFPlotID<-function(score)
{

    ## Given the tendency for HAF score PLOTIDs to not match SDD names, convert them to generally what SDDs use.
    score$PLOTIDC<-0

## Convert existing PlotID to syntax generally used in the SDDs.  This will not work if LMF keys are included as PlotID
      for(i in 1:nrow(score)) {
         a<-as.character(score$PLOTID[i])
	 ## This will back-off from the RHS until a _ is found, then change it to a - and glue the filename back together using
	 ## the -.  PlotIDs tend to differ between the field and SDDs by this - before a (plot) number.  e.g., LA_INTS_10 vs. LA_INTS-10.
         b<-a
	 size<-nchar(a)
	 j<-size+1
	 for(k in 1:size) {
  		j<-j-1
  		cc<-substr(a,j,j)
  		dash<-grepl(cc,pattern="_")
  		if(dash) {
                        if(size-j <6) {		## Sometimes, the left-most "-" is correct, so this will prevent changing a correct "_" to a "-" 
     				b<-paste(substr(a,1,j-1),"-",substr(a,j+1,nchar(a)),sep="")
     				break
                        }
  		}
	 }
         score$PLOTIDC[i]<-b
      }

      return(score)
}

## UpdatePtLocations() - Updates the coords of sdd PTS with TerrADat info IFF the 2 share the same primarykey, or plotkey, or plotnam.

## UpdatePtLocations(terra.spdf,ptsfile)

UpdatePtLocations<-function(terra,pts)
{
  
   if(is.null(pts)) return(pts)		## Nothing to do since pts is NULL
   if(nrow(pts)<=0) return(pts)		## Nothing to do since pts is empty

    pts@data <- cbind(pts@data, pts@coords)
    terra@data <- cbind(terra@data, terra@coords)


    for(i in 1:nrow(pts)) {		
        id1<-pts$TERRA_TERRADAT_ID[i]
        id2<-pts$PLOT_KEY[i]
        id3<-pts$PLOT_NM[i]

        a<-NULL
	b<-NULL
	c<-NULL

        if(!is.na(id1)) a<-grep(terra$PRIMARYKEY,pattern=id1)
	if(!is.na(id2)) b<-grep(terra$PLOTKEY,pattern=id2)
        if(!is.na(id3)) c<-grep(terra$PLOTID,pattern=id3)
        
	z<-0			## Update only if 1 of the following uiquely occurs in TerrADat
        if(length(a)==1) {
		z<-a
	}else if(length(b)==1) {
		z<-b
	}else if(length(c)==1) {
		z<-c
	}

	if(z>0) {		## update coords if a match in TerrADat
		pts@coords[i,1]<-terra@coords[z,1]
 		pts@coords[i,2]<-terra@coords[z,2]
	}
	
    }	

    pts@data$coords.x1<-NULL
    pts@data$coords.x2<-NULL

    return(pts)
}

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






