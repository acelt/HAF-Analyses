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

## clip.arcpy() Clips spatial polygon data frames using ARCPY
# spdf A Spatial Point or Spatial Polygons Data Frame to be clipped
# spdf.clip A Spatial Polygons Data Frame to clip spdf by
# temp.path Optional character string. A pre-existing filepath to use as a temporary folder to write files to. Defaults to a temporary directory from \code{tempdir()}.
# python.search.path Optional character string. The filepath to search for pythonw.exe in. Defaults to "C:/Python27".
clip.arcpy <- function(spdf,
                       spdf.clip,
                       temp.path = NULL,
                       python.search.path = "C:/Python27"
){
  if (!(class(spdf) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame"))) {
    stop("spdf must be a valid Spatial Polygons or Spatial Points Data Frame")
  }
  if (class(spdf.clip) != "SpatialPolygonsDataFrame") {
    stop("spdf.clip must be a valid Spatial Polygons Data Frame")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (is.null(temp.path)) {
    temp.path <- tempdir()
  } else if (!file.exists(temp.path)) {
    stop("If providing a value for temp.path, it must be a valid, pre-existing filepath.")
  }
  
  # Conform the clipping frame to the SPDF to be clipped
  if (spdf@proj4string@projargs != spdf.clip@proj4string@projargs) {
    spdf.clip <- sp::spTransform(spdf.clip, CRSobj = spdf@proj4string)
  }
  
  ## Write out the two current frames
  rgdal::writeOGR(obj = spdf,
                  dsn = temp.path,
                  layer = "inshape",
                  driver = "ESRI Shapefile",
                  overwrite_layer = TRUE)
  rgdal::writeOGR(obj = spdf.clip,
                  dsn = temp.directory,
                  layer = "clipshape",
                  driver = "ESRI Shapefile",
                  overwrite_layer = TRUE)
  
  ## Construct a quick python script to clip spdf by spdf.clip
  arcpy.script <- c("import arcpy",
                    "from arcpy import env",
                    paste0("env.workspace = '", temp.path, "'"),
                    "in_features = 'inshape.shp'",
                    "clip_features = 'clipshape.shp'",
                    "out_feature_class = 'clipresults.shp'",
                    "xy_tolerance = ''",
                    "arcpy.Clip_analysis(in_features, clip_features, out_feature_class,xy_tolerance)"
  )

  
  ## Write the constructed script out
  cat(arcpy.script, file = paste0(temp.path, "/clip.py"), sep = "\n", append = FALSE)
  
  ## Find the local machine's copy of pythonw.exe in python.search.path. There are no failsafes for if this isn't where to find it
  python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
  if (length(python.path) < 1) {
    stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
  } else {
    python.path <- python.path[1]
  }
  
  ## Execute the Python script
  system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/clip.py"), pattern = "/", replacement = "\\\\")))
  
  ## Read in the results and rename the attributes because rgdal::writeOGR() almost certainly truncated them
  clip.results <- rgdal::readOGR(dsn = temp.directory, layer = "clipresults", stringsAsFactors = FALSE)
  names(clip.results@data) <- names(spdf@data)
  
  ## Make sure that the results are in the same projection as the original source SPDF
  if (clip.results@proj4string@projargs != spdf@proj4string@projargs) {
    output <- spTransform(clip.results, CRSobj = spdf@proj4string)
  } else {
    output <- clip.results
  }
  
  return(output)
}## 12/11/2017
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
        
	z<-0			## Update only if 1 of the following uniquely occurs in TerrADat
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

