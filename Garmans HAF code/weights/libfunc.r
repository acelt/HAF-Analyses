## 12/14/2017 - AIMWEIGHTS
## MAIN
##########################################################
#### GLOBAL VARIABLES ####
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




################## Key functions used in MAIN 
## A function that evaluates a parsed text string like the ones in $eval.string.upper and lower. Used in a lapply() later. Probably replaceable with parse() %>% eval() there though
safe.parser <- function(string){
  output <- safely(eval(parse(text = string)))
  return(output[[1]])
}

## For those strange occasions when the safe version gives you errors?
parser <- function(string){
  output <- eval(parse(text = string))
  return(output)
}

## A function to make sure that input strings are correctly formatted for filepaths, .gdb filenames, .xlsx filenames, .csv filenames, and .shp filenames
sanitizer <- function(string, type){
  switch(type,
         filepath = {
           if (!grepl(x = string, pattern = "/$") & !grepl(x = string, pattern = "\\\\$")) {
             string <- paste0(string, "/")
           }
         },
         gdb = {
           if (!grepl(x = string, pattern = "\\.[Gg][Dd][Bb]$")) {
             string <- paste0(string, ".gdb")
           }
         },
         xlsx = {
           if (!grepl(x = string, pattern = "\\.[Xx][Ll][Ss][Xx]$")) {
             string <- paste0(string, ".xlsx")
           }
         },
         csv = {
           if (!grepl(x = string, pattern = "\\.[Cc][Ss][Vv]$")) {
             string <- paste0(string, ".csv")
           }
         },
         shp = {
           if (!grepl(x = string, pattern = "\\.[Ss][Hh][Pp]$")) {
             string <- paste0(string, ".shp")
           }
         }
  )
  return(string)
}

## modifyRU
## Currently designed to erase specified layers from the reporting unit.  This may be important
## when working with LMF points because of the Type I and II strata.

## The erase argument is included because future enhancments may include the ability to clip (e.g., erase=F would indicate clipping operations)

modifyRU<-function(frame,erasethis,erase=T){
  
  if(erase) {
    a<-gErase(frame,erasethis)
    return(a)
  }
  
  
}
##4/13/2017
##  Currently, gClip() only works if there is 1 and only 1 feature class in clip

gClip <- function(frame,
                  clip) {
  clipped <- gIntersection(frame,
                           clip,
                           byid = TRUE,
                           drop_lower_td = TRUE)
  keep <- gsub(row.names(clipped),
               pattern = " 1",
               replacement  = "")
  clipped <- spChFIDs(clipped, keep)
  clipped_data <- as.data.frame(frame@data[keep, ])
  return(SpatialPolygonsDataFrame(clipped, clipped_data))
}


## The problem........
## The number of row.names(clipped) must be equal to or a subset of row.names(frame).  If clip has
## more than 1 feature, row.names(clipped) ends up with replicates of entries in row.names(frame).
##  E.g., if frame has features 1 thru 9 and clip has 2 features, then clipped can have row.names == 1 1, 1 2, 2 1, 2 2, 3 1, 4 1, 5 1, 5 2, etc.
##  THe first number is frame feature, the second is clip feature.  In essence, 1 1, 1 2 means that frame feature 1 shows up in clip feature 1 & 2.

## The gsub () command gets rid of the second number per pair, but you still end up with multiple 1's, multiple 2's....E.g., 1, 1, 2, 2, 3, 4, 5, 5...
## THe clipped_data statement (above) ends up numbering the multiples as #.1 (e.g., 1, 1.1, 2, 2.1....).
## The SpatialPolygonsDataFrame operation returns an error due to unequal lengths (e.g., there are no 1.1 IDs in frame)
## ??????? Currently can't figure out how to deal with duplicate row names; hence, clip can only have 1 feature..........  



# The following is for reference............

## row.names(clipped)<-gsub(" 2","",row.names(clipped))
## keep<-row.names(clipped)
## clipped<- spChFIDs(clipped,keep)
## clipped_data<-as.data.frame(spdf1@data[keep, ])
## clipped <-SpatialPolygonsDataFrame(clipped,clipped_data)
##    return( SpatialPolygonsDataFrame(clipped,frame@data[row.names(clipped), ]))

##    row.names(clipped) <-as.character(gsub(" 0","",row.names(clipped)))
##    not the way to this, but it can work in special cases ->    row.names(clipped)<-row.names(frame)## Strata.Weights() - 
## Calculate weights using the AIM/LMF Integration approach.  
## We derive weights for each stratum by relweight (which equates to point type).
## The weight by stratum by relweight results are stored in tablewgt.df which accumulates duplicates.  
## In the end, get rid of dups. Tablewgt.df can then be used to assign weights to all initial points.

##  The weighting logic WILL become part of the doumentation.  For now, see u:\aim\procs\aimlmf\post stratification of AIM and LMF (v4); eqs. I-V., SLGarman
##   3/3/2017.

# Strata.Weights(pts.spdf,list of weight categories,rel. contribution of pts by strata,area by strata, sampled area by strata)
Strata.Weights<-function(obs.pts,
                         vecs,
                         relpts,
                         Area,
                         Areasampled)
{
  
  tablewgt.df<-NULL
  
  for(i in 1:length(vecs)) {
    aimn<-nrow(obs.pts@data[obs.pts@data[, "WEIGHT.ID"] %in% vecs[i] & obs.pts$RELWEIGHT==1 ,])  ## AIM points with rel contribution of 1
    z<-unique(obs.pts$SEGCODE[obs.pts@data[, "WEIGHT.ID"] %in% vecs[i] & obs.pts$AIMLMF==2 ])    ## Unique segment codes is the no. of selected segments for LMF points
    lmfn<-length(z)
    N<-Area[i]		## total ha of stratum
    N<-N/(160.0/2.47)	## approximate the number of 160 acre segments within the stratum
    ConWgt<- 1.0/((aimn/N) + (lmfn/N))     
    SumRelPts<-relpts[i]
    ConArea <- ConWgt*SumRelPts
    AdjWgt=Areasampled[i]/ConArea
    vecw<-obs.pts$RELWEIGHT[obs.pts@data[, "WEIGHT.ID"] %in% vecs[i]]
    if(Area[i]<=0 || SumRelPts <=0) {
      AdjWgt=0
      ConWgt=0
    }
    
    ## Good time to check if vecs[i] is NA
    if(is.na(vecs[i]))vecs[i]="None"
    
    totalwgt<-0
    if(length(vecw)<=0) {
      temp.df<-data.frame(WEIGHT.ID=vecs[i],RELWEIGHT=0,WGT=0,Observed.pts=0)	## Record even NULL results for stratum without observed pts
      tablewgt.df<-rbind(tablewgt.df,temp.df)
    }else {
      
      for(j in 1:length(vecw)) {
        PtWgt<-(AdjWgt * ConWgt) * vecw[j]
        z<-length(grep(vecw,pattern=vecw[j]))		## quick way to derive the no. of pts by stratum by rel. contribution
        temp.df<-data.frame(WEIGHT.ID=vecs[i],RELWEIGHT=vecw[j],WGT=PtWgt,Observed.pts=z)
        tablewgt.df<-rbind(tablewgt.df,temp.df)
        totalwgt<-totalwgt+PtWgt
      }	
      if(as.integer(totalwgt-Areasampled[i])!=0) {	## Sum of PtWgts(i,j) must be equal to Total sampled area
        print("ERROR, totalwgt != sampled area")
        print(c(totalwgt,Areasampled[i]))
        q()
      }
    }
  }	## ENDOF for(i in 1:length(vecs))
  return(tablewgt.df)
}


###########################################################################################################################################
###  Frame.Weights() - Derive pt weights when using only a frame instead of stratification
##   Frame.Weights(pts file, frame area, sum of relative weights, sampled area)

Frame.Weights<-function(obs.pts,area,relwgts,Sarea) 
{
  aimn<-nrow(obs.pts@data[obs.pts$RELWEIGHT==1 ,])	## AIM points with rel contribution of 1
  z<-unique(obs.pts$SEGCODE[obs.pts$AIMLMF==2 ])    ## Unique segment codes is the no. of selected segments for LMF points
  lmfn<-length(z)
  N<-area/(160.0/2.47)	## approximate the number of 160 acre segments (converted to HA) within the stratum
  ConWgt<- 1.0/((aimn/N) + (lmfn/N))   
  SumRelPts<-relwgts
  ConArea <- ConWgt*SumRelPts
  AdjWgt=Sarea/ConArea
  
  vecw<-obs.pts$RELWEIGHT	## vector of the different rel. weight values
  temp.df<-NULL
  tablewgt.df<-NULL
  
  ## THe following loops thru the rel. weights of the points in the frame and derives
  ##  the weights for each possible rel. weight.  Tablewgt.df accumulates these weights
  ##  and the no. of observered pts per rel. weight.  At the end of the loop, tablewgt.df
  ##  info is rendered to just the unique rel. weights.  This table is then used to set
  ##  weights in the points file.
  
  if(length(vecw)<=0) {
    temp.df<-data.frame(WEIGHT.ID="Sample Frame",RELWEIGHT=0,WGT=0,Observed.pts=0)	## Record even NULL results 
    tablewgt.df<-rbind(tablewgt.df,temp.df)
  }else {
    totalwgt<-0
    for(j in 1:length(vecw)) {
      PtWgt<-(AdjWgt * ConWgt) * vecw[j]
      z<-length(grep(vecw,pattern=vecw[j]))		## quick way to derive the no. of pts by rel. contribution
      temp.df<-data.frame(WEIGHT.ID="Sample Frame",RELWEIGHT=vecw[j],WGT=PtWgt,Observed.pts=z)
      tablewgt.df<-rbind(tablewgt.df,temp.df)
      totalwgt<-totalwgt+PtWgt
    }	
    if(as.integer(totalwgt-Sarea)!=0) {	## Sum of PtWgts(i,j) must be equal to Total sampled area
      print("ERROR, totalwgt != sampled area")
      print(c(totalwgt,Sarea))
      q()
    }
  }
  return(tablewgt.df)
}
## 4/30/2017
## add.pts-> concatentates SDD pts files to the first entry in sdd.src.
## Uses src or path[n] to locate the files specified in data.src.
## Workinglist is a named list with SDD SF, PTS, and STRATA.

## We need to add 4 fields to a pt file that are key for LMF point 
## processing (fields are expected even if LMF points are not used) 
## just like sdd.reader does.

## Pt file consistency checks include the same number of columns, 
## the same field names, and the same stratum names 
## (if the strata file exists).  These 3 attributes have been
## problematic.


add.pts <- function(workinglist,   ## named list 
                    src, ## path 
                    path, ## path to each entry in data.src, else NULL if all paths equates to src        
                    data.src,  ## name of each SDD gdb       
                    omitNAdesignations=F,  ## controls deleting final_desig=NA pts
                    func = "arcgisbinding" ## This can be "readOGR" or "arcgisbinding" 
) {
  
  projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
  
  ## readOGR() wrapped in safely() so that it will return NULL instead of an error
  safe.readOGR <- safely(readOGR, otherwise = NULL)
  
  ## Sanitization
  func <- str_to_upper(func)
  
  ## Checking that func is a valid value
  if (!(func %in% c("ARCGISBINDING", "READOGR"))) {
    print("The argument func needs to be 'arcgisbinding' or 'readOGR'")
  }
  
  
  ## Get strata once if it exists.  
  
  strata<-NULL
  strata<-names(workinglist$strata[1])
  strata<-workinglist$strata[[strata]]
  if(!is.null(strata)) {
    master<-unique(strata$DMNNT_STRTM)   ## master list of strata
    master<-str_to_upper(master) 
  }
  
  
  index=0
  thesrc<-src
  
  ######################################################### cycle thru the list of SDD pts to concatenate
  for(s in data.src) { 
    
    ## If entries in path, then use them
    index<-index+1 
    if(!is.null(path)) {
      thesrc<-path[index]
    }
    
    points<-NULL
    
    
    
    switch(func,
           READOGR = {
             
             #Read in the Points
             points <- safe.readOGR(dsn = paste(thesrc, s, sep = "/"),
                                    layer = "Terra_Sample_Points",
                                    stringsAsFactors = F)[[1]]
             
             if (!is.null(points)) {
               points <- spTransform(points, projection)
               names(points@data) <- str_to_upper(names(points@data))
               
               ## Strip out points with an NA value in the FINAL_DESIG field if requested
               if (omitNAdesignations) {
                 points <- points[!is.na(points@data$FINAL_DESIG),]
               }
             }
           },
           ARCGISBINDING = {
             
             #Read in the Points
             ## Identify/create the filepath to the design points feature class inside the current SDD
             pts <- paste(thesrc, s, "Terra_Sample_Points", sep = "/")
             ## Creates an SPDF with the name pts.[SDD name] using the filepath to that feature class
             assign(x = "points",
                    value = pts %>% arc.open() %>% arc.select %>%
                      #read in the feature class, notice the difference between Polygons and points (different function with different arguments needs)
                      SpatialPointsDataFrame(coords = {arc.shape(.) %>% arc.shape2sp()})%>%spTransform(projection))
             
             if(!is.null(points)) {  
               names(points@data) <- str_to_upper(names(points@data))
               
               ## Strip out points with an NA value in the FINAL_DESIG field if asked
               if (omitNAdesignations) {
                 points <- points[!is.na(points@data$FINAL_DESIG),]
               }
               
             }
           }
    )
    
    
    ## STUPID fixes for the OR SFA SDD
    ##	names(points)[names(points) == "REVISIT"] <- "REVIST"
    ##	names(points)[names(points) == "REVISIT_TERRA_PK"] <- "REVIST_TERRA_PK"
    
    ## Add 4 fields related to LMF pt processing. 
    points@data$RELWEIGHT<-1.0	## Default to a relative weight of 1.  This can change if LMF points are in play.
    points@data$AIMLMF<-1	## Designates that these pts are AIM pts.  Does not change.
    points@data$LMFSTRATA<-0	## The LMF strata; effectively set to null.  If LMF points in play, likely will change to TYPE I (1) or Type II (2) stratum. 
    points@data$FIELDOFFICE<-s	## SDD or Field office 
    
    ## get the primary pts file
    tnam<-names(workinglist$pts[1])
    t<-workinglist$pts[[tnam]]
    masterptsnams<-unique(names(t))
    
    
    if(ncol(points)!=ncol(t)) { 		## Check column lengths
      print("ERROR in add.pts; Columns lengths differ")
    }
    
    
    if(!is.null(strata)) {			##  Check for similar stratum names
      slave<-unique(points$DSGN_STRTM_NM)    	##  List of strata in this point file
      slave<-str_to_upper(slave)
      for(nam in slave) {
        if(nam %in% master==F) {
          print(paste("Error in add.pts; Stratum not in master list ",nam))
        }
      } ## for nam 
    } ## !is.null strata
    
    
    slaveptsnams<-unique(names(points))	## Ensure field names of pts files are the same
    for(nam in slaveptsnams) {
      if(nam %in% masterptsnams==F) {
        print(paste("Error in add.pts; Field names not the same ",nam))
      }
    }
    
    
    
    ## Bind and store back into workinglist$pts using the name of the first 
    ## entry in sdd.src
    newpts<-spRbind(t,points)
    workinglist$pts[[tnam]]<-newpts
    remove(newpts)			##Declutter
  } ################################################################## End of s loop
  
  remove(t)		## Declutter
  remove(points)		
  
  return(workinglist)    
}
## ClipLMFStrata() - Clip a reporting unit to a LMF strata.

##  (lmfoutput list, the reporting unit spdf, reporting unit label, standard projection,strata=1 or =2)
ClipLMFStrata <-function(lmfoutput,RU.spdf,reporting.unit.label,projection,strata)
{ 
  
  s<-names(lmfoutput$lmfstrata[1])		##LMF strata which is BLM SMA only
  clip.spdf<-lmfoutput$lmfstrata[[s]]
  
  clip.spdf<-clip.spdf[clip.spdf$STRATUM==strata ,]	## TYPE I or II LMF area
  RU.spdf<-flex.clip(clip.spdf,RU.spdf,method="arcpy",temp.path=getwd())
  
  RU.spdf <- area.add(spdf = RU.spdf,T,T)   ## Add area, re-project, then ensure that the RU label is set.
  RU.spdf<-spTransform(RU.spdf,projection)  
  RU.spdf$RU<-reporting.unit.label
  return(RU.spdf)
}## filter.dateCLIP.r - Function to eliminate points not matching specified dates AND clips to the specified AOI.
## Operates on, and returns the N pts file in workinglist.

## Points with NA dates are retained,
## else points with visit dates >= target[] <= are first retained, 
## else, points are removed,
## then pts are clipped to the specified AOI.


filter.dateCLIP <- function(importSDD,target,clipper,N) 
{
  s<-names(importSDD$pts[N])
  pts<-importSDD$pts[[s]]
  
  pts$season<-substr(pts$DT_VST,6,10)		## Assumes a consistent format!!!!!!!!
  pts$season<-gsub("-","",pts$season)
  
  for(i in 1:nrow(pts)) {
    pts$keep[i]<-0
    if(is.na(pts$season[i])) {
      pts$keep[i]<-1
    }else if(pts$season[i]=="") {
      pts$keep[i]<-1
    }else {
      if(as.numeric(pts$season[i])>= target[1] & as.numeric(pts$season[i])<=target[2]){pts$keep[i]<-1}
    }
  }
  pts<-pts[pts$keep==1,] 
  
  if(!is.null(pts)) {
    pts$season<-NULL
    pts$keep<-NULL
  }
  
  pts<-flex.clip.pts(pts,clipper,method = "arcpy",temp.path = getwd(),
                     python.search.path = "C:/Python27")
  return(pts)
} ## end of function  
## Sanitizenams.r - convert pts strata and strata names to upper case,
##     & checks to see if all pts strata are in the strata file
##     if a strata file is used.  Stratum names are not fixed, replaced, or otherwise
##     altered in this funciton.  see CheckFixStrata to re-assign pts strata to stratum
##     file names based on spatial overlap.  

##     If the comparison returns a FALSE, then the strata file is NOT checked to see
##     if it needs to be dissolved.  Otherwise, the need to dissolve the strata file
##     is evaluated - if necessary, it is dissolved else nothing happens.  Thus
##     multiple calls to Sanitizenams can occur without running thru the dissolving
##     process multiple, needless times.


##     Sanitizenams(workinglist, entry which indicates the sequential entry in workinglist)

Sanitizenams <- function(import,entry) 
{
  
  pointstratumfieldname = "DSGN_STRTM_NM"
  
  s<-names(import$pts[entry])
  pts<-import$pts[[s]]
  pts@data[, pointstratumfieldname] <- str_to_upper(pts@data[, pointstratumfieldname])
  import$pts[[s]]<-pts
  
  s<-names(import$strata[entry])
  if(!is.null(import$strata[[s]])) {
    strata<-import$strata[[s]]
    strata@data[, "DMNNT_STRTM"] <- str_to_upper(strata@data[, "DMNNT_STRTM"])
    import$strata[[s]]<-strata
    
    ## Compare pts strata against strata names in strata file        
    a<-unique(strata$DMNNT_STRTM)
    tr=1
    for(i in 1:nrow(pts)) {
      b<-(pts$DSGN_STRTM_NM[i] %in% a)
      if(b==FALSE) {
        print(paste(i,"Pt strata not in stratum file",pts$DSGN_STRTM_NM[i],sep="/"))
        tr=0		## we have a pt strata not in the stratum shapefile
      }
    }
    
    ## If the above comparison indicates no discrepancy between pt and stratum names, then
    if(tr) {		## check on the need to dissolve strata file - helps to ensure proper aerial extent summaries in weighter.r
      a<-unique(strata$DMNNT_STRTM)	## unique names
      if(length(a) !=nrow(strata)) {    ## these are not equal when strata polys are not dissolved
        print("Dissolving strata file")
        strata<-flex.dissolve(strata, method = "arcpy",temp.path = getwd())
        import$strata[[s]]<-strata
      }else {
        print("No need to dissolve strata file")
      }
    }else {
      print("Need to dissolve strata file not evaluated due to differences between pt and strata name differences")
    }
  } ## if !is.null
  
  return(import)
} ## end of function  

##  CheckEliminateDUPS() - After filter.dateCLIP operations, the aggregated pts file may contain duplicate pts; specifically,
##                         non Target Sampled pts.  This function is specifically designed to eliminate these Dups, if necessary,
##                         and update the first pts file in the named list (workinglist).

CheckEliminateDUPS<- function(workinglist,pts)
{ 
  ## Check for DUPS - need to separate by AIM, then by LMF
  pts$keeper<-1
  
  a<-pts[pts$AIMLMF==1 ,]
  b<-unique(a$PLOT_NM)
  
  
  if(nrow(a)!=length(b)) {	## If true, then we have dups
    if(length(b) >0) {	## Just to be sure
      for(i in b) {
        c<-grep(pts$PLOT_NM, pattern=i)
        if(length(c)>1) {			# if c>1, then we have dups.  Keep the first 1, delete all others
          for(j in 2:length(c)) {
            pts$keeper[c[j]]<-0
          }
        }
      }
    }
  }
  
  
  a<-pts[pts$AIMLMF==2 ,]
  b<-unique(a$PLOT_KEY)
  if(nrow(a)!=length(b)) {	## If true, then we have dups
    if(length(b) >0) {	## Just to be sure
      for(i in b) {
        c<-grep(pts$PLOT_KEY, pattern=i)
        if(length(c)>1) {			# if c>1, then we have dups.  Keep the first 1, delete all others
          for(j in 2:length(c)) {
            pts$keeper[c[j]]<-0
          }
        }
      }
    }
  }
  pts<-pts[pts$keeper==1 ,]  
  pts$keeper<-NULL
  
  s<-names(workinglist$pts[1])
  workinglist$pts[[s]]<-pts
  
  return(workinglist)
}


## 4/14/2017
## AimLmf_integrate.r - Function to integrate lmf and aim points 

## 1) Starting with LMF points, determine LMF strata of points.  Strata are Type I (rangeland that is SG primary habitat), 
##    Type II (rangeland that is non-prime SG habitat).  See LMF/NRI documentation for actual definition of primary and non
##    prime SG habitat. Uses over(pts,strata) to determine strata of pts.  

## 2) Determine no. of LMF points in the same LMF segment (uniquely numbered quarter-section parcels).  2 possible ways to do this.  The first is to use over(pts,segments)
##    to determine segment no. associated with each LMF point, then determine those points with the same segment number.
##    However, noticed there is the potential for georegistration error! How can you tell?  Well, the PLOTKEY numbers of LMF points 
##    in the same segment have the same prefix (but differ by a 2-digit alphanumeric suffix; e.g., 1234567R1, 1234567R3 are PLOTKEYS for 2 points
##    in the same segment).  Where there is georegistration error, pts with the same PLOTKEY suffix will fall within a segment and outside the segment 
##    (typically resulting in a segment code== NA).  Thus, 2 pts that were supposed to be in the same segment will have different
##    segment membership. Saw this with the first LMF data set I worked with (Bruneau FO)). Decided not to move the point (based on what rule set???).  
##    Life goes on...  SO, this function uses the first method (over()) to set segment codes of pts and to tally no. of pts in each segment, 
##    then scruitinizes the PLOTKEY codes to adjust segment membership where there is a problem.  The rule set is: The same PLOTKEY-code suffix indicates the same segment.  
##    Where this doesn't occur, the pt outside of the segment is assigned (but not physically moved) to the proper segment, and the 
##    no. of pts in the segment is updated after this virtual move.  

##    THe number of LMF points (2nd sample stage) in a segment affects the point-inclusion probability, and thus, the point weight.
##    LMF protocol calls for the establishment of 2 points in a selected segment (1st sample stage).  If only 1 point shows up per segment, then
##    the relative weight is 0.5.  If 2 points show up, then each has a relative weight of 0.5.  If 2 LMF and 1 AIM point
##    are in the same segment, then each has a relative weight of 0.3333.  Etc....

## 3) Determine LMFpt overlap with SDD strata, if stratification is used.

## 4) Determine overlap between SDD pts and LMF TYPE I & II stratum

## 5) Determine overlap between SDD pts and LMF segments

## 6) Determine if SDD (AIM) pts occurr in segments containing LMF points

## 7) Combine the LMF pts with SDD pts and save the resulting info in the SDD list.
##    The final pts file will only have key attributes used in subsequent processing, including 4 attributes included to facilitate LMF processing
##    	aimlmf = 1 if AIM, =2 if LMF pt
##    	lmfstrata = 1 for Type I,  =2 for Type 2
##	relweight - >0 to 1.0  
##      segcode -> lmf segment code


## Returns a list just like importSDD BUT the pts SPDF has the LMF+AIM points, and formatted differently from a 'standard' SDD pts file.
## This function assumes a related sequential order in importSDD and importLMF (i.e., AIM and LMF points for the same field office are listed in the
## same sequential order), but you can have multiple field offices.  The returned list of SPDFs, however, only binds AIM and LMF points of the same field office.
## If you want to aggregate across field offices, you need to do so in the MAIN func after calling this function (at this point in time!).



## importSDD is the list returned by sdd.reader; importLMF is the list returned by lmf.reader
## yearlist is a list of the years to include
AimLmf_integrate <- function(importSDD,importLMF,yearlist) 
{
  projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
  target.values = c("Target Sampled","TS")
  
  workinglist<-importSDD	## This is where we store the new integrated LMF+AIM data
  
  
  index<-0		## This is used to extract the proper SDD files later in the looping
  ###################################################################################### Loop thru lmfpts & corresponding SDDpts
  for(s in names(importLMF$lmfpts) ) {
    index<-index+1
    
    pts<-importLMF$lmfpts[[s]]
    
    
    strata<-importLMF$lmfstrata[[s]]
    seg<-importLMF$lmfsegments[[s]]
    
    
    ## parse data by requested yr(s), but first set up panel
    ## create the panel attribute based on $DTVISIT,  This will work for ca. the next 30 yrs
    for(i in 1:30) {			## Assumes that the earliest yr of LMF data is 2011
      pts$PANEL[grepl(pts$DTVISIT,pattern=(2010+i))==TRUE]<-2010+i
    }
    
    ## Must be an easier way to do the following, but couldn't figure it out!!!!
    pts$a<-1  				## default to 1
    pts$a[pts$PANEL %in% yearlist] <-2     ## set to 2 if panel year(s) in yearlist (user-specified year of interest)
    pts<-pts[pts$a==2,]  			## render pts to those years specified by the user
    pts$a<-NULL	      			## clean up
    
    
    
    pts<-spTransform(pts,projection) 
    strata<-spTransform(strata,projection) 
    seg<-spTransform(seg,projection) 
    
    temp<-over(pts,strata)  ## This contains info from the LMF strata for each row in pts, but lacks specific point info.
    ## THe field STRATUM will be 1 or 2; this is the info we carry back over to pts.
    pts$LMFSTRATA<-temp$STRATUM
    remove(temp)
    temp<-over(pts,seg)     ## SEGCODE (unique segment number) is listed for each row in pts.  This field is carried back over to pts.
    pts$SEGCODE<-temp$SEGCODE
    
    
    
    ## Use PLOTKEY to finalize/set relative weight of each pt.  The following method ensures that if the segcode of 1 point of a pair is NA, 
    ## the segcode of the other point is assigned to both points.  ERROR checking ensures that a final segcode is !NA
    ## Set the relative weight which relates to (but not the same as) the point inclusion probability, where relative weight = 1.0/(no. of pts in a segment)
    
    
    cnt<-nrow(pts)
    for(i in 1:cnt) {
      code<-pts$PLOTKEY[i]    
      codel<-nchar(code)
      code<-substr(code,1,codel-1)   ## reuse code to store PLOTKEY suffix 
      records<-grep(x=pts$PLOTKEY,pattern=code) 
      
      if(length(records)==1) {			## If only 1 pt, then make sure it has a segcode, else terminate
        if(is.na(pts$SEGCODE[records[1]])) {
          print("SEGCODE is NA in aimlmf_integrate") 
          q()
        }else {				        ## Set relative weight and segcnt (no. of pts in a segment) 
          pts$RELWEIGHT[records[1]]<-0.5
          pts$SEGCNT[records[1]]<-1
        }
      }else {				        ## There are >1 pts associated with the PLOTKEY prefix; retrieve the segment codes of each pt 
        a<-pts$SEGCODE[records]
        if(length(unique(a))!=1) {		## This handles 2 pts where segcode of one of them is NA
          b<-(!is.na(pts$SEGCODE[records]))
          if(b[1]) {				## Figure out which one has the NA and which doesn't
            pts$SEGCODE[records[2]]<-a[1]
            a[2]<-a[1]
          }else {
            pts$SEGCODE[records[1]]<-a[2]                    
            a[1]<-a[2]
          } 
          pts$RELWEIGHT[pts$SEGCODE==unique(a)]<-1.0/length(a)  ## After resolving the NA occurrence, Set relative weight for all occurrences of segcode== unique(a) 
          pts$SEGCNT[pts$SEGCODE==unique(a)]<-length(records)   ## Set no. of pts/segments for all pts that occur in segcode== unique(a) 
          
        }else {   					       ## For this PLOTKEY prefix, there are >1 pts and all segment numbers of pts are the same 
          pts$RELWEIGHT[pts$SEGCODE==unique(a)]<-1.0/length(a)   ## Set relative weight for all pts that occur in segcode== unique(a)              
          pts$SEGCNT[pts$SEGCODE==unique(a)]<-length(records)    ## Set no. of pts/segment for all pts that occur in segcode== unique(a) 
          
        }       
      } ## if length(records) else 
    }  ## for (i in 1:length(cnt)
    
    
    
    
    ## Determine overlap with SDD strata.  The key attribute is called DSGN_STRTM_NM; at least create attribute for compatibility with SDDs
    pts$DSGN_STRTM_NM<-NULL
    
    t<-names(importSDD$sf[index])            ##   Pick up the corresponding SDD strata (make sure there is one) 
    if (!is.null(importSDD$strata[[t]])) {
      strataSDD<-importSDD$strata[[t]]
      strataSDD<-spTransform(strataSDD,projection) 
      temp<-over(pts,strataSDD)	## stratum code
      pts$DSGN_STRTM_NM<-temp$DMNNT_STRTM	## set the stratum in pts. NOTE - you can have NA because the LMF pt doesn't
      ## overlap BLM stratum - georegistration slop??
    }
    ##  importLMF$lmfpts[[s]]<-pts	Not sure we want to do this????
    
    ############################################################################## Process SDD points
    ##  Determine overlap between SDD pts and LMF Type I & II strata
    ptsSDD<-importSDD$pts[[t]]
    
    if(!is.null(ptsSDD)) {		## only if we have SDD pts
      
      ## Select pts for the specified years; assumes yr of earliest AIM data is >= 2011
      for(i in 1:30) {
        ptsSDD$theyr[grepl(ptsSDD$PANEL,pattern=(2010+i))==TRUE]<-2010+i
      }    
      
      ## Must be an easier way to do the following, but couldn't figure it out!!!!
      ptsSDD$a<-1  				    ## default to 1
      ptsSDD$a[ptsSDD$theyr %in% yearlist] <-2    ## set to 2 if theyr(s) is in yearlist (user-specified year of interest)
      ptsSDD<-ptsSDD[ptsSDD$a==2,]  		    ## render pts to those years specified by the user
      
      if(nrow(ptsSDD)==0) {
        ptsSDD<-NULL			    ## no pts satisfied the specified time period
      }else {
        ptsSDD$theyr<-NULL  			    ## clean up
        ptsSDD$a<-NULL  			    ## clean up
        
        
        ## overlay with LMF stratum & record 
        ptsSDD<-spTransform(ptsSDD,projection) 
        temp<-over(ptsSDD,strata)
        ptsSDD$LMFSTRATA<-temp$STRATUM
        
        ## Determine overlap between SDD pts and LMF segments
        temp<-over(ptsSDD,seg)
        ptsSDD$SEGCODE<-temp$SEGCODE
        
        ## Determine if LMF and AIM points co-occur in segments
        a<-(!is.na(ptsSDD$SEGCODE))
        records<- grep(a,pattern="TRUE")	##  records contains the row number(s) of AIM points that overlap an LMF segment
        if(length(records)>0) {
          for(i in 1:length(records)) {
            
            ## only deal with target sample AIM points
            if(ptsSDD$FINAL_DESIG[records[i]] %in% target.values) {
              segcnt<-unique(pts$SEGCNT[pts$SEGCODE==ptsSDD$SEGCODE[records[i]]])    ## IF co-occur with LMF pts, then adjust segcnt, then rel. weights of LMF and AIM points
              if(length(segcnt)>0) {
                segcnt<- (segcnt+1)						## No. pts in a segment		
                pts$SEGCNT[pts$SEGCODE==ptsSDD$SEGCODE[records[i]]] <-segcnt	## LMF point, set new segcnt
                relweight<-1.0/segcnt
                pts$RELWEIGHT[pts$SEGCODE==ptsSDD$SEGCODE[records[i]]] <-relweight ## LMF point, set new relweight
                ptsSDD$RELWEIGHT[records[i]]<-relweight				## AIM point
              }
            }
          }
        }
        ## importSDD$pts[[t]]<-ptsSDD	Not sure we want to do this????
      } ## if nrow(ptsSDD)
    } ## !is.null
    
    
    ################################################################################## Make the format of LMF & SDD pt files the same;
    ## Retain key attributes used in subsequent processing, then merge files.  The plot_key field for LMF pts is analagous to the 
    ## AIM primarykey (terra_terradata_id), in that it crosswalks with the monitoring observations stored in the LMF master database.
    
    
    
    ## Create/set attributes in LMF pts file 
    pts$PLOT_NM<-"LMF"
    pts$TERRA_SAMPLE_FRAME_ID<-"LMF"
    pts$TERRA_TERRADAT_ID<- "LMF"
    pts$ACTL_STRTM_NM<-pts$DSGN_STRTM_NM
    pts$DT_VST<-pts$DTVISIT
    pts$PLOT_KEY<-pts$PLOTKEY   
    
    
    
    ## Create similar formats between LMF and the SDD SPDFs.  ONly KEY attributes used in subsequent processing are retained,
    ## including the 5 new attributes related to using/processing LMF pts; aimlmf, lmfstrata, relweight, segcode, fieldoffice.
    ##  NOTE:  may have to explicitly renumber OBJECTIDs to avoid duplicates??
    lmf<- pts[, c("OBJECTID", "PLOT_KEY","PLOT_NM", "TERRA_SAMPLE_FRAME_ID", "TERRA_TERRADAT_ID",  "DSGN_STRTM_NM", "ACTL_STRTM_NM", "PANEL", "FINAL_DESIG", "DT_VST",
                  "AIMLMF", "LMFSTRATA", "RELWEIGHT", "SEGCODE", "FIELDOFFICE")]
    
    if(!is.null(ptsSDD)) {
      sdd<- ptsSDD[, c("OBJECTID", "PLOT_KEY","PLOT_NM", "TERRA_SAMPLE_FRAME_ID", "TERRA_TERRADAT_ID",  "DSGN_STRTM_NM", "ACTL_STRTM_NM", "PANEL", "FINAL_DESIG", "DT_VST",
                       "AIMLMF", "LMFSTRATA", "RELWEIGHT", "SEGCODE", "FIELDOFFICE")]
      
      ## Bind files and store in a local copy of the function argument importSDD
      final<-spRbind(sdd,lmf)
    } else {
      final<-lmf
    }
    workinglist$pts[[t]]<-final
    
  } ############################################################################ endof for s in names(importlmf$lmfpts)
  
  return(workinglist)
  
} ## end of function  

## DeleteFD.r - Function to eliminate points matching the specified FINAL_DESIG types.
##  DeleteFD(workinglist, list of FINAL_DESIG types to eliminate, affected beginning entry, affected ending entry in named list)

DeleteFD <- function(import,target,begin,end) 
{
  for(index in begin:end) {
    s<-names(import$pts[index])
    pts<-import$pts[[s]]
    pts$MEUS<-0
    pts$MEUS[pts$FINAL_DESIG %in% target]<-1
    pts<-pts[pts$MEUS==0 ,]
    pts$MEUS<-NULL
    import$pts[[s]]<-pts
  }
  return(import)
} 


## Adds areas in hectares and/or square kilometers, by polygon ID
area.add <- function(spdf, ## SpatialPolygonsDataFrame to add area values to
                     area.ha = T, ## Add area in hectares?
                     area.sqkm = T, ## Add area in square kilometers?
                     byid = T ## Do it for the whole SPDF or on a per-polygon basis? Generally don't want to toggle this
){
  ## Make sure the SPDF is in Albers equal area projection
  spdf <- spTransform(x = spdf, CRSobj = CRS("+proj=aea"))
  
  ## Add the area in hectares, stripping the IDs from gArea() output
  spdf@data$AREA.HA <- gArea(spdf, byid = byid) * 0.0001 %>% unname()
  ## Add the area in square kilometers, converting from hectares
  spdf@data$AREA.SQKM <- spdf@data$AREA.HA * 0.01
  
  ## Remove the areas that weren't requested. It's more straightforward and computationally cheaper to do it this way than run gArea() more than once
  if (!(area.ha)) {
    spdf@data$AREA.HA <- NULL
  }
  if (!(area.sqkm)) {
    spdf@data$AREA.SQKM <- NULL
  }
  return(spdf)
}## CheckFD_DT - Function to list FINAL_DESIG OR DATE of pts in workinglist.
## entry is the seqeuntial entry in workinglist (passed as import), and
## keyword is FD for FINAL_DESIG or DT for DATE.   


CheckFD_DT<- function(import,entry,keyword) 
{
  
  s<-names(import$pts[entry])
  pts<-import$pts[[s]]
  if(!is.null(pts)) {		## only if we have pts
    for(i in 1:nrow(pts)) {
      if(keyword=="FD")print(paste(i,pts$PLOT_KEY[i],pts$FINAL_DESIG[i],pts$AIMLMF[i],sep=";"))
      if(keyword=="DT")print(paste(i,pts$PLOT_KEY[i],pts$DT_VST[i],pts$AIMLMF[i],sep=";"))
    }
  }
}  

## 4/30/2017
## add.strata-> transfers strata file from first entriy in workinglist to specified workinglist entry



add.strata <- function(workinglist,   ## named list 
                       the_source,    ## the entry in workinglist (same order as sdd.src) with the strata to assign to other SDDs
                       target_sequence  ## numeric vector indicating the entry in workinglist to transfer the strata file of the first entry
) {
  
  
  
  
  ## Get strata once if it exists.  
  
  strata<-NULL
  strata<-names(workinglist$strata[the_source])
  strata<-workinglist$strata[[strata]]
  if(is.null(strata)){
    print("There is no strata file to transfer")
  }
  
  for(i in 1:length(target_sequence)) {
    id=target_sequence[i]
    workinglist$strata[[id]]<-strata
  }
  
  
  
  return(workinglist)
}## Shapefile attribute extraction function where the shapefile attribute table contains the values to assign
attribute.shapefile <- function(shape1,
                                # data.path = "", ## If the shape is in a .gdb feature class then this should be the full path, including the file extension .gdb. If the SPDF is already made, do not specify this argument
                                shape2, ## The name of the shapefile or feature class !!!OR!!! an SPDF
                                attributefield = "", ## Name of the field in the shape that specifies the attribute to assign to the points
                                newfield = "Evaluation.Stratum", ## Name of the new field in the output to assign the values from attributefield to
                                projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"),
                                sliverwarn = F,
                                sliverdrop = F,
                                sliverthreshold = 0.01
){
  set_RGEOS_dropSlivers(sliverdrop)
  set_RGEOS_warnSlivers(sliverwarn)
  set_RGEOS_polyThreshold(sliverthreshold)
  ## Strip the file extension from shape, just in case it was there
  # if (is.character(shape2)) {
  #   shape2 <- str_replace(shape2, pattern = "\\.[Ss][Hh][Pp]$", replacement = "")
  # }
  # ## If this is coming from a geodatabase, extract the shapefile appropriately. Otherwise read in the .shp
  # if (grepl(x = data.path, pattern = "\\.[Gg][Dd][Bb]$")) {
  #   shape2.spdf <- readOGR(dsn = data.path, layer = shape2, stringsAsFactors = F) %>% spTransform(projection)
  # } else if (data.path != "") {
  #   shape2.spdf <- readOGR(dsn = paste0(data.path, "/", shape2, ".shp"), layer = shape2, stringsAsFactors = F) %>% spTransform(projection)
  # } else if (class(shape2)[1] == "SpatialPointsDataFrame" | class(shape2)[1] == "SpatialPolygonsDataFrame") {
  #   shape2.spdf <- shape2 %>% spTransform(projection)
  # }
  if (paste(shape1@proj4string) != paste(shape2@proj4string)) {
    ## Make sure that the points also adhere to the same projection
    shape1 <- shape1 %>% spTransform(projection)
    shape2 <- shape2 %>% spTransform(projection)
  }
  
  
  ## Because there might be overlap between polygons with different evaluation stratum identities, we'll check each eval stratum independently
  for (n in unique(shape2@data[, attributefield])) {
    ## Create a copy of the points to work with on this loop
    current.shape1 <- shape1
    ## Get the data frame from checking the points against the current subset of the polygons
    over.result <- over(current.shape1, shape2[shape2@data[, attributefield] == n,])
    ## Add the values to the newfield column
    current.shape1@data[, newfield] <- over.result[, attributefield]
    ## Make sure that the polygons have unique IDs
    if (class(current.shape1) == "SpatialPolygonsDataFrame") {
      current.shape1 <- spChFIDs(current.shape1, paste(runif(n = 1, min = 0, max = 666666666), row.names(current.shape1), sep = "."))
    }
    ## Store the results from this loop using the naming scheme "over__[current value of n]" with spaces replaced with underscores to prevent parsing errors later
    ## But only if the number of coordinates is greater than 0!
    print(nrow(current.shape1[!is.na(current.shape1@data[, newfield]),]))
    if (nrow(current.shape1[!is.na(current.shape1@data[, newfield]),]) > 0) {
      assign(x = str_replace(paste0("over__", n), " ", "_"),
             ## Only keep the ones that actually took on an attribute
             value = current.shape1[!is.na(current.shape1@data[, newfield]),])
    }
  }
  ## List all the objects in the working environment that start with "over__" and rbind them into a single SPDF
  attributed.spdfs <- ls()[grepl(x = ls(), pattern = "^over__")]
  ## Handle all the situations where there might not be intersections, there's only one attributed SPDF, or we get the expected results
  if (length(attributed.spdfs) > 0) {
    if (length(attributed.spdfs) == 1) {
      output <- get(attributed.spdfs[1])
    } else {
      output <- eval(parse(text = paste0("rbind(`", paste(attributed.spdfs, collapse = "`,`") ,"`)")))
    }
  } else {
    output <- NULL
  }
  return(output)
}
##  CheckFixStrata.r - Checks and fixes the strataum of pts based on overlaying pts with stratum file.
##  Uses the strata file as the MASTER to check & fix pt strata.
##  importSDD is workinglist; entry is the numeric sequence of the pts file in workinglist$pts.


CheckFixStrata <- function(importSDD,entry) 
{
  
  
  projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  
  
  s<-names(importSDD$strata[entry])
  if(is.null(importSDD$strata[[s]])) {
    print("There is no strata for CheckFixStrata")
    return(importSDD)
  }
  strata<-importSDD$strata[[s]]
  
  s<-names(importSDD$pts[entry])
  if(is.null(importSDD$pts[[s]])) {
    print("There is no pts file for CheckFixStrata")
    return(importSDD)
  }
  pts<-importSDD$pts[[s]]
  
  ## project to ensure over() functions properly.
  strata<- spTransform(strata, projection)
  pts<- spTransform(pts, projection)
  
  temp<-over(pts,strata)
  pts$STRATUM<-temp$DMNNT_STRTM
  pts$ERR<-0
  pts$ERR[pts$DSGN_STRTM_NM != pts$STRATUM]<-1
  zz<-grep(pts$ERR,pattern="1")
  if(length(zz)>0) {
    for(i in zz) {
      print(paste("STRATUM ERROR",i,pts$PLOT_NM[i],pts$DSGN_STRTM_NM[i],pts$STRATUM[i],sep="/"))
      pts$DSGN_STRTM_NM[i]<-pts$STRATUM[i]				## Replace
    }
  }else {
    print("No stratum errors")
  }
  pts$STRATUM<-NULL
  pts$ERR<-NULL
  remove(zz)
  
  importSDD$pts[[s]]<-pts		## Put original OR modified pts back into workinglist
  return(importSDD)
}   






## 4/17/2017
## THis version uses gClip() to clip polygons instead of raster::intersect


## Creates a SpatialPolygonsDataFrame from the intersection of two SpatialPolygonsDataFrames
## Basically a wrapping of raster::intersect() now, just with additional opportunity to call area.add() and automatically added unique identifiers
intersector <- function(spdf1, ## A SpatialPolygonsShapefile
                        spdf1.attributefieldname, ## Name of the field in SPDF1 unique to the unit groups or units to take values from
                        spdf1.attributefieldname.output = NULL, ## Optional name of the field in the output SPDF to duplicate values from spdf1.attributefieldname in
                        spdf2, ## A SpatialPolygonsShapefile
                        spdf2.attributefieldname, ## Name of the field in SPDF2 unique to the unit groups or units to take values from
                        spdf2.attributefieldname.output = NULL,  ## Optional name of the field in the output SPDF to duplicate values from spdf2.attributefieldname in
                        area.ha = T, ## Add fields for area in hectares for individual polygons and the sum of those within unique combinations of the input attribute fields
                        area.sqkm = T, ## Add fields for area in square kilometers for individual polygons and the sum of those within unique combinations of the input attribute fields
                        projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") ## Standard NAD83
){
  ## We'll need Alber's equal area projection for area calculations
  projectionAL <- CRS("+proj=aea")
  ## Sanitization
  spdf1 <- spdf1 %>% spTransform(projection)
  spdf2 <- spdf2 %>% spTransform(projection)
  names(spdf1) <- str_to_upper(names(spdf1))
  names(spdf2) <- str_to_upper(names(spdf2))
  spdf1.attributefieldname <- str_to_upper(spdf1.attributefieldname)
  spdf2.attributefieldname <- str_to_upper(spdf2.attributefieldname)
  
  ## Find the intersection of the two SPDFs
  ## intersect.spdf.attribute <- raster::intersect(x = spdf1, y = spdf2)
  intersect.spdf.attribute <-gClip(spdf1,spdf2)
  
  ## Create a single field to serve as a unique identifier to dissolve the polygons by. This concatenates with a known nonsense string so we can split them later
  ## for (n in seq_along(intersect.spdf.attribute@data)) {
  for( n in 1:nrow(intersect.spdf.attribute@data)) {
    intersect.spdf.attribute@data$UNIQUE.IDENTIFIER[n] <- sha1(x = paste0(intersect.spdf.attribute@data[n, spdf1.attributefieldname],
                                                                          intersect.spdf.attribute@data[n, spdf2.attributefieldname]),
                                                               digits = 14)
  }
  
  
  ## If we're adding areas then:
  if (area.ha | area.sqkm) {
    ## Add the areas in hectares and square kilometers for each as called for
    intersect.spdf.attribute <- area.add(spdf = intersect.spdf.attribute,
                                         area.ha = area.ha,
                                         area.sqkm = area.sqkm)
    
    ## Now we summarize() the areas by unique identifier and then merge that with the original data frame and use it to overwrite the original data frame
    ## The arguments to summarize() are specific to what columns exist and what columns are therefore being added, so there are three alternatives
    if (area.ha & area.sqkm) {
      ## When there are both units represented
      ## group_by_() is used instead of group_by() so that we can provide strings as arguments to let us programmatically use the attributefieldname.output values
      intersect.spdf.attribute@data <- group_by(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.HA.UNIT.SUM = sum(AREA.HA), AREA.SQKM.UNIT.SUM = sum(AREA.SQKM)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    } else if (!(area.ha) & area.sqkm) {
      ## When there's no area.ha
      intersect.spdf.attribute@data <- group_by_(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.SQKM.UNIT.SUM = sum(AREA.SQKM)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    } else if (area.ha & !(area.sqkm)) {
      ## When there's no area.sqkm
      intersect.spdf.attribute@data <- group_by_(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.HA.UNIT.SUM = sum(AREA.HA)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    }
  }
  
  ## Create the fields requested if they' exist're specified
  if (!is.null(spdf1.attributefieldname.output)) {
    intersect.spdf.attribute@data[, spdf1.attributefieldname.output] <- intersect.spdf.attribute@data[, spdf1.attributefieldname]
  }
  if (!is.null(spdf2.attributefieldname.output)) {
    intersect.spdf.attribute@data[, spdf2.attributefieldname.output] <- intersect.spdf.attribute@data[, spdf2.attributefieldname]
  }
  
  ## Return the final SPDF, making sure to project it into NAD83 (or whatever projection was provided to override the default)
  return(intersect.spdf.attribute %>% spTransform(projection))
}
## This version was produced/sent by Nelson 12 April 2017, and uses gIntersection in-line.  intersector() 4/17/2017 is very similar but uses gClip()


intersector.alt <- function(spdf1, ## A SpatialPolygonsShapefile
                            spdf1.attributefieldname, ## Name of the field in SPDF1 unique to the unit groups or units to take values from
                            spdf1.attributefieldname.output = NULL, ## Optional name of the field in the output SPDF to duplicate values from spdf1.attributefieldname in
                            spdf2, ## A SpatialPolygonsShapefile
                            spdf2.attributefieldname, ## Name of the field in SPDF2 unique to the unit groups or units to take values from
                            spdf2.attributefieldname.output = NULL,  ## Optional name of the field in the output SPDF to duplicate values from spdf2.attributefieldname in
                            area.ha = T, ## Add fields for area in hectares for individual polygons and the sum of those within unique combinations of the input attribute fields
                            area.sqkm = T, ## Add fields for area in square kilometers for individual polygons and the sum of those within unique combinations of the input attribute fields
                            projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") ## Standard NAD83
){
  ## We'll need Alber's equal area projection for area calculations
  projectionAL <- CRS("+proj=aea")
  ## Sanitization
  if (spdf1@proj4string@projargs != spdf2@proj4string@projargs) {
    ## Make sure that the points also adhere to the same projection
    spdf1 <- spdf1 %>% spTransform(projection)
    spdf2 <- spdf2 %>% spTransform(projection)
  }
  names(spdf1@data) <- str_to_upper(names(spdf1@data))
  names(spdf2@data) <- str_to_upper(names(spdf2@data))
  spdf1.attributefieldname <- str_to_upper(spdf1.attributefieldname)
  spdf2.attributefieldname <- str_to_upper(spdf2.attributefieldname)
  ## Create new columns that we can drop later. This is in case the field names were the same in both SPDFs
  spdf1@data[, paste0(spdf1.attributefieldname, ".spdf1")] <- spdf1@data[, spdf1.attributefieldname]
  spdf2@data[, paste0(spdf2.attributefieldname, ".spdf2")] <- spdf2@data[, spdf2.attributefieldname]
  
  
  intersect.sp.attribute <- rgeos::gIntersection(spdf1,
                                                 spdf2,
                                                 byid = T,
                                                 drop_lower_td = T)
  
  ## Now we need to build the data frame that goes back into this. It's a pain
  ## Get the rownames from the polygons. This will consist of the two row names from spdf1 and spdf2 separated by a " "
  intersection.rownames <- intersect.sp.attribute %>% row.names() %>% strsplit(split = " ")
  
  ## Create an empty data frame that we can add the constructed rows to
  intersection.dataframe <- data.frame()
  ## For each of the intersection polygons, create a row with the attributes from the source polygons
  for (row in 1:length(intersection.rownames)) {
    intersection.dataframe <- rbind(intersection.dataframe,
                                    cbind(
                                      spdf1@data[intersection.rownames[[row]][1],],
                                      spdf2@data[intersection.rownames[[row]][2],]
                                    ))
  }
  rownames(intersection.dataframe) <- row.names(intersect.sp.attribute)
  
  intersect.spdf.attribute <- sp::SpatialPolygonsDataFrame(Sr = intersect.sp.attribute,
                                                           data = intersection.dataframe)
  
  ## Create a single field to serve as a unique identifier to dissolve the polygons by.
  for (n in 1:nrow(intersect.spdf.attribute@data)) {
    intersect.spdf.attribute@data$UNIQUE.IDENTIFIER[n] <- sha1(x = paste0(intersect.spdf.attribute@data[n, paste0(spdf1.attributefieldname, ".spdf1")],
                                                                          intersect.spdf.attribute@data[n, paste0(spdf2.attributefieldname, ".spdf2")]),
                                                               digits = 14)
  }
  
  ## Remove those two columns we made that were just duplicates of existing columns but with .spdf1 or .spdf2 appended
  intersect.spdf.attribute@data <- intersect.spdf.attribute@data[, names(intersect.spdf.attribute@data)[!(intersect.spdf.attribute@data %in% c(paste0(spdf1.attributefieldname, ".spdf1"), paste0(spdf2.attributefieldname, ".spdf2")))]]
  
  ## If we're adding areas then:
  if (area.ha | area.sqkm) {
    ## Add the areas in hectares and square kilometers for each as called for
    intersect.spdf.attribute <- area.add(spdf = intersect.spdf.attribute,
                                         area.ha = area.ha,
                                         area.sqkm = area.sqkm)
    ## Now we summarize() the areas by unique identifier and then merge that with the original data frame and use it to overwrite the original data frame
    ## The arguments to summarize() are specific to what columns exist and what columns are therefore being added, so there are three alternatives
    if (area.ha & area.sqkm) {
      ## When there are both units represented
      ## group_by_() is used instead of group_by() so that we can provide strings as arguments to let us programmatically use the attributefieldname.output values
      intersect.spdf.attribute@data <- group_by(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.HA.UNIT.SUM = sum(AREA.HA), AREA.SQKM.UNIT.SUM = sum(AREA.SQKM)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    } else if (!(area.ha) & area.sqkm) {
      ## When there's no area.ha
      intersect.spdf.attribute@data <- group_by_(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.SQKM.UNIT.SUM = sum(AREA.SQKM)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    } else if (area.ha & !(area.sqkm)) {
      ## When there's no area.sqkm
      intersect.spdf.attribute@data <- group_by_(intersect.spdf.attribute@data, UNIQUE.IDENTIFIER) %>%
        summarize(AREA.HA.UNIT.SUM = sum(AREA.HA)) %>%
        merge(x = intersect.spdf.attribute@data, y = .)
    }
  }
  
  ## Create the fields requested if they' exist're specified
  if (!is.null(spdf1.attributefieldname.output)) {
    intersect.spdf.attribute@data[, spdf1.attributefieldname.output] <- intersect.spdf.attribute@data[, spdf1.attributefieldname]
  }
  if (!is.null(spdf2.attributefieldname.output)) {
    intersect.spdf.attribute@data[, spdf2.attributefieldname.output] <- intersect.spdf.attribute@data[, spdf2.attributefieldname]
  }
  
  ## Return the final SPDF, making sure to project it into NAD83 (or whatever projection was provided to override the default)
  return(intersect.spdf.attribute %>% spTransform(projection))
}## 4/17/2017
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
##4/13/2017
## This is a modified version of Nelson's sdd.reader and is designed to read all the LMF information required to integrate with AIM points

## Returns a named list of SPDFs: lmfpts, lmfstrata, lmfsegments.
## lmfpts is a list of point SPDFs, lmfstrata is a list of strata SPDFs, lmfsegments is a list of segment SPDFs.
## The SPDFs are all named using the lmf filename provided in lmf.src as the suffix and lmf### as the prefix where ### is pts, strata, and segments. 
## If a feature class can't be found, the program blows up  - the user is responsible to make sure files exist!!!!
## lmfstrata NEEDS to be pre-clipped to BLM surface management area - i.e., only BLM lands.

## Unlike AIM SDDs, all BLM LMF data (pt locations & monitoring results) are stored in 1 master geodatabase. A field office shapefile is 
##  used to clip data from this master. The path to this master must be provided.  The field office shapefile must correspond to the names(s) 
##  in lmf.src.  E.g., If lmf.src<-BruneaFO, then the field office shapefile is BruneauFO.shp.... 

##  Data of multiple field offices can be ingested.  For this reason, data.src is used to store the pathname of data files. 
##  Field offices listed in lmf.src must be paired (i.e., same order) with field offices listed in sdd.src for certain processes.  Otherwise,
##  you can acquire as many as necessary and concatentate all into the first entry of the named list called lmfoutput in main.r.



read.lmf <- function(data.src = "",  ## filepath for each entry in lmf.src
                       lmf.src, ## A character string or vector of character strings with the filename[s] for the relevant .shp in the filepath src
                       func = "arcgisbinding", ## This can be "readOGR" or "arcgisbinding" depending on which you prefer to or can use
                       projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
){
  
  
  ## readOGR() wrapped in safely() so that it will return NULL instead of an error
  safe.readOGR <- safely(readOGR, otherwise = NULL)
  
  ## Sanitization
  func <- str_to_upper(func)
  
  
  
  
  ## Cycle thru lmf.src and process each field office in lmf.src.
  
  switch(func,
         READOGR = {
           index<-0
           ## Looped so that it can execute across all the lmfs in the vector (if there are more than one)
           for (s in lmf.src) {
             index<-index+1
             
             lmfstrata <- safe.readOGR(dsn=data.src[index],
                                       layer = paste(s,"_strataSMA",sep="") ,
                                       stringsAsFactors = F)[[1]] ## The [[]] is to get the SPDF (or NULL) out of the list returned by the safely()
             # The spTransform() is just to be safe
             if (!is.null(lmfstrata)) {
               lmfstrata <- spTransform(lmfstrata, projection)
               names(lmfstrata@data) <- str_to_upper(names(lmfstrata@data))  
             }
             ## Stores the current lmfsrata SPDF with the name lmfstrata.[LMF name]
             assign(x = paste("lmfstrata", s, sep = "."), value = lmfstrata)
             
             
             
             ######## Read in the field office shapefile
             lmfFO <- safe.readOGR(dsn = data.src[index],
                                   layer = s,
                                   stringsAsFactors = F)[[1]]
             if (!is.null(lmfFO)) {
               lmfFO <- spTransform(lmfFO, projection)
               names(lmfFO@data) <- str_to_upper(names(lmfFO@data)) 
             }
             
             ## Clip the points from the master.  The master inlcudes all BLM LMF points for all measurement years
             lmfpts<-NULL
             lmfmaster.spdf <- spTransform(lmfmaster.spdf, projection)
             lmfpts<-raster::intersect(lmfmaster.spdf,lmfFO)
             if (!is.null(lmfpts)) {
               lmfpts <- spTransform(lmfpts, projection)
               lmfpts@data$FIELDOFFICE<-s	
               lmfpts@data$AIMLMF<-2		## Set source type
               lmfpts@data$FINAL_DESIG<-"TS"    ## Set final_desig
               names(lmfpts@data) <- str_to_upper(names(lmfpts@data)) 
               
             }
             assign(x = paste("lmfpts", s, sep = "."), value = lmfpts)
             
             
             
             ######## Read in the Segments
             lmfsegments <- safe.readOGR(dsn = data.src[index],
                                         layer = paste(s,"_segments",sep=""),
                                         stringsAsFactors = F)[[1]]
             if (!is.null(lmfsegments)) {
               lmfsegments <- spTransform(lmfsegments, projection)
               names(lmfsegments@data) <- str_to_upper(names(lmfsegments@data)) 
             } 
             assign(x = paste("lmfsegments", s, sep = "."), value = lmfsegments)
           }
         },
         ARCGISBINDING = {
           index<-0
           for (s in lmf.src) {
             index<-index+1
             
             ## Identify/create the filepath to the strata feature class inside the current LMF
             lmfstrata <- paste(data.src[index], s, sep = "/")
             lmfstrata<-paste(lmfstrata,"_strataSMA.shp",sep="")
             ## Creates an SPDF with the name lmfstrata.[LMF name] using the filepath to that feature class
             assign(x = paste("lmfstrata",s, sep = "."),
                    value = lmfstrata %>% arc.open() %>% arc.select %>%
                      SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .) %>%spTransform(projection)
             )
             eval(parse(text = paste0("names(", paste("lmfstrata", s, sep = "."), ") <- str_to_upper(names(", paste("lmfstrata", s, sep = "."), "))")))
             
             #Read in the segments
             lmfsegments <- paste(data.src[index], s, sep = "/")
             lmfsegments<-paste(lmfsegments,"_segments.shp",sep="")
             assign(x = paste("lmfsegments",s, sep = "."),
                    value = lmfsegments %>% arc.open() %>% arc.select %>%
                      SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .) %>%spTransform(projection)
             )
             eval(parse(text = paste0("names(", paste("lmfsegments", s, sep = "."), ") <- str_to_upper(names(", paste("lmfsegments", s, sep = "."), "))")))
             
             #Read in the Field Office, then clip pts from masterdata 
             lmfFO <- paste(data.src[index], s, sep = "/")
             lmfFO <-paste(lmfFO,".shp",sep="")
             assign(x = "lmfFO",
                    value = lmfFO %>% arc.open() %>% arc.select %>%
                      SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .)%>%spTransform(projection))
             names(lmfFO@data) <- str_to_upper(names(lmfFO@data)) 
             
             ## Clip pts from masterdata.  The master inlcudes all BLM LMF points for all measurement years
             lmfpts<-NULL
             lmfmaster.spdf <- spTransform(lmfmaster.spdf, projection)
             lmfpts<-raster::intersect(lmfmaster.spdf,lmfFO)  
             if (!is.null(lmfpts)) {
               lmfpts <- spTransform(lmfpts, projection)
               lmfpts@data$FIELDOFFICE<-s	## set field office for latter reporting
               lmfpts@data$AIMLMF<-2		## Set source type
               lmfpts@data$FINAL_DESIG<-"TS"   ## Set final_desig
               names(lmfpts@data) <- str_to_upper(names(lmfpts@data))
             }           
             assign(x = paste("lmfpts", s, sep = "."), value = lmfpts)
           }
         }
  )
  
  
  
  ## 
  
  
  ## Create a list of the SPDFs.
  ## This programmatically creates a string of the existing object names that start with "lmfstrata." separated by commas
  ## then wraps that in "list()" and runs the whole string through parse() and eval() to execute it, creating a list from those SPDFs
  lmfstrata.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^lmfstrata\\.") & !grepl(x = ls(), pattern = "^lmfstrata.list$")], collapse = "`, `"), "`)")))
  ## Rename them with the correct SDD name because they'll be in the same order that ls() returned them earlier. Also, we need to remove sf.list itself
  names(lmfstrata.list) <- ls()[grepl(x = ls(), pattern = "^lmfstrata\\.") & !grepl(x = ls(), pattern = "^lmfstrata.list$")] %>% str_replace(pattern = "^lmfstrata\\.", replacement = "")
  
  ## Creating the named list of all the pts SPDFs created by the loop
  lmfpts.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^lmfpts\\.") & !grepl(x = ls(), pattern = "^lmfpts.list$")], collapse = "`, `"), "`)")))
  names(lmfpts.list) <- ls()[grepl(x = ls(), pattern = "^lmfpts\\.") & !grepl(x = ls(), pattern = "^lmfpts.list$")] %>% str_replace(pattern = "^lmfpts\\.", replacement = "")
  
  ## Creating the named list of all the strata SPDFs created by the loop
  lmfsegments.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^lmfsegments\\.") & !grepl(x = ls(), pattern = "^lmfsegments.list$")], collapse = "`, `"), "`)")))
  names(lmfsegments.list) <- ls()[grepl(x = ls(), pattern = "^lmfsegments\\.") & !grepl(x = ls(), pattern = "^lmfsegments.list$")] %>% str_replace(pattern = "^lmfsegments\\.", replacement = "")
  
  lmfoutput <- list(lmfstrata = lmfstrata.list, lmfpts = lmfpts.list, lmfsegments = lmfsegments.list)
  
  return(lmfoutput)
}

lmf.reader <- function(data.src = "",  ## filepath for each entry in lmf.src
                       lmf.src, ## A character string or vector of character strings with the filename[s] for the relevant .shp in the filepath src
                       func = "arcgisbinding", ## This can be "readOGR" or "arcgisbinding" depending on which you prefer to or can use
                       projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
){
  
  
  ## readOGR() wrapped in safely() so that it will return NULL instead of an error
  safe.readOGR <- safely(readOGR, otherwise = NULL)
  
  ## Sanitization
  func <- str_to_upper(func)
  
  
  
  
  ## Cycle thru lmf.src and process each field office in lmf.src.
  
  switch(func,
         READOGR = {
           index<-0
           ## Looped so that it can execute across all the lmfs in the vector (if there are more than one)
           for (s in lmf.src) {
             index<-index+1
             
             lmfstrata <- safe.readOGR(dsn=data.src[index],
                                       layer = paste(s,"_strataSMA",sep="") ,
                                       stringsAsFactors = F)[[1]] ## The [[]] is to get the SPDF (or NULL) out of the list returned by the safely()
             # The spTransform() is just to be safe
             if (!is.null(lmfstrata)) {
               lmfstrata <- spTransform(lmfstrata, projection)
               names(lmfstrata@data) <- str_to_upper(names(lmfstrata@data))  
             }
             ## Stores the current lmfsrata SPDF with the name lmfstrata.[LMF name]
             assign(x = paste("lmfstrata", s, sep = "."), value = lmfstrata)
             
             
             
             ######## Read in the field office shapefile
             lmfFO <- safe.readOGR(dsn = data.src[index],
                                   layer = s,
                                   stringsAsFactors = F)[[1]]
             if (!is.null(lmfFO)) {
               lmfFO <- spTransform(lmfFO, projection)
               names(lmfFO@data) <- str_to_upper(names(lmfFO@data)) 
             }
             
             ## Clip the points from the master.  The master inlcudes all BLM LMF points for all measurement years
             lmfpts<-NULL
             lmfmaster.spdf <- spTransform(lmfmaster.spdf, projection)
             lmfpts<-raster::intersect(lmfmaster.spdf,lmfFO)
             if (!is.null(lmfpts)) {
               lmfpts <- spTransform(lmfpts, projection)
               lmfpts@data$FIELDOFFICE<-s	
               lmfpts@data$AIMLMF<-2		## Set source type
               lmfpts@data$FINAL_DESIG<-"TS"    ## Set final_desig
               names(lmfpts@data) <- str_to_upper(names(lmfpts@data)) 
               
             }
             assign(x = paste("lmfpts", s, sep = "."), value = lmfpts)
             
             
             
             ######## Read in the Segments
             lmfsegments <- safe.readOGR(dsn = data.src[index],
                                         layer = paste(s,"_segments",sep=""),
                                         stringsAsFactors = F)[[1]]
             if (!is.null(lmfsegments)) {
               lmfsegments <- spTransform(lmfsegments, projection)
               names(lmfsegments@data) <- str_to_upper(names(lmfsegments@data)) 
             } 
             assign(x = paste("lmfsegments", s, sep = "."), value = lmfsegments)
           }
         },
         ARCGISBINDING = {
           index<-0
           for (s in lmf.src) {
             index<-index+1
             
             ## Identify/create the filepath to the strata feature class inside the current LMF
             lmfstrata <- paste(data.src[index], s, sep = "/")
             lmfstrata<-paste(lmfstrata,"_strataSMA.shp",sep="")
             ## Creates an SPDF with the name lmfstrata.[LMF name] using the filepath to that feature class
             assign(x = paste("lmfstrata",s, sep = "."),
                    value = lmfstrata %>% arc.open() %>% arc.select %>%
                      SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .) %>%spTransform(projection)
             )
             eval(parse(text = paste0("names(", paste("lmfstrata", s, sep = "."), ") <- str_to_upper(names(", paste("lmfstrata", s, sep = "."), "))")))
             
             #Read in the segments
             lmfsegments <- paste(data.src[index], s, sep = "/")
             lmfsegments<-paste(lmfsegments,"_segments.shp",sep="")
             assign(x = paste("lmfsegments",s, sep = "."),
                    value = lmfsegments %>% arc.open() %>% arc.select %>%
                      SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .) %>%spTransform(projection)
             )
             eval(parse(text = paste0("names(", paste("lmfsegments", s, sep = "."), ") <- str_to_upper(names(", paste("lmfsegments", s, sep = "."), "))")))
             
             #Read in the Field Office, then clip pts from masterdata 
             lmfFO <- paste(data.src[index], s, sep = "/")
             lmfFO <-paste(lmfFO,".shp",sep="")
             assign(x = "lmfFO",
                    value = lmfFO %>% arc.open() %>% arc.select %>%
                      SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .)%>%spTransform(projection))
             names(lmfFO@data) <- str_to_upper(names(lmfFO@data)) 
             
             ## Clip pts from masterdata.  The master inlcudes all BLM LMF points for all measurement years
             lmfpts<-NULL
             lmfmaster.spdf <- spTransform(lmfmaster.spdf, projection)
             lmfpts<-raster::intersect(lmfmaster.spdf,lmfFO)  
             if (!is.null(lmfpts)) {
               lmfpts <- spTransform(lmfpts, projection)
               lmfpts@data$FIELDOFFICE<-s	## set field office for latter reporting
               lmfpts@data$AIMLMF<-2		## Set source type
               lmfpts@data$FINAL_DESIG<-"TS"   ## Set final_desig
               names(lmfpts@data) <- str_to_upper(names(lmfpts@data))
             }           
             assign(x = paste("lmfpts", s, sep = "."), value = lmfpts)
           }
         }
  )
  
  
  
  ## 
  
  
  ## Create a list of the SPDFs.
  ## This programmatically creates a string of the existing object names that start with "lmfstrata." separated by commas
  ## then wraps that in "list()" and runs the whole string through parse() and eval() to execute it, creating a list from those SPDFs
  lmfstrata.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^lmfstrata\\.") & !grepl(x = ls(), pattern = "^lmfstrata.list$")], collapse = "`, `"), "`)")))
  ## Rename them with the correct SDD name because they'll be in the same order that ls() returned them earlier. Also, we need to remove sf.list itself
  names(lmfstrata.list) <- ls()[grepl(x = ls(), pattern = "^lmfstrata\\.") & !grepl(x = ls(), pattern = "^lmfstrata.list$")] %>% str_replace(pattern = "^lmfstrata\\.", replacement = "")
  
  ## Creating the named list of all the pts SPDFs created by the loop
  lmfpts.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^lmfpts\\.") & !grepl(x = ls(), pattern = "^lmfpts.list$")], collapse = "`, `"), "`)")))
  names(lmfpts.list) <- ls()[grepl(x = ls(), pattern = "^lmfpts\\.") & !grepl(x = ls(), pattern = "^lmfpts.list$")] %>% str_replace(pattern = "^lmfpts\\.", replacement = "")
  
  ## Creating the named list of all the strata SPDFs created by the loop
  lmfsegments.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^lmfsegments\\.") & !grepl(x = ls(), pattern = "^lmfsegments.list$")], collapse = "`, `"), "`)")))
  names(lmfsegments.list) <- ls()[grepl(x = ls(), pattern = "^lmfsegments\\.") & !grepl(x = ls(), pattern = "^lmfsegments.list$")] %>% str_replace(pattern = "^lmfsegments\\.", replacement = "")
  
  lmfoutput <- list(lmfstrata = lmfstrata.list, lmfpts = lmfpts.list, lmfsegments = lmfsegments.list)
  
  return(lmfoutput)
}


##############################################
## ConcatLMF(lmfoutput) - concatenates the named list lmfoutput (passed here as importLMF)

ConcatLMF<-function(importLMF)
{
  index<-0
  OID<-0
  segID<-0
  for(s in names(importLMF$lmfpts) ) {
    index<-index+1
    if(index==1) {
      pts<-importLMF$lmfpts[[s]]
      for(i in 1:nrow(pts)) {				## THis makes sure we have OBJECTID 
        OID<-OID+1
        pts$OBJECTID[i]<-OID
      }
      lmf<-pts[,c("OBJECTID","PLOTKEY","DTVISIT","FIELDOFFICE","AIMLMF","FINAL_DESIG")]
      strata<-importLMF$lmfstrata[[s]]
      seg<-importLMF$lmfsegments[[s]]
      for(i in 1:nrow(seg)) {				## THis ensures unique seg codes across the aggregate (segcode starts with 1 in each FO)
        segID<-segID+1
        seg$SEGCODE[i]<-segID
      }
    }else {
      pts<-importLMF$lmfpts[[s]]
      for(i in 1:nrow(pts)) {
        OID<-OID+1
        pts$OBJECTID[i]<-OID
      }
      lmf2<-pts[,c("OBJECTID","PLOTKEY","DTVISIT","FIELDOFFICE","AIMLMF","FINAL_DESIG")]
      strata2<-importLMF$lmfstrata[[s]]
      seg2<-importLMF$lmfsegments[[s]]
      lmf<-rbind(lmf,lmf2)
      strata<-rbind(strata,strata2)
      for(i in 1:nrow(seg2)) {
        segID<-segID+1
        seg2$SEGCODE[i]<-segID
      }
      seg<-rbind(seg,seg2)
    }
  }
  index<-0
  for(s in names(importLMF$lmfpts) ) {
    index<-index+1
    if(index==1) {			## Update first entry
      importLMF$lmfpts[[s]]<-lmf
      importLMF$lmfstrata[[s]]<-strata
      importLMF$lmfsegments[[s]]<-seg
    }else {			## Set other entries to null
      importLMF$lmfpts[[s]]<-NULL
      importLMF$lmfstrata[[s]]<-NULL
      importLMF$lmfsegments[[s]]<-NULL
    }
  }
  return(importLMF)
}

## 4/17/2017

#' Importing Sample Design Databases for AIM Sample Designs
#'
#' This function imports one or more Sample Design Database[s] and returns a list of the named lists sf, pts, and strata. The named lists contain SpatialPoints/PolygonsDataFrames of the sample frame, #  points, and strata from each of the geodatabases. The SPDFs are named using the filename of the geodatabase source, so that each list has one SPDF named for each geodatabase imported and those names #  are identical between lists. If a Sample Design Database is missing any one of those features, a NULL value replaces the SPDF.
#' @param src Character string defining the filepath containing the sample design database[s]
#' @param sdd.src Character string or character vector containing the filenames of the geodatabases to import. Each filename should include the extension ".gdb"
#' @param func Character string. Defines whether to use the rgdal or arcgisbinding package to read in the geodatabases. Defaults to "arcgisbinding". Valid values are "arcgisbinding" and "readogr". This #  is not case sensitive
#' @param projection CRS string. Defaults to NAD83 CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"). Is used to reproject all SPDFs in order to perform spatial manipulations

## Reads in SDDs. Returns a named list of lists of SPDFs: sf, pts, strata.
## sf is a list of sample frame SPDFs, pts is a list of point SPDFs, strata is a list of stratfication SPDFs
## The SPDFs are all named using the SDD filename provided in sdd.src so that output$sf$generic_design.gdb has the sample frame that corresponds to output$pts$generic_design.gdb
## The index order is maintained as well, so output[1][1] and output[2][1] correspond to the same SDD source
## If a feature class couldn't be found, there will be a NULL instead of SPDF for that SDD in the list

## You can delete plots with Final_DESIG =NA (gets rid of some junky designations especially prevalent in the older SDDs (<= 2014)
## You can omit picking up pts file, which is necessary when using ONLY LMF points
## You can omit picking up strata file (if it exists), which when using only LMF points forces weights to be based on the frame (portion remaining after reporting-unit clip)
## You can run a check to determine if TerrADat PrimaryKeys exist for Target Sampled points, and if only Target Sampled points have a PrimaryKey.  CUrrently, performing this check
##     corrupts the named list to the point that subsequent code can no longer read the list!!!  For now, only use this option for diagnostic purposes - must set it to F to derive
##     a useable list....

## For pt files that we pick up, we need to add 4 key fields to faciliate processing LMF data.  Subsequent logic requires these fields in SDD pts files even if LMF points are not
## being used.


sdd.reader <- function(srcp = "", ## A character string or vector of character strings with the pathname[s] for the relevant .gdb in the filepath sdd.src
                       sdd.src, ## A character string or vector of character strings with the filename[s] for the relevant .gdb in the filepath src
                       func = "arcgisbinding", ## This can be "readOGR" or "arcgisbinding" depending on which you prefer to or can use
                       validate.keys = F, ## Should the process also produce a data frame in the output design databases that have issues with final designations or TerrAdat primary keys?
                       target.values = c("Target Sampled",
                                         "TS"),
                       omitNAdesignations = F, 	## Strip out plots with a final designation value of NA
                       omit_pts=F,		## omit the SDD pt files - useful when using LMF pts
                       omit_strata=F,		## omit the SDD strata files - useful when using LMF pts
                       projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
) {
  
  ## readOGR() wrapped in safely() so that it will return NULL instead of an error
  safe.readOGR <- safely(readOGR, otherwise = NULL)
  
  ## Sanitization
  func <- str_to_upper(func)
  target.values <- c(target.values,
                     "Target Sampled",
                     "TS") %>% unique() %>% str_to_upper()
  
  
  ## Checking that func is a valid value
  if (!(func %in% c("ARCGISBINDING", "READOGR"))) {
    print("The argument func needs to be 'arcgisbinding' or 'readOGR'")
  }
  
  
  switch(func,
         READOGR = {
           ## Looped so that it can execute across all the SDDs in the vector 
           index<-0;		## Displacement into src
           for (s in sdd.src) {
             index<-index+1
             src<-srcp[index]
             ## Read in the sample frame feature class inside the current SDD.
             sf <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                layer = "Terra_Sample_Frame",
                                stringsAsFactors = F)[[1]] ## The [[]] is to get the SPDF (or NULL) out of the list returned by the safely()
             # The spTransform() is just to be safe, but probably isn't necessary
             if (!is.null(sf)) {
               sf <- spTransform(sf, projection)
               ## Sanitize the column names  
               names(sf@data) <- str_to_upper(names(sf@data)) 
             }
             
             
             
             ## Stores the current sf SPDF with the name sf.[SDD name]
             assign(x = paste("sf", s, sep = "."), value = sf)
             
             
             
             if(omit_strata==TRUE) {
               strata<-NULL
             }else {	
               #Read in the Strata
               strata <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                      layer = "Terra_Strtfctn",
                                      stringsAsFactors = F)[[1]]
               if (!is.null(strata)) {
                 strata <- spTransform(strata, projection)
                 names(strata@data) <- str_to_upper(names(strata@data))
               }
             }
             
             
             assign(x = paste("strata", s, sep = "."), value = strata)
             
             
             if(omit_pts==TRUE) {
               points<-NULL
             }else {
               #Read in the Points
               points <- safe.readOGR(dsn = paste(src, s, sep = "/"),
                                      layer = "Terra_Sample_Points",
                                      stringsAsFactors = F)[[1]]
               
               if (!is.null(points)) {
                 points <- spTransform(points, projection)             	
                 names(points@data) <- str_to_upper(names(points@data))
               }
               
               
               
               ## Strip out points with an NA value in the FINAL_DESIG field if requested
               if (omitNAdesignations) {
                 points <- points[!is.na(points@data$FINAL_DESIG),]
               }
             }
             assign(x = paste("pts", s, sep = "."), value = points)
           }
         },
         ARCGISBINDING = {
           index<-0
           for (s in sdd.src) {
             index<-index+1
             src<-srcp[index]
             ## Identify/create the filepath to the sample frame feature class inside the current SDD
             sf <- paste(src, s, "Terra_Sample_Frame", sep = "/")
             ## Creates an SPDF with the name sf.[SDD name] using the filepath to that feature class
             
             assign(x = paste("sf", s, sep = "."),
                    value = sf %>% arc.open() %>% arc.select %>%
                      SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .)%>%spTransform(projection)
             )
             ##eval(parse(text = paste0("names(", paste("sf", s, sep = "."), ") <- str_to_upper(names(", paste("sf", s, sep = "."), "))")))
             tempsf<-get(paste0(paste("sf", s, sep = ".")))
             names(tempsf) <- str_to_upper(names(tempsf))
             assign(x = paste("sf", s, sep = "."), value = tempsf)   
             
             if(omit_strata) {
               assign(x = paste("strata", s, sep = "."),value = NULL)
             }else {
               #Read in the Strata
               #First check for strata
               ## Identify/create the filepath to the design stratification feature class inside the current SDD
               strata <- paste(src, s, "Terra_Strtfctn", sep = "/")
               #this loads enough of the feature class to tell if there are strata
               strata <- strata %>% arc.open() %>% arc.select
               #check for strata, if there are, then we will finish loading the file. 
               if (nrow(strata) > 0) {
                 ## Identify/create the filepath to the design stratification feature class inside the current SDD
                 strata <- paste(src, s, "Terra_Strtfctn", sep = "/")
                 ## Creates an SPDF with the name strat.[SDD name] using the filepath to that feature class
                 assign(x = paste("strata", s, sep = "."),
                        value = strata %>% arc.open() %>% arc.select %>%
                          SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()},
                                                   data = .)%>%spTransform(projection))
                 ##eval(parse(text = paste0("names(", paste("strata", s, sep = "."), ") <- str_to_upper(names(", paste("strata", s, sep = "."), "))")))
                 tempstrata<-get(paste0(paste("strata", s, sep = ".")))
                 names(tempstrata) <- str_to_upper(names(tempstrata))
                 assign(x = paste("strata", s, sep = "."), value = tempstrata)
                 
               } else {
                 ## If the stratification feature class is empty, we'll just save ourselves some pain and store NULL
                 assign(x = paste("strata", s, sep = "."),
                        value = NULL)
               }
             }
             
             if(omit_pts==TRUE) {
               assign(x = paste("pts", s, sep = "."),
                      value = NULL)
             }else {
               #Read in the Points
               ## Identify/create the filepath to the design points feature class inside the current SDD
               pts <- paste(src, s, "Terra_Sample_Points", sep = "/")
               ## Creates an SPDF with the name pts.[SDD name] using the filepath to that feature class
               assign(x = paste("pts", s, sep = "."),
                      value = pts %>% arc.open() %>% arc.select %>%
                        #read in the feature class, notice the difference between Polygons and points (different function with different arguments needs)
                        SpatialPointsDataFrame(coords = {arc.shape(.) %>% arc.shape2sp()})%>%spTransform(projection))
               ##eval(parse(text = paste0("names(", paste("pts", s, sep = "."), ") <- str_to_upper(names(", paste("pts", s, sep = "."), "))")))
               temppts<-get(paste0(paste("pts", s, sep = ".")))
               names(temppts) <- str_to_upper(names(temppts))
               assign(x = paste("pts", s, sep = "."), value = temppts)
               
               ## Strip out points with an NA value in the FINAL_DESIG field if asked
               if (omitNAdesignations) {
                 ## The eval() here and above not working anymore..  eval(parse(text = paste0(paste("pts", s, sep = "."), " <- ", paste("pts", s, sep = "."), "[!is.na(", paste("pts", s, sep = "."), "@data$FINAL_DESIG),]")))
                 temppts<-get(paste0(paste("pts", s, sep = ".")))
                 temppts <- temppts[!is.na(temppts@data$FINAL_DESIG),]
                 assign(x = paste("pts", s, sep = "."), value = temppts)
                 
               }
               
             }
           }
         }
  )
  
  
  ## Add 4 fields related to LMF pt processing.  SDD pt files require these fields even if LMF points are not being processed.
  ## Looping thru sdd.src to set these fields here eliminates duplicating code inside ReadOGR and inside ARCGISBINDING
  for(s in sdd.src) {
    temp.tmp<-get(paste("pts",s, sep="."))  ## Use a .spdf name that's different from pts.### 'cause a spdf named pts.### will unnecessarily be included in pts.list which just adds junk to the list.....
    if(!is.null(temp.tmp)) {
      temp.tmp@data$RELWEIGHT<-1.0	## Default to a relative weight of 1.  This can change if LMF points are in play.
      temp.tmp@data$AIMLMF<-1		## Designates that these pts are AIM pts.  Does not change.
      temp.tmp@data$LMFSTRATA<-0	## The LMF strata; effectively set to null.  If LMF points in play, likely will change to TYPE I (1) or Type II (2) stratum. 
      temp.tmp@data$FIELDOFFICE<-s	## Field office and optionally the design (i.e., the name of the SDD)
      assign(x = paste("pts", s, sep = "."), value = temp.tmp)
    }
  }
  
  
  
  ## Create a list of the sample frame SPDFs.
  ## This programmatically creates a string of the existing object names that start with "sf." separated by commas
  ## then wraps that in "list()" and runs the whole string through parse() and eval() to execute it, creating a list from those SPDFs
  ##  We want to maintain the order the user specified in sdd.src..................
  
  ## DO the SFs
  a<-ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")]
  b<-a[order(sdd.src)]
  sf.list <- eval(parse(text = paste0("list(`", paste(b, collapse = "`, `"), "`)")))
  
  a<-ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")]
  b<-a[order(sdd.src)]
  names(sf.list) <- b %>% str_replace(pattern = "^sf\\.", replacement = "")
  
  ## DO the pts
  a<-ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")]
  b<-a[order(sdd.src)]
  pts.list <- eval(parse(text = paste0("list(`", paste(b, collapse = "`, `"), "`)")))
  
  a<-ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")]
  b<-a[order(sdd.src)]
  names(pts.list) <- b %>% str_replace(pattern = "^pts\\.", replacement = "")
  
  ## DO the strata
  a<-ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")]
  b<-a[order(sdd.src)]
  strata.list <- eval(parse(text = paste0("list(`", paste(b, collapse = "`, `"), "`)")))
  
  a<-ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")]
  b<-a[order(sdd.src)]
  names(strata.list) <- b %>% str_replace(pattern = "^strata\\.", replacement = "")
  
  
  
  ####################### THE FOLLOWING is an alternative way to create the list; HOWEVER, the order of SDDs in the final list doesn't necessarily match the original order of sdd.src.
  ## We want to maintain the order 'cause we have an option (see MAIN module) to assign the stratum file of a SDD to other SDDs (sometimes necessary), and assignment is based
  ## on the order of the original sdd.src. 
  
  ## THe following picks up the SDD names in ascending order WHICH may differ from the specified order in sdd.src.
  ##sf.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")], collapse = "`, `"), "`)")))
  ## Rename them with the correct SDD name because they'll be in the same order that ls() returned them earlier. Also, we need to remove sf.list itself
  ##names(sf.list) <- ls()[grepl(x = ls(), pattern = "^sf\\.") & !grepl(x = ls(), pattern = "^sf.list$")] %>% str_replace(pattern = "^sf\\.", replacement = "")
  
  ## Creating the named list of all the pts SPDFs created by the loop
  ##pts.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")], collapse = "`, `"), "`)")))
  ##names(pts.list) <- ls()[grepl(x = ls(), pattern = "^pts\\.") & !grepl(x = ls(), pattern = "^pts.list$")] %>% str_replace(pattern = "^pts\\.", replacement = "")
  
  ## Creating the named list of all the strata SPDFs created by the loop
  ##strata.list <- eval(parse(text = paste0("list(`", paste(ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")], collapse = "`, `"), "`)")))
  ##names(strata.list) <- ls()[grepl(x = ls(), pattern = "^strata\\.") & !grepl(x = ls(), pattern = "^strata.list$")] %>% str_replace(pattern = "^strata\\.", replacement = "")
  
  ########################
  
  output <- list(sf = sf.list, pts = pts.list, strata = strata.list)
  
  
  ######################################### Validate.keys processing - NOTE:  the warning!!!
  ## if validate.keys - this corrupts the list, so only use it for diagnostics...
  if (validate.keys) {
    ## Initialize the output data frame
    key.errors.df <- data.frame()
    
    ## Check each design database in turn
    for (sdd in names(output$pts)) {
      ## Get the @data slot from the SPDF for the points for this design
      pts.df <- output$pts[[sdd]] %>% .@data
      ## Sanitize the field names
      names(pts.df) <- str_to_upper(names(pts.df))
      
      ## Grab all the lines where the point was sampled, but there's not a correct PrimaryKey value
      errors.missing.tdat <- pts.df %>%
        filter(FINAL_DESIG %in% target.values,
               !grepl(x = TERRA_TERRADAT_ID, pattern = "^[0-9]{15,24}-[0-9]{1,3}-[0-9]{1,3}$")) %>% .[, c("TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")]
      ## If that turned up anything, then alert the user and add the information about all the points with that error to the output data frame
      if (nrow(errors.missing.tdat) > 0) {
        print(paste0("In ", sdd, ", ", nrow(errors.missing.tdat), " points were designated as 'target sampled' but missing a valid PrimaryKey value. See data frame 'errors' in the output for details."))
        errors.missing.tdat$SDD <- sdd
        errors.missing.tdat$ERROR <- "Point is designated as 'target sampled' but is missing a valid PrimaryKey value"
        key.errors.df <- rbind(key.errors.df, errors.missing.tdat[, c("SDD", "ERROR", "TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")])
      }
      
      ## Grab all the lines where there's a proper PrimaryKey value but there's not a target designation
      errors.missing.desig <- pts.df %>%
        filter(!(FINAL_DESIG %in% target.values),
               grepl(x = TERRA_TERRADAT_ID, pattern = "^[0-9]{15,24}-[0-9]{1,3}-[0-9]{1,3}$")) %>% .[, c("TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")]
      if (nrow(errors.missing.desig) > 0) {
        print(paste0("In ", sdd, ", ", nrow(errors.missing.desig), " points have a valid PrimaryKey value but are not designated as 'target sampled.' See the data frame 'errors' in the output for details."))
        errors.missing.desig$SDD <- sdd
        errors.missing.desig$ERROR <- "Point has a valid PrimaryKey value but is not designated as 'target sampled'"
        key.errors.df <- rbind(key.errors.df, errors.missing.desig[, c("SDD", "ERROR", "TERRA_SAMPLE_FRAME_ID", "PLOT_NM", "TERRA_TERRADAT_ID")])
      }
    }
    
    if (nrow(key.errors.df) < 1) {
      print("No points designated as 'target sampled' were missing valid TerrADat primary key values and no points with valid primary keys were designated as anything but 'target sampled.'")
    }
    
    ## Append this to the output list
    output <- list(output, "errors" = key.errors.df)
  }## if (validate.keys)
  
  
  
  return(output)
}#' Adjusting Weights Calculated from AIM Sample Designs
#'
#' This function takes the point weights data frame output from the function weighter() and a SpatialPolygonsDataFrame defining the weight categories. Returns the data frame supplied as points with the new column ADJWGT containing the adjusted weights.
#' @param points Data frame output from weighter(), equivalent to weighter()[["point.weights"]] or weighter()[[2]].
#' @param wgtcat.spdf SpatialPolygonsDataFrame describing the weight categories for adjusting the weights. Use the output from intersector()
#' @param spdf.area.field Character string defining the field name in wgtcat@data that contains the areas for the weight categories. Defaults to "AREA.HA.UNIT.SUM"
#' @param spdf.wgtcat.field Character string defining the field name in wgtcat@data that contains the unique identification for the weight categories. Defaults to "UNIQUE.IDENTIFIER"
#' @param projection CRS string. Defaults to NAD83 CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"). Is used to reproject all SPDFs in order to perform spatial manipulations
#' @keywords weights
#' @examples
#' weight.adjuster()

weight.adjuster <- function(points, ## The weighted output from weighter(), so weighter()["point.weights"] | weighter()[2] IF YOU RESTRICTED THE SDD INPUT BY THE REPORTING UNIT POLYGON
                            wgtcat.spdf, ## The SPDF that's represents all the weird possible combinations of the reporting unit and strata
                            spdf.area.field = "AREA.HA.UNIT.SUM", ## The name of the field in the SPDF that contains the areas of the weight categories
                            spdf.wgtcat.field = "UNIQUE.IDENTIFIER", ## The name of the field in the SPDF that contains the identifiers for weight categories
                            projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") ## NAD83, standard issue as always
){
  ## Sanitization
  names(points) <- str_to_upper(names(points))
  names(wgtcat.spdf@data) <- str_to_upper(names(wgtcat.spdf@data))
  spdf.area.field <- str_to_upper(spdf.area.field)
  spdf.wgtcat.field <- str_to_upper(spdf.wgtcat.field)
  
  ## Convert points to an SPDF
  points.spdf <- SpatialPointsDataFrame(coords = points[, c("LONGITUDE", "LATITUDE")],
                                        data = points,
                                        proj4string = projection)
  
  ## Attribute the points.spdf with the wgtcat identities from wgtcat.spdf
  points.spdf <- attribute.shapefile(shape1 = points.spdf,
                                     shape2 = wgtcat.spdf,
                                     attributefield = spdf.wgtcat.field,
                                     newfield = spdf.wgtcat.field)
  
  ## Add the areas in using the unique identifier
  data.current <- merge(x = points.spdf@data,
                        y = distinct(wgtcat.spdf@data[, c(spdf.wgtcat.field, spdf.area.field)]))
  
  ## The weighted points attributed by the combination of reporting units and strata
  ## We first restrict to the points that inherited identities (this should've already happened in the previous step, but just to be safe)
  # data.current <- data.attributed[!is.na(data.attributed[, points.wgtcat.field]),]
  
  ## We want to include all the points. So we make a logical vector of just T with a length equal to the number of plots
  sites.current <- (rep(T, nrow(data.current)))
  
  ## Grab the current weights from those points as its own vector
  wgt.current <- data.current$WGT
  
  ## NB: The identity inherited from the shapefile needs to match the field used for name in framesize
  wtcat.current <- data.current[, spdf.wgtcat.field]
  
  ## The framesize information about each of the unique wgtcat identities
  ## I currently have this as an area, but I think it needs to be the inverse of the proportion of the area of the reporting unit that each identity represents
  ## so the framesize value for a particular wgtcat = [area of the whole spdf]/[area of particular wgtcat]
  framesize.current <- wgtcat.spdf@data[, spdf.area.field]
  names(framesize.current) <- wgtcat.spdf@data[, spdf.wgtcat.field]
  
  ## Run the weight adjustment
  data.current$ADJWGT <- adjwgt(sites.current, wgt.current, wtcat.current, framesize.current)
  
  return(data.current)
}
#  Modified version of Nelson Stauffer's weighter_220217.r.

## Processing FLOW:
## Multiple steps.  The first adds coordinates to the pts file, and clips pts and strata (or the frame).
## The second utlimately derives the aerial weights.  There is branching to handle situations where this is a strata file and where there is no strata file.  Comments
## delineate the beginning of the branchings as With strata and If no strata.
## After the end of the second step, general clean up and organization occurs.  TerrADat and LMF master databases are scruitinized for repeat measurements of the points extracted from the SDDs.  THe
## SDDs contain the FIRST occurrence of a point (at least for now). Plots representing repeat measurements are extracted and included in the output summaries.  The Primary Key of plots can be used to
## cross-walk with the master databases to extract indicator values for post-processing assessments.


#  weighter.r - Handles AIM and LMF points.  Key enhancements to original weighter.r are listed below. 
#  7/15/2017 - 1 - Functions prior to weighter.r modify the SDD point files by adding attributes aimlmf, lmfstrata, relweight, segcnt segcode, fieldoffice (name of FO) even if LMF points are not used.  
#                  If LMF points are used, then the SDD point files are further modified with the addition of LMF points.  Weighting logic was modified to accomodate 
#                  the 2-stage design of LMF points. However, this function works if using only AIM pts, only LMF points, or if using AIM+LMF pts.  
#              2-  Some adjustment of attribute case for proper functioning (case adjustment is mostly handled by sdd.reader, lmf.reader, and in the main module).
#              3- Re-ordered and added fields to the dataframes created in this module.
#              4- Added option to pass a reporting unit name when reporting.units.spdf==NULL
#	       5 - Four key data frames (results) are written to csv files for each entry in sdd.src, using s as the first file prefix.  This organizes output results by SDD.  HOWEVER,
#                 all 4 data frames (dfs) are also accumulated and returned as a named list at the end of processing.  THese dfs concatenate info for all entries in sdd.src,
#                 in case the user wants a master list of results.  dfs include 1) a summary of aerial extent and sampled extent with no. pts by rel. type by stratum, 
#                 2) the pts file with pt ID and weights, 3) a strata summary, and 4) a summary of the weights by rel. pt type by stratum (or Frame).       
#              6 - intersector(), intersector.alt(), or Arcpy may be called to clip the frame when a reporting unit is employed.  Currently, the Arcpy feature is active.  
#              7 - saveshapefiles argument allows the export of the sampling frame, and the modified strata and points shapefiles for each entry in sdd.src. 
#              8 - saveru is NULL if not used, otherwise is the name of the reporting unit shapefile written to the src location.  Used to save a modified reporting unit file that may be used in subsequent assessments.
#              9 - fileprefix is NULL if not used, otherwise is the very first prefix of dfs and shapefiles output in this function, EXCEPT the reporting unit shapefile.  Given the convention to name files using the
#                   SDD name, fileprefix provides a unique tag when it is necessary for multiple iterations with the same SDDs but with different reporting units.


## This function produces point weights by design stratum (when the SDD contains them) or by sample frame (when it doesn't)
weighter <- function(sdd.import, ## workinglist from the main module - list of SFs, Pts, & Strata 
                     entry=1,			## Sequential entry in sdd.src used to generate weights
                     reporting.unit.name="NA",  ## if reporting.units.spdf==NULL, we still want to add a name to the reporting unit field
                     reporting.units.spdf = NULL, ## An optional reporting unit SPDF that will be used to clip the SDD import before calculating weights
                     reportingunitfield = "RU", ## If passing a reporting unit SPDF, what field in it defines the reporting unit[s]?
                     saveshapefiles=F,    ## ==T results in exporting sf, pts, and strata shapefiles (if it exists) for every entry in sdd.src after performing any clipping or post-stratification merging
                     saveru=NULL,           ## save reporting unit.  If NULL, do not save reporting unit, otherwise this is the shapefile name of the reporting unit output.
                     fileprefix=NULL,       ## prefix used FOR ALL files (dfs and shapefiles) output in this function, EXCEPT reporting unit shapefile.  IF NULL, then no additional prefix is added to file names.
                     ## Keywords for point fate.
                     target.values = c("Target Sampled",
                                       "TS"),
                     unknown.values = c("Unknown",
                                        "UNK", 
                                        NA),
                     nontarget.values = c("Non-Target",
                                          "NT"),
                     inaccessible.values = c("Inaccessible",
                                             "IA"),
                     unneeded.values = c("Not Needed","NN"),
                     ## These shouldn't need to be changed from these defaults, but better to add that functionality now than regret not having it later
                     fatefieldname = "FINAL_DESIG", ## The field name in the points SPDF to pull the point fate from
                     pointstratumfieldname = "DSGN_STRTM_NM", ## The field name in the points SPDF to pull the design stratum
                     designstratumfield = "DMNNT_STRTM", ## The field name in the strata SPDF to pull the stratum identity from
                     projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0") ## Standard NAD83
                     
){
  ## Sanitization
  if (!is.null(reporting.units.spdf)) {
    names(reporting.units.spdf@data) <- str_to_upper(names(reporting.units.spdf@data))
    ## When importing the RU from previous runs or from ru.r, can end up with duplicate HA and SQKM fields that
    ## corrupts flix.clip.  As a convention, clear AREA.HA and AREA.SQKM
    reporting.units.spdf$AREA.HA<-NULL
    reporting.units.spdf$AREA.SQKM<-NULL
  }
  
  
  ## Create upper case versions of arguments
  fatefieldname <- str_to_upper(fatefieldname)
  pointstratumfieldname <- str_to_upper(pointstratumfieldname)
  designstratumfield <- str_to_upper(designstratumfield)
  reportingunitfield <- str_to_upper(reportingunitfield)
  
  
  
  ## In this version, key results are output for each loop; i.e., each entry in sdd.src (each specified SDD).  
  ## dfs are named similarly to the dfs that accumulate all loop info (listed below). Dfs are initialized within the
  ## the second S loop. 
  ## dfs are named-> aaaa.extent.summary.df, pointweights.df (turns into finalweights.df), stats.df, aaaa.tablewgt.df, where
  ##                 aaaa is obs (observed pts) or all (observed plus otherwise pts).    
  
  ## ALL of the following accumulates info from each loop (GR prefix for GRand total) and are returned in 1 list.
  ## Initialize data frame for stratum info. The results from each loop end up bound to this.
  GRextent.summary.df <- NULL
  ## Initialize data frame for point weight info. The results from each loop end up added to this
  ## In the end, these will all be joined to TerrADat and stripped down to the bare essentials to report out
  GRfinalweights.df <- NULL
  ## Initialize data frame for stratum info, specifically the point counts per year per stratum per project
  GRstats.df <- NULL
  ## Initialize data frame for pt weights by rel. contribution by stratum
  GRtablewgt.df <- NULL
  
  
  ## The fate values that we know about are hardcoded here.
  ## Whatever values are provided in the function arguments get concatenated and then we keep only the unique values from that result
  target.values <- c(target.values,
                     "Target Sampled",
                     "TS") %>% unique() %>% str_to_upper()
  unknown.values <- c(unknown.values,
                      "Unknown",
                      "UNK", 
                      NA) %>% unique() %>% str_to_upper()
  nontarget.values <- c(nontarget.values,
                        "Non-Target",
                        "NT") %>% unique() %>% str_to_upper()
  inaccessible.values <- c(inaccessible.values,
                           "Inaccessible",
                           "IA") %>% unique() %>% str_to_upper()
  unneeded.values <- c(unneeded.values,
                       "Not Needed","NN") %>% unique() %>% str_to_upper()
  
  
  ##################
  ## See if the reporting unit shapefile should be output; if so do it here in case we bail out due to the lack of points 
  if(!is.null(saveru)) {
    writeOGR(reporting.units.spdf, ".",saveru,driver="ESRI Shapefile",overwrite_layer=T)
  }
  
  
  
  
  
  ####################################################################################### Beginning of Clipping operations
  ## For each SDD that was imported, bring in the points and the frame (strata if they exist, otherwise the sample frame) and clip them to reporting units if appropriate
  s <-names(sdd.import$sf[entry])		## Pick up s for the specified entry using SF as the reference.
  
  ## First, bring in the relevant SPDFs
  ## Get the pts file in sdd.src that corresponds to s and call it pts.spdf, then create and init the WGT attribute
  pts.spdf <- sdd.import$pts[[s]]
  
  
  ##  Let's make values of key attributes uppercase
  pts.spdf@data[, fatefieldname] <- str_to_upper(pts.spdf@data[, fatefieldname])
  
  
  pts.spdf@data$WGT <- 0
  ## Add in the REPORTING.UNITS field with the value NA if it's not there already. The only way it'd already be there is if the points were restricted
  if (!("REPORTING.UNIT" %in% names(pts.spdf@data))) {
    pts.spdf@data$REPORTING.UNIT <- reporting.unit.name    ## Even if we aren't using reporting.unit.spdf, we still want to add a name here
  }
  
  ## Add in the Lat/Long coordinates, then transform and add in Alber's equal area projection coords (to faciliate use of the local variance estimator)
  projectionAL <- CRS("+proj=aea")
  pts.spdf@data <- cbind(pts.spdf@data, pts.spdf@coords)
  if(length(grep( names(pts.spdf),pattern="coords.x1"))>0) {
    names(pts.spdf)[names(pts.spdf) == "coords.x1"] <- "LONGITUDE"
    names(pts.spdf)[names(pts.spdf) == "coords.x2"] <- "LATITUDE"
    temp.spdf<- spTransform(pts.spdf,projectionAL)
    pts.spdf@data <- cbind(pts.spdf@data, temp.spdf@coords)
    names(pts.spdf)[names(pts.spdf) == "coords.x1"] <- "XMETERS"
    names(pts.spdf)[names(pts.spdf) == "coords.x2"] <- "YMETERS"
    remove(temp.spdf)		## reduce clutter
  } else {
    names(pts.spdf)[names(pts.spdf) == "x"] <- "LONGITUDE"
    names(pts.spdf)[names(pts.spdf) == "y"] <- "LATITUDE"
    temp.spdf<- spTransform(pts.spdf,projectionAL)
    pts.spdf@data <- cbind(pts.spdf@data, temp.spdf@coords)
    names(pts.spdf)[names(pts.spdf) == "x"] <- "XMETERS"
    names(pts.spdf)[names(pts.spdf) == "y"] <- "YMETERS"
    remove(temp.spdf)		## reduce clutter
  }
  
  
  
  ## Creating the weight identity field.
  ## This will let us analyze the points whether there are new points from other designs being added in that have inherited new identities
  ## Basically, when combining designs, I need a field I can write their new identities into and that I can use no matter what to run the weight calculations later
  pts.spdf@data$WEIGHT.ID <- str_to_upper(pts.spdf@data[, pointstratumfieldname])
  
  ## Get the stratum SPDF for this SDD and call it frame.spdf
  frame.spdf <- sdd.import$strata[[s]]
  
  ## If the frame.spdf was actually NULL, then grab the sample frame to use instead
  if (is.null(frame.spdf)) {
    frame.spdf <- sdd.import$sf[[s]]
  }
  
  
  ## Add the area to frame.spdf ONLY IF there is no reporting unit to clip.  This check SAVES time when the frame is large!!!
  if(is.null(reporting.units.spdf)) {
    frame.spdf <- area.add(frame.spdf, byid = T)
  }
  
  
  #####################################################clip to reporting unit if one was provided
  ## If there's a reporting.units.spdf provided, then we'll assign those identities to the SPDFs from sdd.import and restrict them by reporting.units.spdf
  if (!is.null(reporting.units.spdf)) {
    ## Deal with the points
    pts.spdf <- attribute.shapefile(shape1 = pts.spdf,
                                    shape2 = reporting.units.spdf,
                                    newfield = "REPORTING.UNIT",
                                    attributefield = reportingunitfield)
    
    ## Deal with frame.spdf. This involves an intersection and therefore maybe slow.  CAN USE intersector(), intersector.alt(), or ARCPY
    
    frame.spdf.intersect<- flex.clip(frame.spdf,reporting.units.spdf,method="arcpy",temp.path=getwd() )
    
    #  The following works sometimes, sometimes it doesn't due to the flakey implementation of R clipping functions.....
    #      frame.spdf.intersect <- intersector.alt(spdf1 = frame.spdf,
    #                                          ## This will use the appropriate field for strata or sample frame
    #                                          spdf1.attributefieldname = c("TERRA_SAMPLE_FRAME_ID", designstratumfield)[(c("TERRA_SAMPLE_FRAME_ID", designstratumfield) %in% names(frame.spdf@data))],
    #                                          spdf2 = reporting.units.spdf,
    #                                          spdf2.attributefieldname = reportingunitfield)
    
    ## Replace frame.spdf with this new thing, which is actually the weight categories!
    # frame.spdf <-  frame.spdf.intersect[, c(names(frame.spdf.intersect@data)[!(names(frame.spdf.intersect@data) %in% names(reporting.units.spdf@data))])]
    frame.spdf <-  frame.spdf.intersect
    frame.spdf <- area.add(frame.spdf, byid = T)
    frame.spdf<- spTransform(frame.spdf,projection) 
    
    ## Add REPORTING.UNIT.RESTRICTED
    frame.spdf@data$REPORTING.UNIT.RESTRICTED <- T  
    
  } ## if (!is.null(reporting.units.spdf))
  
  
  #### pts.spdf may be null most likely because the reporting unit is a region without any points.  This can 
  ## happen when performing multiple post-stratifications where the AOI is clipped several times to get down to the 
  ## area(s) that are beyond the previous target frames used in the post-strat process (i.e., most of the
  ## overall AOI is contained within the previous target frames and you are left with 'slivers' of the larger frame
  ## that likely lack any points).  If pts.spdf is NULL, then at least save frame.spdf information so you
  ## can account for the area lacking points.  
  
  ## NOTE: 1) The check for NULL pts is outside of the above clipping procedure to catch any situation where,
  ##          for some reason, pts.spdf is NULL.
  ##       2) The current logic will bail-out of this function whenever it detects pts.spdf==NULL regardless of the
  ##       sequential occurence in sdd.src.  When this occurs, no information is stored about the SDD entries
  ##       that may have had points.  Thus,this is really designed to handle the situation where you are
  ##       dealing with 1 SDD in sdd.src.
  
  
  if(is.null(pts.spdf)) {     ## summarize frame.spdf contents and output frame.spdf, then bail
    framecode<-0
    if(!is.null(sdd.import$strata[[s]]))framecode<-1
    dummy<-bail(s,fileprefix,frame.spdf,framecode)	## returns 1 for fun - no meaning
    dummy<-list
    print("Termination due to the lack of points")
    return(dummy)  ## return an empty list 
  } 
  
  ####################################################################### Endof CLIPPING operations 
  
  
  ## Put the manipulated SPDFs back into the sdd.import for future use
  sdd.import$pts[[s]] <- pts.spdf			## This is where the pts file is replaced in case it was clipped
  if (!is.null(sdd.import$strata[[s]])) {
    sdd.import$strata[[s]] <- frame.spdf
  } else {
    sdd.import$sf[[s]] <- frame.spdf
  }
  ############################################################################ end of clipping
  
  #########################################################################################  Set spdfs for the weighting process
  print(paste("Currently s is", s))
  
  ## Bring in this SDD's points
  pts.spdf <- sdd.import$pts[[s]]
  
  
  ## This is a good place to clear dfs that store info 
  obs.extent.summary.df<-NULL		## This is eventually output as _ptstally_
  all.extent.summary.df<-NULL		## This is maintained, but not output or used for any analyses at this point in time.
  pointweights.df<-NULL		## Becomes finalweights.df
  stats.df<-NULL			## Eventually output as _strata_
  obs.tablewgt.df<-NULL		## Eventually output as _ptstratawgts_
  all.tablewgt.df<-NULL		## Used to set weights in pts shapefile and output as _wgts_
  
  
  ################################################### WITH STRATA.  If the value for the current SDD in the list strata is not NULL, then we have a strata SPDF
  if (!is.null(sdd.import$strata[[s]])) {
    ## since we have stratification, use Design Stratum attribute to determine the number of stratum, tally the extent of each stratum,
    ## then tally the no. of pts by stratum
    
    ## Create a data frame to store the area values in hectares for strata. The as.data.frame() is because it was a tibble for some reason
    area.df <- group_by_(frame.spdf@data, designstratumfield) %>% summarize(AREA.HA.SUM = sum(AREA.HA)) %>% as.data.frame()
    
    ## Working points. This is a holdover, but it saves refactoring later code
    working.pts <- pts.spdf@data
    
    ## Check to see if the panel names contain the intended year (either at the beginning or end of the panel name) and use those to populate the YEAR
    working.pts$YEAR[grepl(x = working.pts$PANEL, pattern = "\\d{4}$")] <- working.pts$PANEL %>%
      str_extract(string = ., pattern = "\\d{4}$") %>% na.omit() %>% as.numeric()
    working.pts$YEAR[grepl(x = working.pts$PANEL, pattern = "^\\d{4}")] <- working.pts$PANEL %>%
      str_extract(string = ., pattern = "^\\d{4}") %>% na.omit() %>% as.numeric()
    
    ## Use the sampling date if we can. This obviously only works for points that were sampled. It overwrites an existing YEAR value from the panel name if it exists
    working.pts$YEAR[!is.na(working.pts$DT_VST)] <- working.pts$DT_VST[!is.na(working.pts$DT_VST)] %>% str_extract(string = ., pattern = "^\\d{4}") %>% as.numeric()
    
    ## To create a lookup table in the case that we're working solely from sampling dates. Let's get the most common sampling year for each panel
    panel.years <- working.pts %>% group_by(PANEL) %>%
      summarize(YEAR = names(sort(summary(as.factor(YEAR)), decreasing = T)[1]))
    
    ## If we still have points without dates at this juncture, we can use that lookup table to make a good guess at what year they belong to
    for (p in panel.years$PANEL) {
      working.pts$PANEL[is.na(working.pts$YEAR) & working.pts$PANEL == p] <- panel.years$YEAR[panel.years$PANEL == p]
    }  
    
    
    
    ## Creating a table of the point counts by point type within each stratum by year by project area ID
    working.pts$key[working.pts$FINAL_DESIG %in% target.values] <- "Observed.pts"
    working.pts$key[working.pts$FINAL_DESIG %in% nontarget.values] <- "Unsampled.pts.nontarget"
    working.pts$key[working.pts$FINAL_DESIG %in% inaccessible.values] <- "Unsampled.pts.inaccessible"
    working.pts$key[working.pts$FINAL_DESIG %in% unneeded.values] <- "Unsampled.pts.unneeded"
    working.pts$key[working.pts$FINAL_DESIG %in% unknown.values] <- "Unsampled.pts.unknown"
    
    
    ## Filter out points from THE FUTURE
    # working.pts <- working.pts %>% filter(!(YEAR > as.numeric(str_extract(string = date(), pattern = "\\d{4}"))))
    
    
    ## N.B. I removed the references to the project area because that should be determined by now and not relevant, but you can add this if you need to
    # "TERRA_PRJCT_AREA_ID",
    pts.summary <- working.pts %>% group_by_("key", "WEIGHT.ID", "RELWEIGHT") %>%
      dplyr::summarize(count = n())
    
    pts.summary$SDD <- s
    
    ## Spreading that
    pts.summary.wide <- tidyr::spread(data = pts.summary,
                                      key = key,
                                      value = count,
                                      fill = 0)
    
    ## We need to know which of the types of points (target, non-target, etc.) are represented
    extant.counts <- names(pts.summary.wide)[grepl(x = names(pts.summary.wide), pattern = ".pts")]
    
    ## N.B. I removed the references to the project area because that should be determined by now and not relevant, but you can add this if you need to
    # "TERRA_PRJCT_AREA_ID",
    ## Only asking for summarize() to operate on those columns that exist because if, for example, there's no Unsampled.pts.unneeded column and we call it here, the function will crash and burn
    ##     stratum.summary <- eval(parse(text = paste0("pts.summary.wide %>% group_by(SDD,", "WEIGHT.ID", ", YEAR) %>% summarize(sum(", paste0(extant.counts, collapse = "), sum("), "))")))
    stratum.summary <- eval(parse(text = paste0("pts.summary.wide %>% group_by(SDD,", "WEIGHT.ID", ") %>% summarize(sum(", paste0(extant.counts, collapse = "), sum("), "))")))
    
    ## Fix the naming because it's easier to do it after the fact than write paste() so that it builds names in the line above
    names(stratum.summary) <- str_replace_all(string = names(stratum.summary), pattern = "^sum\\(", replacement = "")
    names(stratum.summary) <- str_replace_all(string = names(stratum.summary), pattern = "\\)$", replacement = "")
    
    ## Add in the missing columns if some point categories weren't represented
    for (name in c("Observed.pts", "Unsampled.pts.nontarget", "Unsampled.pts.inaccessible", "Unsampled.pts.unneeded", "Unsampled.pts.unknown")[!(c("Observed.pts", "Unsampled.pts.nontarget", "Unsampled.pts.inaccessible", "Unsampled.pts.unneeded", "Unsampled.pts.unknown") %in% names(stratum.summary))]) {
      stratum.summary[, name] <- 0
    }
    
    ## This is where this particular version of the data frame leaves the loop to be returned at the end of the function
    stats.df <- rbind(stats.df, stratum.summary)
    GRstats.df<-rbind(GRstats.df,stratum.summary)
    
    ## ###################################################################################
    ## THe following derives a summary, by stratum, of the total area , the total relative no. of pts, the observed relative no. of pts,
    ## and the total area actually sampled (factoring in the nonresponses).  As used here, relative no. of pts. 
    ## considers the relative contribution of each pt to the overall sample of pts.  Specifically, AIM pts have a
    ## relative contribution of 1.0, and LMF pts have a relative contribution of 0.5 (although there are exceptions to both statements).
    ## Here we use the total relative contribution of pts and the same for observed pts to derive the proportional reduction (due to nonresponses)
    ## in actual sampled area.  Final df is called extent.summary.df (used to be called master.df).
    ##	     
    ## Hereafter, we track info and derive weights for observed pts only, and for all pts (observed and otherwise).  This allows us
    ## to estimate just the observed proportion of a target population AND the entire target population where weights of non-target (non-responses)
    ## plots provide proportion estimates of non-observed area.  Observed pt info is saved as OBS and obs.aaaa, and observed+otherwise is
    ## saved as ALL and ALL.aaaa (all pts).
    
    
    ##  OBS[], ALL[], Area[], aaa.Areasampled[], and vecs[] (vector of stratum) are derived here and used below when deriving pt weights
    
    ## Transform stratum name in area.df to upper case for the following to function properly
    area.df[,designstratumfield] <- str_to_upper(area.df[, designstratumfield]) 
    
    grand<-NULL		## Total rel. contribution by stratum
    OBS<-NULL			## Rel. contribution of observed pts by stratum
    ALL<-NULL			## Rel. contribution of ALL pts by stratum (observed and otherwise)
    Area<-NULL		## ha of each stratum
    Tpts<-NULL		## Total no. of pts by stratum - good place to tally this info; recorded in relpt.summary.df
    vecs<-unique(pts.spdf$WEIGHT.ID)
    for(i in 1:length(vecs)) {
      if(length(area.df$AREA.HA.SUM[area.df$DMNNT_STRTM %in% vecs[i]])<=0) {
        Area<-rbind(Area,c(0))            
      }else {
        Area<- rbind(Area,c(area.df$AREA.HA.SUM[area.df$DMNNT_STRTM %in% vecs[i]]))
      }
      Tpts<-rbind(Tpts,c(nrow(pts.spdf@data[pts.spdf@data[, "WEIGHT.ID"] %in% vecs[i] ,])))
      grand<- rbind(grand,c(sum(pts.spdf$RELWEIGHT[pts.spdf$WEIGHT.ID %in% vecs[i]])))
      OBS<-rbind(OBS,c(sum(pts.spdf$RELWEIGHT[pts.spdf$WEIGHT.ID %in% vecs[i] & pts.spdf$FINAL_DESIG %in% target.values])))	## observed pts
      ALL<-rbind(ALL,c(sum(pts.spdf$RELWEIGHT[pts.spdf$WEIGHT.ID %in% vecs[i] ])))  ## ALL pts
    }
    obs.propsampled<-OBS/grand
    obs.Areasampled<-Area*obs.propsampled
    all.propsampled<-ALL/grand
    all.Areasampled<-Area*all.propsampled
    
    ## Here we save copies that include observed and all FINAL_DESIG
    obs.relpt.summary.df<-data.frame(WEIGHT.ID=vecs,AREA.HA.Total=Area,TOTAL.rel.pts=grand,OBS.rel.pts=OBS,PROP.sampled.pts=obs.propsampled,AREA.HA.sampled=obs.Areasampled,
                                     Total.pts=Tpts,stringsAsFactors=F)
    ## NOTE:  Even though all.relpt.summary and subsequent derivatives of this frame includes ALL response types, the syntax OBS.rel.pts (observed
    #         relative pts) is retained to ensure compatibility with subsequent code!
    all.relpt.summary.df<-data.frame(WEIGHT.ID=vecs,AREA.HA.Total=Area,TOTAL.rel.pts=grand,OBS.rel.pts=ALL,PROP.sampled.pts=all.propsampled,AREA.HA.sampled=all.Areasampled,
                                     Total.pts=Tpts,stringsAsFactors=F)
    
    
    ## Merge stats.df and relpt.summary.df to create a comprehensive summary of the relative contribution and actual no. of pts by stratum, and total and sampled stratum area.
    obs.relpt.summary.df<-merge(obs.relpt.summary.df,stats.df,"WEIGHT.ID","WEIGHT.ID")
    obs.extent.summary.df<-subset(obs.relpt.summary.df, select = -SDD)
    all.relpt.summary.df<-merge(all.relpt.summary.df,stats.df,"WEIGHT.ID","WEIGHT.ID")
    all.extent.summary.df<-subset(all.relpt.summary.df, select = -SDD)
    
    ## I'm not sure who requested this feature, but it's here now
    if (!is.null(reporting.units.spdf)) {
      obs.extent.summary.df$Reporting.Unit.Restricted <- T
      all.extent.summary.df$Reporting.Unit.Restricted <- T
    } else {
      obs.extent.summary.df$Reporting.Unit.Restricted <- F
      all.extent.summary.df$Reporting.Unit.Restricted <- F
    }
    
    ## #######################################################################################################
    ## THe processing immediately above uses vecs (strata that had points) to derive the summary information stored in aaa.extent.summary.df.
    ## However, there can be stratum that lack points and info about these stratum is not included in the above extent.summary.df.
    ## TO provide a comprehensive summary of the AOI, we add the missing stratum (if any) and record the corresponding total area and indicate
    ## the lack of points.  Ideally, if unique(extent.summary.df$WEIGHT.ID) != unique(frame.spdf$DMNNT_STRTM) then we need to add stratum.
    ## HOWEVER, there are instances where extent.summary.df CAN include 'junk' such as NA (where a LMF point doesn't overlap any strata due to
    ## georegistration error), and thus have an inflated no. of strata.  The more definitive assessment is 
    ##                nrow(aaa.extent.summary.df[!is.na(aaa.extent.summary.df$WEIGHT.ID) ,]) != nrow(frame.spdf$DMNNT_STRTM); this assumes that frame.spdf
    ## is whistle clean and only includes THE EXACT number of strata in the AOI.  
    
    ## After adding any necessary info, accumulate info in GRextent.summary.df.  obs.extent.summary.df and all.extent.summary.df will have
    #  identical strata; thus both may have a complete record of all strata OR both will have the same missing stratum.  
    
    if(nrow(obs.extent.summary.df[!is.na(obs.extent.summary.df$WEIGHT.ID) ,]) != nrow(frame.spdf)) {
      a<-str_to_upper(frame.spdf$DMNNT_STRTM)			## The population of stratum in this AOI
      
      ## Probably a better way to do this,but I'm stuck, so loop de loop
      for(st in a) {
        if(length(grep(vecs,pattern=st)) <=0) {        ## <=0 if not included in extent.sumary.df due to lack of pts 
          starea<-frame.spdf$AREA.HA[str_to_upper(frame.spdf$DMNNT_STRTM) %in% st]		## Get stratum area (ha)
          temp.df<-data.frame(WEIGHT.ID=st,AREA.HA.Total=starea,TOTAL.rel.pts=0,OBS.rel.pts=0,PROP.sampled.pts=0,AREA.HA.sampled=0,
                              Total.pts=0,Observed.pts=0,Unsampled.pts.inaccessible=0,Unsampled.pts.unknown=0,
                              Unsampled.pts.nontarget=0,Unsampled.pts.unneeded=0,Reporting.Unit.Restricted=NA)
          obs.extent.summary.df<-rbind(obs.extent.summary.df,temp.df)	## bind the stratum and stratum area along with zero pts for all categories
          all.extent.summary.df<-rbind(all.extent.summary.df,temp.df)	## bind the stratum and stratum area along with zero pts for all categories
        }
      }	
    }
    
    ## Accumulate extent.summary.df info
    GRextent.summary.df<-rbind(GRextent.summary.df,obs.extent.summary.df)
    ######################################################################################################################################################	
    
    ######################################################################################################################################################
    ## Calculate weights using the AIM/LMF Integration approach.  
    ## We derive weights for each stratum by relweight (which equates to point type). 
    ## Most arguments of Strat.Weights() are created 2 steps above. Use only observed pts to set obs.table.df; use all pts to set
    ## all.table.wgt.df.  obs.table.df info is eventually written/stored as _ptstally_  AND all.table.wgt.df is eventually written/stored
    ## as _wgt_ .csv.  Both are used in a subsequent step (proportions.r) to generate weights.
    
    ## Strata.Weights(pts.spdf,list of weight categories,rel. contribution of pts by strata,area by strata, sampled area by strata)
    
    ## Observed pts only
    obs.pts<-pts.spdf[pts.spdf$FINAL_DESIG %in% target.values,]
    obs.tablewgt.df<-Strata.Weights(obs.pts,vecs,OBS,Area,obs.Areasampled)
    obs.tablewgt.df<-unique(obs.tablewgt.df)
    GRtablewgt.df<-rbind(GRtablewgt.df,obs.tablewgt.df)		## Save grand tally
    
    ## All pts
    all.pts<-pts.spdf
    all.tablewgt.df<-Strata.Weights(all.pts,vecs,ALL,Area,all.Areasampled)
    all.tablewgt.df<-unique(all.tablewgt.df)
    ######################################################################################################################################################
    ## Add the weights to the points
    ## First, it is possible to have NA as a stratum (occurs sometimes with LMF pts due to georegistration issues).  For certain search functions within R
    ## can't have NA; so change NA in working.pts,  NA in tablewgt.df was changed above (see Good time to check.....)
    
    working.pts$WEIGHT.ID[is.na(working.pts$WEIGHT.ID)=="TRUE"]<-"NONE"
    
    vecw<-unique(pts.spdf$RELWEIGHT)			## all possible relative contribution values - weights vary with rel.contribution
    
    for (stratum in all.tablewgt.df$WEIGHT.ID) {
      for(relative in vecw) {
        #working.pts$WGT[(working.pts$FINAL_DESIG %in% target.values) & working.pts[, "WEIGHT.ID"] == stratum & working.pts[, "RELWEIGHT"]==relative] <- obs.tablewgt.df$WGT[obs.tablewgt.df$WEIGHT.ID == stratum & obs.tablewgt.df$RELWEIGHT== relative]
        working.pts$WGT[working.pts[, "WEIGHT.ID"] == stratum & working.pts[, "RELWEIGHT"]==relative] <- all.tablewgt.df$WGT[all.tablewgt.df$WEIGHT.ID == stratum & all.tablewgt.df$RELWEIGHT== relative]
      }
    }
    
    
    
    ## All the unassigned weights get converted to 0
    working.pts <- replace_na(working.pts, replace = list(WGT = 0))
    
    
    ## Need to carry these fields over to ListAllPlots()    
    pointsweights.current <- working.pts[, c("AIMLMF", "DT_VST", "PLOT_KEY", "TERRA_TERRADAT_ID", "PLOT_NM", "FINAL_DESIG", "REPORTING.UNIT", "WEIGHT.ID",  "WGT", "LONGITUDE", "LATITUDE", "XMETERS", "YMETERS", "FIELDOFFICE")]
    
    
    ## We're going to put in the field regardless of weight adjustment so that we output a consistent data frame (consider dropping in the future?)
    pointsweights.current$ADJWGT <- NA
    
    
    ################################################################################################################################# IF NO STRATA
    ### THE NO-STRATA PROCESSING NEEDS TO BE VERIFIED (11/24/2017)
  } else if (!is.null(frame.spdf)) {
    ## If there aren't strata available to us in a useful format in the SDD, we'll just weight by the sample frame
    ## since we lack stratification, use the sample frame to derive spatial extent in hectares.
    
    ## Similar to weight calcs when strata are used, we derive observed and all-pts combined tabulations for aaaa.extent.summary.df and
    ## aaaa.tablewgt.df, and base finalweights.df on all pts combined.
    
    area <- sum(frame.spdf@data$AREA.HA)
    
    
    ## Derive proportions for just observed pts and for all pts combined. 
    obs.Pprop <- 1 ## initialize - a proportion of 1.0 means there were no nonresponses
    obs.Sarea <- 0 ## initialize actual sampled area
    all.Pprop <- 1 ## initialize - a proportion of 1.0 means there were no nonresponses
    all.Sarea <- 0 ## initialize actual sampled area
    
    if (sumallpts > 0) {  	## sumallpts is the total number of pts, set just below the end of the combine operation in weighter().
      ## We need to sum the RELWEIGHTS - LMF/AIM MOD
      allwgts<-sum(pts.spdf$RELWEIGHT)
      
      obs.targetwgts<-sum(pts.spdf$RELWEIGHT[pts.spdf@data[, fatefieldname] %in% target.values])
      obs.Pprop <- obs.targetwgts/allwgts   ## realized proportion of the frame that was sampled
      
      all.targetwgts<-sum(pts.spdf$RELWEIGHT)			## sum of all areas, but the targetwgts syntax is retained for code reasons....
      all.Pprop <- all.targetwgts/allwgts   ## realized proportion of the frame for all pts combined (just do it!).
      
      obs.Sarea<-obs.Pprop*area		## Record the actual area(ha) sampled -> (proportional reduction * frame area)
      all.Sarea<-all.Pprop*area
    }
    
    
    
    ##Tabulate key information for this SDD
    obs.temp.df <- NULL
    obs.temp.df <- data.frame(WEIGHT.ID = "Sample Frame",
                              AREA.HA.Total = area,
                              TOTAL.rel.pts=allwgts,
                              OBS.rel.pts=obs.targetwgts,
                              PROP.sampled.pts=obs.Pprop,
                              AREA.HA.sampled=obs.Sarea,
                              Total.pts = sumallpts,
                              Observed.pts = target.count,
                              Unsampled.pts.unknown = unknown.count,
                              Unsampled.pts.nontarget = nontarget.count,
                              Unsampled.pts.inaccessible = inaccessible.count,
                              Unsampled.pts.unneeded = unneeded.count,
                              Reporting.Unit.Restricted = F,
                              stringsAsFactors = F)
    all.temp.df <- NULL
    all.temp.df <- data.frame(WEIGHT.ID = "Sample Frame",
                              AREA.HA.Total = area,
                              TOTAL.rel.pts=allwgts,
                              OBS.rel.pts=all.targetwgts,
                              PROP.sampled.pts=all.Pprop,
                              AREA.HA.sampled=all.Sarea,
                              Total.pts = sumallpts,
                              Observed.pts = target.count,
                              Unsampled.pts.unknown = unknown.count,
                              Unsampled.pts.nontarget = nontarget.count,
                              Unsampled.pts.inaccessible = inaccessible.count,
                              Unsampled.pts.unneeded = unneeded.count,
                              Reporting.Unit.Restricted = F,
                              stringsAsFactors = F)
    if (!is.null(reporting.units.spdf)) {
      obs.temp.df$Reporting.Unit.Restricted <- T
      all.temp.df$Reporting.Unit.Restricted <- T
    }
    
    ## Bind this information to the aaaa.extent.summary.df 
    all.extent.summary.df <- rbind(all.extent.summary.df, all.temp.df)
    obs.extent.summary.df <- rbind(obs.extent.summary.df, obs.temp.df)
    GRextent.summary.df<-rbind(GRextent.summary.df,obs.extent.summary.df)
    
    
    ######  Generate weights which are stored in aaaa.tablewgt.df - contains summary of point weights by the different types (aka, rel. contribution types)
    obs.pts<-pts.spdf[pts.spdf$FINAL_DESIG %in% target.values,]	## Observed pts
    obs.tablewgt.df<-Frame.Weights(obs.pts,area,obs.targetwgts,obs.Sarea)
    
    all.pts<-pts.spdf						## All pts
    all.tablewgt.df<-Frame.Weights(all.pts,area,all.targetwgts,all.Sarea)
    
    all.tablewgt.df<-unique(all.tablewgt.df)
    obs.tablewgt.df<-unique(obs.tablewgt.df)
    GRtablewgt.df<-rbind(GRtablewgt.df,obs.tablewgt.df)
    ###########################################################################
    
    ####### Assign derived weights to points
    
    pointsweights.current <- pts.spdf@data
    pointsweights.current$WGT<-0			## Init WGT to zero
    
    ## If there are points to work with, set wgts to points.
    if (nrow(pointsweights.current) > 0) {
      vecw<-pointsweights.current$RELWEIGHT		## Should this be unique()?
      for(relative in vecw) {
        #pointsweights.current$WGT[(pointsweights.current$FINAL_DESIG %in% target.values) & pointsweights.current[, "RELWEIGHT"]==relative] <- tablewgt.df$WGT[tablewgt.df$RELWEIGHT== relative]
        pointsweights.current$WGT[pointsweights.current[, "RELWEIGHT"]==relative] <- all.tablewgt.df$WGT[all.tablewgt.df$RELWEIGHT== relative]
      }
      
      
      ## We're going to put in the field regardless of weight adjustment so that we output a consistent data frame
      pointsweights.current$ADJWGT <- NA
      pointsweights.current$WEIGHT.ID<-"NA"		## A frame code is not set here.  In proportions.r, we set this attribute to a 
      ## user-specified code.
      
    } ## (nrow(pointsweights.current) > 0)
  } ########################################################################### END OF IF NO STRATA
  
  
  ## 1) Set DATASRC using AIM and LMF labels (for readability in the output), 
  ## 2) Load up temppt.df with pointsweights.current and check for annual replicates in TerraDat and lmf master databases.  
  ##    pointweights.df is used in ListAllPlots() to avoid recording info already accumulated in this data frame.
  ## 3) Add annual info (replicates of the plots in the SDD and in lmfmaster) if there are any to pointweights.df
  ## 4) Place LMF PLOT_KEYs into TERRA_TERRADAT_ID and retain DATASRC; the latter ensures proper interpretation of
  ##    the TERRA_TERRADAT_ID (the primary key used to derive monitoring observations from TerraDat OR the master LMF data base). 
  ## 5) Tiddy-up attribute names in pointweights and select key reporting attributes to output to finalweights.df
  ## 6) Write the key 4 dataframes to csv files.
  
  
  ## set the data source to AIM OR LMF
  pointsweights.current$DATASRC[pointsweights.current$AIMLMF==1]<-"AIM"
  pointsweights.current$DATASRC[pointsweights.current$AIMLMF==2]<-"LMF"
  
  ## Store info about ALL plots, targeted sample or otherwise...  Carry over PLOT_KEY for latter use.
  pointweights.df <- rbind(pointweights.df, pointsweights.current[, c("FIELDOFFICE", "DATASRC","PLOT_KEY","TERRA_TERRADAT_ID", "PLOT_NM","FINAL_DESIG", "REPORTING.UNIT", "WEIGHT.ID",  "WGT", "LONGITUDE", "LATITUDE", "ADJWGT", "XMETERS", "YMETERS")])
  pointweights.df$REPEAT<-0	## =0 means these are initial pts, at least for the analysis at hand
  
  ## We need to find annual replicates of these points (if any) 
  temppt.df<-NULL
  temppt.df <- rbind(temppt.df, pointsweights.current[, c("FIELDOFFICE", "DATASRC", "DT_VST", "PLOT_KEY", "TERRA_TERRADAT_ID", "PLOT_NM","FINAL_DESIG", "REPORTING.UNIT", "WEIGHT.ID",  "WGT", "LONGITUDE", "LATITUDE", "ADJWGT", "XMETERS", "YMETERS")])
  
  
  ## Find annual replicates of these plots, but don't duplicate what is already stored in pointweights.df       
  annual.df<-NULL      
  annual.df<-ListAllPlots(temppt.df,pointweights.df)
  
  ## Add the annual replicates, if any
  if(!is.null(annual.df)){
    annual.df$REPEAT<-1        ## these are repeat measures.  This info is used when deriving aerial weights
    pointweights.df <- rbind(pointweights.df, annual.df[, c("FIELDOFFICE", "DATASRC", "PLOT_KEY", "TERRA_TERRADAT_ID", "PLOT_NM","FINAL_DESIG", "REPORTING.UNIT", "WEIGHT.ID",  "WGT", "LONGITUDE", "LATITUDE", "ADJWGT", "XMETERS", "YMETERS","REPEAT")])
  }
  
  
  ## Need to tiddy-up pointweights.df, then we store the final-desired format in finalweights.df
  ## If we keep DATASRC, we can set LMF PLOT_KEY code into TERRA_TERRADAT_ID and interpret the latter properly
  zz<-pointweights.df$PLOT_KEY[pointweights.df$DATASRC=="LMF"]
  pointweights.df$TERRA_TERRADAT_ID[pointweights.df$DATASRC=="LMF"]<-zz
  
  ## Rename the fields to what we want them to be in the output
  names(pointweights.df)[names(pointweights.df) == "TERRA_TERRADAT_ID"] <- "PRIMARYKEY"
  names(pointweights.df)[names(pointweights.df) == "PLOT_KEY"] <- "PLOTKEY"
  names(pointweights.df)[names(pointweights.df) == "PLOT_NM"] <- "PLOTID.SDD"
  names(pointweights.df)[names(pointweights.df) == "WEIGHT.ID"] <- "Wgt.Category"
  names(pointweights.df)[names(pointweights.df) == "FIELDOFFICE"] <- "SDD/LMF"
  
  finalweights.df = pointweights.df[, c("SDD/LMF", "DATASRC","PLOTKEY","PRIMARYKEY", "PLOTID.SDD", "FINAL_DESIG", "REPORTING.UNIT", "Wgt.Category", "WGT", "ADJWGT", "LONGITUDE", "LATITUDE", "XMETERS", "YMETERS", "REPEAT")]
  
  ## AIM PLOTKEY can be 16 digits long.  The .csv file records these properly.  WHen reading the .csv into EXCEL, the last digit of 
  ## a 16-digit number is set to 0, using the default format.  So to readily view these .csv files in EXCEL, we have to play some games!
  ## SOLUTION - add A as a prefix to all AIM PLOTKEY values, and save as a NEW attribute called FPLOTKEY - F stands for fake-out.
  finalweights.df$FPLOTKEY<-finalweights.df$PLOTKEY		## Effectively, set LMF PLOTKEY into this attribute.  THEN, set 
  ## AIM PLOTKEY after adding the A prefix.
  finalweights.df$FPLOTKEY[finalweights.df$DATASRC=="AIM"]<-paste("A",finalweights.df$PLOTKEY,sep="")[finalweights.df$DATASRC=="AIM"]
  
  ## Accumulate finalweights.df
  GRfinalweights.df<-rbind(GRfinalweights.df,finalweights.df) 
  
  
  ##  Output the 4 dfs to the current directory using s as the file prefix, OR fileprefixs if fileprefix is !null
  t<-s
  if(!is.null(fileprefix))t<-paste0(fileprefix,t)
  
  write.csv(finalweights.df, file = paste0(t, "_wgt_", Sys.Date()),row.names=F)
  write.csv(obs.extent.summary.df, file = paste0(t, "_ptstally_", Sys.Date()),row.names=F)
  write.csv(stats.df, file = paste0(t, "_stratpts_", Sys.Date()),row.names=F)
  write.csv(obs.tablewgt.df, file = paste0(t, "_ptstratwgt_", Sys.Date()),row.names=F)
  ####################################################################################################################
  
  ##  Save shapefiles if (saveshapefiles)
  
  ## You can save the sf, pts, and stratum shapefiles for every s for diagnostic et al. purposes. NOTE:  As of 11/28/2017, 
  ## the run#Pts....shp file is required if you want to merge condition class with these pts at the end of Proportions.R (the
  ## subsequent step used to generate proportional estimates and CIs).
  
  ## 3 methods for exporting a shapefile - each with it's own particulars (like dropping field names or shortening field names)
  
  ##writeOGR(file, ".", "crcr", driver="ESRI Shapefile")  # write out a new shapefile (including .prj component), but tends to truncate attribute names
  ## file %>% arc.write ("u:/aim/geo/bruneau/filename", data = .)   ## assumes arcgisbinding
  ## writePolyShape(counties.mp, "counties-maptools")  #  drops projection and drops fields such as area.sqkm?
  
  if(saveshapefiles) {
    print(paste("Exporting shapefiles for", s))
    
    ## If s ends in .gdb, then eliminate this suffix (otherwise writePolyShape results in a .shp file without this extention but a corresponding .dbf file with this extension in its name - 
    ## the fatal result is an unreadable shapfile ????)   
    nam<-s
    a<-substr(s,nchar(s)-3,nchar(s))
    if(a==".gdb") nam<-substr(s,1,nchar(s)-4)
    src<-getwd() %>% sanitizer(type = "filepath")
    
    ## Also, if the name ends in _SDD, we can't seem to ingest it in Proportions.r (which may ingest the PTS file to set condition class)
    a<-substr(nam,nchar(nam)-3,nchar(nam))
    if(a=="_SDD") nam<-substr(nam,1,nchar(nam)-4)
    src<-getwd() %>% sanitizer(type = "filepath")
    
    
    file<-sdd.import$sf[[s]] 			## Sample frame
    ##writePolyShape(file, paste("SF",nam,sep="_"))
    t<-"SF"
    if(!is.null(fileprefix))t<-paste0(fileprefix,t)
    a<-paste(t,nam,sep="_")
    file %>% arc.write (paste(src,a), data = .)
    
    file<-sdd.import$strata[[s]]		## Strata
    ##if(!is.null(file))writeOGR(file, ".",paste("STRATA",nam,sep="_"),driver="ESRI Shapefile",overwrite_layer=T)
    if(!is.null(file)){
      t<-"STRATA"
      if(!is.null(fileprefix))t<-paste0(fileprefix,t)
      a<-paste(t,nam,sep="_")
      file %>% arc.write (paste(src,a), data = .)
    }
    
    file<-sdd.import$pts[[s]]			## Pts
    ##if(!is.null(file)) writeOGR(file, ".",paste("PTS",nam,sep="_"),driver="ESRI Shapefile",overwrite_layer=T)
    if(!is.null(file)){
      t<-"PTS"
      if(!is.null(fileprefix))t<-paste0(fileprefix,t)
      a<-paste(t,nam,sep="_")
      ###############################
      ## Long-handed method to set WGT into the pts shapefile - couldn't get a short-cut approach to work!
      ##  Use PLOTKEY to transfer wgts - this is the cleanest - cause LMF and/or AIM pts within the same
      ##  strata can have different weights (e.g., 2 LMF and 1 AIM pts falls within the same segment!).
      ## THIS MAY not work for LMF repeat measures given how repeat measures MAY be coded in terms of PLOTKEY.
      ## CHECK on this when LMF begins repeat measurements!  Will work with AIM pts given that PLOTKEY SHOULD be
      ## the same for repeat measurements (the date suffix of the PRIMARYKEY, however, will differ).
      
      if(nrow(finalweights.df>0)) {
        for(i in 1:nrow(finalweights.df)) {
          if(finalweights.df$DATASRC[i]=="AIM") {
            if(!is.na(finalweights.df$PLOTKEY[i])) {
              b<-grep(file$PLOT_KEY,pattern=finalweights.df$PLOTKEY[i])
              if(length(b)==1) {
                file$WGT[b]<-finalweights.df$WGT[i]
              }
            }
          }else { 			## If LMF point
            if(!is.na(finalweights.df$PLOTKEY[i])) {
              b<-grep(file$PLOT_KEY,pattern=finalweights.df$PLOTKEY[i])
              if(length(b)==1) {
                file$WGT[b]<-finalweights.df$WGT[i]
              }
            }
          } ## if then else 
        }## for i in 1:nrow
      }## if nrow
      
      ## file$WGT of non-response pts will not be set using the above, cause they lack a plotkey.
      ## ID the non-responses by strata in finalweights.df, then find and set corresponding wgt into file$WGT
      if(nrow(finalweights.df[is.na(finalweights.df$PLOTKEY) ,]) >0) {	## If T, then we have some nonresponses
        b<-finalweights.df[is.na(finalweights.df$PLOTKEY) ,]
        ## FOR NOW, should only have AIM non-responses, so use PLOTID.SDD and PLOT_NM to transfer WGT values
        if(nrow(b)>0) {
          for(i in 1:nrow(b)){
            if(!is.na(b$PLOTID.SDD[i])) {
              c<-grep(file$PLOT_NM,pattern=b$PLOTID.SDD[i])
              if(length(c)==1) {
                file$WGT[c]<-b$WGT[i]
              } 
            }
          } 
        }
      }
      ###############################  END of adding WGT to file
      #file %>% arc.write (paste(src,a), data = .)		
      writeOGR(file, ".",a,driver="ESRI Shapefile",overwrite_layer=T)
      
      ## TRY THIS rgdal::writeOGR(obj =a, dsn = getwd(), layer = clip.RU[id1], driver = "ESRI Shapefile", overwrite_layer = TRUE)
      
    }## !is.null(file)
    
  } ## ENDIF (saveshapefiles)
  
  
  
  
  #########################################################################################################
  
  ## TO help interpret the GrandTotal dfs, add a delimiter at the end of each SDD that was processed.
  
  dummy.df<-data.frame(WEIGHT.ID="x",AREA.HA.Total="x",TOTAL.rel.pts="x",OBS.rel.pts="x",PROP.sampled.pts="x",AREA.HA.sampled="x",
                       Total.pts="x",Observed.pts="x",Unsampled.pts.inaccessible="x",Unsampled.pts.unknown="x",
                       Unsampled.pts.nontarget="x",Unsampled.pts.unneeded="x",Reporting.Unit.Restricted="x")
  
  GRextent.summary.df<-rbind(GRextent.summary.df,dummy.df)
  remove(dummy.df)
  
  dummy.df<-data.frame(WEIGHT.ID="x",RELWEIGHT="x",WGT="x",Observed.pts="x")
  GRtablewgt.df<-rbind(GRtablewgt.df,dummy.df)
  remove(dummy.df)
  
  dummy.df<-c("x", "x","x", "x", "x", "x", "x", "x", "x", "x", "x", "x", "x")		## Delimiter
  GRfinalweights.df<-rbind(GRfinalweights.df,dummy.df)
  
  
  ## Return a named list with 4 data frames. 
  ## Information on the extent of strata (or frame) incl. sampled area and pts by strata, pt info incl. weights, summary of pts  by strata, and weights by stratum by rel. contribution (pt type)
  return(list(strata.samplearea = GRextent.summary.df,
              point.weights = GRfinalweights.df,
              strata.stats = GRstats.df,
              stratum.pttype.wgts=GRtablewgt.df))
}
## bail.r 
## When weighter() detects a NULL pts.spdf, bail() is called to summarize info about the frame.spdf
## which could be either the sample frame or the stratum file.  The summary info along with
## the frame shapefile is output so that the area lacking points can be accounted for in deriving
## the true sampled area.

bail<-function(s,		## SDD name in sdd.src
               fileprefix,	## user specified prefix to add before s when naming output files
               frame.spdf,	## either the sample frame or the stratum file
               framecode	## ==1 if frame.spdf is a stratum file, else it is a sample frame
){
  
  
  ## figure out the filename to output frame info
  t<-s
  if(!is.null(fileprefix))t<-paste0(fileprefix,t)
  
  if(framecode) {			## if true then frame.spdf is a stratum file
    final.df<-NULL
    for(i in 1:nrow(frame.spdf)) {
      temp.df<-data.frame(Strata=frame.spdf$DMNNT_STRTM[i],Area.ha=frame.spdf$AREA.HA[i])
      final.df<-rbind(final.df,temp.df)            
    }
    ## write.csv(final.df, file = paste0(t, "NOPOINTS", Sys.Date()),row.names=F) 
    if(!is.null(fileprefix)) {
      write.csv(final.df, file = paste0(fileprefix,"NOPOINTS"),row.names=F,quote=F)
    }else {
      write.csv(final.df, file = "NOPOINTS",row.names=F,quote=F)
    }
  }else {	                                ## else frame.spdf is the sample frame 	
    final.df<-NULL
    for(i in 1:nrow(frame.spdf)) {
      temp.df<-data.frame(Strata=frame.spdf$SAMPLE_FRAME_GROUP[i],Area.ha=frame.spdf$AREA.HA[i])
      final.df<-rbind(final.df,temp.df)
    }
    ##write.csv(final.df, file = paste0(t, "NOPOINTS", Sys.Date()),row.names=F)
    if(!is.null(fileprefix)) {
      write.csv(final.df, file = paste0(fileprefix,"NOPOINTS"),row.names=F,quote=F)
    }else {
      write.csv(final.df, file = "NOPOINTS",row.names=F,quote=F)
    }
  }
  
  
  
  ## output the frame shapefle
  
  ## If s ends in .gdb, then eliminate this suffix  
  nam<-s
  a<-substr(s,nchar(s)-3,nchar(s))
  if(a==".gdb") nam<-substr(s,1,nchar(s)-4)
  
  
  src<-getwd() %>% sanitizer(type = "filepath")
  t<-"NOPOINTS"
  if(!is.null(fileprefix))t<-paste0(fileprefix,t)
  a<-paste(t,nam,sep="_")
  frame.spdf %>% arc.write (paste(src,a), data = .)      
  
  dummy<-1   
  return(dummy)
}	 

##4/13/2017
##  Currently, gErase() only works if there is 1 and only 1 feature class in erasethis

gErase <- function(frame,erasethis) {
  delta  <- gDifference(frame,erasethis)
  keep<- row.names(delta)
  erase_data<-as.data.frame(frame@data[keep, ])
  return(SpatialPolygonsDataFrame(delta,erase_data))
}


##### From trycatch in weighter.R


#  current.drop <- get_RGEOS_dropSlivers()
#  current.warn <- get_RGEOS_warnSlivers()
#  current.tol <- get_RGEOS_polyThreshold()


#           sliverdrop = T
#          sliverwarn = T
#         sliverthreshold = 0.01



# frame.spdf.temp <- tryCatch(
#  expr = {
#   set_RGEOS_dropSlivers(sliverdrop)
#  set_RGEOS_warnSlivers(sliverwarn)
# set_RGEOS_polyThreshold(sliverthreshold)
# print(paste0("Attempting using set_RGEOS_dropslivers(", sliverdrop, ") and set_RGEOS_warnslivers(", sliverwarn, ") and set_REGOS_polyThreshold(", sliverthreshold, ")"))
#gDifference(spgeom1 = frame.spdf.temp,
#           spgeom2 = frame.spdf,
#          drop_lower_td = T) %>% SpatialPolygonsDataFrame(data = frame.spdf.temp@data)
# },
## 12/13/2017
## PlotTracking - uses the PlotTracking spreadsheet to populate the SDD.  Used whenever the SDD has not been updated
##                with the PlotTracking info.

## Checks to make sure that the PLOTID in the PlotTracking spreadsheet occurs in TerraDat (if FINAL_DESIG==TS, then PLOTID must occur in TerraDat), and that  all PLOTIDs of the spreadsheet  occurs in the SDD.
## If not, the errors are printed out (but processing continues).  THERE is a stand-alone version of the following processing that performs these checks and allows one to fix the problems before running
## the weighting procedures.  HOWEVER, to ensure that fixes were in fact implemented properly, these checks are retained in this module.


##  path.nam must contain the path(s) of the plot tracking spreadsheet & correspond to the order of SDDs in sdd.src (which should be the same order as in 
##                the named list called workinglist.



PlotTracking <- function(path.nam, ## List of filepaths to each spreadsheet
                         sheetname,  		## THe name of the sheet in the excel file to ingest
                         terra.spdf,  		## TerraDat
                         workinglist, 		## named list containing the SDD sample frame, pts, and strata of the entries in sdd.src
                         DeleteOverDraw=T	## delete overdraw pts IFF they were not used
) {
  
  
  ## Read the spreadsheet, standardize nomenclature, and store key attributes in retain
  
  index<-0		## tracks the accession order being processes; used to pick up the SDD in workinglist
  for(s in path.nam) {		## The order in path.nam (the complete path  name of a Plot Tracking spreadsheet) must correspond to the order in sdd.src (i.e., the same order as in working list)
    
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
    xcel$DATE.VISITED<-as.character(xcel$DATE.VISITED)
    
    ## Translate date to YYYY-MM-DD
    for(i in 1:nrow(xcel)) {
      if(!is.na(xcel$DATE.VISITED[i])) {
        a<-substr(xcel$DATE.VISITED[i],5,8)
        b<-substr(xcel$DATE.VISITED[i],1,4)
        b<-paste0(b,"-")
        b<-paste0(b,substr(a,1,2))
        b<-paste0(b,"-")
        b<-paste0(b,substr(a,3,4))
        xcel$DATE.VISITED[i]<-b
      }
    } 
    
    
    
    xcel$PANEL[xcel$PANEL=="OverSample"]<-"OverSample"		## This may need to be specified to each spreadsheet???
    xcel$PLOTSTATUS[xcel$PLOTSTATUS %in% c("Sampled")]<-"TS"
    xcel$PLOTSTATUS[xcel$PLOTSTATUS=="Rejected"]<-"IA"
    names(xcel)[names(xcel) == "ACTUAL.LATITUDE..Y."] <- "LAT"
    names(xcel)[names(xcel) == "ACTUAL.LONGITUDE..X."] <- "LON"
    
    retain<-data.frame(xcel[, c("PLOTID","PANEL","PLOTSTATUS","DATE.VISITED","LAT","LON")])		## store plotid, panel, and plotstatus
    
    
    
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
          print(paste("PlotName not in TerraDat ", retain$ID[i]))		## mismatch
        }
      }
    }
    
    
    ###################### pick up the pts file of the corresponding SDD
    index<-index+1
    ptsname<-names(workinglist$pts[index]) 
    pts<-workinglist$pts[[ptsname]]
    
    
    ####################### Compare plot tracking panel with SDD point draw.
    ## 			Use Over to compare
    t<-c("Year1")						## probably need to make this more robust (e.g., what if we're dealing with Year 2 panels) OR needs to be passed to this function 
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
    ## 			should be noted as Unknowns, and the latter as NA and eventually eliminated from further analyses.
    
    a<-grep(retain$PANEL,pattern="Year") 		
    if(length(a)>0) {
      for(i in a) {
        if(is.na(retain$PLOTSTATUS[i]))retain$PLOTSTATUS[i]<-"Unknown"
      }
    }
    
    
    
    #####################################  Special processing to prep DT_VST for receiving updated dates (had some problems and here is the solution)
    pts$FU<-"NA"
    for(i in 1:nrow(pts)) {
      if(!is.na(pts$DT_VST[i]))pts$FU[i]<-as.character(pts$DT_VST[i])
    }
    
    
    ####################### Loop thru retain, find PLOTID in the SDD, and set key attributes.  If PLOTID not in the SDD (hit==0), then print out the 'mising' PLOTID.
    print("Updating INFO in SDD from PlotTracking, showing LAT/LON changes")    
    tr<-0 
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
            if(!is.na(retain$DV[i])) pts$FU[j]<-retain$DV[i]
            if(!is.na(retain$LAT[i])) {	## Set Coords if provided
              print(paste(pts@coords[j,2],retain$LAT[i],pts@coords[j,1],retain$LON[i],sep=" "))
              pts@coords[j,2]<-retain$LAT[i]
              pts@coords[j,1]<-retain$LON[i]
            }
            
          }
        }
      }
      if(hit==0) {
        print(paste("Couldn't find the plot in the SDD ",retain$PLOTID[i],sep=" "))
        tr<-1
      } 
    }
    
    if(tr==0)print("ALL plot-tracking plots occur in SDD")
    
    pts$DT_VST<-NULL
    pts$DT_VST<-pts$FU
    pts$FU<-NULL
    
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
    
    
    ####################  Store final pts file
    workinglist$pts[[ptsname]]<-pts
    
    #################### Check to see if FINAL_DESIG is TS, then there should be a date visited
    target<-c("Target Sampled","TS")
    a<-pts[pts$FINAL_DESIG %in% target ,]
    b<-a[!is.na(a$DT_VST) ,]
    print(paste("No. TS/DT_VST ",nrow(a),nrow(b),sep=" "))
    
  } ## end of for s
  
  
  ## Declutter and save space
  remove(store.df)
  remove(retain)
  remove(xcel)
  remove(xcelIN)
  remove(path.nam)
  
  
  ## Return the modified SDD pts file(s) contained in the named list
  return(workinglist)
  
} ## end of function

## clean.sdd.r - Function to eliminate points not matching specified years and OVERDRAW pts that have no evidence of being used.
## Operates on, and returns the pts file(s) based on the specified begin and end sequence in workinglist.



clean.sdd <- function(importSDD,yearlist,begin,end) 
{
  projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
  valid.fates = c("Target Sampled","TS","Non-Target","NT","Inaccessible","IA")
  unknown.fates = c("Unknown","UNK",NA,"Not Needed","NN","NOT NEEDED")
  
  ###################################################################################### Loop thru SDD$pts
  for(index in begin:end) {
    
    s<-names(importSDD$pts[index])
    pts<-importSDD$pts[[s]]
    
    if(!is.null(pts)) {		## only if we have SDD pts
      
      ######################## Select pts for the specified years; assumes yr of earliest AIM data is 2011
      for(i in 1:30) {
        pts$theyr[grepl(pts$PANEL,pattern=(2010+i))==TRUE]<-2010+i
        pts$theyrD[grepl(pts$DT_VST,pattern=(2010+i))==TRUE]<-2010+i
      }    
      
      ## Must be an easier way to do the following, but couldn't figure it out!!!!
      pts$a<-1  				## default to 1
      pts$a[pts$theyr %in% yearlist] <-2    	## set to 2 if theyr is in yearlist (user-specified year of interest)
      pts$a[pts$theyrD %in% yearlist] <-2    	## set to 2 if theyrD is in yearlist (user-specified year of interest)
      pts<-pts[pts$a==2,]  		    	## render pts to those years specified by the user
      
      if(nrow(pts)==0) {
        pts<-NULL			    	## no pts satisfied the specified time period
      } else {
        pts$theyr<-NULL       		## clean up
        pts$theyrD<-NULL
        pts$a<-NULL
      }
      
      ###################### if !is.null(pts), then eliminate OVERSAMPLE pts that lack evidence of being accessed
      if(!is.null(pts)) {
        
        pts$a<-0
        pts$b<-0
        pts$a[grepl(pts$PANEL,pattern="OverSample")==TRUE]<-1
        pts$a[grepl(pts$PANEL,pattern="Oversample")==TRUE]<-1		## Spelling issue
        pts$b[pts$FINAL_DESIG %in% unknown.fates] <-1
        pts<-pts[pts$a!=1 | pts$b!=1,]   ## pts$a==1 & pts$b==1 are OverSamples with fates in unknown.fates
      }
      
      if(!is.null(pts)) {
        pts$a<-NULL
        pts$b<-NULL
      }
      
      
    } ## !is.null
    
    importSDD$pts[[s]]<-pts
    
  }  ## for index
  
  return(importSDD)
  
} ## end of function  

## Replace.list.r - Function to replace entries in the named list workinglist.

## Replace.list(workinglist, the new spdf or NA, keyword = frame pts or strata, action =1 to replace & 0 = set to NULL)
##    NOTE:  keyword is case sensitive.  The user is reponsible to ensure that the 'new' spdf is the correct format.  

## Replacement of the FIRST entry in workinglist is the default - currently no option to replace other than
##             the first entry in the named list.

## This function has limited functionality!!  Once you set an entry to NULL, you CAN NOT properly replace the entry
## with a new spdf because the SDD name is set to NA.

Replace.list <- function(import,file.spdf,keyword,action) 
{
  if(keyword=="frame") {
    if(action==1){
      s<-names(import$sf[1])
      import$sf[[s]]<-file.spdf
    }else if(action==0) {
      s<-names(import$sf[1])
      import$sf[[s]]<-NULL
    }
  }else if(keyword=="pts") {
    if(action==1){
      s<-names(import$pts[1])
      import$pts[[s]]<-file.spdf
    }else if(action==0) {
      s<-names(import$pts[1])
      import$pts[[s]]<-NULL
    }
  }else if(keyword=="strata") {
    if(action==1) {
      s<-names(import$strata[1])
      import$strata[[s]]<-file.spdf
    }else if(action==0){
      s<-names(import$strata[1])
      import$strata[[s]]<-NULL
    }
  }else {
    print("ERROR in keyword in Replace.list.r")		## For now, post the error & continue processing
  }
  
  return(import)
}## Modification of flex.erase() written by Nelson Stauffer

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


#####################################################################  Clip pts to polygon

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

##################################################################################################
## Written by Nelson Stauffer

#' Erase a Spatial Polygons Data Frame from another, either with rgeos or ArcPy
#' @description Spatial manipulations in R can get, for lack of a better term, squirrelly. Even with multiple failsafes in place to try to compensate for sliver geometry, \code{rgeos::gDifference()} still isn't as robust as one might hope. This wrapper removes the geometry of one SPDF from another via a system call to \code{Python} using the library \code{ArcPy} or via \code{rgeos::gDifference()}.
#' @param spdf A Spatial Polygons Data Frame to remove FROM.
#' @param spdf.erase  A Spatial Polygons Data Frame to remove geometry from \code{spdf} WITH.
#' @param method Character string. This must either be \code{"arcpy"} or \code{"rgeos"} and determines which approach will be used to frames from one another if \code{combine} is \code{TRUE}. If \code{"arcpy"} is used, then R must have write permissions to the folder \code{temp.path} and a valid install of ArcPy. This is preferable to \code{"rgeos"} because the functions involved tend to crash at random when handling very small remainder geometries. Case insensitive. Defaults to \code{"arcpy"}.
#' @param temp.path Optional character string. If \code{erase} is \code{"arcpy"} this must be the path to a folder that R has write permissions to so that a subfolder called arcpy_temp can be created and used for ArcPy erasure steps.
#' @param python.search.path Character string. The filepath for the folder containing \code{pythonw.exe}. Defaults to \code{"C:/Python27"}.
#' @param sliverdrop Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_dropSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverwarn Optional logical value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_warnSlivers()} to temporarily set the environment during the erasure attempt. Defaults to \code{TRUE}.
#' @param sliverdrop Optional numeric value. If \code{erase} is \code{"rgeos"} this will be passed to \code{rgeos::set_RGEOS_polyThreshold()} to temporarily set the environment during the erasure attempt. Defaults to \code{0.01}.
#' @return The remaining geometry and data in \code{spdf} after \code{spdf.erase} has been removed from it.
#' @export



flex.erase <- function(spdf,
                       spdf.erase,
                       method = "arcpy",
                       temp.path = "",
                       python.search.path = "C:/Python27",
                       sliverdrop = T,
                       sliverwarn = T,
                       sliverthreshold = 0.01
){
  if (class(spdf) != "SpatialPolygonsDataFrame") {
    stop("spdf must be a valid Spatial Polygons Data Frame")
  }
  if (class(spdf.erase) != "SpatialPolygonsDataFrame") {
    stop("spdf.erase must be a valid Spatial Polygons Data Frame")
  }
  if (!(stringr::str_to_upper(method) %in% c("ARCPY", "RGEOS"))) {
    stop("method must be either 'arcpy' or 'rgeos'.")
  }
  if (!file.exists(python.search.path)) {
    stop("python.search.path must be a valid, pre-existing filepath.")
  }
  if (!file.exists(temp.path)) {
    stop("temp.path must be a valid, pre-existing filepath.")
  }
  
  if (spdf@proj4string@projargs != spdf.erase@proj4string@projargs) {
    spdf.erase <- sp::spTransform(spdf.erase, CRSobj = spdf@proj4string)
  }
  switch(stringr::str_to_upper(method),
         "ARCPY" = {
           ## Create a temp directory
           temp.directory <- paste0(temp.path, "/arcpy_temp")
           dir.create(temp.directory, showWarnings = FALSE)
           
           ## Write out the two current frames
           rgdal::writeOGR(obj = spdf, dsn = temp.directory, layer = "inshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)
           rgdal::writeOGR(obj = spdf.erase, dsn = temp.directory, layer = "eraseshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)
           
           ## Construct a quick python script to erase frame.spdf from frame.spdf.temp
           arcpy.script <- c("import arcpy",
                             "from arcpy import env",
                             paste0("env.workspace = '", temp.directory, "'"),
                             "in_features = 'inshape.shp'",
                             "erase_features = 'eraseshape.shp'",
                             "out_feature_class = 'eraseresults.shp'",
                             "xy_tolerance = ''",
                             "arcpy.Erase_analysis(in_features, erase_features, out_feature_class)"
           )
           ## Write the constructed script out
           cat(arcpy.script, file = paste0(temp.directory, "/erase.py"), sep = "\n", append = F)
           
           ## Find the local machine's copy of pythonw.exe in C:/Python27. There are no failsafes for if this isn't where to find it
           python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
           if (length(python.path) < 1) {
             stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
           } else {
             python.path <- python.path[1]
           }
           
           ## Execute the Python script
           system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/erase.py"), pattern = "/", replacement = "\\\\")))
           
           ## Read in the results and rename the attributes because rgdal::writeOGR() truncated them
           erase.results <- rgdal::readOGR(dsn = temp.directory, layer = "eraseresults", stringsAsFactors = FALSE)
           names(erase.results@data) <- names(spdf@data)    ## double check the use of names(spdf...)
           
           if (erase.results@proj4string@projargs != spdf@proj4string@projargs) {
             output <- spTransform(erase.results, CRSobj = spdf@proj4string)
           } else {
             output <- erase.results
           }
           ## Remove the temp folder and files
           if (grepl(method, pattern = "arcpy", ignore.case = TRUE)) {
             system(paste("cmd /c rmdir", stringr::str_replace_all(temp.directory, pattern = "/", replacement = "\\\\"), "/s /q"))
           }
         }, "RGEOS" = {
           ## This lets rgeos deal with tiny fragments of polygons without crashing
           ## This and the following tryCatch() may be unnecessary since the argument drop_lower_td = TRUE was added, but it works so I'm leaving it
           current.drop <- rgeos::get_RGEOS_dropSlivers()
           current.warn <- rgeos::get_RGEOS_warnSlivers()
           current.tol <- rgeos::get_RGEOS_polyThreshold()
           
           rgeos::set_RGEOS_dropSlivers(sliverdrop)
           rgeos::set_RGEOS_warnSlivers(sliverwarn)
           rgeos::set_RGEOS_polyThreshold(sliverthreshold)
           message(paste0("Attempting using rgeos::set_RGEOS_dropslivers(", sliverdrop, ") and rgeos::set_RGEOS_warnslivers(", sliverwarn, ") and set_REGOS_polyThreshold(", sliverthreshold, ")"))
           ## Making this Albers for right now for gBuffer()
           ## The gbuffer() is a common hack to deal with ring self-intersections, which it seems to do just fine here?
           sp.temp <- rgeos::gDifference(spgeom1 = rgeos::gBuffer(sp::spTransform(spdf, CRS("+proj=aea")),
                                                                  byid = TRUE,
                                                                  width = 0.1),
                                         spgeom2 = rgeos::gBuffer(sp::spTransform(spdf.erase,
                                                                                  CRS("+proj=aea")),
                                                                  byid = TRUE,
                                                                  width = 0.1),
                                         drop_lower_td = TRUE)
           if (!is.null(frame.sp.temp)) {
             output <- sp::spTransform(sp::SpatialPolygonsDataFrame(sp.temp,
                                                                    data = spdf@data[1:length(sp.temp@polygons),]),
                                       CRSobj = spdf@proj4string)
           } else {
             output <- NULL
           }
           
           rgeos::set_RGEOS_dropSlivers(current.drop)
           rgeos::set_RGEOS_warnSlivers(current.warn)
           rgeos::set_RGEOS_polyThreshold(current.tol)
           
         }
  )
  return(output)
}


################################################################################################
## Modification of flex.erase() written by Nelson Stauffer

## flex.dissolve() - dissolve spatial polygon data frames using ARCPY.  Fixing slivers sometimes creates an additional stratum,ru,inference combination.
##                   This function ensures a minimimal polygonal version of the final map, which then can be populated with aerial fields.

## THIS version works only to dissolve a stratum file; i.e.,  after dissolving, STRATUM field name is reset to DMNNT_STRTM

flex.dissolve <- function(spdf,
                          method = "arcpy",
                          temp.path = "",
                          python.search.path = "C:/Python27"
){
  if (class(spdf) != "SpatialPolygonsDataFrame") {
    stop("spdf must be a valid Spatial Polygons Data Frame")
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
  
  spdf$SHAPE_LENG<-NULL
  spdf$SHAPE_AREA<-NULL
  
  ## Create a temp directory
  temp.directory <- paste0(temp.path, "/arcpy_temp")
  dir.create(temp.directory, showWarnings = FALSE)
  
  ## Write out the two current frames
  rgdal::writeOGR(obj = spdf, dsn = temp.directory, layer = "inshape", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  
  
  ## Dissolve_management (in_features, out_feature_class, {dissolve_field}, {statistics_fields}, {multi_part}, {unsplit_lines})
  
  
  ## Construct a quick python script to dissolve spdf
  
  ##  For future reference  "arcpy.Dissolve_management(in_features, out_feature_class,[\"DMNNT_S\", \"RU\",\"infernc\"])"
  
  arcpy.script <- c("import arcpy",
                    "from arcpy import env",
                    paste0("env.workspace = '", temp.directory, "'"),
                    "in_features = 'inshape.shp'",
                    "out_feature_class = 'outshape.shp'",
                    "arcpy.Dissolve_management(in_features, out_feature_class,[\"DMNNT_S\"])"
  )
  ## Write the constructed script out
  cat(arcpy.script, file = paste0(temp.directory, "/dissolve.py"), sep = "\n", append = F)
  
  ##system("copy arcpy_temp\dissolve.py+dend.txt arcpy_temp\dissolve.py")           
  ##system("dend.bat")
  
  ## Find the local machine's copy of pythonw.exe in C:/Python27. There are no failsafes for if this isn't where to find it
  python.path <- paste0(python.search.path, "/", list.files(path = python.search.path, pattern = "pythonw.exe", recursive = TRUE))
  if (length(python.path) < 1) {
    stop(paste0("Unable to find pythonw.exe in the folder or subfolders of ", python.search.path))
  } else {
    python.path <- python.path[1]
  }
  
  ## Execute the Python script
  system(paste(python.path, stringr::str_replace_all(paste0(temp.directory, "/dissolve.py"), pattern = "/", replacement = "\\\\")))
  
  ## Read in the results and rename the attributes because rgdal::writeOGR() truncated them
  out.results <- rgdal::readOGR(dsn = temp.directory, layer = "outshape", stringsAsFactors = FALSE)
  spdf$SHAPE_LENG<-NULL
  spdf$SHAPE_AREA<-NULL
  spdf$OBJECTID<-NULL
  
  ## We seem to truncate attributes so the following doesn't function properly   names(out.results@data) <- names(spdf@data)
  
  ## Here is a good place to reset the stratum name
  names(out.results)[names(out.results) == "DMNNT_S"] <- "DMNNT_STRTM"
  
  if (out.results@proj4string@projargs != spdf@proj4string@projargs) {
    output <- spTransform(out.results, CRSobj = spdf@proj4string)
  } else {
    output <- out.results
  }
  ## Remove the temp folder and files
  if (grepl(method, pattern = "arcpy", ignore.case = TRUE)) {
    system(paste("cmd /c rmdir", stringr::str_replace_all(temp.directory, pattern = "/", replacement = "\\\\"), "/s /q"))
  }
  
  return(output)
}
## MainFunctions
############# This file contains a collection of functions that are called from the MAIN module.  There were extracted and stored here
##            to facilitate iterative processing of multiple combinations of reporting units for the same relative AOI and suite of SDDs.
##            E.g., When processing Lakeview seasonal habitat, SDDs are the same for all seasons.  The code in Main prior to these functions
##            sets up the necessary pts, sf, and strata data (stored in workinglist).  Iterative calls to these functions performs the
##            3-step post-stratification for each of the 3 seasons.

ReadReportingUnit<-function(sf,		## sf is the path.filename of a reporting unit
                            reporting.unit.label,	## THe name of the reporting unit - needs to have a numeric that starts at 1
                            prefix  	## prefix of the file to record the area of a reporting unit - appended to record changes in aerial extent
) {
  
  projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  
  ## Creates an SPDF 
  assign(x = "RU.spdf",
         value = sf %>% arc.open() %>% arc.select %>%
           SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .)%>%spTransform(projection))
  
  
  ## Record the RU area.  Can be used to compare with rendered RU to determine exactly how much RU area is included in the surveyed target population   
  outnam<-c(prefix)    ## File name to output aerial extent of the RU
  if(!is.null(RU.spdf)) {
    RU.spdf <- area.add(spdf = RU.spdf,
                        area.ha = T,
                        area.sqkm = T)
    record.df<-data.frame(ACTION="Initial area (SQKM) of RU",C2="",SQKM=RU.spdf$AREA.SQKM)    
    write.table(record.df,paste(outnam,Sys.Date(),sep="_"),row.names=F,col.names=F,quote=F)
  }	
  RU.spdf<-spTransform(RU.spdf,projection) 
  
  
  ## Explicitly set a reporting_unit (RU) field, and assign reporting unit numbers starting at 1. The RU field is included in call to weighter()
  ## The value of the reporting.unit.label is used in post-processing assessment of scored points.  The critical feature is the # (1-n); you can
  ## use any prefix or suffix.  
  
  RU.spdf$RU<-reporting.unit.label
  
  return(RU.spdf)
  
}



## Clip the RU
ClipRU<-function(clip,		## Vector of path/file names
                 RU.spdf,	## the working reporting unit
                 reporting.unit.label,
                 outnam
){
  
  projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  
  for(nam in clip) {
    ## Creates an SPDF 
    assign(x = "clip.spdf",
           value = nam %>% arc.open() %>% arc.select %>%
             SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .)%>%spTransform(projection))
    
    RU.spdf<-flex.clip(clip.spdf,RU.spdf,method="arcpy",temp.path=getwd())
    RU.spdf <- area.add(spdf = RU.spdf,T,T)
    RU.spdf<-spTransform(RU.spdf,projection)    ## area.add changes the projection
    record.df<-data.frame(ACTION="Modified RU area (SQKM)",C2=paste("By clipping ",nam),SQKM=RU.spdf$AREA.SQKM)
    write.table(record.df,paste(outnam,Sys.Date(),sep="_"),row.names=F,col.names=F,quote=F,append=T)
  }
  
  
  ## THE reporting unit field can be deleted in the above clipping.  Add it again to be sure.
  RU.spdf$RU<-reporting.unit.label
  
  return(RU.spdf)
  
}




## Erase the RU
EraseRU<-function(erase,		## Vector of path/file names
                  RU.spdf,	## the working reporting unit
                  reporting.unit.label,
                  outnam
){
  
  projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
  
  for(nam in erase) {
    RU.spdf$AREA.HA<-NULL
    RU.spdf$AREA.SQKM<-NULL
    
    ## Creates an SPDF 
    assign(x = "erase.spdf",
           value = nam %>% arc.open() %>% arc.select %>%
             SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .)%>%spTransform(projection))
    
    RU.spdf<-spTransform(RU.spdf,projection)    ## area.add changes the projection
    ##   RU.spdf<-modifyRU(RU.spdf,erase.spdf,erase=T)
    RU.spdf<-flex.erase(RU.spdf,erase.spdf,method="arcpy",temp.path=getwd())
    RU.spdf <- area.add(spdf = RU.spdf,T,T)
    RU.spdf<-spTransform(RU.spdf,projection)    ## area.add changes the projection
    record.df<-data.frame(ACTION="Modified RU area (SQKM)",C2=paste("By erasing ",nam),SQKM=RU.spdf$AREA.SQKM)
    write.table(record.df,paste(outnam,Sys.Date(),sep="_"),row.names=F,col.names=F,quote=F,append=T)
  }
  
  ## Just to be sure
  RU.spdf<-spTransform(RU.spdf,projection) 
  
  ## THE reporting unit field can be deleted in the above erase.  Add it again to be sure.
  RU.spdf$RU<-reporting.unit.label
  
  return(RU.spdf)
}



## DeleteFP.r - Function to eliminate points matching the specified sample frame.
## Operates on, and returns the first pts file in workinglist which is passed as importSDD.



DeleteFP <- function(importSDD,framename) 
{
  
  s<-names(importSDD$pts[1])
  pts<-importSDD$pts[[s]]
  pts<-pts[pts$TERRA_SAMPLE_FRAME_ID!=framename,]
  importSDD$pts[[s]]<-pts
  return(importSDD)
} ## end of function  

## filter.date.r - Function to eliminate points not matching specified dates.
## Updates the N pts file in workinglist and returns the named list (workinglist).
## This should be performed before clipping to the RU.  

## Points with NA dates are retained,
## else points with visit dates >= target[] <= are retained,
## else, points are removed.


filter.date <- function(importSDD,target,N) 
{
  s<-names(importSDD$pts[N])
  pts<-importSDD$pts[[s]]
  
  pts$season<-substr(pts$DT_VST,6,10)		## Assumes a consistent format!!!!!!!!
  pts$season<-gsub("-","",pts$season)
  
  for(i in 1:nrow(pts)) {
    pts$keep[i]<-0
    if(is.na(pts$season[i])) {
      pts$keep[i]<-1
    }else if(pts$season[i]=="") {
      pts$keep[i]<-1
    }else {
      if(as.numeric(pts$season[i])>= target[1] & as.numeric(pts$season[i])<=target[2]){pts$keep[i]<-1}
    }
  }
  pts<-pts[pts$keep==1,] 
  
  if(!is.null(pts)) {
    pts$season<-NULL
    pts$keep<-NULL
  }
  
  importSDD$pts[[s]]<-pts
  return(importSDD)
} ## end of function  
## UpdatePtLocations() - Updates the coords of sdd PTS with TerrADat info IFF the 2 share the same primarykey, or plotkey, or plotnam.

## UpdatePtLocations(workinglist,terra.spdf,N)

UpdatePtLocations<-function(workinglist,terra,N)
{
  
  s<-names(workinglist$pts[N])
  pts<-workinglist$pts[[s]]
  
  #  if(is.null(pts)) return(pts)		## Nothing to do since pts is NULL
  #  if(nrow(pts)<=0) return(pts)		## Nothing to do since pts is empty
  
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
  
  workinglist$pts[[s]]<-pts
  
  return(workinglist)
}

## MergePts() - concatenates pts files already in the named list.

##      MergePts(workinglist, from this entry in the named list, with this entry in the named list,action) 
##               action = 0 only performs checks for similar column nos. and names, and compares strata nomenclature, action =1
##               does the same but will then perform the merger if similar column nos. and names.

MergePts<-function(workinglist,from,to,action)
{
  
  names.df<-NULL
  
  ## pick up the pts 
  ptsfromname<-names(workinglist$pts[from])
  ptsfrom<-workinglist$pts[[ptsfromname]]
  
  ptstoname<-names(workinglist$pts[to])
  ptsto<-workinglist$pts[[ptstoname]]
  
  er1<-0
  if(ncol(ptsto)!=ncol(ptsfrom)) { 		## Check column lengths
    print(paste("ERROR in MergePts; Column lengths differ", ncol(ptsto),ncol(ptsfrom),sep=" "))
    er1<-1
  }
  
  er2<-0
  masterptsnams<-unique(names(ptsfrom))
  slaveptsnams<-unique(names(ptsto))	## Ensure field names of pts files are the same
  for(nam in slaveptsnams) {
    if(nam %in% masterptsnams==F) {
      print(paste("Error in MergePts; Field names not the same ",nam,sep=" "))
      er2<-1
      temp.df<-data.frame(nam)
      names.df<-rbind(names.df,temp.df)
    }
  }
  masterptsnams<-unique(names(ptsto))
  slaveptsnams<-unique(names(ptsfrom))	## Ensure field names of pts files are the same
  for(nam in slaveptsnams) {
    if(nam %in% masterptsnams==F) {
      print(paste("Error in MergePts; Field names not the same ",nam,sep=" "))
      er2<-1
      temp.df<-data.frame(nam)
      names.df<-rbind(names.df,temp.df)
    }
  }
  
  if(er1==0)print("MergePts - column lengths are equal")
  if(er2==0)print("MergePts - column attributes the same")
  
  if(action==1) {
    ## Bind and store back into workinglist$pts  
    newpts<-spRbind(ptsto,ptsfrom)
    workinglist$pts[[ptstoname]]<-newpts
    remove(newpts)		## Declutter
    print("MergePts - pts merged")
  }
  
  remove(ptsto)		## Declutter
  remove(ptsfrom)		
  
  
  
  
  return(workinglist)    
}
