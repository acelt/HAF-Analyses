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
