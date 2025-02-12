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

