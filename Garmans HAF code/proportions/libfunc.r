## arcpy.r = Libraries and functions used in PROPORTION.R
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
library(binom)
library(CoinMinD)
library(MCMCpack)
library(coda)
Library(MASS)
arc.check_product()


## I.  flex.dissolve, flex.clip, and flex.erase functions.
####################################################

## Modification of flex.erase() written by Nelson Stauffer

## flex.dissolve() - dissolve spatial polygon data frames using ARCPY.  Fixing slivers sometimes creates an additional stratum,ru,inference combination.
##                   This function ensures a minimimal polygonal version of the final map, which then be populated with aerial fields.



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
           arcpy.script <- c("import arcpy",
                             "from arcpy import env",
                             paste0("env.workspace = '", temp.directory, "'"),
                             "in_features = 'inshape.shp'",
                             "out_feature_class = 'outshape.shp'",
                             "arcpy.Dissolve_management(in_features, out_feature_class,[\"DMNNT_S\", \"RU\",\"infernc\"])"
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
           names(out.results@data) <- names(spdf@data)

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
                             "xy_tolerance = '.5'",
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

## Binomial & Goodman multinomial simultaneous CIs () - Derives binomial CI, Wilson's method, and Goodman's MS CI

# mydesign is siteID=mysiteID, wgt=wgt,xcoord=xcoord,ycoord=ycoord,stratum=stratum
# mydata.cat - see PROPORTION.R; contains plot Ids, condition scores,....
# conf.levelF is conf.level/100


BIGoodCI<-function(mydata.cat,mydesign,conf.levelF)
{
  
   storeit<-NULL
   TotalN<-0
   TotalWgt<-0
   cat<-unique(mydata.cat$CatVar)	## Unique condition classes
   for(i in 1:length(cat)) {		## Essentially loop for each condition class, derive N and the weights by condition class, & 
				        ## derive Grand totalN and totalwgt.
     mass<-0
     s<-cat[i]
     z<-grep(mydata.cat$CatVar,pattern=s)
     if(length(z)>0) {
       for(j in z){
         mass<-mass+mydesign$wgt[j]
       }
     }
     TotalN<-TotalN+length(z)
     TotalWgt<-TotalWgt+mass
     y<-data.frame(Class=s,N=length(z),WGT=mass)
     storeit<-rbind(storeit,y)
   }

   # Convert weights to proportions
    storeit$PROP<-0
    storeit$NewN<-0
    TN<-0		## THe total N generated from the proportion calculations
    y<-NULL
    for(i in 1:nrow(storeit)) {
       storeit$PROP[i]<-(storeit$WGT[i]/TotalWgt)
       storeit$NewN[i]<-storeit$PROP[i]*TotalN
       TN<-TN+storeit$NewN[i]
       y<-cbind(y,storeit$NewN[i])
    }

    x<-c(y)
    y<-binom.confint(x = x, n = TN, conf.level =conf.levelF, methods = c("wilson"))

    ######## Summarize the BIONOMIAL results and add some fluff to facilitate interpretation
    ci<-conf.levelF*100
    lcinam<-paste0("LCI",as.character(ci),".P")
    ucinam<-paste0("UCI",as.character(ci),".P")
    binom<-data.frame(Method="Wilson",Class=storeit$Class,N=storeit$N,WGT=storeit$WGT,Est.P=y$mean*100,lcinam=y$lower*100,ucinam=y$upper*100)
    savebinom<-binom    ## Used as a template for Goodman's results
    TP<-sum(binom$Est.P)
    z<-data.frame(Method=NA,Class=NA,N=TN,WGT=TotalWgt,Est.P=TP,lcinam=NA,ucinam=NA)
    binom<-rbind(binom,z)
    ## Add a space at the bottom
    zz<-data.frame(Method=NA,Class=NA,N=NA,WGT=NA,Est.P=NA,lcinam=NA,ucinam=NA)
    binom<-rbind(binom,zz)
    names(binom)[names(binom) == "lcinam"] <- lcinam 
    names(binom)[names(binom) == "ucinam"] <- ucinam 


   ####### We also derive Goodman's multinomial simultaneous CIs (GM).  Clean-up and add some fluff to facilitate interpretation
    y<-capture.output(GM(x,conf.levelF))

    a<-savebinom  
    a$Method<-"Goodman" 
    nclasses<-nrow(a)		## no. of CIs to extract from y

 
    ## The following is a very weird way to extract the numeric CI values from y[3] & y[5].  WTF!
    b<-y[3]		## y[3] is list of lower CI
    lci.df<-ExtractGM(b)	## lci.df values are converted to percentage values in ExtractGM		
    b<-y[5]		## y[5] is list of upper CI
    uci.df<-ExtractGM(b)

    a$lcinam<-lci.df
    a$ucinam<-uci.df
    a<-rbind(a,z)

    names(a)[names(a) == "lcinam"] <- lcinam 
    names(a)[names(a) == "ucinam"] <- ucinam 


    binom<-rbind(binom,a)

    return(binom)
}

##ExtractGM() - Extract the CIs from capture.output(GM) - Goodman's multinomial simultaneous CI function.  Weird way but only way
##              I could extract the values.  Below is an example of what capture.output(GM()) returns.

# [1] "Original Intervals"                "Lower Limit"                      
# [3] "[1] 0.3278298 0.3619442 0.1314142" "Upper Limit"                      
# [5] "[1] 0.4622324 0.4980913 0.2374017" "Adjusted Intervals"               
# [7] "Lower Limit"                       "[1] 0.3278298 0.3619442 0.1314142"
# [9] "Upper Limit"                       "[1] 0.4622324 0.4980913 0.2374017"
# [11] "Volume"                            "[1] 0.00193941"   


ExtractGM<-function(b)		## b is y[3] or y[5]
{
    leny<-nchar(b)
    begin<-0
    end<-0
    ci.df<-NULL
    for(i in 1:leny) {
        c<-substr(b,i,i)
        if(c==" "){		## Find blanks.  Beginning of number is +1, end of number is -1 from i.
          if(begin==0) {
             begin<-i+1
	  } else {
            end<-i-1
            d<-substr(b,begin,end)
            ci.df<-rbind(ci.df,as.numeric(d))
            begin<-i+1
            end<-0
          }
       }
    }
    d<-substr(b,begin,leny)		## The last number has no blank to the RHS, thus use total length
    ci.df<-rbind(ci.df,as.numeric(d))
    ci.df<-ci.df*100.0			## Convert to percentage value
    return(ci.df)
}## InferenceArea() - Set inference by reporting unit & aerial summaries into strata.spdf
## InferenceArea(strata file, aerial summaries, RU names,

InferenceArea<-function(strata.spdf,summary.df,RUname)
{
## At the end of summary is the total - don't include in the following looping (hence, inum-1).
   inum=nrow(summary.df)
   inum<-inum-1

## Transfer aerial extent info in summary.df to the shapefile
   strata.spdf$AREA.HA.Total<-0
   strata.spdf$AREA.HA.Sampled<-0
   strata.spdf$Proportion<-0

   for(i in 1:inum) {
       for(j in 1:nrow(strata.spdf)) {
         if(str_to_upper(strata.spdf$DMNNT_STRTM[j]) %in%summary.df$WEIGHT.ID[i] & strata.spdf$RU[j] %in%summary.df$REPORTING.UNIT[i]) {
             if(strata.spdf$Inference[j] ==1 && summary.df$AREA.HA.Sampled[i]>0) {
                strata.spdf$AREA.HA.Total[j]<-summary.df$AREA.HA.Total[i]
                strata.spdf$AREA.HA.Sampled[j]<-summary.df$AREA.HA.Sampled[i]
                strata.spdf$Proportion[j]<-summary.df$Proportion[i]
             }
             if(strata.spdf$Inference[j] ==0 && summary.df$AREA.HA.Sampled[i]==0) {
                strata.spdf$AREA.HA.Total[j]<-summary.df$AREA.HA.Total[i]
                strata.spdf$AREA.HA.Sampled[j]<-summary.df$AREA.HA.Sampled[i]
                strata.spdf$Proportion[j]<-summary.df$Proportion[i]
             }

	 }
       }
   }

     zz<-unique(strata.spdf$RU)
     for(i in zz) {
     	strata.spdf$RUname[strata.spdf$RU==i]<-RUname[i]
     }

     ## Can have AREA.HA.Total set to zero which means the stratum didn't actually occur in the reporting unit, so delete these cases here
     strata.spdf<-strata.spdf[strata.spdf$AREA.HA.Total>0 ,]
     return(strata.spdf)
}
##  PropEstimates() - generates the proportional estimates for both Normal and Binomial (Wilson's) method
##  PropEstimates(list of pt wgts, HAF scores, confidence level for NORMAL, confidence level for BINOMIAL,
##                tabprefix, the tab counter,sheetnam1,sheetnam2,append=F or T, Option,verbose)
##  sheetnam1 - tab for storing NORMAL estimates, sheetnam2 - tab for storing BINOMIAL estimates
##  APPend should be F for the first call to this function, thereafter =T
##  Option should be set to "NT" when working with non-target pts, else anything else.  With Option=NT, ts.df$PLOTKEY
##  is set to the PLOTID.SDD for non-target pts.
##  verbose=0 means output to _results.xlsx, =1 means suppress this output.

PropEstimates<-function(ts.df,score,conf.level,conf.levelF,tabprefix,tabcounter,sheetnam1,sheetnam2,APPend,Option,verbose)
{

## The following begins to set up the input to cat.analysis which derives mean proportions and CI

   mysiteID<-(SiteID=ts.df$PLOTKEY)		## PLOTKEY
   mysites <- data.frame(siteID=mysiteID, Active=rep(TRUE, nrow(ts.df)))
   mysubpop <- data.frame(siteID=mysiteID, All.Sites=rep("All Sites", nrow(ts.df)))
     wgt<-(ts.df$WGT)
     stratum<-(ts.df$Wgt.Category)
     xcoord<-ts.df$XMETERS
     ycoord<-ts.df$YMETERS
   mydesign <- data.frame(siteID=mysiteID, wgt=wgt,xcoord=xcoord,ycoord=ycoord,stratum=stratum)


## Sum the area of each stratum - set mypopsize
   strata<-unique(ts.df$Wgt.Category)


   store.df<-NULL
   temp.df<-NULL
   starea.df<-NULL
   for(i in 1:length(strata)) {
     targetwgts<-sum(ts.df$WGT[ts.df[, "Wgt.Category"] %in% strata[i]])
     temp.df<-data.frame(Strata=as.character(strata[i]),AREA.HA=targetwgts)
     starea.df<-rbind(starea.df,temp.df)
     a<-c(a=starea.df$AREA.HA[i])
     b<-starea.df$Strata[i]
     names(a)<-c(as.character(b)) 
     store.df<-c(store.df,a)
   }
   mypopsize <- list(All.Sites=c(store.df))


## Associate the suitability scores with the pts produced by the weighting process - set mydata.cat 

   site<-data.frame(mysiteID)
   site$CatVar<-NA				## Set this to NA to 

   for(i in 1:nrow(score)) {
      a<-grep(site$mysiteID,pattern=score$Plot.Identifier[i])
      if(length(a)!=1) {
         print(paste("Scored point Not found-> ",score$Plot.Identifier[i]))	
      }else {						
        site$CatVar[a]<-as.character(score$Suitability[i])			## MUTARE select the correct "score" field in HAF spreadsheet
      }
   }  

   if(Option=="NT") {			## IF we are dealing with non-target pts. then use ts.df$FINAL_DESIG as the CatVar score
     if(nrow(site)>0) {
       for(i in 1:nrow(site)) {
         if(is.na(site$CatVar[i])) {
            a<-grep(ts.df$PLOTID.SDD,pattern=site$mysiteID[i])
            if(length(a)==1) {
               site$CatVar[i]<-as.character(ts.df$FINAL_DESIG[a])
            }             
         }
       }
     }
   }
   mydata.cat <- data.frame(site)

########## Derive aerial proportions and store results as proportions.csv
   ## Have to see if we only have 1 pt per stratum, cause cat.analysis drops the pt, and if all
   ## pts are dropped, cat.analysis crashes

   if(length(strata)==nrow(site)) {
	a<-c("Insufficient samples for Normal CIs")
   }else {	
   	a<-cat.analysis(sites=mysites, subpop=mysubpop, design=mydesign,
   	data.cat=mydata.cat, popsize=mypopsize,vartype="Local",conf=conf.level)	## vartype="SRS or "Local"

	   ## Indicate that these results are based on Normal approx.
	   a$Type<-"Normal/cat.analysis"
   }
   fnam<-paste0(tabprefix[tabcounter],"proportions.csv")
   # write.csv(a,paste(src,fnam,sep="/"),row.names=F,quote=F)
   write.table(a,fnam,row.names=F,quote=F,sep=",",append=APPend)	## Initiates this file
   write.table("",fnam,row.names=F,quote=F,sep=",",append=T)

   if(verbose==0) {
   	fnam<-paste0(tabprefix[tabcounter],"_results.xlsx")
   	write.xlsx(a,fnam,sheetName=sheetnam1,col.names=T,row.names=F,append=APPend,showNA=F)   ## Initiates this spreadsheet
   }	

   ################ Also generate Bimonial, and Goodman's multi-simultaneous  CIs
   #mydesign is siteID=mysiteID, wgt=wgt,xcoord=xcoord,ycoord=ycoord,stratum=stratum
   #mydata.cat - see above; contains plot Ids, condition scores,....

   binom<-BIGoodCI(mydata.cat,mydesign,conf.levelF)

   fnam<-paste0(tabprefix[tabcounter],"proportions.csv")
   #write.csv(binom,paste(src,fnam,sep="/"),row.names=F,quote=F,append=T)
   write.table(binom,fnam,row.names=F,quote=F,sep=",",append=T)
   write.table("",fnam,row.names=F,quote=F,sep=",",append=T)

   if(verbose==0) {
   	fnam<-paste0(tabprefix[tabcounter],"_results.xlsx")
   	write.xlsx(binom,fnam,sheetName=sheetnam2,col.names=T,row.names=F,append=T,showNA=F)
   }
   return(mydata.cat)		## Contains PLOTKEY as mysiteID and condition class as CatVar.  Used to populate pts files.
 ############################
}
## SetAIMPlotKey() - creates and sets PLOTKEY for AIM pts stored in wgts.df

## As a convention, we'll use plotkey for the cross-walk between HAF scores and the weighted points.  
## wgts files likely will only have PRIMARYKEY for AIM, so convert PRIMARYKEY to PLOTKEY.  PRIMARYKEY of LMF 
## plots are set; PLOTID of LMF pts is set to LMF.  Non-target pts have a PLOTID (PLOTID.SDD) but NA for PRIMARYKEY.
## SetAIMPlotKey() creates and sets PLOTKEY.  PLOTKEY will have the PLOTKEY for AIM pts.  PRIMARYKEY is transferred to 
## PLOTKEY and to PLOTID.SDD for LMF pts. Non-target pts should have PRIMARYKEY set to NA.  By default, PLOTKEY of non-target
## pts is set to NA.
    
SetAIMPlotKey<-function(wgts.df)
{


   wgts.df$PLOTKEY<-NA
   wgts.df$temp<-wgts.df$PLOTID.SDD		## helps to transfer info to 'final' PLOTID.SDD 
   wgts.df$PLOTID.SDD<-NULL
   wgts.df$PLOTID.SDD<-NA			## Reset this field, esp. setting it to primarykey for LMF pts.
   ## Convert primarykey to plotkey.  Set LMF 
      for(i in 1:nrow(wgts.df)) {
          if(wgts.df$DATASRC[i]=="AIM") {
             if(!is.na(wgts.df$PRIMARYKEY[i])) {		## Can have non-responses which lack a primarykey
             	code<-nchar(as.character(wgts.df$PRIMARYKEY[i]))
             	wgts.df$PLOTKEY[i]<-substr(as.character(wgts.df$PRIMARYKEY[i]),1,code-10)
	     } 
             wgts.df$PLOTID.SDD[i]<-as.character(wgts.df$temp[i])		## handles all AIM pts
          }else {
             wgts.df$PLOTKEY[i]<-as.character(wgts.df$PRIMARYKEY[i])
             wgts.df$PLOTID.SDD[i]<-as.character(wgts.df$PRIMARYKEY[i])
          }
      }
      wgts.df$temp<-NULL
      return(wgts.df)
}

##  SetHAFPlotID() - ensures that HAF scores are using PLOTKEY as the plotID and the field is called
##                   Plot.Identifier 
##   SetHafPlotID(wgts.df is the weights info, score is the Excel spreadsheet scores)

SetHAFPlotID<-function(wgts.df,score,option)
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




## Now extract the PLOTKEYs from wgts.df by cross-walking score$PLOTIDC with wgts.df$PLOTID.SDD
      score$Plot.Identifier<-"NA"

      if(nrow(score)>0) {     ## Just to be sure
        for(i in 1:nrow(score)) {
            a<-grep(wgts.df$PLOTID.SDD,pattern=score$PLOTIDC[i])
            if(length(a)!=1) {
              print(paste("ERROR, cant find HAF score",score$PLOTIDC[i],sep="-"))
            }else {
               score$Plot.Identifier[i]<-as.character(wgts.df$PLOTKEY[a])
            }
        }
      }

      if(option==1) {		## only retain the matching pts -e.g., when dealing with allotments
	  score<-score[score$Plot.Identifier!="NA" ,]
      }
      
      b<-score[score$Plot.Identifier=="NA" ,]
      if(nrow(b)>0) {
         print("ERROR, not all score$Plot.Identifier are set")
         print(b)
      }
      return(score)
}	
############################################################################################################################################
## SetPts() - Derives and inserts tally of no. of pts by condition class, reporting unit, by strata into strata.spdf (finalmap shapefile)
## SetPts(strata.spdf, wgts.df) - returns modified strata.spdf

SetPts<-function(strata.spdf,wgts.df,score) 
{

########################################## ADD ON to record pts by condition class in finalmap.shp
##                                         There is some duplication of processing in main.r; however,
##                                         here we are considering all pts not just FINAL_DESIG==TS.
     strata.spdf$PTS_S<-0		## Suitable, Marginal, Unsuitable, Omitted, N (non-responses)
     strata.spdf$PTS_M<-0
     strata.spdf$PTS_U<-0
     strata.spdf$PTS_O<-0		## You can have weighted pts that were omitted from the HAF scoring
     strata.spdf$TotalObsN<-0		## Total weighted pts
     strata.spdf$PTS_N<-0
     strata.spdf$TotalPts<-0		## Total N
     strata.spdf$IA<-0			## N and HA by non-response types IA, NT, and UNK
     strata.spdf$IA_ha<-0
     strata.spdf$NT<-0
     strata.spdf$NT_ha<-0
     strata.spdf$UNK<-0
     strata.spdf$UNK_ha<-0
     strata.spdf$NonR_ha<-0		## Area of non-response pts (their weights)
     strata.spdf$NonRProportion<-0	## Proportion of total stratum area as non-response area	
     strata.spdf$AllInference<-0	## Inference code (1-Y, 0=N) when accounting for non-target pts								
     strata.spdf$Area<-0		## Sum of observed area and non-response area (HA)
     strata.spdf$TAREA<-0		## Proportion of $Area given total stratum area ($AREA.HA.Total)


   wgts.df$RUnum<-0		## numeric representation of reporting unit
   wgts.df$CLASS<-NA		## Condition class
   strata.spdf$AllInference<-strata.spdf$Inference		## begin to set up all inference field (1=Y, 0=N)

   unknown.values <- c("Unknown","UNK",NA)
   nontarget.values <- c("Non-Target","NT")
   inaccessible.values <- c("Inaccessible","IA")
####################################### 
									

########################################## Set Condition Class
      ## Couldn't get short-hand method for the following to work; thus looping....
      ## wgts.df$CLASS[(wgts.df$PLOTKEY %in% score$Plot.Identifier)]<-as.character(score$Suitability) - functions but returns an error

      for(i in 1:nrow(wgts.df)) {
         if(!is.na(wgts.df$PLOTKEY[i])) {			## Non-target pts shouldn't have a PLOTKEY in wgts.df					
            j<-grep(score$Plot.Identifier,pattern=wgts.df$PLOTKEY[i])     
            if(length(j)<=0) print(paste("WARNING; Weighted Pts not in HAF scores",wgts.df$PLOTKEY[i],sep="; "))
            if(length(j)>1) print(paste("ERROR; too many matches in setting condition class",i,sep="; "))    
            if(length(j)==1) {
                wgts.df$CLASS[i]<-as.character(score$Suitability[j])
            }
         }
      }
########################################## Set numeric code for reporting unit.  Limited to 2-digit code!
      wgts.df$RUnum<- wgts.df$REPORTING.UNIT %>% str_extract(pattern="[0-9]{1,2}")

########################################## Tally pts by strata, conditions class, FINAL_DESIG, reporting unit.
##                                         Scruitinize strata.spdf to ID the levels of each of the above variables.
   
     if(nrow(strata.spdf)>0) {
       for(i in 1:nrow(strata.spdf)) {
              s<-strata.spdf$DMNNT_STRTM[i]	## strata in strata.spdf
              ru<-strata.spdf$RU[i]		## reporting unit in strata.spdf
	      temp<-wgts.df[wgts.df$Wgt.Category %in% s ,]
	      temp<-temp[temp$RUnum %in% ru ,]
              strata.spdf$TotalPts[i]<-nrow(temp)		## total no. of pts
              savetemp<-temp					## need this if there are non-responses
              temp1<-temp[temp$FINAL_DESIG %in% target ,]	## TS points
              strata.spdf$PTS_N[i]<-nrow(temp)-nrow(temp1)	## temp1 is target sampled, temp is all points.  Delta is # of non-responses

	      ## Tally by condition class.  
              temp2<-temp1[temp1$CLASS %in% "Suitable" ,] 
              strata.spdf$PTS_S[i]<-nrow(temp2)	
              strata.spdf$TotalObsN[i]<-nrow(temp2)
              temp2<-temp1[temp1$CLASS %in% "Marginal" ,] 
              strata.spdf$PTS_M[i]<-nrow(temp2)	
              strata.spdf$TotalObsN[i]<-strata.spdf$TotalObsN[i]+nrow(temp2)
              temp2<-temp1[temp1$CLASS %in% "Unsuitable" ,] 
              strata.spdf$PTS_U[i]<-nrow(temp2)	
              strata.spdf$TotalObsN[i]<-strata.spdf$TotalObsN[i]+nrow(temp2)
              temp2<-temp1[is.na(temp1$CLASS) ,] 		## TS points with no condition class are weighted pts absent in the HAF score
              strata.spdf$PTS_O[i]<-nrow(temp2)			## omitted pts
              strata.spdf$TotalObsN[i]<-strata.spdf$TotalObsN[i]+nrow(temp2)

              ## Deal with non-responses, if any
              total<-0
              if(strata.spdf$PTS_N[i]>0) {
                  strata.spdf$AllInference[i]<-1		## If any non-target pts, ensure all inference is set
                  b<-savetemp[savetemp$FINAL_DESIG %in% inaccessible.values,]
                  strata.spdf$IA[i]<-nrow(b)
                  strata.spdf$IA_ha[i]<-sum(b$WGT)
                  total<-total+sum(b$WGT)
                  b<-savetemp[savetemp$FINAL_DESIG %in%  nontarget.values,]
                  strata.spdf$NT[i]<-nrow(b)
                  strata.spdf$NT_ha[i]<-sum(b$WGT)
                  total<-total+sum(b$WGT)
                  b<-savetemp[savetemp$FINAL_DESIG %in%     unknown.values,]
                  strata.spdf$UNK[i]<-nrow(b)
                  strata.spdf$UNK_ha[i]<-sum(b$WGT)
                  total<-total+sum(b$WGT)
     		  strata.spdf$NonR_ha[i]<-total
              }
              strata.spdf$Area[i]<-strata.spdf$AREA.HA.Sampled[i]+total			## observed + non-taget area (HA)
              strata.spdf$TAREA[i]<-strata.spdf$Area[i]/strata.spdf$AREA.HA.Total[i]	## Proportion of total strata area
	      strata.spdf$NonRProportion[i]<-strata.spdf$NonR_ha[i]/strata.spdf$AREA.HA.Total[i] ## Proportion of non-response area  
       }
     }
 ############################################################################################################################################

## Reset names of pt tallies here.  We can change these names to enhance interpretation of the shapefile without 
## making code changes above.  Perhaps, once these names are finalized, we can mod the code above and eliminate this section.
     names(strata.spdf)[names(strata.spdf) == "PTS_S"] <- "SuitableN"
     names(strata.spdf)[names(strata.spdf) == "PTS_M"] <- "MarginalN"
     names(strata.spdf)[names(strata.spdf) == "PTS_U"] <- "UnsuitN"
     names(strata.spdf)[names(strata.spdf) == "PTS_O"] <- "OmittedN"
     names(strata.spdf)[names(strata.spdf) == "PTS_N"] <- "NonRespN"
     names(strata.spdf)[names(strata.spdf) == "TotalPts"] <- "TotalN"
     names(strata.spdf)[names(strata.spdf) == "Area"] <- "HA.AllPts"
     names(strata.spdf)[names(strata.spdf) == "TAREA"] <- "AllProportion"
     names(strata.spdf)[names(strata.spdf) == "Inference"] <- "ObsInference"
     names(strata.spdf)[names(strata.spdf) == "Proportion"] <- "ObsProportion"
     names(strata.spdf)[names(strata.spdf) == "NonR_ha"] <- "HA.NonR"
     names(strata.spdf)[names(strata.spdf) == "IA"] <- "IA_N"
     names(strata.spdf)[names(strata.spdf) == "NT"] <- "NT_N"
     names(strata.spdf)[names(strata.spdf) == "UNK"] <- "UNK_N"
     names(strata.spdf)[names(strata.spdf) == "IA_ha"] <- "IA_HA"
     names(strata.spdf)[names(strata.spdf) == "NT_ha"] <- "NT_HA"
     names(strata.spdf)[names(strata.spdf) == "UNK_ha"] <- "UNK_HA"



     return(strata.spdf)
}
## SetWgtKeys() - initiate fake plotkeys for TS pts not yet entered into the SDD and TerrADat.
##                 After the call to this function, call SetPtKeys() to set pts.spdf with the
##                  info erived here.

SetWgtKeys<-function(wgts.df)
{

  target<-c("Target Sampled","TS","TARGET SAMPLED")

  counter<-99123456
  
   for(i in 1:nrow(wgts.df)) {
      if(wgts.df$FINAL_DESIG[i] %in% target) {
          if(is.na(wgts.df$PLOTKEY[i])) {
              counter<-counter+1
              wgts.df$PLOTKEY[i]<-as.numeric(counter)
          }
      }
   }
  return(wgts.df)
}




################################################################################################
## SetPtKeys() - sets the contrived plotkeys set in wgts.df (above) into the pts.spdf file.

SetPtKeys<-function(pts.spdf,wgts.df)
{

  ## Save actual plotkeys, then reset pts.spdf PLOT_KEY before saving PTS file at end of processing.
  pts.spdf$PLOT_KEY_SAVE<-pts.spdf$PLOT_KEY

  target<-c("Target Sampled","TS","TARGET SAMPLED")

   for(i in 1:nrow(pts.spdf)) {
      if(pts.spdf$FINAL_DESI[i] %in% target) {
          if(is.na(pts.spdf$PLOT_KEY[i])) {
              a<-wgts.df[wgts.df$PLOTID.SDD %in% pts.spdf$PLOT_NM[i] ,]
              pts.spdf$PLOT_KEY[i]<-a$PLOTKEY
          }
      }
   }
  return(pts.spdf)
}

#wgts.df$PLOTID.SDD
#wgts.df$PLOTKEY
#wgts.df$FINAL_DESIG

#pts.spdf$PLOT_NM
#pts.spdf$PLOT_KEY
#pts.spdf$FINAL_DESI


## SumArea() - Summarize across strata totals, record in areasums.csv and _results.xlsx
## SumArea(adjusted strata area, the tab prefix, the tab counter,sheetname for _results.xlsx, name for area_sampled column,
##  output to _results.xlsx Y or N)

SumArea<-function(areas.df,tabprefix,tabcounter,sheetname,Option,verbose)
{
   strata<-unique(areas.df$WEIGHT.ID)
   totlarea<-0
   samparea<-0
   temp.df<-NULL
   areasum.df<-NULL
   for(i in 1:length(strata)) {
     total<-sum(areas.df$AREA.HA.Total[areas.df[, "WEIGHT.ID"] %in% strata[i]])
     sample<-sum(areas.df$AREA.HA.sample[areas.df[, "WEIGHT.ID"] %in% strata[i]])
     totlarea<-totlarea+total
     samparea<-samparea+sample
     prop=sample/total
     temp.df<-data.frame(Strata=as.character(strata[i]),AREA.HA.Total=total,AREA.HA.Sampled=sample,Proportion=prop)
     areasum.df<-rbind(areasum.df,temp.df)
   }
   temp.df<-data.frame(Strata="TOTALS",AREA.HA.Total=totlarea,AREA.HA.Sampled=samparea,Proportion=samparea/totlarea)
   areasum.df<-rbind(areasum.df,temp.df)
   names(areasum.df)[names(areasum.df)=="AREA.HA.Sampled"]<-Option

   fnam<-paste0(tabprefix[tabcounter],"areasums.csv")
   write.table(areasum.df,paste(src,fnam,sep="/"),row.names=F,quote=F,sep=",",append=T)
   write.table("",paste(src,fnam,sep="/"),row.names=F,quote=F,sep=",",append=T)

   if(verbose==0) {
   	fnam<-paste0(tabprefix[tabcounter],"_results.xlsx")
   	write.xlsx(areasum.df,fnam,sheetName=sheetname,col.names=T,row.names=F,append=T,showNA=F)
   }

   return(areasum.df)
}

## SumStratArea() - summarize total area and sampled area by strata by reporting unit, then record to areasums.csv and _results.xlsx
## SumStratArea(adjusted area df, reporting unit names, the tab prefix, the tab counter,sheetname for _results.xlsx,APPend=F or T, name
## for the area_sampled column, output to _results.xlsx Y or N)
## APPend should be F in the first call (per tab) to this function, else T
## verbose = 0 means output results to _results.xlsx, =1 to suppress this output.

SumStratArea<-function(areas.df,RUname,tabprefix,tabcounter,sheetname,APPend,Option,verbose)
{
   ## areas.df is a summary of total area and sampled area by strata by reporting unit.  Summarize and include in areasums.csv
   summary.df<-data.frame
   summary.df<-areas.df

   ## Add reporting unit name to summary.df
   zz<-unique(summary.df$REPORTING.UNIT)
   for(i in zz) {
        summary.df$RUname[summary.df$REPORTING.UNIT==i]<-RUname[i]
   }
   b<-data.frame(WEIGHT.ID="Total",AREA.HA.Total=sum(summary.df$AREA.HA.Total),AREA.HA.sampled=sum(summary.df$AREA.HA.sampled),REPORTING.UNIT="All",RUname="NA")
   summary.df<-rbind(summary.df,b)
   summary.df$Proportion<-summary.df$AREA.HA.sampled/summary.df$AREA.HA.Total
   names(summary.df)[names(summary.df)=="AREA.HA.sampled"]<-Option

   fnam<-paste0(tabprefix[tabcounter],"areasums.csv")
   write.table(summary.df,fnam,row.names=F,quote=F,sep=",",append=APPend)
   write.table("",fnam,row.names=F,quote=F,sep=",",append=T)	## Add a space to the output file

   if(verbose==0) {
   	fnam<-paste0(tabprefix[tabcounter],"_results.xlsx")
   	write.xlsx(summary.df,fnam,sheetName=sheetname,col.names=T,row.names=F,append=T,showNA=F)
   }
   return(summary.df)
}
## SumStrataCols() - sums the appropriate columns of strata.spdf used in the finalmap shapefiles.

SumStrataCols<-function(strata.spdf)
{

   ## Save areas needed to derive total proportions
   a<-sum(strata.spdf$AREA.HA.Total)
   b<-sum(strata.spdf$AREA.HA.Sampled)
   c<-b/a

   d<-sum(strata.spdf$HA.NonR)
   e<-d/a

   f<-sum(strata.spdf$HA.AllPts)
   g<-f/a


   temp.df<-data.frame(DMNNT_STRTM=NA,RU=NA,ObsInference=NA,AREA.HA.Total=a,AREA.HA.Sampled=b,
            ObsProportion=c,RUname=NA,SuitableN=sum(strata.spdf$SuitableN),MarginalN=sum(strata.spdf$MarginalN),
            UnsuitN=sum(strata.spdf$UnsuitN),OmittedN=sum(strata.spdf$OmittedN),TotalObsN=sum(strata.spdf$TotalObsN),
            NonRespN=sum(strata.spdf$NonRespN),TotalN= sum(strata.spdf$TotalN),IA_N=sum(strata.spdf$IA_N),
            IA_HA= sum(strata.spdf$IA_HA),NT_N=sum(strata.spdf$NT_N), NT_HA=sum(strata.spdf$NT_HA),
            UNK_N=sum(strata.spdf$UNK_N),UNK_HA=sum(strata.spdf$UNK_HA),
            HA.NonR=d,NonRProportion=e,AllInference=NA,HA.AllPts=f,AllProportion=g)   

   strata.df<-data.frame(strata.spdf)			## convert spdf to df
   strata.df<-rbind(strata.df,temp.df) 

   return(strata.df)
} 

## SumStratWgts() - sums the wgts by strata by reporting unit in ts.df and transfers it to areas.df
## SumStratWgts(the wgts,strata area,max possible no. of reporting units)

SumStratWgts<-function(tss.df,area.df,index)
{
        ## Clear AREA.HA.sampled 'cause those numbers may be wrong if some weighted plots were not in the HAF scores.
   	## Reset area sampled within areas.df using weights and reporting unit info in ts.df.
        ## Also, when working with non-target pts, the original AREA.HA.sampled doesn't include the non-target pts area (part of
        ## the schema implemented in AIMWEIGHTS).
   	## index should be the max number of reporting units (maxindex)
	
	## Translate REPORTING.UNIT (e.g., RU1) in tss.df to a NUMBER (e.g., 1)
        tss.df$REPORT.NUM<-0
        tss.df$REPORT.NUM<- tss.df$REPORTING.UNIT %>% str_extract(pattern="[0-9]{1,2}")

        ## Here we re-set the numbers using a potential subset of plots
        area.df$AREA.HA.sampled<-0
        d <- tss.df %>% dplyr::group_by(Wgt.Category,REPORT.NUM)  %>% dplyr::summarize(area = sum(WGT))
   	for(i in 1:index) {			## reporting unit numeric code - assumes we number them 1-n & index is the max
       		dd<-data.frame(d[grepl(d$REPORT.NUM,pattern=i)==TRUE ,])	## This will work for up to 2 digit RUs
                if(nrow(dd)>0) {
                    for(j in 1:nrow(dd)) {
                       area.df$AREA.HA.sampled[area.df$WEIGHT.ID %in% dd$Wgt.Category[j] & area.df$REPORTING.UNIT==i]<-dd$area[j]
                    }
                }
   	}
        return(area.df)
 }
## StrataRU() - transforms the strata file into strata by reporting unit 
## StrataRU(clip,indexb,indexe,list of RU numbers)

StrataRU<-function(strata.spdf,clip,indexb,indexe,RU)
{
    strata.spdf$RU<-0
    index<-0

    for(nam in clip) {
       index<-index+1
       if(index>=indexb & index <=indexe) {
          if(!is.na(nam)) {
       		## Creates an SPDF 
       		assign(x = "clip.spdf",
       		value = nam %>% arc.open() %>% arc.select %>%
       		SpatialPolygonsDataFrame(Sr = {arc.shape(.) %>% arc.shape2sp()}, data = .)%>%spTransform(projection))

      		RU.spdf<-flex.clip(strata.spdf,clip.spdf,method="arcpy",temp.path=getwd())
       		RU.spdf$RU<-RU[index]
       		strata.spdf<-flex.erase(strata.spdf,clip.spdf,method="arcpy",temp.path=getwd())

       		uid<-1
       		n <- length(slot(RU.spdf, "polygons"))
       		poly.data <- spChFIDs(RU.spdf, as.character(uid:(uid+n-1)))
       		uid <- uid + n

       		n <- length(slot(strata.spdf, "polygons"))
       		temp.data <- spChFIDs(strata.spdf, as.character(uid:(uid+n-1)))
       		uid <- uid + n
       		strata.spdf <- spRbind(temp.data,poly.data)
	  }
       }
    }

    ## Small slivers can end up as RU==0; clean-up here.
    strata.spdf<-strata.spdf[strata.spdf$RU>0 ,]
    return(strata.spdf)
}
