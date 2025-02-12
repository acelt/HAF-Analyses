## QuickCheck.r
## Pulls in the wgts files to compare with HAF spreadsheet
## MAIN
## Warner, OR.  12/15/2017
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





## 

   target<-c("Target Sampled","TS","TARGET SAMPLED")

## set working directory
   setwd("c:/projects/fohaf/or/warner/winter")			# Mutare  spring, summer, winter

## set list
    res<-c("run1OR_LakeviewDO_2016_2020_SDD.gdb_wgt_2017-12-17",
"run2OR_LakeviewDO_2016_2020_SDD.gdb_wgt_2017-12-17",
"run3OR_LakeviewDO_2016_2020_SDD.gdb_wgt_2017-12-17",
"run4OR_LakeviewDO_2016_2020_SDD.gdb_wgt_2017-12-17")


    store.df<-NULL

    for(s in 1:length(res)) {
         a<-read.csv(res[s])
         print(paste("No. of rows in weights ",nrow(a),sep=" "))
         a<-a[a$FINAL_DESIG %in% target ,]
         print(paste("No. of TS rows in weights ",nrow(a),sep=" "))
         b<-a[a$DATASRC=="LMF" ,]
         if(nrow(b)>0){
            b$PLOTKEY<-b$PRIMARYKEY
            temp<-data.frame(b$PLOTKEY,b$PRIMARYKEY,b$PLOTID.SDD,b$REPORTING.UNIT)
            store.df<-rbind(store.df,temp)
         }
         c<-a[a$DATASRC=="AIM" ,]
         if(nrow(c)>0) {
           for(i in 1:nrow(c)) {
               len<-nchar(as.character(c$PRIMARYKEY[i]))
               c$PLOTKEY[i]<-(substr(c$PRIMARYKEY[i],1,len-10)) 
            }
            b<-c
            temp<-data.frame(b$PLOTKEY,b$PRIMARYKEY,b$PLOTID.SDD,b$REPORTING.UNIT)
            store.df<-rbind(store.df,temp)
         }
    }

    names(store.df)[names(store.df) == "b.PLOTKEY"] <- "PLOTKEY"
    names(store.df)[names(store.df) == "b.PRIMARYKEY"] <- "PRIMARYKEY"
    names(store.df)[names(store.df) == "b.REPORTING.UNIT"] <- "REPORTING.UNIT"
    names(store.df)[names(store.df) == "b.PLOTID.SDD"] <- "PLOTID"

    ## Ingest the HAF spreadsheet
    scored<-c("c:/projects/fohaf/or/warner/haf/OR_2017_Warners_NOC_HAF_Analysis_Request_20171206_v2.xlsx")
    #score<-(read.xlsx(scored,sheetName="S-3 Nesting Early Brood Rearing",stringAsFactors=F))	## Assumes all tabs are the same
   # score<-(read.xlsx(scored,sheetName="S-4 Upland Summer Late Brood",stringAsFactors=F))	## Assumes all tabs are the same
    score<-(read.xlsx(scored,sheetName="S-6 Winter",stringAsFactors=F))	## Assumes all tabs are the same
    #score<-(read.xlsx(scored,sheetName="S-4 Combined Grass_Forbs",stringAsFactors=F))	## Assumes all tabs are the same

    ## Gotta get rid of trailing blanks in the spreadsheet
    score<-score[!is.na(score$Plot.Identifier) ,]

    #############################################################
    ## The following changes score plot.Identifier to plot name, adjusts the plotkey of store.df to plot name for following comparison.
    ## USE when plot.identifier is actuall plot name not plotkey
    score$PLOTID<-score$Plot.Identifier
    score<-SetHAFPlotID(score)
    score$Plot.Identifier<-score$PLOTIDC
    store.df$PLOTKEY<-store.df$PLOTID
    #################################################


    ## Go thru spreadsheet and check off the corresponding plots in store.df     
       store.df$USED<-0
       for(i in 1:nrow(score)) {
          id<-score$Plot.Identifier[i]
          a<-grep(store.df$PLOTKEY,pattern=id)
          if(length(a)>1) print(paste("Used more than once in store.df",id,length(a),sep="/"))
          if(length(a)==1) {
            store.df$USED[a]<-1
          }
          if(length(a)==0) {
            print(paste("HAF pt not present in spatial wgt analysis",id,sep="/"))
          }
       }
  
       ## list the AIM/LMF pts that occurred in the spatial analysis but not in the HAF spreadsheets
          a<-store.df[store.df$USED==0 ,]
          print("PLOTKEYs in the spatial analysis not in HAF spreadsheets")
          print(a$PLOTKEY)

