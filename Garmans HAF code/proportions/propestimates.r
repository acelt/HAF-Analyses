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
