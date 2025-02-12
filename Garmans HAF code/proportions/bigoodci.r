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

