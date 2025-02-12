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

