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
}