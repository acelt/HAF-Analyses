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

