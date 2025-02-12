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
