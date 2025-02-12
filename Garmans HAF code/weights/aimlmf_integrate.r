## 4/14/2017
## AimLmf_integrate.r - Function to integrate lmf and aim points 

## 1) Starting with LMF points, determine LMF strata of points.  Strata are Type I (rangeland that is SG primary habitat), 
##    Type II (rangeland that is non-prime SG habitat).  See LMF/NRI documentation for actual definition of primary and non
##    prime SG habitat. Uses over(pts,strata) to determine strata of pts.  

## 2) Determine no. of LMF points in the same LMF segment (uniquely numbered quarter-section parcels).  2 possible ways to do this.  The first is to use over(pts,segments)
##    to determine segment no. associated with each LMF point, then determine those points with the same segment number.
##    However, noticed there is the potential for georegistration error! How can you tell?  Well, the PLOTKEY numbers of LMF points 
##    in the same segment have the same prefix (but differ by a 2-digit alphanumeric suffix; e.g., 1234567R1, 1234567R3 are PLOTKEYS for 2 points
##    in the same segment).  Where there is georegistration error, pts with the same PLOTKEY suffix will fall within a segment and outside the segment 
##    (typically resulting in a segment code== NA).  Thus, 2 pts that were supposed to be in the same segment will have different
##    segment membership. Saw this with the first LMF data set I worked with (Bruneau FO)). Decided not to move the point (based on what rule set???).  
##    Life goes on...  SO, this function uses the first method (over()) to set segment codes of pts and to tally no. of pts in each segment, 
##    then scruitinizes the PLOTKEY codes to adjust segment membership where there is a problem.  The rule set is: The same PLOTKEY-code suffix indicates the same segment.  
##    Where this doesn't occur, the pt outside of the segment is assigned (but not physically moved) to the proper segment, and the 
##    no. of pts in the segment is updated after this virtual move.  

##    THe number of LMF points (2nd sample stage) in a segment affects the point-inclusion probability, and thus, the point weight.
##    LMF protocol calls for the establishment of 2 points in a selected segment (1st sample stage).  If only 1 point shows up per segment, then
##    the relative weight is 0.5.  If 2 points show up, then each has a relative weight of 0.5.  If 2 LMF and 1 AIM point
##    are in the same segment, then each has a relative weight of 0.3333.  Etc....

## 3) Determine LMFpt overlap with SDD strata, if stratification is used.

## 4) Determine overlap between SDD pts and LMF TYPE I & II stratum

## 5) Determine overlap between SDD pts and LMF segments

## 6) Determine if SDD (AIM) pts occurr in segments containing LMF points

## 7) Combine the LMF pts with SDD pts and save the resulting info in the SDD list.
##    The final pts file will only have key attributes used in subsequent processing, including 4 attributes included to facilitate LMF processing
##    	aimlmf = 1 if AIM, =2 if LMF pt
##    	lmfstrata = 1 for Type I,  =2 for Type 2
##	relweight - >0 to 1.0  
##      segcode -> lmf segment code


## Returns a list just like importSDD BUT the pts SPDF has the LMF+AIM points, and formatted differently from a 'standard' SDD pts file.
## This function assumes a related sequential order in importSDD and importLMF (i.e., AIM and LMF points for the same field office are listed in the
## same sequential order), but you can have multiple field offices.  The returned list of SPDFs, however, only binds AIM and LMF points of the same field office.
## If you want to aggregate across field offices, you need to do so in the MAIN func after calling this function (at this point in time!).



## importSDD is the list returned by sdd.reader; importLMF is the list returned by lmf.reader
## yearlist is a list of the years to include
AimLmf_integrate <- function(importSDD,importLMF,yearlist) 
{
     projection = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
     target.values = c("Target Sampled","TS")

     workinglist<-importSDD	## This is where we store the new integrated LMF+AIM data


     index<-0		## This is used to extract the proper SDD files later in the looping
     ###################################################################################### Loop thru lmfpts & corresponding SDDpts
     for(s in names(importLMF$lmfpts) ) {
        index<-index+1

        pts<-importLMF$lmfpts[[s]]


        strata<-importLMF$lmfstrata[[s]]
        seg<-importLMF$lmfsegments[[s]]


	## parse data by requested yr(s), but first set up panel
	## create the panel attribute based on $DTVISIT,  This will work for ca. the next 30 yrs
	 for(i in 1:30) {			## Assumes that the earliest yr of LMF data is 2011
	     pts$PANEL[grepl(pts$DTVISIT,pattern=(2010+i))==TRUE]<-2010+i
	 }

         ## Must be an easier way to do the following, but couldn't figure it out!!!!
         pts$a<-1  				## default to 1
         pts$a[pts$PANEL %in% yearlist] <-2     ## set to 2 if panel year(s) in yearlist (user-specified year of interest)
         pts<-pts[pts$a==2,]  			## render pts to those years specified by the user
         pts$a<-NULL	      			## clean up



        pts<-spTransform(pts,projection) 
        strata<-spTransform(strata,projection) 
        seg<-spTransform(seg,projection) 

        temp<-over(pts,strata)  ## This contains info from the LMF strata for each row in pts, but lacks specific point info.
                                ## THe field STRATUM will be 1 or 2; this is the info we carry back over to pts.
        pts$LMFSTRATA<-temp$STRATUM
        remove(temp)
        temp<-over(pts,seg)     ## SEGCODE (unique segment number) is listed for each row in pts.  This field is carried back over to pts.
        pts$SEGCODE<-temp$SEGCODE
         


        ## Use PLOTKEY to finalize/set relative weight of each pt.  The following method ensures that if the segcode of 1 point of a pair is NA, 
        ## the segcode of the other point is assigned to both points.  ERROR checking ensures that a final segcode is !NA
        ## Set the relative weight which relates to (but not the same as) the point inclusion probability, where relative weight = 1.0/(no. of pts in a segment)


        cnt<-nrow(pts)
        for(i in 1:cnt) {
           code<-pts$PLOTKEY[i]    
           codel<-nchar(code)
           code<-substr(code,1,codel-1)   ## reuse code to store PLOTKEY suffix 
           records<-grep(x=pts$PLOTKEY,pattern=code) 

           if(length(records)==1) {			## If only 1 pt, then make sure it has a segcode, else terminate
              if(is.na(pts$SEGCODE[records[1]])) {
              	print("SEGCODE is NA in aimlmf_integrate") 
              	q()
              }else {				        ## Set relative weight and segcnt (no. of pts in a segment) 
                pts$RELWEIGHT[records[1]]<-0.5
                pts$SEGCNT[records[1]]<-1
              }
           }else {				        ## There are >1 pts associated with the PLOTKEY prefix; retrieve the segment codes of each pt 
              a<-pts$SEGCODE[records]
              if(length(unique(a))!=1) {		## This handles 2 pts where segcode of one of them is NA
                 b<-(!is.na(pts$SEGCODE[records]))
                 if(b[1]) {				## Figure out which one has the NA and which doesn't
                    pts$SEGCODE[records[2]]<-a[1]
                    a[2]<-a[1]
                 }else {
                    pts$SEGCODE[records[1]]<-a[2]                    
                    a[1]<-a[2]
                 } 
                pts$RELWEIGHT[pts$SEGCODE==unique(a)]<-1.0/length(a)  ## After resolving the NA occurrence, Set relative weight for all occurrences of segcode== unique(a) 
                pts$SEGCNT[pts$SEGCODE==unique(a)]<-length(records)   ## Set no. of pts/segments for all pts that occur in segcode== unique(a) 

              }else {   					       ## For this PLOTKEY prefix, there are >1 pts and all segment numbers of pts are the same 
                pts$RELWEIGHT[pts$SEGCODE==unique(a)]<-1.0/length(a)   ## Set relative weight for all pts that occur in segcode== unique(a)              
                pts$SEGCNT[pts$SEGCODE==unique(a)]<-length(records)    ## Set no. of pts/segment for all pts that occur in segcode== unique(a) 

              }       
           } ## if length(records) else 
        }  ## for (i in 1:length(cnt)




        ## Determine overlap with SDD strata.  The key attribute is called DSGN_STRTM_NM; at least create attribute for compatibility with SDDs
        pts$DSGN_STRTM_NM<-NULL
       
        t<-names(importSDD$sf[index])            ##   Pick up the corresponding SDD strata (make sure there is one) 
	if (!is.null(importSDD$strata[[t]])) {
            strataSDD<-importSDD$strata[[t]]
            strataSDD<-spTransform(strataSDD,projection) 
            temp<-over(pts,strataSDD)	## stratum code
            pts$DSGN_STRTM_NM<-temp$DMNNT_STRTM	## set the stratum in pts. NOTE - you can have NA because the LMF pt doesn't
                                                ## overlap BLM stratum - georegistration slop??
        }
        ##  importLMF$lmfpts[[s]]<-pts	Not sure we want to do this????

	############################################################################## Process SDD points
	##  Determine overlap between SDD pts and LMF Type I & II strata
	ptsSDD<-importSDD$pts[[t]]

        if(!is.null(ptsSDD)) {		## only if we have SDD pts

		## Select pts for the specified years; assumes yr of earliest AIM data is >= 2011
		for(i in 1:30) {
	     		ptsSDD$theyr[grepl(ptsSDD$PANEL,pattern=(2010+i))==TRUE]<-2010+i
		}    

        	## Must be an easier way to do the following, but couldn't figure it out!!!!
        	ptsSDD$a<-1  				    ## default to 1
        	ptsSDD$a[ptsSDD$theyr %in% yearlist] <-2    ## set to 2 if theyr(s) is in yearlist (user-specified year of interest)
        	ptsSDD<-ptsSDD[ptsSDD$a==2,]  		    ## render pts to those years specified by the user
         
                if(nrow(ptsSDD)==0) {
		     ptsSDD<-NULL			    ## no pts satisfied the specified time period
                }else {
        		ptsSDD$theyr<-NULL  			    ## clean up
        		ptsSDD$a<-NULL  			    ## clean up


			## overlay with LMF stratum & record 
        		ptsSDD<-spTransform(ptsSDD,projection) 
			temp<-over(ptsSDD,strata)
			ptsSDD$LMFSTRATA<-temp$STRATUM
	
			## Determine overlap between SDD pts and LMF segments
			temp<-over(ptsSDD,seg)
			ptsSDD$SEGCODE<-temp$SEGCODE
	 
			## Determine if LMF and AIM points co-occur in segments
			a<-(!is.na(ptsSDD$SEGCODE))
			records<- grep(a,pattern="TRUE")	##  records contains the row number(s) of AIM points that overlap an LMF segment
                        if(length(records)>0) {
				for(i in 1:length(records)) {

       	    				## only deal with target sample AIM points
            				if(ptsSDD$FINAL_DESIG[records[i]] %in% target.values) {
        					segcnt<-unique(pts$SEGCNT[pts$SEGCODE==ptsSDD$SEGCODE[records[i]]])    ## IF co-occur with LMF pts, then adjust segcnt, then rel. weights of LMF and AIM points
						if(length(segcnt)>0) {
             						segcnt<- (segcnt+1)						## No. pts in a segment		
	     						pts$SEGCNT[pts$SEGCODE==ptsSDD$SEGCODE[records[i]]] <-segcnt	## LMF point, set new segcnt
             						relweight<-1.0/segcnt
	     						pts$RELWEIGHT[pts$SEGCODE==ptsSDD$SEGCODE[records[i]]] <-relweight ## LMF point, set new relweight
             						ptsSDD$RELWEIGHT[records[i]]<-relweight				## AIM point
						}
					}
	     			}
	 		}
	 		## importSDD$pts[[t]]<-ptsSDD	Not sure we want to do this????
		} ## if nrow(ptsSDD)
	} ## !is.null


	 ################################################################################## Make the format of LMF & SDD pt files the same;
	 ## Retain key attributes used in subsequent processing, then merge files.  The plot_key field for LMF pts is analagous to the 
	 ## AIM primarykey (terra_terradata_id), in that it crosswalks with the monitoring observations stored in the LMF master database.
           


         ## Create/set attributes in LMF pts file 
 	 pts$PLOT_NM<-"LMF"
 	 pts$TERRA_SAMPLE_FRAME_ID<-"LMF"
 	 pts$TERRA_TERRADAT_ID<- "LMF"
 	 pts$ACTL_STRTM_NM<-pts$DSGN_STRTM_NM
 	 pts$DT_VST<-pts$DTVISIT
 	 pts$PLOT_KEY<-pts$PLOTKEY   



     	 ## Create similar formats between LMF and the SDD SPDFs.  ONly KEY attributes used in subsequent processing are retained,
         ## including the 5 new attributes related to using/processing LMF pts; aimlmf, lmfstrata, relweight, segcode, fieldoffice.
##  NOTE:  may have to explicitly renumber OBJECTIDs to avoid duplicates??
     	 lmf<- pts[, c("OBJECTID", "PLOT_KEY","PLOT_NM", "TERRA_SAMPLE_FRAME_ID", "TERRA_TERRADAT_ID",  "DSGN_STRTM_NM", "ACTL_STRTM_NM", "PANEL", "FINAL_DESIG", "DT_VST",
           "AIMLMF", "LMFSTRATA", "RELWEIGHT", "SEGCODE", "FIELDOFFICE")]
 
         if(!is.null(ptsSDD)) {
     	 	sdd<- ptsSDD[, c("OBJECTID", "PLOT_KEY","PLOT_NM", "TERRA_SAMPLE_FRAME_ID", "TERRA_TERRADAT_ID",  "DSGN_STRTM_NM", "ACTL_STRTM_NM", "PANEL", "FINAL_DESIG", "DT_VST",
           "AIMLMF", "LMFSTRATA", "RELWEIGHT", "SEGCODE", "FIELDOFFICE")]

     	 	## Bind files and store in a local copy of the function argument importSDD
	 	final<-spRbind(sdd,lmf)
	  } else {
	 	final<-lmf
	  }
      	  workinglist$pts[[t]]<-final

   } ############################################################################ endof for s in names(importlmf$lmfpts)

   return(workinglist)

} ## end of function  

   