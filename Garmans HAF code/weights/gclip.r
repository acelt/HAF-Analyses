##4/13/2017
##  Currently, gClip() only works if there is 1 and only 1 feature class in clip

gClip <- function(frame,clip) {
   clipped <- gIntersection(frame,clip,byid=T,drop_lower_td=TRUE)
   keep<-gsub(" 1","",row.names(clipped))
   clipped<- spChFIDs(clipped,keep)
   clipped_data<-as.data.frame(frame@data[keep, ])
   return(SpatialPolygonsDataFrame(clipped,clipped_data))
}


## The problem........
## The number of row.names(clipped) must be equal to or a subset of row.names(frame).  If clip has
## more than 1 feature, row.names(clipped) ends up with replicates of entries in row.names(frame).
##  E.g., if frame has features 1 thru 9 and clip has 2 features, then clipped can have row.names == 1 1, 1 2, 2 1, 2 2, 3 1, 4 1, 5 1, 5 2, etc.
##  THe first number is frame feature, the second is clip feature.  In essence, 1 1, 1 2 means that frame feature 1 shows up in clip feature 1 & 2.

## The gsub () command gets rid of the second number per pair, but you still end up with multiple 1's, multiple 2's....E.g., 1, 1, 2, 2, 3, 4, 5, 5...
## THe clipped_data statement (above) ends up numbering the multiples as #.1 (e.g., 1, 1.1, 2, 2.1....).
## The SpatialPolygonsDataFrame operation returns an error due to unequal lengths (e.g., there are no 1.1 IDs in frame)
## ??????? Currently can't figure out how to deal with duplicate row names; hence, clip can only have 1 feature..........  



# The following is for reference............

## row.names(clipped)<-gsub(" 2","",row.names(clipped))
## keep<-row.names(clipped)
## clipped<- spChFIDs(clipped,keep)
## clipped_data<-as.data.frame(spdf1@data[keep, ])
## clipped <-SpatialPolygonsDataFrame(clipped,clipped_data)
##    return( SpatialPolygonsDataFrame(clipped,frame@data[row.names(clipped), ]))

##    row.names(clipped) <-as.character(gsub(" 0","",row.names(clipped)))
##    not the way to this, but it can work in special cases ->    row.names(clipped)<-row.names(frame)