## UpdatePtLocations() - Updates the coords of sdd PTS with TerrADat info IFF the 2 share the same primarykey, or plotkey, or plotnam.

## UpdatePtLocations(terra.spdf,ptsfile)

UpdatePtLocations<-function(terra,pts)
{
  
   if(is.null(pts)) return(pts)		## Nothing to do since pts is NULL
   if(nrow(pts)<=0) return(pts)		## Nothing to do since pts is empty

    pts@data <- cbind(pts@data, pts@coords)
    terra@data <- cbind(terra@data, terra@coords)


    for(i in 1:nrow(pts)) {		
        id1<-pts$TERRA_TERRADAT_ID[i]
        id2<-pts$PLOT_KEY[i]
        id3<-pts$PLOT_NM[i]

        a<-NULL
	b<-NULL
	c<-NULL

        if(!is.na(id1)) a<-grep(terra$PRIMARYKEY,pattern=id1)
	if(!is.na(id2)) b<-grep(terra$PLOTKEY,pattern=id2)
        if(!is.na(id3)) c<-grep(terra$PLOTID,pattern=id3)
        
	z<-0			## Update only if 1 of the following uniquely occurs in TerrADat
        if(length(a)==1) {
		z<-a
	}else if(length(b)==1) {
		z<-b
	}else if(length(c)==1) {
		z<-c
	}

	if(z>0) {		## update coords if a match in TerrADat
		pts@coords[i,1]<-terra@coords[z,1]
 		pts@coords[i,2]<-terra@coords[z,2]
	}
	
    }	

    pts@data$coords.x1<-NULL
    pts@data$coords.x2<-NULL

    return(pts)
}

