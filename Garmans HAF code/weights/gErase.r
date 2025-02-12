##4/13/2017
##  Currently, gErase() only works if there is 1 and only 1 feature class in erasethis

gErase <- function(frame,erasethis) {
   delta  <- gDifference(frame,erasethis)
   keep<- row.names(delta)
   erase_data<-as.data.frame(frame@data[keep, ])
   return(SpatialPolygonsDataFrame(delta,erase_data))
}


##### From trycatch in weighter.R


          #  current.drop <- get_RGEOS_dropSlivers()
          #  current.warn <- get_RGEOS_warnSlivers()
          #  current.tol <- get_RGEOS_polyThreshold()


          #           sliverdrop = T
           #          sliverwarn = T
            #         sliverthreshold = 0.01



           # frame.spdf.temp <- tryCatch(
            #  expr = {
             #   set_RGEOS_dropSlivers(sliverdrop)
              #  set_RGEOS_warnSlivers(sliverwarn)
               # set_RGEOS_polyThreshold(sliverthreshold)
                # print(paste0("Attempting using set_RGEOS_dropslivers(", sliverdrop, ") and set_RGEOS_warnslivers(", sliverwarn, ") and set_REGOS_polyThreshold(", sliverthreshold, ")"))
                #gDifference(spgeom1 = frame.spdf.temp,
                 #           spgeom2 = frame.spdf,
                  #          drop_lower_td = T) %>% SpatialPolygonsDataFrame(data = frame.spdf.temp@data)
              # },
