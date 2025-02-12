## 4/30/2017
## add.strata-> transfers strata file from first entriy in workinglist to specified workinglist entry



add.strata <- function(workinglist,   ## named list 
                       the_source,    ## the entry in workinglist (same order as sdd.src) with the strata to assign to other SDDs
                       target_sequence  ## numeric vector indicating the entry in workinglist to transfer the strata file of the first entry
) {




  ## Get strata once if it exists.  

    strata<-NULL
    strata<-names(workinglist$strata[the_source])
    strata<-workinglist$strata[[strata]]
    if(is.null(strata)){
      print("There is no strata file to transfer")
    }

    for(i in 1:length(target_sequence)) {
        id=target_sequence[i]
        workinglist$strata[[id]]<-strata
    }



 return(workinglist)
}