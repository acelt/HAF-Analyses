###### Setup ######
#### Attach package(s)

devtools::install_github("nstauffer/aim.analysis")
library(aim.analysis)
library(spdplyr)
#### Defaults
name <- "Hamlin Valley Project Area Spring SUA"
ru <- "Spring"
projection <- sp::CRS("+proj=aea")


#### Filepaths and filenames
### Filepaths
# The filepath to the folder where we have all the spatial data
### All geoprocessing done in Arc

# Spatial join all points with LMF segments, stratum, and calculate area in hectares for the stratum 
# LMF PLOTKEy 016491202819B1 fell 0.000059 decimal degrees from the edge of sagebrush steppe stratum (post-stratum id 6)
# Otherwise did not fall in strata. Was manually assigned in field calculator due to likely GPS/precision issue

path_spatial <- "C:\\Users\\alaurencetraynor\\Documents\\2019\\Analysis\\Hamlin Valley\\inputs\\spatial"

# The filepath to the folder where we have the Excel stuff
path_tabular <- "C:\\Users\\alaurencetraynor\\Documents\\2019\\Analysis\\Hamlin Valley\\inputs\\tabular"


### Filenames
reporting_unit_filenames <- "Hamlin Valley.gdb/breeding.shp"

design_filenames <- c("UT_CedarCityFO_2017_2021SDD.gdb")

terradat_filename <- "Hamlin Valley.gdb/hamlinvalley_terradat_points_strtm_seg_spring"

additional_points_filename <- "Hamlin Valley.gdb/hamlinvalley_lmf_points_seg_strtm_spring"

benchmarks_filename <-  "Hamlin Valley Benchmarks Spring SUA.xlsm"

## Variable names

terradat_idvar <- "PrimaryKey"
LMF_idvar <- "PLOTKEY" # For some reason the PrimaryKeys in LMF are not reading as unique

# Date of analysis
date <- Sys.Date()

# Confidence level in percent
confidence <- 80

output_path <- "C:\\Users\\alaurencetraynor\\Documents\\2019\\Analysis\\Hamlin Valley\\outputs\\2019 plots"


# Core filename for all outputs
output_filename <- "HamlinValley_spring_benchmarks2019"

# The geodatabase and layer names for the spatial data sources
# are embedded in the code below

# The fates in sample designs which correspond to sampled/observed
observed_fates <- c("TS", "Target Sampled", "Sampled")

# The fates in sample designs which correspond to unneeded
# (e.g. unused oversample or points designated for future sampling)
invalid_fates <- c("NN", NA, "Non Needed", "", "Not Sampled")

# The LMF segments polygons
# Dont need to import LMF segments here as we have already join to points in Arc
segments <- NULL

# The variable containing the LMF segment identity/membership
# This is found in the points and the segment polygons
segment_var <- "segcode"

# The variable that contains the areas in HECTARES in wgtcats
# I calculated area in ArcGIS
wgtcat_area_var <- "Area_hectares"

# The unique ID for poststratification polygons
wgtcats_var <- "poststratum_id"

aim_fatevar <- "FINAL_DESIG"

###### Read in ######
#### Reporting unit(s)
reporting_units_polygons <- read_shapefile(filename = reporting_unit_filenames,
                                           filepath = path_spatial , 
                                           projection = projection)

names(reporting_units_polygons@data)[names(reporting_units_polygons@data) == "season"] <- "Reporting.Unit"


#### Sample designs

designs <- aim.analysis::read.dd(src = path_spatial ,
                                 dd.src = design_filenames,
                                 projection = projection)

#### TerrADat

terradat_points <- read_shapefile(filename = terradat_filename,
                                  filepath = path_spatial , 
                                  projection = projection)

terradat_points@data[["aim"]] <- TRUE 
terradat_points@data[["lmf"]] <- FALSE
#### The additional_points
additional_points <- read_shapefile(filename = additional_points_filename , 
                                    filepath = path_spatial , 
                                    projection = projection)
## No LMF points 
additional_points@data[["aim"]] <- FALSE
additional_points@data[["lmf"]] <- TRUE

#### Benchmark Tool
benchmarks <- read_benchmarks(filename = benchmarks_filename , 
                              filepath = path_tabular)
head(benchmarks)
#### Plot Status Workbook?


###### Sort out points ######

terradat_points@data[["unique_id"]] <- terradat_points@data[[terradat_idvar]]

additional_points@data[["unique_id"]] <- additional_points@data[[LMF_idvar]]

terradat_points@data[setdiff(names(additional_points@data), names(terradat_points@data))] <- NA
additional_points@data[setdiff(names(terradat_points@data), names(additional_points@data))] <- NA

# Make sure names are in the same order
if (!identical(names(terradat_points), names(additional_points))) {
  additional_points@data <- additional_points@data[, names(terradat_points)]
}

# Make sure this equals number of plots
length(unique(terradat_points$unique_id))
length(unique(additional_points$unique_id))

names(additional_points@data) <- names(terradat_points@data)

# But not any points that are already in there

# additional_points <- additional_points[!(additional_points@data[["SiteID"]] %in% terradat_points@data[["SiteID"]]), ]

terradat_points <- rbind(additional_points,
                         terradat_points) 

#Make sure all points retained 
length(unique(terradat_points$unique_id))


terradat_points <- attribute.shapefile(spdf1 = terradat_points,
                                       spdf2 = reporting_units_polygons,
                                       attributefield = "Reporting.Unit")

names(reporting_units_polygons)

terradat_points@data[["FinalDesignation"]] <- "Target Sampled"

terradat_points$unique_id
### Add benchmark values
# First is benchmark groups
benchmark_group_lut <- read_benchmarkgroups(path = path_tabular,
                                            filename = benchmarks_filename,
                                            id_var = "PrimaryKey")

# Then apply the benchmarks by benchmark group
# terradat_points@data[["OtherShrubHgt_Avg"]] <- as.numeric(terradat_points@data[["OtherShrubHgt_Avg"]])
# terradat_points@data[["SagebrushHgt_Avg"]] <- as.numeric(terradat_points@data[["SagebrushHgt_Avg"]])

#Removing PrimaryKey and then replacing it with unique_id because was non-unique for LMF

terradat_points@data[["Benchmark.Group"]] <- terradat_points@data[["ProjectArea"]]
benchmark_group_lut[["unique_id"]] <- benchmark_group_lut[["PrimaryKey"]]

# Add benchmark groups
tdat_df <- merge(x = terradat_points@data,
                 y = benchmark_group_lut,
                 by.x = "PrimaryKey",
                 by.y = "PrimaryKey",
                 all.x = FALSE)

# Add "real" indicator names
# Read in default look up table from the package
indicator_lut <- read.csv(paste0(path_tabular,
                                 "/indicator_lut.csv"), 
                          stringsAsFactors = FALSE)

#Rename to match the conventions here
names(indicator_lut) <- c("indicator.var", "indicator.name")

benchmarks <- merge(x = benchmarks , 
                    y = indicator_lut , 
                    by.x = "Indicator" , 
                    by.y = "indicator.name")

#Which indicators appear in the data as variables and in look up table
indicator_vars_present <- unique(names(tdat_df)[names(tdat_df) %in% benchmarks[["indicator.var"]]])

#Name idvars

idvars <- c("PrimaryKey" , "Benchmark.Group")

# Make it tall 

#make aim & lmf column names an object plus segcode to keep in tall table

aim_lmf <- c("aim" , "lmf", "segcode")

data_tall <- tidyr::pivot_longer(data = tdat_df[, c(idvars, aim_lmf , indicator_vars_present)],
                                 cols = tidyselect::one_of(indicator_vars_present), 
                                 names_to = "indicator.var",
                                 values_to = "value")


# Add benchmark eval strings
data_tall <- merge(x = data_tall,
                   y = benchmarks,
                   by = c("Benchmark.Group", "indicator.var"),
                   all.x = FALSE)

# Names of the variables with eval strings
eval_vars <- c("evalstring1" , "evalstring2")

# Apply the benchmarks with the lapply() stolen from apply_benchmarks()

benchmark_vector <- sapply(X = 1:nrow(data_tall), 
                           data = data_tall,
                           eval_vars = eval_vars,
                           FUN = function(X, data, eval_vars) {
                             all(sapply(X = eval_vars,
                                        data = data[X, ],
                                        FUN = function(X, data) {
                                          evalstring <- gsub(data[[X]][1],
                                                             pattern = "(x){1}", 
                                                             replacement = data[["value"]])
                                          eval(parse(text = evalstring))
                                        }))
                           })

terradat_benchmarked <- data_tall[benchmark_vector, ]

### Now that we have the benchmarks populated, will read in strata/shapefile info

# Read in strata polygons

wgtcat_df <-sf::st_read(dsn = paste(path_spatial, "Hamlin Valley.gdb", sep = "/"),
                        layer = "AIM_LMF_strata_union_spring",
                        stringsAsFactors = FALSE)


sf::st_geometry(wgtcat_df) <- NULL
head(wgtcat_df)
wgtcats <- as.data.frame(wgtcat_df)[,c(paste(c(wgtcat_area_var,wgtcats_var)),"design_strata")]
colnames(wgtcats)[2] <- "wgtcat"

### Read in design points
### Attribute with stratum info in Arc before reading in 
hamlinvalley_design_points <- sf::st_read(dsn = paste(path_spatial, "Hamlin Valley.gdb", sep = "/") ,
                           layer = "Terra_Sample_Points_Clip_Seg_Spring",
                           stringsAsFactors = FALSE)


## Standardize projection
hamlinvalley_design_points <- sf::st_transform(hamlinvalley_design_points,
                                crs = projection)

## Make spatial 
hamlinvalley_design_points <- methods::as(hamlinvalley_design_points, "Spatial")
### There is only 1 design in this project

hamlinvalley_design_points@data[["unique_id"]] <-hamlinvalley_design_points@data[["PLOT_KEY"]]
hamlinvalley_design_points@data[["fate"]] <- hamlinvalley_design_points@data[[aim_fatevar]]
hamlinvalley_design_points@data[["wgtcat"]] <- hamlinvalley_design_points@data[[wgtcats_var]]
hamlinvalley_design_points@data[["segment"]] <- hamlinvalley_design_points@data[[segment_var]]

# Checking against the ratings confirms if all the rated plots are represented (important)
# and if all the sampled_plots are rated (not important, but informative)
if (!all(terradat_benchmarked[["PrimaryKey"]] %in% terradat_points$unique_id)) {
  warning("NOT ALL RATED AIM PLOTS ARE PRESENT IN sampled_points!")
}


# Now combine:
# All design_points which were not observed. This removes any points that overlap with sampled_points but also
# any that were observed but somehow not in TerrADat and deemed unsuitable for rating
# which therefore means they also should not be included in weighting
# All sampled points (already restricted to the ones which were rated)

terradat_points@data[["fate"]] <- terradat_points@data[["FinalDesignation"]]
terradat_points@data[["wgtcat"]] <- terradat_points@data[[wgtcats_var]]
terradat_points@data[["segment"]] <- terradat_points@data[[segment_var]]

#For some reason I had to reproject these so that I could rbind even though they were already projected to same CRS
proj4string(terradat_points) <- projection
proj4string(hamlinvalley_design_points) <- projection

# dont actually need design points here
aim_points <-  terradat_points[, c("unique_id", "fate", "wgtcat", "segment")]


# Take out invalid fates, e.g. things that don't get included in weighting
aim_points <- aim_points[!(aim_points$fate %in% invalid_fates), ]

# Add in coordinates (needed for the analysis step)
aim_points <-  aim.analysis::add_coords(aim_points,
                                        xynames = c("x", "y"))

aim_points$wgtcat <- as.character(aim_points$wgtcat) 

# separate out lmf point for code below

lmf_points <- aim_points
lmf_points <- subset(lmf_points, grepl("^20",lmf_points@data$unique_id))
aim_points <- subset(aim_points, !grepl("^20",aim_points@data$unique_id))

#### Run raw code instead of function to analyze

# These are being redefined here as though they were arguments to the function
aim_idvar <- "unique_id"
lmf_idvar <- "unique_id"
aim_fatevar <- "fate"
wgtcat_var <- "wgtcat"
segments <- NULL
segment_var <- "segment"
verbose <- FALSE

if (length(wgtcat_var) != 1 | class(wgtcat_var) != "character") {
  stop("wgtcat_var must be a single character string")
}
if (!is.null(wgtcat_area_var)) {
  if (length(wgtcat_area_var) != 1 | class(wgtcat_area_var) != "character") {
    stop("wgtcat_area_var must be a single character string")
  }
}
if (length(segment_var) != 1 | class(segment_var) != "character") {
  stop("segment_var must be a single character string")
}

if (!is.null(wgtcats)) {
  if (!(class(wgtcats) %in% c("SpatialPolygonsDataFrame", "data.frame"))) {
    stop("wgtcats must be a spatial polygons data frame or data frame")
  }
  if (nrow(wgtcats) < 1) {
    stop("wgtcats contains no observations/data")
  }
  if (!(wgtcat_var %in% names(wgtcats))) {
    stop(paste("The variable", wgtcat_var, "does not appear in wgtcats@data"))
  }
}

if (!is.null(segments)) {
  if (!(class(segments) %in% "SpatialPolygonsDataFrame")) {
    stop("segments must be a spatial polygons data frame")
  }
  if (nrow(segments) < 1) {
    stop("segments contains no observations/data")
  }
  if (!(segment_var %in% names(segments))) {
    stop(paste("The variable", segment_var, "does not appear in segments@data"))
  }
}

if (is.null(aim_fatevar)) {
  warning("No fate variable specified for AIM points. Assuming all were observed/sampled.")
  aim_fatevar <- "fate"
  aim_points[["fate"]] <- "observed"
  lmf_points[["fate"]] <- "observed"
  observed_fates <- "observed"
} else {
  if (length(aim_fatevar) > 1 | class(aim_fatevar) != "character") {
    stop("The aim fate variable must be a single character string")
  }
  if (!aim_fatevar %in% names(aim_points)) {
    stop(paste("The variable", aim_fatevar, "does not appear in aim_points@data"))
  } else {
    aim_points[["fate"]] <- aim_points[[aim_fatevar]]
  }
  if (is.null(observed_fates)) {
    warning("No observed fates provided. Assuming all AIM points were observed/sampled unless specified otherwise with invalid_fates")
    observed_fates <- unique(aim_points[["fate"]])
    observed_fates <- observed_fates[!(observed_fates %in% invalid_fates)]
  }
}

lmf_points[["fate"]] <- observed_fates[1]

if (!is.null(observed_fates)) {
  if (!any(aim_points[["fate"]] %in% observed_fates)) {
    warning("No AIM points have a fate specified as observed.")
  }
}


# Harmonize projections
if (is.null(projection)) {
  if (!is.null(wgtcats)) {
    projection <- wgtcats@proj4string
  } else if (!is.null(segments)) {
    projection <- segments@proj4string
  }
}

if (!is.null(projection)) {
  if (class(aim_points) %in% c("SpatialPointsDataFrame")) {
    if (!identical(projection, aim_points@proj4string)) {
      aim_points <- sp::spTransform(aim_points,
                                    CRSobj = projection)
    }
  }
   if (class(lmf_points) %in% c("SpatialPointsDataFrame")) {
     if (!identical(projection, lmf_points@proj4string)) {
       lmf_points <- sp::spTransform(lmf_points,
                                     CRSobj = projection)
     }
   }
  
   if (!is.null(wgtcats)) {
     if (class(lmf_points) %in% c("SpatialPolygonsDataFrame")) {
       if (!identical(projection, wgtcats)) {
         wgtcats <- sp::spTransform(wgtcats,
                                    CRSobj = projection)
       }
     }
  }
  if (!is.null(segments)) {
    if (!identical(projection, segments@proj4string)) {
      segments <- sp::spTransform(segments,
                                  CRSobj = projection)
    }
  }
 }

# Assign the weight categories
if (class(wgtcats) %in% "SpatialPolygonsDataFrame") {
  if (!(wgtcat_var %in% names(wgtcats))) {
    stop("The variable ", wgtcat_var, " does not appear in wgtcats")
  }
  aim_points[["wgtcat"]] <- sp::over(aim_points, wgtcats)[[wgtcat_var]]
  lmf_points[["wgtcat"]] <- sp::over(lmf_points, wgtcats)[[wgtcat_var]]
} else {
  if (!(wgtcat_var %in% names(aim_points))) {
    stop("The variable ", wgtcat_var, " does not appear in aim_points")
  }
  if (!(wgtcat_var %in% names(lmf_points))) {
     stop("The variable ", wgtcat_var, " does not appear in lmf_points")
   }
  aim_points[["wgtcat"]] <- aim_points[[wgtcat_var]]
  lmf_points[["wgtcat"]] <- lmf_points[[wgtcat_var]]
}


# Assign the LMF segment codes
if (is.null(segments)) {
  if (!(segment_var %in% names(aim_points))) {
    stop("The variable ", segment_var, " does not appear in aim_points")
  }
   if (!(segment_var %in% names(lmf_points))) {
     stop("The variable ", segment_var, " does not appear in lmf_points")
   }
  aim_points[["segment"]] <- aim_points[[segment_var]]
  lmf_points[["segment"]] <- lmf_points[[segment_var]]
} else {
  if (!(segment_var %in% names(segments))) {
    stop("The variable ", segment_var, " does not appear in segments")
  }
  aim_points[["segment"]] <- sp::over(aim_points, segments)[[segment_var]]
  lmf_points[["segment"]] <- sp::over(lmf_points, segments)[[segment_var]]
}


# Just harmonize the idvar names for now
aim_points[["unique_id"]] <- aim_points[[aim_idvar]]
lmf_points[["unique_id"]] <- lmf_points[[lmf_idvar]]

# Add reporting units
aim_points[["reporting_unit"]] <- ru
lmf_points[["reporting_unit"]] <- ru

# If somehow LMF points aren't in a segment, that's a major problem
 if (any(is.na(lmf_points[["segment"]]))) {
  stop(paste("The following LMF points did not spatially intersect any segment polygons:",
              paste(lmf_points[is.na(lmf_points[["segment"]]), "unique_id"], collapse = ", ")))
 }



# TODO: Stick a check in here that the segment ID from the polygons
# matches the one derived from the LMF plot ID
# Probably just warn if not?


# Get data frames
if (class(aim_points) %in% "SpatialPointsDataFrame") {
  aim_df <- aim_points@data
} else {
  aim_df <- aim_points
}
 if (class(lmf_points) %in% "SpatialPointsDataFrame") {
   lmf_df <- lmf_points@data
 } else {
   lmf_df <- lmf_points
}

# NOTE THAT THIS FILTERS OUT ANYTHING FLAGGED AS NOT NEEDED IN THE FATE
# So that'd be unused oversamples or points from the FUTURE that no one would've sampled anyway
aim_df <- aim_df[!(aim_df[["fate"]] %in% invalid_fates), c("unique_id", "fate", "wgtcat", "segment")]
aim_df[["aim"]] <- TRUE
aim_df[["lmf"]] <- FALSE

# We only have target sampled LMF points available to us, so we don't need to filter them
 lmf_df <- lmf_df[, c("unique_id", "fate", "wgtcat", "segment")]
 lmf_df[["aim"]] <- FALSE
 lmf_df[["lmf"]] <- TRUE

# Combine them
combined_df <- unique(rbind(aim_df, lmf_df))

# There shouldn't be any that don't belong to a wgtcat anymore
combined_df <- combined_df[!is.na(combined_df[["wgtcat"]]), ]

# Add an observed variable for easy reference later
combined_df[["observed"]] <- combined_df[["fate"]] %in% observed_fates

# To make the lookup table, drop any points that fell outside LMF segments
combined_segmentsonly_df <- combined_df[!is.na(combined_df[["segment"]]), ]

# Create a segment relative weight lookup table
segment_relwgt_lut <- do.call(rbind,
                              lapply(X = split(combined_segmentsonly_df, combined_segmentsonly_df[["segment"]]),
                                     FUN = function(X){
                                       # These are the count of AIM points with any valid fate
                                       aim_count <- sum(X[["aim"]])
                                       
                                       # We also need the count of LMF points with any valid fate, but that's complicated
                                       # We only have the sampled LMF points so we can count those
                                       lmf_sampled_count <- sum(X[["lmf"]])
                                       # To get the number of evaluated but not sampled points:
                                       # The LMF plot keys end in a digit that represents the intended sampling order within a segment
                                       # 1 and 2 are considered base points and were intended to be sampled
                                       # If a sampled LMF plot's plot key ends in 3, that means that one or both of the base points
                                       # were evaluated and rejected rather than sampled, which brings the evaluated LMF plot count
                                       # to three for the segment.
                                       # This just asks if the third point was used
                                       lmf_oversample_used <- any(grepl(X[["unique_id"]][X[["lmf"]]],
                                                                        pattern = "\\D3$"))
                                       
                                       # Likewise, if only one LMF plot was sampled in a segment, that means the other two were
                                       # evalurated and rejected rather than sampled, also bringing the total to three.
                                       # So if there was only one sampled or if the oversample was used, there were three evaluated
                                       if (sum(X[["lmf"]]) == 1 | lmf_oversample_used) {
                                         lmf_count <- 3
                                       } else {
                                         # This will fire only if there sampled count was 2, but better to be safe here
                                         lmf_count <- lmf_sampled_count
                                       }
                                       
                                       
                                       # The relative weight for points falling within a segment is calculated as
                                       # 1 / (number of points)
                                       relative_weight <- 1 / sum(aim_count, lmf_count)
                                       
                                       output <- data.frame("segment" = X[["segment"]][1],
                                                            "relwgt" = relative_weight,
                                                            stringsAsFactors = FALSE)
                                       return(output)
                                     }))

# Add the relative weights to the combined AIM and LMF points
 combined_df <- merge(x = combined_df,
                      y = segment_relwgt_lut,
                      by = "segment",
                      all.x = TRUE)


# Anywhere there's an NA associated with an AIM point, that's just one that fell outside the segments
combined_df[is.na(combined_df[["relwgt"]]) & combined_df[["aim"]], "relwgt"] <- 1


if (is.null(wgtcat_area_var)) {
  if (class(wgtcats) %in% "SpatialPolygonsDataFrame") {
    wgtcat_df <- aim.analysis::add.area(wgtcats)@data
  } else {
    stop("No name for a variable in wgtcats containing the area in hectares was provided and wgtcats is not a spatial polygons data frame so area can't be calculated")
  }
} else {
  if (!(wgtcat_area_var %in% names(wgtcats))) {
    if (class(wgtcats) %in% "SpatialPolygonsDataFrame") {
      wgtcat_df <- aim.analysis::add.area(wgtcats)@data
    } else {
      stop("The variable ", wgtcat_area_var, " does not appear in wgtcats")
    }
  } else {
    warning("Trusting that the variable ", wgtcat_area_var, " in wgtcats contains the areas in hectares")
    if (class(wgtcats) %in% "SpatialPolygonsDataFrame") {
      wgtcat_df <- wgtcats@data
      wgtcat_df[["AREA.HA"]] <- wgtcat_df[[wgtcat_area_var]]
    } else {
      wgtcat_df <- wgtcats
      wgtcat_df[["AREA.HA"]] <- wgtcat_df[[wgtcat_area_var]]
    }
  }
}


wgtcat_df[["wgtcat"]] <- wgtcat_df[[wgtcat_var]]

# Making sure that these aren't spread out over multiple observations
if (verbose) {
  message("Summarizing wgtcat_df by wgtcat to calculate areas in case the wgtcats are split into multiple observations")
}
wgtcat_df <- dplyr::summarize(dplyr::group_by(wgtcat_df,
                                              wgtcat),
                              "hectares" = sum(AREA.HA))

wgtcat_areas <- setNames(wgtcat_df[["hectares"]], wgtcat_df[["wgtcat"]])


# had a issue here reading wgtcats as integers
wgtcat_df$wgtcat <-  as.character(wgtcat_df$wgtcat)
  
weight_info <- lapply(X = wgtcat_df[["wgtcat"]],
                      wgtcat_areas = wgtcat_areas,
                      points = combined_df,
                      wgtcat_var = wgtcat_var,
                      FUN = function(X, wgtcat_areas, points, wgtcat_var){
                        # Area of this polygon
                        area <- wgtcat_areas[X]
                        
                        # All of the points (AIM and LMF) falling in this polygon
                        points <- points[points[["wgtcat"]] == X, ]
                        
                        # If there are in fact points, do some weight calculations!
                        if (nrow(points) > 0 & any(points[["observed"]])) {
                          # The number of observed AIM points with a relative weight of 1
                          # So, not sharing an LMF segment with any LMF points
                          aim_standalone <- sum(combined_df[["aim"]] & combined_df[["relwgt"]] == 1)
                          # The number of unique segments selected for LMF points
                          lmf_segment_count <- length(unique(points[["segment"]]))
                          
                          # The approximate number of 160 acre segments in this polygon
                          # Obviously, not all of the segments were selected in the first stage
                          # of the LMF design, but this is how many were available in this polygon
                          approximate_segment_count <- area / (160 / 2.47)
                          
                          # This is the sum of the relative weights of the OBSERVED points
                          # This does not include the inaccessible, rejected, or unknown points
                          # It does include both AIM and LMF, however
                          sum_observed_relwgts <- sum(points[points[["observed"]], "relwgt"])
                          # This is the sum of the relative weights of all the AIM points, regardless of fate
                          sum_relwgts <- sum(points[["relwgt"]])
                          
                          # The units are segments per point
                          # The segments are 120 acre quads (quarter sections?) that the first stage of LMF picked from
                          # Steve Garman called this "ConWgt" which I've expanded to conditional_weight
                          # but that's just a guess at what "con" was short for
                          conditional_weight <- approximate_segment_count / (aim_standalone + lmf_segment_count)
                          conditional_area <- conditional_weight * sum_observed_relwgts
                          
                          # What's the observed proportion of the area?
                          observed_proportion <- sum_observed_relwgts / sum_relwgts
                          # And how many acres is that then?
                          # We can derive the "unknown" or "unsampled" area as the difference
                          # between the polygon area and the observed area
                          observed_area <- area * observed_proportion
                          
                          # Then this adjustment value is calculated
                          weight_adjustment <- observed_area / conditional_area
                          
                          
                          
                          # Put everything about the wgtcat in general in one output data frame
                          output_wgtcat <- data.frame(wgtcat = X,
                                                      area = area,
                                                      area_units = "hectares",
                                                      approximate_segment_count = approximate_segment_count,
                                                      sum_observed_relwgts = sum_observed_relwgts,
                                                      sum_relwgts = sum_relwgts,
                                                      observed_proportion = observed_proportion,
                                                      observed_area = observed_area,
                                                      unobserved_area = area - observed_area,
                                                      conditional_weight = conditional_weight,
                                                      conditional_area = conditional_area,
                                                      weight_adjustment = weight_adjustment,
                                                      point_count = sum(points[["observed"]]),
                                                      observed_point_count = nrow(points),
                                                      stringsAsFactors = FALSE)
                          
                          # But much more importantly add the calculated weights to the points
                          output_points <- points
                          
                          # This handles situations where there were points, but none of them were observed
                          # In that case, weight_adjustment will be 0 / 0 = NaN
                          # That's fine because we can identify that this polygon is still in the inference area
                          # but that its entire area is "unknown" because no data were in it
                          if (is.nan(weight_adjustment)) {
                            output_points[["wgt"]] <- NA
                          } else {
                            # The point weight is calculated here.
                            # This is Garman's formula and I don't have the documentation justifying it on hand
                            output_points[["wgt"]] <- weight_adjustment * conditional_weight * points[["relwgt"]]
                            message("Checking weight sum for ", X)
                            # I'm rounding here because at unrealistically high levels of precision it gets weird and can give false positives
                            if (round(sum(output_points[["wgt"]]), digits = 3) != round(area, digits = 3)) {
                              warning("The sum of the point weights (", sum(output_points[["wgt"]]), ") does not equal the polygon area (", area, ") for ", X)
                            }
                          }
                          
                        } else {
                          # Basically just empty data frames
                          output_wgtcat <- data.frame(wgtcat = X,
                                                      area = area,
                                                      area_units = "hectares",
                                                      approximate_segment_count = area / (160 / 2.47),
                                                      sum_observed_relwgts = NA,
                                                      sum_relwgts = NA,
                                                      observed_proportion = 0,
                                                      observed_area = 0,
                                                      unobserved_area = area,
                                                      conditional_weight = NA,
                                                      conditional_area = NA,
                                                      weight_adjustment = NA,
                                                      point_count = 0,
                                                      observed_point_count = 0,
                                                      stringsAsFactors = FALSE)
                          output_points <- NULL
                        }
                        
                        return(list(points = output_points,
                                    wgtcat = output_wgtcat))
                      })

point_weights <- do.call(rbind,
                         lapply(X = weight_info,
                                FUN = function(X){
                                  verbose = TRUE
                                  X[["points"]]
                                }))
wgtcat_summary <- do.call(rbind,
                          lapply(X = weight_info,
                                 FUN = function(X){
                                   X[["wgtcat"]]
                                 }))

#### Run analysis ####

all_points <- rbind(aim_points[, c("unique_id", "x", "y")],
                     lmf_points[, c("unique_id", "x", "y")])

# Add the ratings to the point info and convert to a data frame
all_points <- merge(x = all_points@data,
                    y = terradat_benchmarked,
                    by.x = "unique_id",
                    by.y = "PrimaryKey",
                    all.x = FALSE)

# Add in the indicator, which for these will always be just be the indicator since it's for benchmarks and not habitat
all_points[["indicator"]] <- all_points$Indicator
# all_points$Suitability.S3 <- as.character(all_points$Suitability.S3) # analyze function giving "lossy cast error" - need to convert factors to characters

# Add in the season (this is defined up above) e.g. "S-4 Upland Summer Late Brood-rearing Habitat" if for grouse
# If for benchmark add reporting unit (redundant but making sure I don't break the code)
all_points[["reporting_unit"]] <- ru
point_weights[["reporting_unit"]] <- ru

# And analyze
head(all_points)

#point_weights <- as.data.frame(point_weights)

analysis <- aim.analysis::analyze(benchmarked_points = all_points,
                                  point_weights = point_weights,
                                  id_var = "unique_id",
                                  indicator_var = "indicator",
                                  value_var = "Condition.Category",
                                  x_var = "x",
                                  y_var = "y",
                                  reporting_var = "reporting_unit",
                                  weight_var = "wgt",
                                  conf = confidence)


adjusted_counts <- (sum(analysis[["NResp"]][analysis[["Category"]] != "Total"]) * analysis[["Estimate.P"]][analysis[["Category"]] != "Total"] / 100)

goodman_cis <- function(counts,
                        alpha = 0.2,
                        chisq = "best",
                        verbose = FALSE){
  if (!is.numeric(counts) | length(counts) < 2) {
    stop("counts must be a numeric vector with at least two values")
  }
  
  if (!(chisq %in% c("A", "B", "best"))) {
    stop("The only valid values for chisq are 'A', 'B', and 'best'.")
  }
  
  # Goodman describes the upper and lower bounds with the equations:
  # Lower estimated pi_i = {A + 2n_i - {A[A + 4n_i(N - n_i) / N]}^0.5} / [2(N + A)]
  # Upper estimated pi_i = {A + 2n_i + {A[A + 4n_i(N - n_i) / N]}^0.5} / [2(N + A)]
  
  # n_i is the "observed cell frequencies in population of size N" (aka count of observations) from a category
  # so that's the incoming argument counts. We'll rename for consistency with the original math (and statistics as a discipline)
  n <- counts
  
  # N is the population those counts make up, or, in lay terms, the total observation count
  N <- sum(counts)
  
  # k is the number of categories the population has been sorted into
  # Useful for degrees of freedom
  k <- length(counts)
  
  # "A is the upper alpha * 100-th percentage point of the chi-square distribution with k - 1 degrees of freedom"
  # and B is an alternative which uses alpha / k and one degree of freedom
  # Goodman states that B should be less than A for situations
  # where k > 2 AND alpha is 0.1, 0.05, or 0.01.
  chisq_quantiles <- c("A" = stats::qchisq(p = 1 - alpha,
                                           df = k - 1),
                       "B" = stats::qchisq(p = 1 - (alpha / k),
                                           df = 1))
  
  
  # According to Goodman, A and B are both valid options for the chi-square quantile
  # So the user can specify which they want or just ask for the one that minimizes the confidence intervals
  chisq_quantile <- switch(chisq,
                           "A" = {chisq_quantiles["A"]},
                           "B" = {chisq_quantiles["B"]},
                           "best" = {
                             pick <- which.min(chisq_quantiles)
                             if (verbose){
                               switch(names(chisq_quantiles)[pick],
                                      "A" = message("The chi-square quantile calculation that will provide the tighter confidence intervals is A, the upper alpha X 100-th percentage point of the chi-square distribution with k - 1 degrees of freedom"),
                                      "B" = message("The chi-square quantile calculation that will provide the tighter confidence intervals is B, the upper alpha / k X 100-th percentage point of the chi-square distribution with 1 degree of freedom"))
                             }
                             chisq_quantiles[pick]
                           })
  
  # Calculate the bounds!
  # Note that these ARE symmetrical, just not around the proportions.
  # They're symmetrical around A + 2 * n / (2 * (N + A))
  # The variable A has been replaced with chisq_quantile because it may be A or B, depending
  # Since the only multi-value vector involved here is n, these will be vectors of length k,
  # having one value for each of the values in n and in the same order as n
  lower_bounds <- (chisq_quantile + 2 * n - sqrt(chisq_quantile * (chisq_quantile + 4 * n * (N - n) / N))) / (2 * (N + chisq_quantile))
  upper_bounds <- (chisq_quantile + 2 * n + sqrt(chisq_quantile * (chisq_quantile + 4 * n * (N - n) / N))) / (2 * (N + chisq_quantile))
  
  # A proportion can never be greater than 1 or less than 0 (duh)
  # So we'll add bounds any CIs in case that happens
  # That's definitely a thing that can happen if the magnitude of sqrt(A * (A + 4 * n * (N - n) / N))
  # is large enough
  lower_bounds[lower_bounds < 0] <- 0
  upper_bounds[upper_bounds > 1] <- 1
  
  # Build the output
  output <- data.frame(count = n,
                       proportion = n / N,
                       lower_bound = lower_bounds,
                       upper_bound = upper_bounds,
                       stringsAsFactors = FALSE,
                       row.names = NULL)
  
  # What are the categories called? If anything, that is
  k_names <- names(n)
  
  if (!is.null(k_names)) {
    output[["category"]] <- k_names
    output <- output[, c("category", "count", "proportion", "lower_bound", "upper_bound")]
  }
  
  return(output)
}

split_estimates <- split(analysis,  analysis[, c("Indicator")])
                           
                    split_estimates_with_ci <- lapply(X = split_estimates,
                    categories = c("Suitable" , "Unsuitable", "Total"),
                                                              FUN = function(X, categories){
                                                                estimates <- X
                                                                # Junk!!!!! Keep only the good variables
                                                                string_variables <- c("Subpopulation", "Indicator", "Category")
                                                                numeric_variables <- c("NResp", "Estimate.P", "StdError.P", "Estimate.U", "StdError.U")
                                                                good_variables <- c(string_variables, numeric_variables)
                                                               
                                                                estimates <- analysis[, good_variables]
                                                               
                                                                # Remove "Total" junk
                                                                estimates <- estimates[estimates[["Category"]] != "Total", ]
                                                               
                                                                # Add 0s in for categories in the vector categories but not in the data frame yet
                                                                # First by making an empty, single-observation version of the data frame
                                                                empty_df <- estimates[1, ]
                                                                for (var in numeric_variables) {
                                                                  empty_df[[var]] <- 0
                                                                }
                                                                # Then add in a new row for each missing category with that template
                                                                missing_categories <- categories[!(categories %in% estimates[["Category"]])]
                                                                for (category in missing_categories) {
                                                                  new_row <- empty_df
                                                                  new_row[["Category"]] <- category
                                                                  rbind(estimates, new_row)
                                                                }
                                                               
                                                                # Make a vector of the adjusted counts named with the category names (from the data frame because they'll be in the right order) probably using setNames()
                                                                total_observations <- sum(estimates[["NResp"]])
                                                                adjusted_counts <- total_observations * estimates[["Estimate.P"]] / 100
                                                               
                                                                # Calculate CIs
                                                                cis <- goodman_cis(counts = adjusted_counts,
                                                                                   alpha = 0.2,
                                                                                   chisq = "best")
                                                               
                                                                # cbind the CIs to the data frame
                                                                output <- cbind(estimates,
                                                                                cis)
                                                               
                                                                # return it all!
                                                                return(output)
                                                              })


### Format the outputs
output <- bind_rows(split_estimates) 
head(output)
output <- output %>% select(-LCB80Pct.P, -UCB80Pct.P)

fates <- unique(c(aim_points[["fate"]]))


 point_counts <- do.call(rbind,
                         lapply(X = split(rbind(aim_points@data[, c("wgtcat", "fate")], lmf_points@data[, c("wgtcat", "fate")]),
                                          rbind(aim_points@data[, c("wgtcat", "fate")], lmf_points@data[, c("wgtcat", "fate")])$wgtcat),
                                fates = fates,
                                FUN = function(X, fates){
                                  output <- data.frame(wgtcat = X[["wgtcat"]][1],
                                                       stringsAsFactors = FALSE)
                                  for (fate in fates) {
                                    output[[paste0("count_", fate)]] <- sum(X[["fate"]] %in% fate)
                                  }
                                  rownames(output) <- NULL
                                  return(output)
                                }))


point_counts <- do.call(rbind,
                        lapply(X = split(rbind(aim_points@data[, c("wgtcat", "fate")]),
                                         rbind(aim_points@data[, c("wgtcat", "fate")])),
                               fates = fates,
                               FUN = function(X, fates){
                                 output <- data.frame(wgtcat = X[["wgtcat"]][1],
                                                      stringsAsFactors = FALSE)
                                 for (fate in fates) {
                                   output[[paste0("count_", fate)]] <- sum(X[["fate"]] %in% fates)
                                 }
                                 rownames(output) <- NULL
                                 return(output)
                               }))

wgtcat_summary <- merge(x = wgtcat_summary,
                        y = point_counts,
                        all.x = TRUE)

for (fate in fates) {
  wgtcat_summary[[paste0("count_", fate)]][is.na(wgtcat_summary[[paste0("count_", fate)]])] <- 0
}


wgtcat_summary[["in_inference"]] <- !is.na(wgtcat_summary[["sum_relwgts"]])

wgtcat_summary <- wgtcat_summary[, c("wgtcat", "area", "area_units",
                                     "observed_point_count", paste0("count_", fate),
                                     "observed_proportion",
                                     "observed_area", "unobserved_area",
                                     "in_inference")]

names(wgtcat_summary) <- c("poststratum_id", "area", "area_units",
                           "observed_point_count", paste0("count_", fate),
                           "observed_proportion",
                           "observed_area", "unobserved_area",
                           "in_inference")

wgtcats$wgtcat <-  as.character(wgtcats$wgtcat)
wgtcat_summary <- merge(x = wgtcat_summary,
                        y = wgtcats[, c("design_strata", "wgtcat")],
                        by.x = "poststratum_id",
                        by.y = "wgtcat")

wgtcat_summary <- wgtcat_summary[, c("poststratum_id", "area", "area_units",
                                     "observed_point_count", paste0("count_", fate),
                                     "observed_proportion",
                                     "observed_area", "unobserved_area",
                                     "in_inference", "design_strata")]

# Add in where they're from
aim_points[["source"]] <- "AIM"

lmf_points[["source"]] <- "LMF"

all_points <- rbind(aim_points[, c("unique_id", "fate", "wgtcat", "source")],
                     lmf_points[, c("unique_id", "fate", "wgtcat", "source")])


all_points <- sp::merge(x = all_points,
                        y = point_weights,
                        all = FALSE)

# make wider for shapefile

terradat_benchmarked_wide <-  terradat_benchmarked[, c("PrimaryKey", "Indicator", "Condition.Category")] %>%
  tidyr::pivot_wider(id_cols = "PrimaryKey", names_from = "Indicator", values_from = "Condition.Category")
# merge plot names from terradat


all_points <- sp::merge(x = all_points,
                        y = terradat_benchmarked_wide,
                        by.x = "unique_id",
                        by.y = "PrimaryKey",
                        all.x = TRUE , 
                        duplicateGeoms = TRUE)

names(all_points@data) <- c("PrimaryKey", "Final Designation", "Weight Category", "Source", "LMF segment", "aim", "lmf", "observed", 
                            "relwgt", "Weight", "Reporting Unit","Non-Noxious Perennial Grass Cover","Non-Noxious Shrub Cover","Sagebrush Cover","Average Sagebrush Height")

all_points <- all_points[, c("PrimaryKey", "Final Designation", "Weight Category", "Source", "LMF segment", 
                             "Weight", "Reporting Unit","Non-Noxious Perennial Grass Cover","Non-Noxious Shrub Cover","Sagebrush Cover","Average Sagebrush Height")]

all_points <- sp::merge(x = all_points,
                        y = terradat_points@data[,c("PrimaryKey","PlotID")],
                        by = "PrimaryKey",
                        all.x = TRUE , 
                        duplicateGeoms = TRUE)

point_weights <- sp::merge(x = point_weights,
                           y = terradat_points@data[,c("PrimaryKey","PlotID")],
                           by.x = "unique_id",
                           by.y ="PrimaryKey",
                           all.x = TRUE , 
                           duplicateGeoms = TRUE)
analysis <- output

###### Write results ######

analysis <- output

names(analysis) <- c("Reporting Unit",
                     "Project Area",
                     "Indicator",
                     "Rating",
                     "Number of plots",
                     "Estimated percent of sampled area", 
                     "Standard error of estimated percent of sampled area",
                     "Estimated hectares",
                     "Standard error of estimated hectares",
                      paste0(c("Lower confidence bound of hectares (", "Upper confidence bound of hectares ("), confidence, "%, Goodman multinomial) ") ,
                      paste0(c("Lower confidence bound of percent area (", "Upper confidence bound of percent area ("), confidence, "%, Goodman multinomial) "))
                 

write.csv(point_weights,
          file = paste0(output_path, "/",
                        output_filename,
                        "_pointweights_",
                        date, ".csv"),
          row.names = FALSE)

write.csv(wgtcat_summary,
          file = paste0(output_path, "/",
                        output_filename,
                        "_wgtcatsummary_",
                        date, ".csv"),
          row.names = FALSE)
write.csv(analysis,
          file = paste0(output_path, "/",
                        output_filename,
                        "_analysis_",
                        date, ".csv"),
          row.names = FALSE)

rgdal::writeOGR(all_points,
                dsn = output_path,
                layer = paste0(output_filename,
                               "_points_",
                               date),
                driver = "ESRI Shapefile",
                overwrite_layer = TRUE)

## graphs

library(ggplot2)
library(scales)
#################################################################

# Graphics


analysis1 <-subset(analysis, Rating=="Suitable"|Rating =="Unsuitable")

# add in zeros for not meeting
analysis_zero <-  analysis1
analysis_zero$Rating <- "Unsuitable"
analysis_zero$`Number of plots` <- 0
analysis_zero$`Estimated percent of sampled area` <-  0
analysis_zero$`Estimated hectares` <-  0
analysis_zero$`Lower confidence bound of percent area (80%, Goodman multinomial) ` <-  0
analysis_zero$`Upper confidence bound of percent area (80%, Goodman multinomial) ` <- 0
analysis_zero$`Lower confidence bound of hectares (80%, Goodman multinomial) ` <- 0
analysis_zero$`Upper confidence bound of hectares (80%, Goodman multinomial) ` <- 0
analysis1 <-  rbind(analysis1, analysis_zero[1,])

analysis1$Rating <-  as.factor(analysis1$Rating)
analysis1$Rating <- factor(analysis1$Rating,levels(factor(analysis1$Rating))[c(2,1)])


ggplot2::ggplot(data = analysis1,
                ggplot2::aes(y = `Estimated hectares`,
                             x = Rating, fill = Rating))+
  ggplot2::geom_col(alpha = 0.6, position = "dodge")+
  ggplot2::geom_errorbar(ggplot2::aes(ymin = `Lower confidence bound of hectares (80%, Goodman multinomial) `,
                                      ymax = `Upper confidence bound of hectares (80%, Goodman multinomial) `),
                         width = 0.5)+
  facet_wrap(.~Indicator)+
  coord_flip()+
  theme_minimal(base_size = 16)+
  labs(x = "",
       title = "GRSG Spring Seasonal Use Area")+
  scale_fill_manual(values = c("#FF0000",
                               "#3CE922"))+
  scale_y_continuous(labels = comma)

ggsave(device = "jpeg", dpi = 400, width = 17, filename = "hamlin_spring.jpeg", path = output_path)

