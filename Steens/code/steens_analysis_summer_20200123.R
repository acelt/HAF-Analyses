############################################################
#### Sage-Grouse Seasonal Habitat Analysis for Steens, OR
#### Summer habitat
#### Code/analysis script by Rachel Burke, Nelson Stauffer
#### Completed 2020-01-23
############################################################
#### Setup ####
############################################################
### Attach packages
# library(tidyverse)
# Note that aim.analysis v0.8.1 was used
# devtools::install_github("nstauffer/aim.analysis")
# library(aim.analysis)

### Set the constants
# Date of analysis
date <- "20200123"

# Confidence level in percent
confidence <- 80

# Filepaths to data
data_spatial_path <- "data/spatial"
data_tabular_path <- "data/tabular"
output_path <- "output"

# Filename for ratings from analysis requestor
ratings_filename <- "HAF_Oregon_2019_Steens_SiteScale_NOCANALYSISREQUEST.xlsx"

# Core filename for all outputs
output_filename <- "steens_analysis_summer"

# The geodatabase and layer names for the spatial data sources
# are embedded in the code below

# Habitat type (corresponds to sheet name in ratings Excel workbook)
seasonal_habitat_type = "S-4 Upland Summer Late Brood"

# Projection to use. Alber's Equal Area is useful for calculating areas
projection <- sp::CRS("+proj=aea")

# The variable in AIM sample designs that contains the unique identifier for each point
aim_idvar <- "PLOT_NM"
# The variable in AIM sample designs that contains the fate for each point
aim_fatevar = "FINAL_DESIG"
# The variable in TerrADat that contains the unique identifier for each point matching AIM designs'
tdat_idvar <- "PlotID"
# The variable in LMF points that contains the unique identifier for each point
lmf_idvar <- "PLOTKEY"

# The fates in sample designs which correspond to sampled/observed
observed_fates = c("TS", "Target Sampled")
# The fates in sample designs which correspond to unneeded
# (e.g. unused oversample or points designated for future sampling)
invalid_fates = c("NN", NA, "Non Needed", "")

# The LMF segments polygons
# Here NULL because segments were assigned to points in Arc already
segments <- NULL
# The variable containing the LMF segment identity/membership
# This is found in the points and the segment polygons
segment_var <- "segcode"

wgtcats <- NULL
# The variable that contains the areas in HECTARES in wgtcats
wgtcat_area_var = "hectares"
# The variables which combined in wgtcats and the points contain the post-strata identities/memeberships
wgtcat_var <- c("DesignCombo", "StrataCombo")

############################################################
#### Reading in data ####
############################################################
# Make the data frame wgtcats with the polygons
# Due to accumulated geometry errors in geoprocessing, we need the data frame for areas
# because R can't handle the geometry as smoothly as Arc
wgtcat_df <-sf::st_read(dsn = paste(data_spatial_path, "Site_Scale_20191218.gdb", sep = "/"),
                        layer = "LakeviewBB_SFA_SteensPAC_BurnsDO_Final_LMF_Summer_cli",
                        stringsAsFactors = FALSE)
sf::st_geometry(wgtcat_df) <- NULL
wgtcats <- as.data.frame(dplyr::summarize(dplyr::group_by(wgtcat_df,
                                                          DesignCombo,
                                                          StrataCombo),
                                          wgtcat = dplyr::first(StrataCombo),
                                          hectares = sum(AREA_HA)),
                         stringsAsFactors = FALSE)
# We overwrite wgtcat from the step above.
# This creates a unique id for the poststrata because StrataCombo isn't actually unique
wgtcats[["wgtcat"]] <- 1:nrow(wgtcats)

#### Read in the ratings for the season
ratings <- readxl::read_excel(path = paste(data_tabular_path, ratings_filename, sep = "/"),
                              sheet = seasonal_habitat_type)

#### Read in design information
### Steens PAC Design
pac_design_points <- sf::st_read(dsn = paste(data_spatial_path, "Site_Scale_20191218.gdb", sep = "/"),
                                 layer = "pac_design_segs_summer_20191218",
                                 stringsAsFactors = FALSE)
## Standardize projection
pac_design_points <- sf::st_transform(pac_design_points,
                                      crs = projection)
## Make spatial
pac_design_points <- methods::as(pac_design_points, "Spatial")



### Burns DO Design
burns_design_points <- sf::st_read(dsn = paste(data_spatial_path, "Site_Scale_20191218.gdb", sep = "/"),
                                   layer = "burns_design_segs_summer_20191218",
                                   stringsAsFactors = FALSE)
## Standardize projection
burns_design_points <- sf::st_transform(burns_design_points,
                                        crs = projection)
## Make spatial
burns_design_points <- methods::as(burns_design_points, "Spatial")

### SFA Design
sfa_design_points <- sf::st_read(dsn = paste(data_spatial_path, "Site_Scale_20191218.gdb", sep = "/"),
                                 layer = "orwa_sfa_design_segs_summer_20191218",
                                 stringsAsFactors = FALSE)
## Standardize projection
sfa_design_points <- sf::st_transform(sfa_design_points,
                                      crs = projection)
## Make spatial
sfa_design_points <- methods::as(sfa_design_points, "Spatial")


#### Read in TerrADat data
tdat_points <- sf::st_read(dsn = paste(data_spatial_path, "Site_Scale_20191218.gdb", sep = "/"),
                           layer = "steens_terradat_points_segs_summer_20191217",
                           stringsAsFactors = FALSE)
## Standardize projection
tdat_points <- sf::st_transform(tdat_points,
                                crs = projection)
## Make spatial 
tdat_points <- methods::as(tdat_points, "Spatial")

### Read in additional Points (these are sampled points not yet ingested into TerrADat by NOC)
additional_points <- sf::st_read(dsn = paste(data_spatial_path, "Site_Scale_20191218.gdb", sep = "/"),
                                 layer = "steens_sampled_points_segs_summer_20191220",
                                 stringsAsFactors = FALSE)
## Standardize projection
additional_points <- sf::st_transform(additional_points,
                                      crs = projection)
## Make spatial 
additional_points <- methods::as(additional_points, "Spatial")

# Read in the LMF points
lmf_points <- sf::st_read(dsn = paste(data_spatial_path, "Site_Scale_20191218.gdb", sep = "/"),
                          layer = "steens_lmf_points_segs_summer_20191217",
                          stringsAsFactors = FALSE)
lmf_points <- sf::st_transform(lmf_points,
                               crs = projection)
lmf_points <- methods::as(lmf_points, "Spatial")


### Make naming consistent across all data
# Move unique IDs to unique_id in TerrADat
tdat_points@data[["unique_id"]] <- tdat_points@data[[tdat_idvar]]
# Set fate variable in TerrADat
tdat_points@data[["fate"]] <- observed_fates[1]
# Add in unique identifiers for poststrata using wgtcats as a lookup table
tdat_points <- sp::merge(x = tdat_points,
                         y = wgtcats[, c("DesignCombo", "StrataCombo", "wgtcat")],
                         by = c("DesignCombo", "StrataCombo"))
# Move LMF segment membership to segment
tdat_points@data[["segment"]] <- tdat_points@data[[segment_var]]

## Repeat for all point data
# PAC design
pac_design_points@data[["unique_id"]] <- pac_design_points@data[[aim_idvar]]
pac_design_points@data[["fate"]] <- pac_design_points@data[[aim_fatevar]]
pac_design_points <- sp::merge(x = pac_design_points,
                               y = wgtcats[, c("DesignCombo", "StrataCombo", "wgtcat")],
                               by = c("DesignCombo", "StrataCombo"))
pac_design_points@data[["segment"]] <- pac_design_points@data[[segment_var]]
# SFA design
sfa_design_points@data[["unique_id"]] <- sfa_design_points@data[[aim_idvar]]
sfa_design_points@data[["fate"]] <- sfa_design_points@data[[aim_fatevar]]
sfa_design_points <- sp::merge(x = sfa_design_points,
                               y = wgtcats[, c("DesignCombo", "StrataCombo", "wgtcat")],
                               by = c("DesignCombo", "StrataCombo"))
sfa_design_points@data[["segment"]] <- sfa_design_points@data[[segment_var]]
# Burns DO design
burns_design_points@data[["unique_id"]] <- burns_design_points@data[[aim_idvar]]
burns_design_points@data[["fate"]] <- burns_design_points@data[[aim_fatevar]]
burns_design_points <- sp::merge(x = burns_design_points,
                                 y = wgtcats[, c("DesignCombo", "StrataCombo", "wgtcat")],
                                 by = c("DesignCombo", "StrataCombo"))
burns_design_points@data[["segment"]] <- burns_design_points@data[[segment_var]]
# Additional points
additional_points@data[["unique_id"]] <- additional_points@data[[aim_idvar]]
# The additional points were all sampled, so that information is added here
additional_points@data[["fate"]] <- additional_points@data[[aim_fatevar]]
additional_points <- sp::merge(x = additional_points,
                               y = wgtcats[, c("DesignCombo", "StrataCombo", "wgtcat")],
                               by = c("DesignCombo", "StrataCombo"))
additional_points@data[["segment"]] <- additional_points@data[[segment_var]]
# LMF points
lmf_points@data[["unique_id"]] <- lmf_points@data[[lmf_idvar]]
# The LMF points were all sampled, so that information is added here
lmf_points@data[["fate"]] <- observed_fates[1]
lmf_points <- sp::merge(x = lmf_points,
                        y = wgtcats[, c("DesignCombo", "StrataCombo", "wgtcat")],
                        by = c("DesignCombo", "StrataCombo"))
lmf_points@data[["segment"]] <- lmf_points@data[[segment_var]]

# The additional points' plot IDs were truncated to just the ending digits
# They come from the PAC design, so this makes a lookup vector from original name to truncated
# plotid_lookup <- setNames(pac_design_points[["unique_id"]],
#                           stringr::str_replace(pac_design_points[["unique_id"]], pattern = "\\D*(?=\\d+$)", replacement = ""))
# And this replaces the truncated IDs wil the full ones
# additional_points[["unique_id"]] <- plotid_lookup[additional_points[["unique_id"]]]

### Mash the design files together, keeping only necessary variables
design_points <- rbind(pac_design_points[, c("unique_id", "fate", "wgtcat", "segment")],
                       sfa_design_points[, c("unique_id", "fate", "wgtcat", "segment")],
                       burns_design_points[, c("unique_id", "fate", "wgtcat", "segment")]) 

### Combine the sampled points
sampled_points <- rbind(additional_points[, c("unique_id", "fate", "wgtcat", "segment")],
                        tdat_points[, c("unique_id", "fate", "wgtcat", "segment")])

# Checking overlap here between observed points in design_points and sampled_points
sum(sampled_points$unique_id %in% design_points$unique_id)
sum(design_points$unique_id[design_points$fate %in% observed_fates] %in% sampled_points$unique_id)

# Checking against the ratings confirms if all the rated plots are represented (important)
# and if all the sampled_plots are rated (not important, but informative)
if (!all(ratings[["Plot Identifier"]][ratings[["AIM"]] == "AIM"] %in% sampled_points$unique_id)) {
  warning("NOT ALL RATED AIM PLOTS ARE PRESENT IN sampled_points!")
}
all(sampled_points$unique_id %in% ratings[["Plot Identifier"]])

# Filter sampled_plots by the ratings. Necessary only if sampled plots did not qualify for rating
# e.g. the plot was sampled at the wrong time of year to provide useful information about the seasonal use
sampled_points <- sampled_points[sampled_points$unique_id %in% ratings[["Plot Identifier"]], ]

# Check LMF
# Checking against the ratings confirms if all the rated plots are represented (important)
# and if all the sampled_plots are rated (not important, but informative)
if (!all(ratings[["Plot Identifier"]][ratings[["AIM"]] == "LMF"] %in% lmf_points$unique_id)) {
  warning("NOT ALL RATED LMF PLOTS ARE PRESENT IN lmf_points!")
}
all(lmf_points$unique_id %in% ratings[["Plot Identifier"]][ratings[["AIM"]] == "LMF"])

# Filter sampled_plots by the ratings. Necessary only if sampled plots did not qualify for rating
# e.g. the plot was sampled at the wrong time of year to provide useful information about the seasonal use
lmf_points <- lmf_points[lmf_points$unique_id %in% ratings[["Plot Identifier"]], ]

# Now combine:
# All design_points which were not observed. This removes any points that overlap with sampled_points but also
# any that were observed but somehow not in TerrADat and deemed unsuitable for rating
# which therefore means they also should not be included in weighting
# All sampled points (already restricted to the ones which were rated)
aim_points <- rbind(design_points[!(design_points$fate %in% observed_fates), c("unique_id", "fate", "wgtcat", "segment")], 
                    sampled_points[, c("unique_id", "fate", "wgtcat", "segment")])
# Take out invalid fates, e.g. things that don't get included in weighting
aim_points <- aim_points[!(aim_points$fate %in% invalid_fates), ]

# Add in coordinates (needed for the analysis step)
lmf_points <- aim.analysis::add_coords(lmf_points,
                                       xynames = c("x", "y"))
aim_points <-  aim.analysis::add_coords(aim_points,
                                        xynames = c("x", "y"))

############################################################
#### Calculate weights ####
### Note that this is the contents of aim.analysis::weight_aimlmf() verbatim
### As a function, it was producing errors that it does not as bare code
############################################################
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
aim_points[["reporting_unit"]] <- seasonal_habitat_type
lmf_points[["reporting_unit"]] <- seasonal_habitat_type

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
                                  X[["points"]]
                                }))
wgtcat_summary <- do.call(rbind,
                          lapply(X = weight_info,
                                 FUN = function(X){
                                   X[["wgtcat"]]
                                 }))

############################################################
#### Analysis ####
############################################################
# Combine the points
all_points <- rbind(aim_points[, c("unique_id", "x", "y")],
                    lmf_points[, c("unique_id", "x", "y")])

# Add the ratings to the point info and convert to a data frame
all_points <- merge(x = all_points@data,
                    y = ratings,
                    by.x = "unique_id",
                    by.y = "Plot Identifier",
                    all.x = FALSE)

# Add in the indicator, which for these will always be "Habitat suitability"
all_points[["indicator"]] <- "Habitat suitability"

# Add in the season (this is defined up above) e.g. "S-4 Upland Summer Late Brood-rearing Habitat"
all_points[["reporting_unit"]] <- seasonal_habitat_type
point_weights[["reporting_unit"]] <- seasonal_habitat_type

# And analyze
analysis <- aim.analysis::analyze(benchmarked_points = all_points,
                                  point_weights = point_weights,
                                  id_var = "unique_id",
                                  indicator_var = "indicator",
                                  value_var = "Suitability",
                                  x_var = "x",
                                  y_var = "y",
                                  reporting_var = "reporting_unit",
                                  weight_var = "wgt",
                                  conf = confidence)

############################################################
#### Formatting outputs ####
############################################################


analysis <- analysis[, c("Subpopulation",
                         "Indicator",
                         "Category",
                         "NResp",
                         "Estimate.P",
                         "StdError.P",
                         "Estimate.U",
                         "StdError.U")]

adjusted_counts <- (sum(analysis[["NResp"]][analysis[["Category"]] != "Total"]) * analysis[["Estimate.P"]][analysis[["Category"]] != "Total"] / 100)

# Basic math, but important!!!
alpha <- 1 - confidence / 100
category_count <- length(adjusted_counts)
observation_count <- sum(adjusted_counts)
proportions <- adjusted_counts / observation_count

# Get a value for the quantile based on the alpha and the number of categories
chi <- stats::qchisq(p = 1 - (alpha / category_count),
                     df = 1)

# Calculate the bounds!
# Note that these are symmetrical, just not around the proportion.
# They're symmetrical around (chi + 2 * counts) / (2 * (chi + observation_count))

lower_bounds <- (chi + 2 * adjusted_counts - sqrt(abs(chi^2 + 4 * adjusted_counts * chi * (1 - proportions)))) / (2 * (chi + observation_count))
upper_bounds <- (chi + 2 * adjusted_counts + sqrt(abs(chi^2 + 4 * adjusted_counts * chi * (1 - proportions)))) / (2 * (chi + observation_count))

# A proportion can never be greater than 1 or less than 0
# So we'll add bounds any CIs in case that happens
# That's definitely a thing that can happen if the magnitude of sqrt(chi^2 + 4 * counts * chi * (1 - count_ratios))
# is large enough
lower_bounds_capped <- lower_bounds
lower_bounds_capped[lower_bounds < 0] <- 0
upper_bounds_capped <- upper_bounds
upper_bounds_capped[upper_bounds > 1] <- 1

# Build the output  
goodman_cis <- data.frame(count = adjusted_counts,
                          proportion = proportions,
                          lower_bound = lower_bounds_capped,
                          upper_bound = upper_bounds_capped)

goodman_cis[["Category"]] <- analysis[["Category"]][analysis[["Category"]] != "Total"]

analysis <- merge(x = analysis,
                  y = goodman_cis[, c("Category", "lower_bound", "upper_bound")])

analysis[["lower_bound"]] <- analysis[["lower_bound"]] * 100
analysis[["upper_bound"]] <- analysis[["upper_bound"]] * 100

# Calculate the hectares using the Goodman binomial confidence intervals from percentage
analysis[[paste0("LCB", confidence, "Pct.U.Goodman")]] <- analysis[["lower_bound"]] / 100 * analysis[["Estimate.U"]]
analysis[[paste0("UCB", confidence, "Pct.U.Goodman")]] <- analysis[["upper_bound"]] / 100 * analysis[["Estimate.U"]]

names(analysis) <- c("Habitat Type",
                     "Indicator",
                     "Rating",
                     "Number of plots",
                     "Estimated percent of sampled area",
                     "Standard error of estimated percent of sampled area",
                     "Estimated hectares",
                     "Standard error of estimated hectares",
                     paste0(c("Lower confidence bound of percent area (", "Upper confidence bound of percent area ("), confidence, "%, Goodman binomial) "),
                     paste0(c("Lower confidence bound of hectares (", "Upper confidence bound of hectares ("), confidence, "%, Goodman binomial) "))


fates <- unique(c(aim_points[["fate"]], lmf_points@data[["fate"]]))

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

wgtcat_summary <- merge(x = wgtcat_summary,
                        y = wgtcats[, c("DesignCombo", "StrataCombo", "wgtcat")],
                        by.x = "poststratum_id",
                        by.y = "wgtcat")

wgtcat_summary <- wgtcat_summary[, c("poststratum_id", "area", "area_units",
                                     "observed_point_count", paste0("count_", fate),
                                     "observed_proportion",
                                     "observed_area", "unobserved_area",
                                     "in_inference", "DesignCombo", "StrataCombo")]

# Add in where they're from
aim_points[["source"]] <- "AIM"
lmf_points[["source"]] <- "LMF"

all_points <- rbind(aim_points[, c("unique_id", "fate", "wgtcat", "source")],
                    lmf_points[, c("unique_id", "fate", "wgtcat", "source")])

all_points <- sp::merge(x = all_points,
                        y = point_weights,
                        all = FALSE)

all_points <- sp::merge(x = all_points,
                        y = ratings[, c("Plot Identifier", "Suitability")],
                        by.x = "unique_id",
                        by.y = "Plot Identifier",
                        all.x = TRUE)

names(all_points@data) <- c("plot_id", "fate", "wgtcat", "source", "segment", "aim", "lmf", "observed", 
                            "relwgt", "wgt", "reporting_unit", "rating")

all_points <- all_points[, c("plot_id", "rating", "fate", "reporting_unit", "wgtcat", "source", "wgt")]



############################################################
#### Writing outputs ####
############################################################
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
