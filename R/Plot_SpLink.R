#' Extract SpeciesLink species occurrence data from a polygon and get IUCN Red List status
#'
#' This function extracts SpeciesLink species occurrence data from a polygon and retrieves
#' the IUCN Red List status for each species found. The output is a data frame
#' with columns for species name and IUCN Red List status.
#'
#' @param polygon A spatial polygon object (class "sf") representing the study area.
#' @param occurrences A data frame with SpeciesLink species occurrence data, containing columns for scientific name, longitude, and latitude
#' @param occurrences2 A data frame with additional SpeciesLink species occurrence data, in the same format as \code{occurrences}
#' @param filename A character string representing the name of the output file
#' @return A data frame (if species are present) with two columns: scientific name and IUCN Red List status
#'
#' @export
#' @importFrom sf st_as_sf st_buffer st_crs st_distance st_transform
#' @importFrom rgbif occ_data
#' @importFrom raster as.data.frame nrow print unique
#' @importFrom dplyr %>% distinct select bind_rows case_when mutate left_join coalesce
#' @importFrom units as_units
#'
#' @references
#' More information about SpeciesLink can be found at https://specieslink.net/


Plot_SpLink <- function(polygon, occurrences, occurrences2, file_name) {
  ###################################################################
  #PREPARING DATA
  ###################################################################

  # Read in the necessary libraries
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("raster", quietly = TRUE)
  requireNamespace("rgbif", quietly = TRUE)
  requireNamespace("units", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)

  # Stop is polygon is not of type 'sf'
  if (!inherits(polygon, "sf")) {
    stop("Input polygon must be an 'sf' object")
  }
  # Stop if occurrences are not of type dataframe
  if (!is.data.frame(occurrences) || !is.data.frame(occurrences2)) {
    stop("Input occurrences and occurrences2 must be data frames")
  }

  # Prepare occurrence data
  occurrences <- occurrences[complete.cases(occurrences[,c("longitude","latitude")]),] # remove NAs
  occurrences <- st_as_sf(occurrences, coords = c("longitude","latitude"), crs = st_crs("EPSG:4326")) # convert to sf

  occurrences2 <- occurrences2[complete.cases(occurrences2[,c("longitude","latitude")]),] # remove NAs
  occurrences2 <- st_as_sf(occurrences2, coords = c("longitude","latitude"), crs = st_crs("EPSG:4326")) # convert to sf

  ###################################################################
  #MAKE BUFFER
  ###################################################################

  # First set the correct CRS for the polygon
  polygon <- st_transform(polygon, crs = 31983)

  # Create buffer
  buffer <- st_buffer(polygon, dist = units::as_units(5, "kilometer"), byid = TRUE)

  # Put back to correct CRS for GBIF
  buff <- st_transform(buffer, crs = 4326)
  polygon <- st_transform(polygon, crs = 4326)

  ###################################################################
  #GET SPECIES DATA
  ###################################################################

  # Get occurrence data in the polygon and in the buffer
  occurrences_in_polygon <- occurrences[polygon,]
  occurrences_in_buffer <- occurrences[buff,]
  occurrences2_in_polygon <- occurrences2[polygon,]
  occurrences2_in_buffer <- occurrences2[buff,]

  # Combine occurrence data into one data frame
  all_occurrences <- as.data.frame(rbind(unlist(c(occurrences_in_polygon, occurrences_in_buffer,
                           occurrences2_in_polygon, occurrences2_in_buffer))))
  all_occurrences_sf <- st_as_sf(all_occurrences, coords = c("geometry1","geometry2"), crs = st_crs("EPSG:4326"))

  ###################################################################
  #EXTRACT DISTANCE INFORMATION
  ###################################################################

  # Find the distance to the polygon
  if(nrow(all_occurrences_sf) > 0){
    all_occurrences_sf$gDists_m <- st_distance(all_occurrences_sf, polygon)
    all_occurrences_sf$scientificName <- all_occurrences_sf$scientificname

    # Create a shorter dataframe with necessary columns
    short <- all_occurrences_sf %>%
      dplyr::select(kingdom, scientificName, gDists_m) %>%
      dplyr::distinct() # Keep only unique species

    ###################################################################
    #EXTRACT GBIF INFORMATION
    ###################################################################

    # Get gbif data
    myspecies <- short$scientificName # create a string of species
    gbif_data <- rgbif::occ_data(scientificName = myspecies)

    if (is.null(gbif_data$data)) { # If the data is NULL
      iucn <- myspecies %>%
        lapply(function(x) {
          if (is.null(gbif_data[[x]]$data)) { # if it can't be found in this data format
            data.frame(scientificName = x, iucnRedListCategory = "NA") # add NA to IUCN
          } else { # if the data is in the second format
            iucnRedListCategory <- unique(gbif_data[[x]]$data$iucnRedListCategory) # take the iucn category for each unique species
            iucnRedListCategory <- na.omit(iucnRedListCategory) # omit NAs
            data.frame(scientificName = x, iucnRedListCategory = iucnRedListCategory) # create data frame
          }
        }) %>%
        bind_rows() # Combine data frames
    } else {
      # Get the correct status with the names from rgbif
      spc <- gbif_data$data %>%
        dplyr::select(scientificName, iucnRedListCategory) %>%
        dplyr::distinct(scientificName, iucnRedListCategory, .keep_all = TRUE) %>%
        na.omit()
      iucn <- spc
    }
  } else {
    iucn <- data.frame()
  }

  # Create new column in iucn with matching "scientificName" from short
  matching_names <- short %>%
    dplyr::mutate(match = dplyr::case_when(
      grepl(paste("^", substr(iucn$scientificName, 1, 4), sep=""), scientificName) ~ scientificName,
      TRUE ~ NA_character_
    )) %>%
    dplyr::select(match, scientificName)

  # Join with IUCN data and coalesce Red List categories
  species <- matching_names %>%
    dplyr::left_join(iucn, by = c("match" = "scientificName")) %>%
    dplyr::mutate(iucnRedListCategory = dplyr::coalesce(iucn$iucnRedListCategory, iucnRedListCategory)) %>%
    dplyr::distinct(match, iucnRedListCategory, .keep_all = TRUE)

  # Print and save results
  cat("Note the below list must be observed with reference to distance of the plot. Plants, fungi and insects may not be able to disperse as easily as mammals and birds and should therefore only be taken into consideration for being present when they were found inside the plot. Mammals and birds occuring within the 5km buffer, could have a range within the plot as well. Species that could be present in the plot:\n")
  print(as.data.frame(species))
  file_name <- paste0(file_name, "_SpeciesLink.csv") # add file extension
  write.csv(species, file_name, row.names = FALSE) # save data frame as csv
}



