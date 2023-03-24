#' Extract SpeciesLink species occurrence data from a polygon and get IUCN Red List status
#'
#' This function extracts SpeciesLink species occurrence data from a polygon and retrieves
#' the IUCN Red List status for each species found. The output is a data frame
#' with columns for species name and IUCN Red List status.
#'
#' @param polygon A spatial polygon object (class "sf") representing the study area.
#' @param occurrences A dataframe with SpeciesLink occurrence data for your region
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


Plot_SpLink <- function(polygon, occurrences, file_name) {
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
  if (!is.data.frame(occurrences)) {
    stop("Input occurrences and occurrences2 must be data frames")
  }

  # Prepare occurrence data
  occurrences <- occurrences[complete.cases(occurrences[,c("longitude","latitude")]),] # remove NAs
  occurrences <- st_as_sf(occurrences, coords = c("longitude","latitude"), crs = st_crs("EPSG:4326")) # convert to sf

  ###################################################################
  #MAKE BUFFER
  ###################################################################

  # First set the correct CRS for the polygon
  polygon <- st_transform(polygon, crs = 31983)

  # Create buffer
  buffer <- st_buffer(polygon, dist = units::as_units(10, "kilometer"), byid = TRUE, nQuadSegs = -1)

  # Put back to correct CRS for GBIF
  buff <- st_transform(buffer, crs = 4326)
  polygon <- st_transform(polygon, crs = 4326)

  ###################################################################
  #GET SPECIES DATA
  ###################################################################

  # Get occurrence data in the polygon and in the buffer
  if ("Z" %in% colnames(st_coordinates(polygon))) {
    polygon <- st_zm(polygon)
  }
  occurrences_in_polygon <- occurrences[polygon,]
  occurrences_in_buffer <- occurrences[buff,]

  # Check if data frames are empty
  if (nrow(occurrences_in_polygon) > 0 | nrow(occurrences_in_buffer) > 0) {
    # Combine non-empty data frames using rbind()
    non_empty_dfs <- Filter(function(x) nrow(x) > 0, list(occurrences_in_polygon, occurrences_in_buffer))
    all_occurrences <- do.call(rbind, non_empty_dfs)
    all_occurrences_sf <- st_as_sf(all_occurrences, coords = c("geometry1","geometry2"), crs = st_crs("EPSG:4326"))
  } else {
    # Use empty data frame if all data frames are empty
    all_occurrences_sf <- data.frame()
  }

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
      dplyr::distinct(kingdom, scientificName, gDists_m) # Keep only unique species
    short <- subset(short, short$scientificName != "")

    ###################################################################
    #EXTRACT GBIF INFORMATION
    ###################################################################

    # Get gbif data
    myspecies <- unique(short$scientificName)  # create a string of species
    gbif_data <- rgbif::occ_data(scientificName = myspecies, limit = 1)

    ###################################################################
    #GET IUCN STATUS
    ###################################################################
    if (is.null(gbif_data$data)) { # if the data is not in this format
      if (!is.null(gbif_data)){ # if data is in this format
        df <- data.frame()
        suppressWarnings(for (i in as.numeric(1:length(gbif_data))) {
          if(!is.null(gbif_data[[i]]$data$iucnRedListCategory)){
            scientificName <- gbif_data[[i]]$data$scientificName
          }else{
            scientificName <- "NA"
          }
          if(!is.null(gbif_data[[i]]$data$iucnRedListCategory)){
            iucnRedListCategory <- gbif_data[[i]]$data$iucnRedListCategory
          }else{
            iucnRedListCategory <- "NA"
          }
          df <- rbind(df, cbind(scientificName, iucnRedListCategory))
        })
        # Get the correct status with the names from rgbif
        iucn <- df %>%
          dplyr::distinct(scientificName, iucnRedListCategory, .keep_all = TRUE) %>%
          na.omit()
      }else{ # if it is still NULL
        iucn <- data.frame(scientificName = "NA", iucnRedListCategory = "NA")
      }
    }else {
      # Get the correct status with the names from rgbif
      spc <- gbif_data$data %>%
        dplyr::select(scientificName, iucnRedListCategory) %>%
        dplyr::distinct(scientificName, iucnRedListCategory, .keep_all = TRUE) %>%
        na.omit()
      iucn <- spc
    }

    # Create a new column in iucn with matching "scientificName" from short
    iucn$match <- NA_character_
    iucn$gDists <- NA_character_
    for (i in 1:nrow(short)) {
      prefix <- substr(short$scientificName[i], 1, 20)
      match_idx <- which(grepl(paste0("^", prefix), iucn$scientificName))
      if (length(match_idx) > 0) {
        iucn$match[match_idx] <- short$scientificName[i]
        iucn$gDists[match_idx] <- short$gDists_m[i]
      }
    }

    # Join with IUCN data and coalesce Red List categories
    species <- iucn %>%
      dplyr::left_join(iucn, by = c("match" = "scientificName")) %>%
      dplyr::mutate(iucnRedListCategory = dplyr::coalesce(iucn$iucnRedListCategory, iucnRedListCategory)) %>%
      dplyr::distinct(match, iucnRedListCategory, gDists.x, .keep_all = TRUE) %>%
      dplyr::select(match, scientificName, iucnRedListCategory, gDists.x)

    species <- na.omit(species)
    ###################################################################
    #REPORT RESULTS
    ###################################################################

    # Print and save results
    cat("Note the below list must be observed with reference to distance of the plot. Plants, fungi and insects may not be able to disperse as easily as mammals and birds and should therefore only be taken into consideration for being present when they were found inside the plot. Mammals and birds occuring within the 10km buffer, could have a range within the plot as well. Species that could be present in the plot:\n")
    print(as.data.frame(species))

    ###################################################################
    #SAVE RESULTS
    ###################################################################
    file_name <- paste0(file_name, "_SpeciesLink.csv") # add file extension
    write.csv(species, file_name, row.names = FALSE) # save data frame as csv
  }else{
    cat("No species found\n")
  }
}



