#' Extract Natura2000 species occurrence data from a polygon and get IUCN Red List status
#'
#' This function takes a spatial polygon object (class "sf") representing a study area and
#' searches for any species that are either present within the polygon or located nearby
#' Natura2000 sites. It then determines the associated IUCN Red List status of the
#' identified species.
#'
#' @param polygon A spatial polygon object (class "sf") representing the study area.
#' @param Natura An "sf" object representing the Natura2000 sites.
#' @param species A CSV file containing the species associated with the Natura2000 sites.
#' @param file_name A character string representing the name of the output file.
#' @return A data frame (if species are present) with two columns: scientific name and IUCN Red List status.
#'
#' @export
#' @importFrom sf st_crs st_distance st_intersection st_transform
#' @importFrom rgbif occ_data
#' @importFrom raster %in% merge nrow print unique which.min
#' @importFrom dplyr distinct %>%
#'
#' @references
#' More information about Natura2000 can be found at https://ec.europa.eu/environment/nature/natura2000/


Plot_Natura2000 <- function(polygon, Natura, species, file_name){
  ###################################################################
  #PREPARING DATA
  ###################################################################

  # Read in the necessary libraries
  requireNamespace("raster", quietly = TRUE)
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("rgbif", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  # Stop is polygon is not of type 'sf'
  if (!inherits(polygon, "sf")) {
    stop("Input polygon must be an 'sf' object")
  }

  # Set correct Coordinate Reference System (CRS)
  polygon <- st_transform(polygon, crs = st_crs(Natura))
  # Check if both objects have the same CRS
  identical(st_crs(Natura), st_crs(polygon)) # should return TRUE

  ###################################################################
  #FIND OVERLAP WITH NATURA AREAS
  ###################################################################

  # Find out if the polygon overlaps Natura 2000 areas
  if (any(as.numeric(st_distance(polygon, Natura))) <= 0) { # if the distance to a Natura site is 0m
    int <- st_intersection(Natura, polygon) # take the intersection of the two shapefiles to find the Natura site
    sites <- unique(int$SITECODE) # extract the sitecode from the Natura shapefile
  } else { # if the polygon is not in a natura site
    gDists <<- st_distance(polygon, Natura) # calculate the distances to all Natura sites
    sites <- Natura$SITECODE[which.min(gDists)] # take the site code of the nearest site
    print("The distance to the nearest Natura 2000 site is:") # print statement
    print(paste(min(gDists), "m")) # print distance to nearest site
    print("Species list below should be observed with attention to distance above and may thus contain species that will not be present at the site.")
  }

  ###################################################################
  #MATCH WITH SPECIES DATA
  ###################################################################

  # Match species list with shape file by site code
  sp <- species[species$SITECODE %in% sites, ] # use the site code to extract species from the species file
  if(nrow(sp) > 0){ # if there are species
    # Get the groups
    short <- sp[c("SPECIESNAME", "SPGROUP")] # get the species name and species group column
    short <- unique(short) # only keep unique features
    colnames(short) <- c("scientificName", "class") # change column names

    ###################################################################
    #MATCH WITH GBIF INFORMATION
    ###################################################################

    # Get gbif data according to species names from Natura
    myspecies <- short$scientificName # create a vector of species names
    gbif_data <- rgbif::occ_data(scientificName = myspecies) # extract species info from gbif
  } else { # if there are no species associated with the site code
    short <- sp[c("SPECIESNAME", "SPGROUP")] # extract the two columns
    colnames(short) <- c("scientificName", "class") # change column names
    gbif_data <- short # change name to use further on in the script
    myspecies <- NULL # make myspecies NULL
  }

  ###################################################################
  #ADD IUCN RED LIST INFORMATION
  ###################################################################

  # Create an empty dataframe
  iucn <- data.frame()

  # Collect redlist data from GBIF and clean the dataframes
  if(is.null(gbif_data$data) == TRUE){
    if(is.null(myspecies)){ # if there are no species
      scientificName <- "NA" # make NA
      iucnRedListCategory <- "NA" # make NA
      p <- cbind(scientificName, iucnRedListCategory)  # column bind
      iucn <- rbind(iucn, p) # row bind
    }else{ # if there are species
      for (scientificName in myspecies) { # for every species in myspecies list
        if(is.null(gbif_data[[scientificName[1]]]$data) == TRUE){ # if it is NULL
          iucnRedListCategory <- "NA" # make NA
          p <- cbind(scientificName, iucnRedListCategory) # column bind
        }else{ # if there are species
          if(length(gbif_data[[scientificName[1]]]$data$iucnRedListCategory) > 0){ # if the length of gbif is longer than 0
            iucnRedListCategory <- unique(gbif_data[[scientificName[1]]]$data$iucnRedListCategory) # get the unique species with iucn info
            iucnRedListCategory <- na.omit(iucnRedListCategory) # omit NAs from iucn column
            p <- cbind(scientificName, iucnRedListCategory) # column bind
          }else{
            iucnRedListCategory <- "NA" # make NA
            p <- cbind(scientificName, iucnRedListCategory) # column bind
          }
        }
        iucn <- rbind(iucn, p) # bind with empty dataframe
      }
      iucn <- raster::merge(iucn, short, by = "scientificName") # merge with short to get the nice names
      iucn <- iucn %>% dplyr::distinct(scientificName, iucnRedListCategory, class, .keep_all = TRUE) # keep only unique species
    }
  }else{
    # Get the correct status with the names form rgbif
    spc <- gbif_data$data[, c("scientificName", "iucnRedListCategory", "class")] # get columns
    spc <- spc %>% dplyr::distinct(scientificName, iucnRedListCategory, class, .keep_all = TRUE) # keep unique species
    iucn <- spc # make iucn name
  }
  iucn$distance <- rep(min(gDists), nrow(iucn)) # add distance column to nearest Natura site

  ###################################################################
  #REPORT RESULTS
  ###################################################################

  print("Not observed, but possibly present species according to Natura2000") # print statement
  print(iucn) # print dataframe

  ###################################################################
  #SAVE RESULTS
  ###################################################################

  file_name <- paste0(file_name, "_Natura.csv") # add file extension
  write.csv(iucn, file_name, row.names = FALSE) # save data frame as csv
}



