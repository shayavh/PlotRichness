#' Find the species from Natura2000 present in a plot
#'
#' The function takes a plot as input and searches for any species
#' from the Natura2000 sites that are either present within the plot
#' or located nearby. It then determines the associated International
#' Union for Conservation of Nature (IUCN) status of the identified
#' species.
#'
#' @param polygon An sf object of a polygon.
#' @param Natura An sf object with Natura2000 sites.
#' @param species A csv file with the species associated to Natura2000 sites.
#' @param file_name A file name "" you want to save the final csv to.
#' @return Species (if present) in a plot with their IUCN and residence status
#'
#' @export
#' @import sp
#' @import rgbif
#' @import rgeos
#' @import rgdal
#' @import raster
#' @import dplyr

Plot_Natura2000 <- function(polygon, Natura, species, file_name){
  # Read in the necessary libraries
  libs <- c("sp", "rgeos", "rgdal", "raster", "sf", "rgbif", "dplyr")
  lapply(libs, require, character.only = TRUE)

  # Set correct Coordinate Reference System (CRS)
  polygon <- st_transform(polygon, crs = st_crs(Natura))
  # Check if both objects have the same CRS
  identical(st_crs(Natura), st_crs(polygon)) # should return TRUE

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

  # Match species list with shape file by site code
  sp <- species[species$SITECODE %in% sites, ] # use the site code to extract species from the species file
  if(nrow(sp) > 0){ # if there are species
    # Get the groups
    short <- sp[c("SPECIESNAME", "SPGROUP")] # get the species name and species group column
    short <- unique(short) # only keep unique features
    colnames(short) <- c("scientificName", "class") # change column names

    # Get gbif data according to species names from Natura
    myspecies <- short$scientificName # create a vector of species names
    gbif_data <- occ_data(scientificName = myspecies) # extract species info from gbif
  } else { # if there are no species associated with the site code
    short <- sp[c("SPECIESNAME", "SPGROUP")] # extract the two columns
    colnames(short) <- c("scientificName", "class") # change column names
    gbif_data <- short # change name to use further on in the script
    myspecies <- NULL # make myspecies NULL
  }

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
      iucn <- merge(iucn, short, by = "scientificName") # merge with short to get the nice names
      iucn <- iucn %>% distinct(scientificName, iucnRedListCategory, class, .keep_all = TRUE) # keep only unique species
    }
  }else{
    # Get the correct status with the names form rgbif
    spc <- gbif_data$data[, c("scientificName", "iucnRedListCategory", "class")] # get columns
    spc <- spc %>% distinct(scientificName, iucnRedListCategory, class, .keep_all = TRUE) # keep unique species
    iucn <- spc # make iucn name
  }
  iucn$distance <- rep(min(gDists), nrow(iucn)) # add distance column to nearest Natura site
  print("Not observed, but possibly present species according to Natura2000") # print statement
  print(iucn) # print dataframe

  file_name <- paste0(file_name, "_Natura.csv") # add file extension
  write.csv(iucn, file_name, row.names = FALSE) # save data frame as csv
}



