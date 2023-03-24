#' Extract GBIF species occurrence data from a polygon and get IUCN Red List status
#'
#' The function takes a spatial polygon object (class "sf") representing the study area and
#' returns a data frame with three columns: scientific name, IUCN Red List status, and
#' residence (either permanent or temporary). The function downloads GBIF data for the species
#' present within the study area or located nearby and determines the associated IUCN status.
#'
#' @param polygon A spatial polygon object (class "sf") representing the study area
#' @param epsg A 4-digit code for the projected coordinate system related to the `polygon` object in meters
#' @param file_name A character string representing the name of the output file
#' @return A data frame with three columns: scientific name, IUCN Red List status, and residence
#'        (either permanent or temporary). The function will return a statement if no species were found.
#'
#' @export
#' @importFrom sf st_as_sf st_buffer st_crs st_transform
#' @importFrom rgbif occ_data
#' @importFrom raster %in% as.data.frame nrow print subset
#' @importFrom wellknown sf_convert
#' @importFrom CoordinateCleaner clean_coordinates
#' @importFrom countrycode countrycode
#' @importFrom dplyr %>% distinct filter group_by mutate n n_distinct ungroup
#' @importFrom ggplot2 geom_sf ggplot
#'
#' @references
#' More information about GBIF can be found at https://www.gbif.org/


Plot_GBIF <- function(polygon, EPSG, file_name){

  ###################################################################
  #PREPARING DATA
  ###################################################################

  # Read in the necessary libraries
  requireNamespace("sf", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("raster", quietly = TRUE)
  requireNamespace("rgbif", quietly = TRUE)
  requireNamespace("wellknown", quietly = TRUE)
  requireNamespace("CoordinateCleaner", quietly = TRUE)
  requireNamespace("countrycode", quietly = TRUE)
  requireNamespace("ggplot2", quietly = TRUE)

  # Remove sf warning
  options(warn = -1)

  # Stop is polygon is not of type 'sf'
  if (!inherits(polygon, "sf")) {
    stop("Input polygon must be an 'sf' object")
  }

  # Set correct CRS to buffer
  polygon <- st_transform(polygon, crs = EPSG)

  # Create buffer of 10 km
  buffer <- st_buffer(polygon, dist = units::as_units(10, "kilometer"), byid = TRUE, nQuadSegs = -1)

  # Put back to correct CRS for GBIF
  buff <- st_transform(buffer, crs = 4326)
  polygon <- st_transform(polygon, crs = 4326)

  # Define kingdoms
  classes <- c(359,
               212,
               11592253,
               6,
               5,
               52,
               42,
               5967481,
               63,
               14,
               67,
               64,
               7188530)

  # Mammalia = 359
  # Aves = 212
  # Squamata = 11592253 - Reptiles
  # Plantae = 6
  # Arthropoda = 54
  # Fungi = 5
  # Mollusca = 52
  # Annelida = 42
  # Nematoda = 5967481
  # Nemertea = 63
  # Tardigrada = 14
  # Acanthocephala = 67
  # Nematomorpha = 64
  # Amphibia = 7188530


  ###################################################################
  #RETREIVING DATA FROM GBIF
  ###################################################################

  # Create an empty dataframe to save the data for each kingdom
  records <- data.frame()

  # Iterate over kingdoms and get species data
  for (class in classes) {
    if (class == 212 || class == 359) {
      wkt_buf <- wellknown::sf_convert(buff)
      species_points <- rgbif::occ_data(geometry = wkt_buf,
                                        classKey = class,
                                        hasCoordinate = TRUE,
                                        year = '2000,2023',
                                        basisOfRecord = c("HUMAN_OBSERVATION",
                                                          "LIVING_SPECIMEN",
                                                          "MATERIAL_CITATION",
                                                          "MATERIAL_SAMPLE",
                                                          "OBSERVATION",
                                                          "PRESERVED_SPECIMEN"))


      # Extract data
      species_pointsHO <- species_points$HUMAN_OBSERVATION$data
      species_pointsLS <- species_points$LIVING_SPECIMEN$data
      species_pointsMC <- species_points$MATERIAL_CITATION$data
      species_pointsMS <- species_points$MATERIAL_SAMPLE$data
      species_pointsOB <- species_points$OBSERVATION$data
      species_pointsPS <- species_points$PRESERVED_SPECIMEN$data

      # Choose columns of interest
      cols <- c("key",
                "scientificName",
                "decimalLatitude",
                "decimalLongitude",
                "acceptedScientificName",
                "taxonRank",
                "taxonKey",
                "iucnRedListCategory",
                "countryCode",
                "datasetKey",
                "kingdom",
                "phylum",
                "class",
                "month",
                "year")

      # Create empty dataframe
      tmp <- data.frame(key=1,
                        scientificName = "name",
                        decimalLatitude = 1,
                        decimalLongitude = "1",
                        acceptedScientificName = "name",
                        taxonRank = "name",
                        taxonKey = 1,
                        iucnRedListCategory = "name",
                        countryCode = "name",
                        datasetKey = "name",
                        kingdom = "name",
                        phylum = "name",
                        class = "name",
                        month = 1,
                        year = 1 )

      #List the dataframes that have data
      species_points_list <-list(species_pointsHO,
                                 species_pointsLS,
                                 species_pointsMC,
                                 species_pointsMS,
                                 species_pointsOB,
                                 species_pointsPS)

      #Extract data to create a new dataframe called tmp
      for(species_points in species_points_list) {
        if(!is.null(species_points)){
          tmp <- rbind (tmp, subset(species_points, select = cols))
        }else{
          tmp <- tmp
        }
      }
      tmp <- tmp[-1,]
      # Remove data from Pl@ntnet
      tmp <- tmp[!tmp$datasetKey %in% c("14d5676a-2c54-4f94-9023-1e8dcd822aa0",
                                        "7a3679ef-5582-4aaa-81f0-8c2545cafc81"), ]
    } else {
      # Create a wkt of the polygon as these are only slow moving species
      wkt_pol <- wellknown::sf_convert(polygon)
      if(class == 6 || class == 5){
        species_points_slow <- rgbif::occ_data(geometry = wkt_pol,
                                               kingdomKey = class,
                                               hasCoordinate = TRUE,
                                               year = '2000,2023',
                                               basisOfRecord = c("HUMAN_OBSERVATION",
                                                                 "LIVING_SPECIMEN",
                                                                 "MATERIAL_CITATION",
                                                                 "MATERIAL_SAMPLE",
                                                                 "OBSERVATION",
                                                                 "PRESERVED_SPECIMEN"))
      }
      if(class == 54 || class == 52 || class == 42 || class == 5967481 || class == 63 ||class == 14 || class == 67 || class == 64){
        species_points_slow <- rgbif::occ_data(geometry = wkt_pol,
                                               phylumKey = class,
                                               hasCoordinate = TRUE,
                                               year = '2000,2023',
                                               basisOfRecord = c("HUMAN_OBSERVATION",
                                                                 "LIVING_SPECIMEN",
                                                                 "MATERIAL_CITATION",
                                                                 "MATERIAL_SAMPLE",
                                                                 "OBSERVATION",
                                                                 "PRESERVED_SPECIMEN"))
      }else{
        species_points_slow <- rgbif::occ_data(geometry = wkt_pol,
                                               classKey = class,
                                               hasCoordinate = TRUE,
                                               year = '2000,2023',
                                               basisOfRecord = c("HUMAN_OBSERVATION",
                                                                 "LIVING_SPECIMEN",
                                                                 "MATERIAL_CITATION",
                                                                 "MATERIAL_SAMPLE",
                                                                 "OBSERVATION",
                                                                 "PRESERVED_SPECIMEN"))
      }
      # Extract data
      species_points_slowHO <- species_points_slow$HUMAN_OBSERVATION$data
      species_points_slowLS <- species_points_slow$LIVING_SPECIMEN$data
      species_points_slowMC <- species_points_slow$MATERIAL_CITATION$data
      species_points_slowMS <- species_points_slow$MATERIAL_SAMPLE$data
      species_points_slowOB <- species_points_slow$OBSERVATION$data
      species_points_slowPS <- species_points_slow$PRESERVED_SPECIMEN$data

      # Identify key columns
      cols_slow <- c("key",
                     "scientificName",
                     "decimalLatitude",
                     "decimalLongitude",
                     "acceptedScientificName",
                     "taxonRank",
                     "taxonKey",
                     "iucnRedListCategory",
                     "countryCode",
                     "datasetKey",
                     "kingdom",
                     "phylum",
                     "class",
                     "month",
                     "year")

      # Create an empty dataframe
      tmp_slow <- data.frame(key=1,
                             scientificName = "name",
                             decimalLatitude = 1,
                             decimalLongitude = "1",
                             acceptedScientificName = "name",
                             taxonRank = "name",
                             taxonKey = 1,
                             iucnRedListCategory = "name",
                             countryCode = "name",
                             datasetKey = "name",
                             kingdom = "name",
                             phylum = "name",
                             class = "name",
                             month = 1,
                             year = 1 )

      #List the dataframes that have data
      species_points_list_slow <- list(species_points_slowHO,
                                       species_points_slowLS,
                                       species_points_slowMC,
                                       species_points_slowMS,
                                       species_points_slowOB,
                                       species_points_slowPS)

      # Extract data to create a new dataframe called tmp_slow
      for(species_points_slow in species_points_list_slow) {
        if(!is.null(species_points_slow)){
          tmp_slow <- rbind (tmp_slow, subset(species_points_slow, select = cols_slow))
        }else{
          tmp_slow <- tmp_slow
        }
      }
      tmp_slow <- tmp_slow[-1,]
      # Remove data from Pl@ntnet
      tmp <- tmp_slow[!tmp_slow$datasetKey %in% c("14d5676a-2c54-4f94-9023-1e8dcd822aa0",
                                                  "7a3679ef-5582-4aaa-81f0-8c2545cafc81"), ]
    }
    records <- rbind(records, tmp) # bind data from different kingdoms
  }
  # Combine the data from fast and slow moving species and remove empty rows
  records <- records[records$key != 1, ]

  ###################################################################
  # CLEANING THE DATA WHEN THERE IS DATA
  ###################################################################

  if(nrow(records) > 1) {

    #----------------------------------------------------------------------

    # Remove records w. more than 50 taxon having exactly the same coordinates
    records <- records %>%
      dplyr::mutate(latLon = paste0(decimalLatitude, "_", decimalLongitude)) %>%
      dplyr::group_by(latLon) %>%
      dplyr::filter(dplyr::n() <= 50) %>%
      dplyr::ungroup()

    #----------------------------------------------------------------------

    # Use the coordinate cleaner package to clean the coordinates
    records_prep <- records %>%
      dplyr::mutate(countryCode = countrycode::countrycode(countryCode,
                                                           origin = 'iso2c',
                                                           destination = 'iso3c'),
                    decimalLatitude = as.numeric(decimalLatitude),
                    decimalLongitude = as.numeric(decimalLongitude))

    cleaned_records <- suppressWarnings(CoordinateCleaner::clean_coordinates(x = records_prep,
                                                                             lon = "decimalLongitude",
                                                                             lat = "decimalLatitude",
                                                                             countries = "countryCode",
                                                                             species = "scientificName",
                                                                             tests = c(#"centroids",
                                                                               "capitals",
                                                                               "country",
                                                                               "equal",
                                                                               "gbif",
                                                                               "institutions",
                                                                               "zeros",
                                                                               "duplicates"),
                                                                             value = "clean",
                                                                             verbose = TRUE,
                                                                             report = TRUE))

    #Remove NAs
    cleaned_records_fin <- na.omit(cleaned_records)

    #----------------------------------------------------------------------

    # Define the resident types - Temporary or permanent based on the number of months
    DT_res <-  cleaned_records_fin %>%
      dplyr::group_by(scientificName) %>%
      dplyr::mutate(No_month = ifelse(class == "Mammalia" || class == "Aves", dplyr::n_distinct(month), 12),
                    Resident = ifelse(No_month <= 4, "Seasonal/Temporary", "Yearly/Permanent"))

    #----------------------------------------------------------------------

    # Make spatial
    species_sf <- st_as_sf(DT_res, coords = c("decimalLongitude","decimalLatitude"),
                           crs = 4326, # projection, this is NAD83
                           remove = FALSE)
    # Keep the species inside the polygon
    inpoly <- species_sf[polygon,]

    ###################################################################
    # FIND SPECIES NEARBY WHEN THEY ARE NOT IN THE POLYGON
    ###################################################################
    if (nrow(inpoly) == 0) {

      # Plot the polygon and points together
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = buff, fill = "red", alpha = 0.5) +
        ggplot2::geom_sf(data = polygon, fill = "white", alpha = 0.5) +
        ggplot2::geom_sf(data = species_sf, color = "blue", size = 1)

      # Keep columns of interest and only distinct species
      cr <- as.data.frame(species_sf[, c("scientificName", "iucnRedListCategory", "class", "Resident")])

      cr <- cr %>%
        dplyr::distinct(scientificName, iucnRedListCategory, class, Resident, .keep_all = TRUE)

      # Print out the results and save them in the environment
      print("No species detected inside the polygon, but there were some nearby:")
      print(cr)
      cr <<- as.data.frame(cr)

      file_name <- paste0(file_name, "_nearby.csv") # add file extension
      write.csv(cr, file_name, row.names = FALSE) # save data frame as csv

    }else{
      ###################################################################
      # KEEP SPECIES THAT ARE INSIDE THE POLYGON WHEN THERE
      ###################################################################

      # Plot the polygon and points together
      ggplot2::ggplot() +
        ggplot2::geom_sf(data = polygon, fill = "white", alpha = 0.5) +
        ggplot2::geom_sf(data = inpoly, color = "blue", size = 1)

      # Keep columns of interest and only distinct species
      cr <- as.data.frame(inpoly[, c("scientificName", "iucnRedListCategory", "class", "Resident")])

      cr <- cr %>%
        dplyr::distinct(scientificName, iucnRedListCategory, class, Resident, .keep_all = TRUE)

      # Print out the results and save them in the environment
      print("Species observed in the plot")
      print(cr)
      cr <<- as.data.frame(cr)

      file_name <- paste0(file_name, ".csv") # add file extension
      write.csv(cr, file_name, row.names = FALSE) # save data frame as csv

    }
  }else{
    # Output species names and no per IUCN category
    print("No species detected inside or near the polygon.")
  }
}




