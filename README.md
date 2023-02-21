
# PlotRichness

### Automated retrieval and cleaning of species data in R

**PlotRichness** provides functions to quickly automatically retrieve
species information at the plot level from open-source databases like
the Global Biodiversity Information Facility (GBIF), Natura2000 and
SpeciesLink. It’s main goal is to fill the gap of extracting data from
open-source databases in an easy manner. Additionally it cleans the data
on the plot level. It makes use of the rgbif package to get species
information and their associated International Union for Conservation of
Nature and Natural Resources (IUCN). In addition, it uses
freely-available shape- and csv-files to get more specific species data
for Europe (e.g. Natura2000) and Brazil (SpeciesLink).

The main user relevant functions are:

-   `Plot_GBIF` - Retrieve species data for a plot from GBIF.
-   `Plot_Natura2000` - Retrieve species data for Europe plots
    (additional to GBIF)

Objects from the following spatial classes are supported:

-   \[sf\] (<https://cran.r-project.org/package=sf>)

## Installation

To install the development version of PlotRichness you can install the
[remotes](https://cran.r-project.org/package=remotes) package. It can be
installed like this:

``` r
remotes::install_github("shayavh/PlotRichness")
```

## Usage

The most basic calls are:

``` r
Plot_GBIF(polygon, 3301, "file_name") # 3301 is the EPSG for Estonia - change according to shapefile location
Plot_Natura2000(polygon, Natura, species, "file_name")
```

## Necessary Data for **Plot_Natura2000** function

Natura 2000 files can be found at:

<https://cmshare.eea.europa.eu/s/HHGPnNsjqq5BdEa/download>

<https://www.eea.europa.eu/data-and-maps/data/natura-11/natura-2000-tabular-data-12-tables/natura-2000-comma-separated-values-files/at_download/file>

After download you should have:

-   *Natura2000_end2021_rev1_epsg3035.shp* - Which will function as the
    **Natura** object in the **Plot_Natura2000** function.
-   *Natura2000_end2019_SPECIES.csv* - Which will function as the
    **species** object in the **Plot_Natura2000** function.

## How to cite

Shaya van Houdt, 2023. Automated retrieval and cleaning of species
information from open-source databases.
<https://github.com/shayavh/PlotRichness>

## Contact

Please file bug reports and feature requests at
<https://github.com/shayavh/PlotRichness/issues>
