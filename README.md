
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
Plot_GBIF(polygon, 3301, "file_name")
Plot_Natura2000(polygon, Natura, species, "file_name")
```

## How to cite

Package PlotRichness can be cited as: Shaya van Houdt, 2023. Automated
retrieval and cleaning of species information from open-source
databases.

## Contact

Please file bug reports and feature requests at
<https://github.com/shayavh/PlotRichness/issues>
