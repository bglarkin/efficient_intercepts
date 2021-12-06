Effect of sampling date on cover estimate
================
Beau Larkin
2021-12-06

-   [Description](#description)
-   [Resources](#resources)
    -   [Packages, libraries, and
        functions](#packages-libraries-and-functions)
    -   [API keys](#api-keys)
    -   [Global functions and styles:
        `theme_bgl`](#global-functions-and-styles-theme_bgl)
    -   [Data](#data)

# Description

This is an addendum to the vegetation sampling guidance report produced
in early 2021. The purpose here is to produce some graphics and
supporting summaries about how vegetation cover changes over the course
of a sesason, and how we can handle, reduce, or otherwise manage it with
surveys at MPG Ranch.

# Resources

## Packages, libraries, and functions

Packages and multiple data sources must be added to the local
environment before knitting this notebook.

``` r
# Quick-loading resources
packages_needed = c("tidyverse", "knitr", "rjson", "plotrix", "colorspace", "devtools")
packages_installed = packages_needed %in% rownames(installed.packages())
if (any(!packages_installed))
  install.packages(packages_needed[!packages_installed])
for (i in 1:length(packages_needed)) {
  library(packages_needed[i], character.only = T)
}
```

``` r
# Big R Query
# ggmap package installed from GitHub using `devtools` (not shown)
packages_needed = c("bigrquery", "ggmap") # comma delimited vector of package names
packages_installed = packages_needed %in% rownames(installed.packages())
if (any(!packages_installed))
  install.packages(packages_needed[!packages_installed])
for (i in 1:length(packages_needed)) {
  library(packages_needed[i], character.only = T)
}
```

## API keys

API keys for data access are pulled from local resources and are not
available in the hosted environment. Code not shown here.

## Global functions and styles: `theme_bgl`

``` r
# Load text file from local working directory
source(paste0(getwd(), "/styles.txt"))
```

## Data

Survey metadata

Plant species and cover data from point-intercept surveys in 2011-12,
2016, and 2021. Plant data are joined with survey metadata to filter the
data to the survey periods with the greatest effort (2011-12, 2016, and
2021). Data are simplified and summarized to show sums of cover in plant
functional groups at each date of annual surveys.
