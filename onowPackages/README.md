# ONOW Extended Metric Functions
includes derived attributes, keyword extraction & categorization
TODO: text mining + categorization

## to create packages:
```r
library(roxygen2); library(devtools)
getwd()  # the working directory
# setwd()
dir()
devtools::create("onowPackages")  # creates package directories & files
# then save script w/ function in directory
devtools::document(paste0(normalizePath(getwd()), "\\onowPackages\\R"))  # then document
# install("onowPackages")  # only run once
library(onowPackages)

# after package successfully created & documented, should be able to see...
onowPackages::em_derived_attributes()
?em_keywords
```
