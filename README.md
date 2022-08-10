# Readme

## Installation of the package

### Installing R

#### Installing RStudio

Install RStudio from
<https://www.rstudio.com/products/rstudio/download/>

### Load Package

Load the package from Github with

```r
library(remotes)
install_github("edgarsit/dnv")

library(dnv)
```

or from the downloaded directory with

```r
library(remotes)
install_local("path/to/directory")
library(dnv)
```

If you have outdated libraries installed, R might prompt you to update them. You can choose to update them at this time or update them later.

### Use the library

Generate the inital graph by calling `table.to.graph`. Then adjust positions with `graph.to.positions`. Finally, generate the set of images with `generate.images`.

```r
data("AE_relationships")
aeList <- table.to.graph(AE_relationships, "Subject", "cycle")[[1]]
allG <- table.to.graph(AE_relationships, "Subject", "cycle")[[2]]
positions = graph.to.positions(allG, 1920, 1080)
```

Adjust the postions here

```r
dir.create("./results", recursive = TRUE, showWarnings = FALSE)
generate.images(positions, aeList, allG, "Neuro", "mixed", 129, 45, "./results")
```
