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
install_github("edgarsit/test.package")
library(test.package)
```

or from the downloaded directory with

```r
library(remotes)
install_local("path/to/directory")
library(test.package)
```

If you have outdated libraries installed, R might prompt you to update them. You can choose to update them at this time or update them later.

### Use the library

Generate the inital graph by calling `table.to.graph`. Then adjust positions with `graph.to.positions`. Finally, generate the set of images with `generate.images`.

```r
data("AE_relationships")
aeList <- table.to.graph(AE_relationships, "Subject", "cycle")[[1]]
allG <- table.to.graph(AE_relationships, "Subject", "cycle")[[2]]
positions = graph.to.positions(allG, outputWidth, outputHeight)
```

Adjust the postions here

```r
generate.images(positions, aeList, allG, "Neuro", "mixed", 129, 45, "./results")
```
