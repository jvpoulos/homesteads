packages <- c("reldist","dplyr","data.table","reshape","reshape2","stringr","caret","zoo","tidyr","readr","readxl","scales","ggplot2","wesanderson",
              "glmnet","ggplot2","latex2exp","bcv","parallel","doParallel","boot","tidyverse","mgcv","imputeTS","mice","missForest","VIM","mtsdi",
              "devtools","gridExtra","grid","ggpubr","Matrix")

map.packages <- c("rgdal","rgeos","maptools","ggmap","sp","ifultools","raster")

weights <- c("cluster","HMisc","weights") # install cluster -> HMisc -> weights

install.packages(c(packages,weights,map.packages),repos = "http://cran.us.r-project.org")

# Retired map packages (2023)
if("rgdal" %in% rownames(installed.packages()) == FALSE) {
  remotes::install_url("https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.6-7.tar.gz", type="source")
}

if("rgeos" %in% rownames(installed.packages()) == FALSE) {
  remotes::install_url("https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.6-4.tar.gz", type="source")
}

if("maptools" %in% rownames(installed.packages()) == FALSE) {
  remotes::install_url("https://cran.r-project.org/src/contrib/Archive/maptools/maptools_1.1-8.tar.gz", type="source")
}

if("ifultools" %in% rownames(installed.packages()) == FALSE) {
  remotes::install_url("https://cran.r-project.org/src/contrib/Archive/ifultools/ifultools_2.0-26.tar.gz", type="source")
}

remotes::install_url("https://cran.r-project.org/src/contrib/Archive/broom/broom_1.0.4.tar.gz", type="source")

library(devtools) 
install_github("jvpoulos/MCPanel") # propensity weighting of objective fn.; ADH clips gradients

options(repos = c(
  skranz = 'https://skranz.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
install.packages('ParallelTrendsPlot')

# doMPI
doMPI <- TRUE
if(doMPI){
  install.packages("Rmpi")
  install.packages("doMPI", dependencies=TRUE, repos = "http://cran.us.r-project.org")
}