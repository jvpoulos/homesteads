packages <- c("reldist","dplyr","data.table","reshape","reshape2","stringr","caret","zoo","tidyr","readr","readxl","scales","ggplot2","wesanderson",
              "glmnet","ggplot2","latex2exp","bcv","parallel","doParallel","boot","tidyverse","mgcv","imputeTS","mice","missForest","VIM","plm",
              "devtools","gridExtra","grid","ggpubr","Matrix")

map.packages <- c("rgdal","rgeos","maptools","ggmap","sp","ifultools","broom","raster")

weights <- c("cluster","HMisc","weights") # install cluster -> HMisc -> weights

install.packages(c(packages,weights,map.packages),repos = "http://cran.us.r-project.org")

library(devtools) 
install_github("jvpoulos/MCPanel") # propensity weighting of objective fn.; ADH clips gradients