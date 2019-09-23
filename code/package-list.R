packages <- c("reldist","dplyr","data.table","reshape","reshape2","stringr","caret","zoo","tidyr","readr","readxl","scales","ggplot2","wesanderson","plm","imputeTS",
              "glmnet","ggplot2","latex2exp","missMDA","bcv","parallel","doParallel","boot","tidyverse","mgcv")

map.packages <- c("rgdal","rgeos","maptools","ggmap","sp","ifultools","broom","raster")

weights <- c("cluster","HMisc","weights") # install cluster -> HMisc -> weights

install.packages(c(packages,weights,map.packages))