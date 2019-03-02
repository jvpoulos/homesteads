# Railroads, 1826-1911

# Packages
library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
library(ggmap)
library(sp)
library(ifultools)
library(broom)
library(raster)
library(reshape2)
library(tidyr)
library(dplyr)
library(caret)

# download.data <- FALSE
# 
# data.directory <-"~/Dropbox/github/land-reform/data/"
# 
# # Download U.S. historical county map data
# if(download.data){
# setwd(data.directory)
# url <- "http://publications.newberry.org/ahcbp/downloads/gis/US_AtlasHCB_Counties_Gen05.zip"
# map.data <- basename(url)
# download.file(url,map.data)
# unzip(map.data)
# }
# 
# # Load map data
# setwd(paste0(data.directory,"US_AtlasHCB_Counties_Gen05/US_HistCounties_Gen05_Shapefile")) # set directory to map files
# county.map <- readOGR(dsn = ".", "US_HistCounties_Gen05")
# 
# county.map <- spTransform(county.map, CRS("+proj=longlat +datum=WGS84"))  # transform CRS
# 
# # Subset to counties as of 12/31/1911
# county.map <- county.map[county.map$START_N<=19111231 & county.map$END_N >= 19111231,]
# 
# # Keep continental U.S.
# map.drop <- c("Alaska Department","Alaska District","Hawaii Annexation","Hawaii Territory")
# county.map <- county.map[!county.map$STATE_TERR %in% map.drop ,]
# 
# # Convert to df
# county.f <- tidy(county.map,region="ID_NUM")
# 
# # Merge back info
# county.f <- merge(county.f,county.map@data, by.x = "id", by.y = "ID_NUM")
# 
# # Make county names proper
# county.f$id <- properCase(as.character(county.f$id))
# 
# # Download historical railroads data
# if(download.data){
# setwd(paste0(data.directory))
# url <- "https://my.vanderbilt.edu/jeremyatack/files/2016/05/RR1826-1911Modified0509161.zip"
# rr.data <- basename(url)
# download.file(url,rr.data)
# unzip(rr.data)
# }
# 
# # Load rr data
# setwd(paste0(data.directory,"RR1826-1911Modified0509161")) # set directory to rr files
# county.rr <- readOGR(dsn = ".", "RR1826-1911Modified050916")
# 
# county.rr <- spTransform(county.rr, CRS("+proj=longlat +datum=WGS84"))  # transform CRS
# 
# county.rr@data$id <- seq(0, nrow(county.rr@data)-1,1)
# 
# # Subset RR in operation by 1911
# county.rr <- county.rr[county.rr$InOpBy <= 1911,]
# 
# # Convert to df
# county.r <- tidy(county.rr, region="id")
# 
# # Merge back info
# rr.df <- merge(county.r,county.rr@data, by="id")
# 
# # Plot railroad
# 
# # 1911
# rr.1911 <- ggplot() + 
#   geom_polygon(data = county.f, aes(x = long, y = lat, group = group), fill = 'grey', color = "grey", size = 0.25, alpha = .75) +
#   geom_point(data = rr.df, aes(x = long, y = lat, group=group), color = "red", size = 0.35) +
#   ggtitle("1911") +
#   theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
#         axis.text.y = element_blank(),axis.title.x = element_blank(),
#         axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
#                                                 panel.grid.major=element_blank()) 
# 
# ggsave(paste0(results.directory,"plots/rr-1911.png"), rr.1911, width=11, height=8.5) 
# 
# 
# # 1862 (1911 county borders)
# rr.1862 <- ggplot() + 
#   geom_polygon(data = county.f, aes(x = long, y = lat, group = group), fill = 'grey', color = "grey", size = 0.25, alpha = .75) +
#   geom_point(data = rr.df[rr.df$InOpBy<=1862,], aes(x = long, y = lat, group=group), color = "red", size = 0.35) +
#   ggtitle("1862 (1911 county borders)") +
#   theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
#         axis.text.y = element_blank(),axis.title.x = element_blank(),
#         axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
#                                                 panel.grid.major=element_blank()) 
# 
# ggsave(paste0(results.directory,"plots/rr-1862.png"), rr.1862, width=11, height=8.5) 

################### Load railroads.RData

#load("/media/jason/Dropbox/github/land-reform/data/railroads.RData")

county.rr <- readRDS(paste0(data.directory,"county_rr.rds"))
county.map <- readRDS(paste0(data.directory,"county_map.rds"))

detach("package:raster", unload=TRUE) # avoid dplyr select error

# Intersect lines with polygons

ID <- over( county.rr , county.map )

county.rr.data <- cbind( county.rr@data , ID )

rr.inter <- county.rr.data[!is.na(county.rr.data$ID_NUM), ] # county-rail observations # drop 363 w/o county info

rr.inter <- rr.inter %>% 
  group_by(ID_NUM,InOpBy) %>% 
  summarise_each(funs(sum(., na.rm = TRUE)),track) %>% # county/year sums of track
  dplyr::select(ID_NUM,InOpBy,track)

# Get intersections for all years

rr.inter.m <- dcast(rr.inter, InOpBy~ID_NUM)

rr.inter.m[is.na(rr.inter.m)] <- 0

rr.inter.m <- melt(rr.inter.m, id.vars = c("InOpBy"),
            variable.name = "ID_NUM",
            value.name = "track")

colnames(rr.inter.m) <- c("InOpBy", "ID_NUM", "track")

rr.inter.m <- merge(rr.inter.m, county.map@data, by="ID_NUM") # merge back county info

rr.inter.m$state <- state.abb[match(rr.inter.m$STATE_TERR,state.name)] # state abbr

# Create access measures - county level
rr.inter.m.county <- rr.inter.m %>% 
  filter(!is.na(state)) %>% # rm DC & territories
  filter(InOpBy <= 1868) %>% # rm post-treatment
  arrange(FIPS,InOpBy)  %>% # sort by county/year
  group_by(FIPS) %>% 
  mutate(cumulative.track = cumsum(track), # cumulative sum of track miles by county
         track2 = cumulative.track/AREA_SQMI) %>% # cumulative track miles per square mile
  dplyr::select(InOpBy,FIPS,cumulative.track,track2,state,AREA_SQMI)

rr.inter.m.county <- rr.inter.m.county[!duplicated(rr.inter.m.county[c("InOpBy","FIPS")]),] # keep one county-year obs

# Create access measures - state level
rr.inter.m.state <- rr.inter.m %>% 
  filter(!is.na(state)) %>% # rm DC & territories
  filter(InOpBy <= 1868) %>% # rm post-treatment
  arrange(state,InOpBy)  %>% # sort by state/year
  group_by(state) %>% 
  mutate(cumulative.track = cumsum(track), # cumulative sum of track miles
         track2 = cumulative.track/AREA_SQMI) %>% # cumulative track miles per square mile
  dplyr::select(InOpBy,FIPS,cumulative.track,track2,state,AREA_SQMI)

rr.inter.m.state <- rr.inter.m.state[!duplicated(rr.inter.m.state[c("InOpBy","state")]),] # keep one state-year obs

rr.inter.m.state <- as.data.frame(rr.inter.m.state)

rr.inter.m.state$year <- rr.inter.m.state$InOpBy