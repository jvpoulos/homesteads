#####################################
### Map gini coefficients by decade      ###
#####################################

# Packages
require(ggplot2)
require(rgdal)
require(rgeos)
require(maptools)
require(ggmap)
require(sp)
require(spdep)
require(ifultools)
require(tidyr)
require(broom)
require(maps)

download.data <- FALSE

patient <- TRUE

data.directory <-"~/Dropbox/github/land-reform/data/"

# Download U.S. historical county map data
if(download.data){
  setwd(data.directory)
  url <- "http://publications.newberry.org/ahcbp/downloads/gis/US_AtlasHCB_Counties_Gen05.zip"
  map.data <- basename(url)
  download.file(url,map.data)
  unzip(map.data)
}

# Load map data
setwd(paste0(data.directory,"US_AtlasHCB_Counties_Gen05/US_HistCounties_Gen05_Shapefile")) # set directory to map files
county.map <- readOGR(dsn = ".", "US_HistCounties_Gen05")

county.map <- spTransform(county.map, CRS("+proj=longlat +datum=WGS84"))  # transform CRS

# Subset to counties as of 12/31/1911
county.map <- county.map[county.map$START_N<=19111231 & county.map$END_N >= 19111231,]

# Keep continental U.S.
map.drop <- c("Alaska","Alaska Department","Alaska District", "Alaska Territory", "Hawaii", "Hawaii Annexation","Hawaii Territory")
county.map <- county.map[!county.map$STATE_TERR %in% map.drop ,]
county.map$STATE_TERR <- droplevels(county.map$STATE_TERR)

# Convert to df
county.f <- tidy(county.map,region="ID_NUM")

# Merge back info
county.f <- merge(county.f,county.map@data, by.x = "id", by.y = "ID_NUM")

county.f$FIPS <- as.numeric(as.character(county.f$FIPS))

# get state data
state_map <- map_data("state")

state_map$state_code <- setNames(state.abb, toupper(state.name))[toupper(state_map$region)] 

state_map$aland.gini <- 0

state_map$southern.pub <- ifelse(state_map$state_code %in% southern.pub, 1, 0)
state_map$western.pub <- ifelse(state_map$state_code %in% western.pub, 1, 0)

# 1870

county.70 <- merge(census.ts[census.ts$year==1870,],
                   county.f, by.x="fips", by.y = "FIPS", all.y=TRUE) 

gini.70 <- ggplot(county.60, aes(long, lat, group = group, fill = aland.gini)) + geom_polygon() + 
  coord_equal()  + scale_fill_gradient(low = "#ffcccc", high = "#ff0000", na.value = "white") + labs(fill="Adjusted land inequality in 1870") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_map(aes(map_id = id, colour = 'black'), map = county.f) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") +
  geom_path( data = state_map[state_map$western.pub==1,] , colour = "purple", size=1.2) +
  geom_path( data = state_map[state_map$southern.pub==1,] , colour = "green", size=1.2)

ggsave(paste0(results.directory,"plots/gini-1870.png"), gini.70, width=11, height=8.5) 

# 1900

county.00 <-merge(census.ts[census.ts$year==1900,],
                  county.f, by.x="fips", by.y = "FIPS", all.y=TRUE) 

homestead.00 <- ggplot(county.00, aes(long, lat, group = group, fill = aland.gini)) + geom_polygon() + 
  coord_equal()  + scale_fill_gradient(low = "#ffcccc", high = "#ff0000", na.value = "white") + labs(fill="Adjusted land inequality in 1900") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_map(aes(map_id = id, colour = 'black'), map = county.f) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") +
  geom_path( data = state_map[state_map$western.pub==1,] , colour = "purple", size=1.2) +
  geom_path( data = state_map[state_map$southern.pub==1,] , colour = "green", size=1.2)

ggsave(paste0(results.directory,"plots/gini-1900.png"), homestead.00, width=11, height=8.5) 

# Plot all decennials for gif

if(patient){
  for(t in seq(1860,1950,10)){
    county.t <- merge(census.ts[census.ts$year==t,],
                      county.f, by.x="fips", by.y = "FIPS", all.y=TRUE) 
    
    gini.t <- ggplot(county.t, aes(long, lat, group = group, fill = aland.gini)) + geom_polygon() + 
      coord_equal()  + scale_fill_gradient(low = "#ffcccc", high = "#ff0000", na.value = "white") + labs(fill=paste("Adjusted land inequality in",t)) + 
      theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
            axis.text.y = element_blank(),axis.title.x = element_blank(),
            axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                    panel.grid.major=element_blank()) +
      #  ggtitle(paste("Log per-capita cumulative homesteads in",t)) +
      theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),
            plot.title = element_text(hjust = 0.5)) + #rm margins
      geom_map(aes(map_id = id, colour = 'black'), map = county.f) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") +
      geom_path( data = state_map[state_map$western.pub==1,] , colour = "purple", size=1.2) +
      geom_path( data = state_map[state_map$southern.pub==1,] , colour = "green", size=1.2)
    
    ggsave(paste0(results.directory,"plots/gini-plots/", t, '.png'), gini.t, width=11, height=8.5) 
  }
}
# in Bash
# convert -delay 300 -loop 0 *.png gini.gif