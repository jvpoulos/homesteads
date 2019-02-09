#####################################
### Map homesteads by decade      ###
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

## Merge patents data

patents.d <- as.data.frame((patents.decennial))

patents.d$state <- setNames(state.name, state.abb)[patents.d$state_code] 

# get fips
fips.codes <- data.frame(read_excel(paste0(lr.data.directory,'US_FIPS_Codes.xls'), skip = 1))
fips.codes$FIPS.State <- as.numeric(fips.codes$FIPS.State)
fips.codes$FIPS.County <- as.numeric(fips.codes$FIPS.County)

fips.codes <- fips.codes[!duplicated(fips.codes$FIPS.State),][c("State","FIPS.State")]

patents.d <- merge(patents.d, fips.codes, by.x =c("state"), by.y=c("State"), all.x=TRUE)

#colnames(patents.d)[9] <- 'county.fips'

patents.d$fips <- as.numeric(patents.d$FIPS.State)*1000 + patents.d$county_code/10

# get state data
state_map <- map_data("state")

state_map$state_code <- setNames(state.abb, toupper(state.name))[toupper(state_map$region)] 

state_map$homesteads.pc <- 0

state_map$southern.pub <- ifelse(state_map$state_code %in% southern.pub, 1, 0)
state_map$western.pub <- ifelse(state_map$state_code %in% western.pub, 1, 0)

# 1870

county.70 <- merge(patents.d[patents.d$year2==1870,],
                   county.f, by.x="fips", by.y = "FIPS", all.y=TRUE) 

homestead.70 <- ggplot(county.70, aes(long, lat, group = group, fill = homesteads.pc)) + geom_polygon() + 
  coord_equal()  + scale_fill_gradient(low = "#ffcccc", high = "#ff0000", na.value = "white") + labs(fill="Log per-capita cumulative homesteads in 1870") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) +
# ggtitle("Log per-capita cumulative homesteads in 1870") +
 theme(plot.title = element_text(hjust = 0.5)) +
  geom_map(aes(map_id = id, colour = 'black'), map = county.f) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") +
geom_path( data = state_map[state_map$western.pub==1,] , colour = "purple", size=1.2) +
  geom_path( data = state_map[state_map$southern.pub==1,] , colour = "green", size=1.2)

ggsave(paste0(results.directory,"plots/homestead-1870.png"), homestead.70, width=11, height=8.5) 

# 1900

county.00 <- merge(patents.d[patents.d$year2==1900,],
                   county.f, by.x="fips", by.y = "FIPS", all.y=TRUE) 

homestead.00 <- ggplot(county.00, aes(long, lat, group = group, fill = homesteads.pc)) + geom_polygon() + 
  coord_equal()  + scale_fill_gradient(low = "#ffcccc", high = "#ff0000", na.value = "white") + labs(fill="Log per-capita cumulative homesteads in 1900") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) +
#  ggtitle("Log per-capita cumulative homesteads in 1900") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_map(aes(map_id = id, colour = 'black'), map = county.f) + scale_colour_manual(values=c('black'),guide=FALSE) + theme(legend.position="top") +
  geom_path( data = state_map[state_map$western.pub==1,] , colour = "purple", size=1.2) +
  geom_path( data = state_map[state_map$southern.pub==1,] , colour = "green", size=1.2)

ggsave(paste0(results.directory,"plots/homestead-1900.png"), homestead.00, width=11, height=8.5) 

# Plot all decennials for gif

if(patient){
  for(t in sort(unique(patents.d$year2))){
    county.t <- merge(patents.d[patents.d$year2==t,],
                       county.f, by.x="fips", by.y = "FIPS", all.y=TRUE) 
    
    homestead.t <- ggplot(county.t, aes(long, lat, group = group, fill = homesteads.pc)) + geom_polygon() + 
      coord_equal()  + scale_fill_gradient(low = "#ffcccc", high = "#ff0000", na.value = "white") + labs(fill=paste("Log per-capita cumulative homesteads in",t)) + 
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
    
    ggsave(paste0(results.directory,"plots/homestead-plots/", t, '.png'), homestead.t, width=11, height=8.5) 
  }
}
# in Bash
# convert -delay 300 -loop 0 *.png homesteads.gif