# Railroads, 1826-1911

# Packages
require(ggplot2)
require(rgdal)
require(rgeos)
require(maptools)
require(ggmap)
require(sp)
require(spdep)
require(ifultools)
require(broom)
library(raster)

download.data <- FALSE

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
map.drop <- c("Alaska Department","Alaska District","Hawaii Annexation","Hawaii Territory")
county.map <- county.map[!county.map$STATE_TERR %in% map.drop ,]

# Convert to df
county.f <- tidy(county.map,region="ID_NUM")

# Merge back info
county.f <- merge(county.f,county.map@data, by.x = "id", by.y = "ID_NUM")

# Make county names proper
county.f$id <- properCase(as.character(county.f$id))

# Download historical railroads data
if(download.data){
setwd(paste0(data.directory))
url <- "https://my.vanderbilt.edu/jeremyatack/files/2016/05/RR1826-1911Modified0509161.zip"
rr.data <- basename(url)
download.file(url,rr.data)
unzip(rr.data)
}

# Load rr data
setwd(paste0(data.directory,"RR1826-1911Modified0509161")) # set directory to rr files
county.rr <- readOGR(dsn = ".", "RR1826-1911Modified050916")

county.rr <- spTransform(county.rr, CRS("+proj=longlat +datum=WGS84"))  # transform CRS

county.rr@data$id <- seq(0, nrow(county.rr@data)-1,1)

# Subset RR in operation by 1911
county.rr <- county.rr[county.rr$InOpBy <= 1911,]

# Convert to df
county.r <- tidy(county.rr, region="id")

# Merge back info
rr.df <- merge(county.r,county.rr@data, by="id")

# Plot railroad

# 1911
rr.1911 <- ggplot() + 
  geom_polygon(data = county.f, aes(x = long, y = lat, group = group), fill = 'grey', color = "grey", size = 0.25, alpha = .75) +
  geom_point(data = rr.df, aes(x = long, y = lat, group=group), color = "red", size = 0.35) +
  ggtitle("1911") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) 

ggsave(paste0(results.directory,"plots/rr-1911.png"), rr.1911, width=11, height=8.5) 


# 1862 (1911 county borders)
rr.1862 <- ggplot() + 
  geom_polygon(data = county.f, aes(x = long, y = lat, group = group), fill = 'grey', color = "grey", size = 0.25, alpha = .75) +
  geom_point(data = rr.df[rr.df$InOpBy<=1862,], aes(x = long, y = lat, group=group), color = "red", size = 0.35) +
  ggtitle("1862 (1911 county borders)") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(),axis.title.x = element_blank(),
        axis.title.y = element_blank()) + theme(panel.grid.minor=element_blank(), 
                                                panel.grid.major=element_blank()) 

ggsave(paste0(results.directory,"plots/rr-1862.png"), rr.1862, width=11, height=8.5) 

# Intersect lines with polygons

rr.inter.1911 <- gIntersects(county.rr[county.rr$InOpBy == 1911,], county.map, byid=TRUE)

rr.inter.1911 <- data.frame("access"=apply(rr.inter.1911, 1, any)) # collapse to county IDs (y-axis)
rr.inter.1911$access[rr.inter.1911$access=="FALSE"] <- 0
rr.inter.1911$access[rr.inter.1911$access=="TRUE"] <- 1

# merge back county info
rr.inter.1911$id <- as.numeric(row.names(rr.inter.1911)) +1

rr.inter.1911 <- merge(rr.inter.1911, county.map@data, by.x="id", by.y="ID_NUM")

