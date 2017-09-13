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
library(reshape2)
library(tidyr)
library(dplyr)
require(caret)

download.data <- FALSE

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


################### Load railraods.RData
# Intersect lines with polygons

ID <- over( county.rr , county.map )

county.rr.data <- cbind( county.rr@data , ID )

rr.inter <- county.rr.data[!is.na(county.rr.data$ID_NUM), ] # county-rail observations # drop 363 w/o county info

rr.inter <- rr.inter %>% 
  group_by(ID_NUM,InOpBy) %>% 
  summarise_each(funs(sum(., na.rm = TRUE)),track) %>% # county/year sums of track
  select(ID_NUM,InOpBy,track)

# Get intersections for all years

rr.inter.m <- dcast(rr.inter, InOpBy~ID_NUM)

rr.inter.m[is.na(rr.inter.m)] <- 0

rr.inter.m <- melt(rr.inter.m, id.vars = c("InOpBy"),
            variable.name = "ID_NUM",
            value.name = "track")

colnames(rr.inter.m) <- c("InOpBy", "ID_NUM", "track")

rr.inter.m <- merge(rr.inter.m, county.map@data, by="ID_NUM") # merge back county info

rr.inter.m$state <- state.abb[match(rr.inter.m$STATE_TERR,state.name)] # state abbr

# Create cumulative miles per sq mile measure
rr.inter.m <- rr.inter.m %>% 
  filter(!is.na(state)) %>% # rm DC & territories
  arrange(ID_NUM,InOpBy)  %>% # sort by county/year
  group_by(ID_NUM) %>% 
  mutate(cumulative.track = cumsum(track), # cumulative sum of track miles by county
         track2 = cumulative.track/AREA_SQMI) %>% # cumulative track miles per square mile
  select(ID_NUM,InOpBy,FIPS,cumulative.track,track2,state,AREA_SQMI)

## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# features are southern state land states

# Summarize by category

rr.1 <- rr.inter.m
  
rr.1$cat <- NA
rr.1$cat[rr.1$state %in% southern.pub] <- "Treated"
rr.1$cat[rr.1$state %in% southern.state] <- "Control"

rr.1$year <- rr.1$InOpBy

# Create control and treated sums
cats.rr.1 <- rr.1 %>% 
  select(year, cat, track2)  %>%
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

cats.rr.1 <- select(cats.rr.1, -ID_NUM)

cats.rr.1.r <- reshape(data.frame(cats.rr.1), idvar = "year", timevar = "cat", direction = "wide")

rr.1.control <- rr.1[!is.na(rr.1$cat) & rr.1$cat=="Control",] # discard treated since we have treated time-series

track2.1 <- reshape(data.frame(rr.1.control)[c("track2","ID_NUM","year")], idvar = "year", timevar = "ID_NUM", direction = "wide") # county-level analysis 

#Labels

track2.1.y <- cats.rr.1.r[c("year", "track2.Treated")]
track2.1.y <- track2.1.y[!is.na(track2.1.y$track2.Treated),]

# Splits

track2.1.years <- intersect(track2.1$year,track2.1.y$year) # common track2.1 years in treated and control

track2.1.x.train <- track2.1[track2.1$year %in% track2.1.years & track2.1$year < 1866,]
track2.1.x.val <- track2.1[track2.1$year %in% track2.1.years & (track2.1$year >= 1866  & track2.1$year <= 1876),]
track2.1.x.test <- track2.1[track2.1$year %in% track2.1.years & track2.1$year > 1876,]

track2.1.y.train <- track2.1.y[track2.1.y$year %in% track2.1.years & track2.1.y$year < 1866,]
track2.1.y.val <- track2.1.y[track2.1.y$year %in% track2.1.years & (track2.1.y$year >= 1866 & track2.1.y$year <= 1876),]
track2.1.y.test <- track2.1.y[track2.1.y$year %in% track2.1.years &track2.1.y$year > 1876,]

# Preprocess
track2.1.x.train <- data.frame(sapply(track2.1.x.train, as.numeric))
track2.1.x.train[is.na(track2.1.x.train)] <- 0 # fill NA with 0 before scale
track2.1.pre.train <- preProcess(track2.1.x.train[!colnames(track2.1.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
track2.1.x.train[!colnames(track2.1.x.train) %in% c("year")] <- predict(track2.1.pre.train, track2.1.x.train[!colnames(track2.1.x.train) %in% c("year")] )

track2.1.x.val <- data.frame(sapply(track2.1.x.val, as.numeric))
track2.1.x.val[!colnames(track2.1.x.val) %in% c("year")] <- predict(track2.1.pre.train, track2.1.x.val[!colnames(track2.1.x.val) %in% c("year")] ) # use training values for val set 

track2.1.x.test <- data.frame(sapply(track2.1.x.test, as.numeric))
track2.1.x.test[!colnames(track2.1.x.test) %in% c("year")] <- predict(track2.1.pre.train, track2.1.x.test[!colnames(track2.1.x.test) %in% c("year")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/railroads/analysis-12/treated/"

write.csv(track2.1.x.train[!colnames(track2.1.x.train) %in% c("year")], paste0(data.directory,"track2-x-train.csv"), row.names=FALSE) 
write.csv(track2.1.x.val[!colnames(track2.1.x.val) %in% c("year")] , paste0(data.directory,"track2-x-val.csv"), row.names=FALSE) 
write.csv(track2.1.x.test[!colnames(track2.1.x.test) %in% c("year")] , paste0(data.directory,"track2-x-test.csv"), row.names=FALSE) 
write.csv(track2.1.y.train[!colnames(track2.1.y.train) %in% c("year")], paste0(data.directory,"track2-y-train.csv"), row.names=FALSE) 
write.csv(track2.1.y.val[!colnames(track2.1.y.val) %in% c("year")], paste0(data.directory,"track2-y-val.csv"), row.names=FALSE) 
write.csv(track2.1.y.test[!colnames(track2.1.y.test) %in% c("year")], paste0(data.directory,"track2-y-test.csv"), row.names=FALSE) 

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is non-southern public land states (not MO)
# Controls are MO, state land states

rr.3 <- rr.inter.m

# Summarize by category

rr.3$cat <- NA
rr.3$cat[rr.3$state %in% setdiff(setdiff(pub.states,southern.pub), "MO")] <- "Treated"
rr.3$cat[rr.3$state %in% c("MO",state.land.states)] <- "Control"

rr.3$year <- rr.3$InOpBy

# Create control and treated sums
cats.rr.3 <- rr.3 %>% 
  select(year, cat, track2)  %>%
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

cats.rr.3 <- select(cats.rr.3, -ID_NUM)

cats.rr.3.r <- reshape(data.frame(cats.rr.3), idvar = "year", timevar = "cat", direction = "wide")

rr.3.control <- rr.3[!is.na(rr.3$cat) & rr.3$cat=="Control",] # discard treated since we have treated time-series

track2.3 <- reshape(data.frame(rr.3.control)[c("track2","ID_NUM","year")], idvar = "year", timevar = "ID_NUM", direction = "wide") # county-level analysis

# Labels

track2.3.y <- cats.rr.3.r[c("year", "track2.Treated")]
track2.3.y <- track2.3.y[!is.na(track2.3.y$track2.Treated),]

# Splits

track2.3.years <- intersect(track2.3$year,track2.3.y$year) # common track2.3 years in treated and control

track2.3.x.train <- track2.3[track2.3$year %in% track2.3.years & track2.3$year < 1889,]
track2.3.x.val <- track2.3[track2.3$year %in% track2.3.years & (track2.3$year >= 1889 & track2.3$year <= 1899),]
track2.3.x.test <- track2.3[track2.3$year %in% track2.3.years & track2.3$year >= 1899,]

track2.3.y.train <- track2.3.y[track2.3.y$year %in% track2.3.years & track2.3.y$year < 1889,]
track2.3.y.val <- track2.3.y[track2.3.y$year %in% track2.3.years & (track2.3.y$year >= 1889 & track2.3.y$year <= 1899),]
track2.3.y.test <- track2.3.y[track2.3.y$year %in% track2.3.years & track2.3.y$year >= 1899,]

# Preprocess

track2.3.x.train <- data.frame(sapply(track2.3.x.train, as.numeric))
track2.3.x.train[is.na(track2.3.x.train)] <- 0 # fill NA with 0 before scale
track2.3.pre.train <- preProcess(track2.3.x.train[!colnames(track2.3.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
track2.3.x.train[!colnames(track2.3.x.train) %in% c("year")] <- predict(track2.3.pre.train, track2.3.x.train[!colnames(track2.3.x.train) %in% c("year")] )

track2.3.x.val <- data.frame(sapply(track2.3.x.val, as.numeric))
track2.3.x.val[!colnames(track2.3.x.val) %in% c("year")] <- predict(track2.3.pre.train, track2.3.x.val[!colnames(track2.3.x.val) %in% c("year")] ) # use training values for val set 

track2.3.x.test <- data.frame(sapply(track2.3.x.test, as.numeric))
track2.3.x.test[!colnames(track2.3.x.test) %in% c("year")] <- predict(track2.3.pre.train, track2.3.x.test[!colnames(track2.3.x.test) %in% c("year")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/railroads/analysis-34/treated/"

write.csv(track2.3.x.train[!colnames(track2.3.x.train) %in% c("year")], paste0(data.directory,"track2-x-train.csv"), row.names=FALSE) 
write.csv(track2.3.x.val[!colnames(track2.3.x.val) %in% c("year")] , paste0(data.directory,"track2-x-val.csv"), row.names=FALSE) 
write.csv(track2.3.x.test[!colnames(track2.3.x.test) %in% c("year")] , paste0(data.directory,"track2-x-test.csv"), row.names=FALSE) 
write.csv(track2.3.y.train[!colnames(track2.3.y.train) %in% c("year")], paste0(data.directory,"track2-y-train.csv"), row.names=FALSE) 
write.csv(track2.3.y.val[!colnames(track2.3.y.val) %in% c("year")], paste0(data.directory,"track2-y-val.csv"), row.names=FALSE) 
write.csv(track2.3.y.test[!colnames(track2.3.y.test) %in% c("year")], paste0(data.directory,"track2-y-test.csv"), row.names=FALSE) 