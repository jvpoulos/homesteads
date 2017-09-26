#####################
### County/State censuses   ###
#####################

library(reldist)
library(dplyr)

## Load data
census.data.directory <- "~/Dropbox/github/ok-lottery/data/"

years <- c(seq(1790,1960,10),1962,1967,1970,1983) 

census.county <- sapply(years, function(i) {
  census.county.i <- read.csv(paste0(census.data.directory,"census-county/",i,".csv"), stringsAsFactors=FALSE)
  census.county.i <- cbind(census.county.i, "year"=rep(i, nrow(census.county.i)))
}
)

names(census.county) <- years

## Create variables for:
## non-slave population (1790-1970,1983)
## (adjusted) land gini (1860-1950)
## pc taxes (1870,1880)
## avg farm size (1860-1950)
## farm tenancy (1880-1950, 1962, 1967)
## farm wages as share of adult male pop. (1870, 1900)
## farm output as share of total # farms (1870-1900)


# 1790
census.county[[1]] <- census.county[[1]] %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop-stot, #nonslave pop
         adultm = wm16) # White males aged 16+ 

# 1800
census.county[[2]] <- census.county[[2]] %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop-stot,
         adultm=sum(wm1625,wm2644,wm45)) # White males aged 16+

# 1810
census.county[[3]] <- census.county[[3]] %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop-stot,
         adultm=sum(wm2644,wm45)) # White males aged 26+ 

# 1820
census.county[[4]] <- census.county[[4]] %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop-(sftot+smtot),
         adultm=sum(wm2644,wm45,fcm2644,fcm45)) # White males aged 26+ & Free colored males aged 26+

# 1830
census.county[[5]] <- census.county[[5]] %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop-stot,
         adultm=sum(wm2029,wm3039,wm4049,wm5059,wm6069,wm7079,wm8089,wm9099,wm100,
                    fcm2435,fcm3654,fcm5599,fcm100)) # White males aged 20+ & Free colored males aged 24+

# 1840
census.county[[6]] <- census.county[[6]] %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop-(sftot+smtot),
         adultm=sum(wm2029,wm3039,wm4049,wm5059,wm6069,wm7079,wm8089,wm9099,wm100,
                    fcm2435,fcm3654,fcm5599,fcm100)) # White males aged 20+ & Free colored males aged 24+

# 1850 # Total public school inc (PSY)
census.county[[7]] <- census.county[[7]] %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop-stot,
         adultm = sum(wm2029,wm3039,wm4049,wm5059,wm6069,wm7079,wm8089,wm9099,wm100) + sum(fcm2029,fcm3039,fcm4049,fcm5059,fcm6069,fcm7079,fcm8089,fcm9099,fcm100)) # non-slave adult males 20+

#1860 # no farms
census.county[[8]] <- census.county[[8]] %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop-stot,
    land.gini = gini(c((farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm500*749.5),(farm1000)*1000)),
    farms= sum(farm39,farm1019,farm2049,farm5099,farm100,farm500,farm1000),
    adultm = sum(wm2029,wm3039,wm4049,wm5059,wm6069,wm7079,wm8089,wm9099,wm100) + sum(fcm2029,fcm3039,fcm4049,fcm5059,fcm6069,fcm7079,fcm8089,fcm9099,fcm100), # free adult males 20+
    p = farms/adultm, # ratio of farms to adult males
    aland.gini = p*land.gini + (1-p),
    farmsize = sum(c((farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm500*749.5),(farm1000)*1000))/farms)

#1870, 1880

for(i in c(9:10)){ 
  census.county[[i]] <- census.county[[i]] %>%
    filter(!is.na(farms)) %>%
    group_by(state,county) %>%
    mutate(ns.pop = totpop, # just total pop.
           land.gini = gini(c((farm02*1),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm500*749.5),(farm1000)*1000)),
           adultm = m21tot, # Males aged 21+
           p = farms/adultm, 
           aland.gini = p*land.gini + (1-p),
           output= farmout/farms, 
           taxpc = taxcoun/ns.pop) # Amount of county taxes
}

census.county[[9]] <- census.county[[9]] %>% # no wages for 1880
  group_by(state,county) %>%
  mutate(wages = farmlab/adultm) # farm wages as share of adult male pop.

census.county[[10]] <- census.county[[10]] %>% # no avg. farm size 1870
  group_by(state,county) %>%
  mutate(farmsize = sum(c((farm02*1),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm500*749.5),(farm1000)*1000))/farms,
         tenancy = sum(farmten,farmsc)/farms)

# 1890
census.county[[11]] <- census.county[[11]] %>%
  filter(!is.na(farms)) %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop,
         land.gini = gini(c((farm09*4.9),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm500*749.5),(farm1000)*1000)),
         adultm = sum(nbwm21,fbwm21,colm21), # native, foreign, colored males 21+
         p = farms/adultm,
         aland.gini = p*land.gini + (1-p),
         farmsize = farmsize,
         output= farmout/farms, 
         tenancy = (sum(fa09te,fa1019te,fa2049te,fa5099te,fa100te,fa500te,fa1000te,
                        fa09sc,fa1019sc,fa2049sc,fa5099sc,fa100sc,fa500sc,fa1000sc))/(farms))

# 1900
census.county[[12]] <- census.county[[12]] %>%
  filter(!is.na(farms)) %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop,
         land.gini = gini(c((farm12*1.5),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm175*217),(farm260*379.5),(farm500*749.5),(farm1000)*1000)),
         adultm = m21, # Males aged 21+
         p = farms/adultm, 
         aland.gini = p*land.gini + (1-p),
         farmsize = farmsize,
         wages = farmlab/adultm,
         output= farmout/farms, 
         tenancy=(sum(farmwhct,farmcoct,farmwhst,farmcost)/(farms)))

# 1910, 1920 # no farm size
census.county[[13]]$m21 <- census.county[[13]]$mvote # voting age is 21
for(i in c(13:14)){ 
census.county[[i]] <- census.county[[i]] %>%
  filter(!is.na(farms)) %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop,
         land.gini = gini(c((farm02*1),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm175*217),(farm260*379.5),(farm500*749.5),(farm1000)*1000)),
         adultm = m21, # Males aged 21+
         p = farms/adultm, 
         aland.gini = p*land.gini + (1-p),
         farmsize = sum(c((farm02*1),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm175*217),(farm260*379.5),(farm500*749.5),(farm1000)*1000))/farms,
         tenancy=(farmten)/(farms))
}

# 1930
census.county[[15]] <- census.county[[15]] %>%
  filter(!is.na(farms)) %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop,
         land.gini = gini(c((farm02*1),(farm39*6),(farm1019*14.5),(farm2049*34.5),(farm5099*74.5),(farm100*299.5),(farm175*217),(farm260*379.5),(farm500*749.5),(farm1000*2999.5),(farm5000)*5000)),
         adultm = m21, # Males aged 21+
         p = farms/adultm, 
         aland.gini = p*land.gini + (1-p),
         farmsize = farmsize,
         tenancy=(farmten)/(farms))

#1940 
census.county[[16]] <- census.county[[16]] %>%
  filter(!is.na(farms)) %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop,
         land.gini = gini(c((farm02*1.5),(farm39*6),(farm1029*19.5),(farm3049*39.5),(farm5069*59.5),
                    (farm7099*84.5),(farm100*119.5),(farm140*159.5),(farm175*177),(farm180*199.5),
                    (farm220*239.5),(farm260*319.5),(farm380*439.5),(farm500*599.5),(farm700*849.5),(farm1000*1000))),
         adultm = m21, # Males aged 21+
         p = farms/adultm,  
         aland.gini = p*land.gini + (1-p),
         farmsize = farmsize,
         tenancy=(farmten)/(farms))

#1950
census.county[[17]] <- census.county[[17]] %>%
  filter(!is.na(farms2)) %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop,
         land.gini = gini(c((farm02*1.5),c(farm39*6),(farm1029*19.5),(farm3049*39.5),(farm5069*59.5),
                    (farm7099*84.5),(farm100*119.5),(farm140*159.5),(farm180*199.5),
                    (farm220*239.5),(farm260*379.5),(farm500*749.5),(farm1000*1000))),
         farms = farms2,
         adultm = m25, # Males aged 25+
         p = farms/adultm, 
         aland.gini = p*land.gini + (1-p),
         farmsize = farmsize,
         tenancy=(farmten)/(farms))

#1960
census.county[[18]] <- census.county[[18]] %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop,
         adultm=NA)  # in county III

# 1962
census.county[[19]] <- census.county[[19]] %>%
  group_by(state,county) %>%
  mutate(tenancy = var145) # % tenant farms 1959


# 1967
census.county[[20]] <- census.county[[20]] %>%
  group_by(state,county) %>%
  mutate(tenancy = var125) # percent of farms operated by tenants 1964

#1970
census.county[[21]] <- census.county[[21]] %>%
  group_by(state,county) %>%
  mutate(ns.pop = totpop,
         adultm=sum(m21,m2224,m2534,m3544,m4554,m5559,m6061,m6264,m6574,m75))  #Males aged 21


#1983
census.county[[22]] <- census.county[[22]] %>%
  group_by(state,county) %>%
  mutate(ns.pop = var3, # Total population, April 1, 1980
         adultm=NA)  #Males aged 21

## Create time-series 

census.list <- list(census.county[[1]],census.county[[2]],census.county[[3]],census.county[[4]],census.county[[5]],census.county[[6]],
                    census.county[[7]],census.county[[8]],census.county[[9]],census.county[[10]],census.county[[11]],
                      census.county[[12]],census.county[[13]],census.county[[14]],census.county[[15]],census.county[[16]],
                    census.county[[17]],census.county[[18]],census.county[[19]],census.county[[20]],census.county[[21]],census.county[[22]])

census.ts <- do.call(rbind, census.list) 

census.ts <- subset(census.ts, select=c("year","name","state","county","fips","land.gini", "aland.gini","ns.pop","adultm","farms","farmsize","tenancy","wages","output"))

# Rm inf in wages

census.ts$wages[is.infinite(census.ts$wages)] <- NA

# Fix county codes

census.ts$county[census.ts$name=="Jackson"] <- 1570
census.ts$county[census.ts$name=="Chatham"] <- 510 

# County name proper
Proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
census.ts$name <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(census.ts$name), perl=TRUE)
census.ts$name[census.ts$name=="Mcintosh"] <- "McIntosh"

# Create separate state dataset

census.ts.state <- census.ts[census.ts$county==0,]

census.ts.state$state <- state.abb[match(census.ts.state$name,state.name)] # state abbr

census.ts.state <- census.ts.state[!is.na(census.ts.state$state),] #rm non-state obs

census.ts <- census.ts[census.ts$county!=0,] # rm state totals from census.ts