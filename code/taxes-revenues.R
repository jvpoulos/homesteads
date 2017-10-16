data.directory <- "~/Dropbox/github/ok-lottery/data/"

library(reshape2)
library(readxl)

### Taxes and Revenues

# county census data

years <- c(seq(1870,1950,10),1967,1972,1983,1988,1994,'1998a','1998b')

census.county <- sapply(years, function(i) {
  # import census file
  census.county.i <- read.csv(paste0(data.directory,"census-county/",i,".csv"), stringsAsFactors=FALSE)
  
  census.county.i <- subset(census.county.i, !is.na(county) & county!=0 & ! state %in% c(98,5,81)) # drop RI, DC, AK
  
  ## 1870, 1880
  
  if(i %in% c(1870,1880)){
  # pc taxes, 1870, 1880
  census.county.i$taxpc1 <- census.county.i$taxcoun/census.county.i$totpop # just county taxes
  census.county.i$taxpc2 <- (census.county.i$taxcoun + census.county.i$taxlocal)/census.county.i$totpop # county+mcd taxes
  }
  
  census.county.i <- cbind(census.county.i, "year"=rep(i, nrow(census.county.i)))
})

## 1890 

cntynames103 <- read.fwf("~/Dropbox/ok-lottery-local/data/TieboutData/County/Gov/cntynames103.txt", 
                    widths = c(11,9,13,12,11,13,11,12,12,12,11,12,13,11),
                         header=FALSE,
                    col.names=c("state","fips","ctyname","gr90","gr02","gr13","tax22","revcm32","revc32","taxcm32",
                                "taxc32","rev42","tax42","flag42"),
                    stringsAsFactors=FALSE)

cntynames103$fips <- as.numeric(cntynames103$fips)
cntynames103$state.fips <- round(cntynames103$fips/1000)

cntynames103$county.fips <- ((cntynames103$fips/1000)-cntynames103$state.fips)*1000

cntynames103 <- subset(cntynames103, !is.na(fips) & county.fips!=0 & state.fips!=44 & state.fips!=11)

cntynames103$gr90[cntynames103$gr90 < 0] <- NA # make missing NA

cntynames103.1890 <- merge(cntynames103, census.county[[3]], by=c('fips')) 

cntynames103.1890$grpc <- cntynames103.1890$gr90/cntynames103.1890$totpop # pc gross receipts, 1890

# 1902 (need to weight 1900 and 1910 pop. data)

cntynames103$gr02[cntynames103$gr02 < 0] <- NA

cntynames103.1902 <- merge(cntynames103, census.county[[4]], by=c('fips'))
cntynames103.1902 <- merge(cntynames103.1902, census.county[[5]], by=c('fips'))

cntynames103.1902$grpc <- cntynames103.1902$gr02/(0.8*cntynames103.1902$totpop.x + 0.2*cntynames103.1902$totpop.y) # pc gross receipts, 1902

# 1913 (weight 1910 + 1920 data)

cntynames103$gr13[cntynames103$gr13 < 0] <- NA

cntynames103.1913 <- merge(cntynames103, census.county[[5]], by=c('fips'))

cntynames103.1913 <- merge(cntynames103.1913, census.county[[6]], by=c('fips'))

cntynames103.1913$grpc <- cntynames103.1913$gr02/(0.7*cntynames103.1913$totpop.x + 0.3*cntynames103.1913$totpop.y) # pc gross receipts, 1913

# 1922 (weight 1920 + 1930 data)

cntynames103$tax22[cntynames103$tax22 < 0] <- NA
cntynames103$tax22 <- cntynames103$tax22*(10**3)

cntynames103.1922 <- merge(cntynames103, census.county[[6]], by=c('fips'))
cntynames103.1922 <- merge(cntynames103.1922, census.county[[7]], by=c('fips'))

cntynames103.1922$taxpc1 <- cntynames103.1922$tax22/(0.8*cntynames103.1922$totpop.x + 0.2*cntynames103.1922$totpop.y) # pc taxes, 1922

## 1932 (weight 1930 + 1940 data)

cntynames103$revcm32[cntynames103$revcm32 < 0] <- NA
cntynames103$revcm32 <- cntynames103$revcm32*(10**3)

cntynames103$revc32[cntynames103$revc32 < 0] <- NA
cntynames103$revc32 <- cntynames103$revc32*(10**3)

cntynames103$taxcm32[cntynames103$taxcm32 < 0] <- NA
cntynames103$taxcm32 <- cntynames103$taxcm32*(10**3)

cntynames103$taxc32[cntynames103$taxc32 < 0] <- NA
cntynames103$taxc32 <- cntynames103$taxc32*(10**3)

# * SPECIAL addition: we have data for "greater NY": Bronx, Kings, New York, Queens, Richmond;
# *	So we will put all this data in New York here;
# * NB: 1962 has New York (county) but none of the others;
# *	--> this is typical for the 1957- data (or have Kings only);
# * NB: no data for county only;

cntynames103$taxcm32[cntynames103$fips==36061] <- (520368)*10^3 
cntynames103$revcm32[cntynames103$fips==36061] <- (681500)*10^3

cntynames103.1932 <- merge(cntynames103, census.county[[7]], by=c('fips'))
cntynames103.1932 <- merge(cntynames103.1932, census.county[[8]], by=c('fips'))

cntynames103.1932$pop32 <- 0.8*cntynames103.1932$totpop.x + 0.2*cntynames103.1932$totpop.y
cntynames103.1932$pop32[cntynames103.1932$fips==36061] <- 7090300

# generate final data

cntynames103.1932$taxpc1 <- cntynames103.1932$taxc32/cntynames103.1932$pop32 # county tax PC
cntynames103.1932$taxpc2 <- cntynames103.1932$taxcm32/cntynames103.1932$pop32 # county+mcd tax PC
cntynames103.1932$revpc1 <- cntynames103.1932$revc32/cntynames103.1932$pop32 # county rev PC
cntynames103.1932$revpc2 <- cntynames103.1932$revcm32/cntynames103.1932$pop32 # county+mcd rev PC

## 1942 (weight 1940 + 1950 data)

cntynames103$tax42[cntynames103$tax42 < 0 | cntynames103$flag42 >1] <- NA
cntynames103$tax42 <- cntynames103$tax42*(10**3)

cntynames103$rev42[cntynames103$rev42 < 0 | cntynames103$flag42 >1] <- NA
cntynames103$rev42 <- cntynames103$rev42*(10**3)

cntynames103.1942 <- merge(cntynames103, census.county[[8]], by=c('fips'))
cntynames103.1942 <- merge(cntynames103.1942, census.county[[9]], by=c('fips'))

cntynames103.1942$taxpc <- cntynames103.1942$tax42/(0.8*cntynames103.1942$totpop.x + 0.2*cntynames103.1942$totpop.y) # pc tax, 1942
cntynames103.1942$revpc <-cntynames103.1942$rev42/(0.8*cntynames103.1942$totpop.x + 0.2*cntynames103.1942$totpop.y)# pc rev, 1942

## 1957 (weight 1950 and 1960)

cnty62 <- read.table("~/Dropbox/ok-lottery-local/data/TieboutData/County/Age/1960/cnty62/cnty62.txt", sep=",",
                   header = TRUE,
                   stringsAsFactors=FALSE)
  
# drop: state level obsverations;
# RI --> no county-level spending;
# drop DC --> what is local gov?;
#	NB: ICPSR code;	

cnty62 <- subset(cnty62, county!=0000 & state!=5 & state!=55)

cnty62$pop96[cnty62$pop96 <0] <- NA

cnty62$rev57[cnty62$rev57 <0] <- NA

cnty62$rev57 <- cnty62$rev57*(10**3)

cnty62.1957 <- merge(cnty62, census.county[[9]], by = c("state","county"))

cnty62.1957$revpc <- cnty62.1957$rev57/(0.3*cnty62.1957$pop96 + 0.7*cnty62.1957$totpop) # pc (local) revenues, 1957

## 1962: CCDB 1967 

census.county[[10]]$pop60 <- census.county[[10]]$var1 # total pop. 1960

census.county[[10]]$rev62 <- census.county[[10]]$var46 # Local governments: total general revenue ($000s) 1962
census.county[[10]]$rev62 <- census.county[[10]]$rev62*(10**3) # denominated *$1,000

census.county[[10]]$ptax62 <- census.county[[10]]$var48 # Local governments: percent property taxes in gen'l revenue 1962

census.county[[10]]$tax62 <- (census.county[[10]]$ptax62/100)*census.county[[10]]$rev62

# (weight 1960 + 1970 data)
census.county[[11]]$pop70 <- census.county[[11]]$var3 # Total population, 1970

census.county.1962 <- merge(census.county[[10]], census.county[[11]], by='fips')

census.county.1962$taxpc <- census.county.1962$tax62/(0.8*census.county.1962$pop60 + 0.2*census.county.1962$pop70)

census.county.1962$revpc <- census.county.1962$rev62/(0.8*census.county.1962$pop60 + 0.2*census.county.1962$pop70)

## 1967: CCDB 1972

cnty72 <- read.table("~/Dropbox/ok-lottery-local/data/TieboutData/County/Gov/1970/cnty_cty72/cnty72.txt", sep=",",
                     header = TRUE,
                     stringsAsFactors=FALSE)

cnty72 <- subset(cnty72, !is.na(county) & county!=0 & ! state %in% c(44,2,11))   # SPECIAL for 1967 CCDB --> some observation have no county code;

cnty72$pop70[cnty72$pop70 <0] <- NA

cnty72$rev67[cnty72$rev67 <0] <- NA

cnty72$ptax67[cnty72$ptax67 <0] <- NA
  
cnty72$rev67 <- cnty72$rev67*(10^5)
cnty72$tax67 <- (cnty72$ptax67/10^3)*cnty72$rev67

cnty72.1967 <- merge(cnty72, census.county[[10]], by.x=c("state","county"), by.y=c("statefip", "counfip"))

cnty72.1967$taxpc <- cnty72.1967$tax67/(0.3*cnty72.1967$pop60 + 0.7*cnty72.1967$pop70)
cnty72.1967$revpc <- cnty72.1967$rev67/(0.3*cnty72.1967$pop60 + 0.7*cnty72.1967$pop70)

## 1972: CCDB 1977

cnty77 <- read.table("~/Dropbox/ok-lottery-local/data/TieboutData/County/Gov/1970/cnty_cty77/cnty77.txt", sep=",",
                     header = TRUE,
                     stringsAsFactors=FALSE)

cnty77 <- subset(cnty77, !is.na(county) & county!=0 & ! state %in% c(44,2,11))   # SPECIAL for 1967 CCDB --> some observation have no county code;

cnty77$pop70[cnty77$pop70 <0] <- NA

cnty77$rev72[cnty77$revf72 > 0] <- NA # NB: missing value flag (see p2 CCDB 1977 data base dictionary);

cnty77$rev72 <- cnty77$rev72*(10^6/10) # million $ with 1 implied decimal;

cnty77$tax72[cnty77$taxf72>0] <- NA

cnty77$tax72 <- cnty77$tax72*(10^6/10) # million $ with 1 implied decimal;

# weight 1970 + 1980 data

cnty80 <- read.table("~/Dropbox/ok-lottery-local/data/TieboutData/County/Gov/1980/1980data.txt", sep=",",
                     header = TRUE,
                     stringsAsFactors=FALSE)

cnty80 <- subset(cnty80, !is.na(county) & county!=0 & ! state %in% c(44,2,11))

cnty77.1972 <- merge(cnty77, cnty80, by=c("state","county"))

cnty77.1972$taxpc <- cnty77.1972$tax72/(0.8*cnty77.1972$pop70 + 0.2*cnty77.1972$pop80)

cnty77.1972$revpc <- cnty77.1972$rev72/(0.8*cnty77.1972$pop70 + 0.2*cnty77.1972$pop80)

## 1977 data: CCDB 1983

cnty83 <- subset(census.county[[12]], !is.na(county) & county!=0 & ! state %in% c(98,5,81))

cnty83$rev77 <- cnty83$var148 # "General Revenue of Local Government (Excluding Interlocal), 1977; thousands $"

cnty83$rev77[cnty83$flag148 > 0 ] <- NA

cnty83$rev77 <- cnty83$rev77*10^3 # thousand $

cnty83$tax77 <- cnty83$var151 # Taxes of local government ($000s), 1977
cnty83$tax77[cnty83$flag151 >0]  <- NA
cnty83$tax77 <- cnty83$tax77*10^3 # thousand $;

cnty83$pop70 <- cnty83$var2 # Total population, April 1, 1970
cnty83$pop70[cnty83$flag2>0] <- NA

cnty83$pop80 <- cnty83$var3 # Total population, April 1, 1980
cnty83$pop80[cnty83$flag3>0] <- NA

cnty83$taxpc <- cnty83$tax77/(0.3*cnty83$pop70 + 0.7*cnty83$pop80)
cnty83$revpc <- cnty83$rev77/(0.3*cnty83$pop70 + 0.7*cnty83$pop80)

## 1982 data: CCDB 1988

# no missing value code --> but 0's are clearly missing (ex: NY, NY OR Queens, NY);
cnty80$rev82[cnty80$rev82<=0] <- NA
cnty80$rev82 <-  cnty80$rev82*10^6	# million $;

cnty80$tax82[cnty80$tax82 <= 0] <- NA
cnty80$tax82 <- cnty80$tax82*10^6 # million $;

#	weight 1980 + 1990 data;

cnty90 <- read.table("~/Dropbox/ok-lottery-local/data/TieboutData/County/ForeignBorn/1990/1990.txt", sep=",",
                     header = TRUE,
                     stringsAsFactors=FALSE)

cnty90 <- subset(cnty90, !is.na(county) & county!=0 & ! state %in% c(44,2,11))

cnty80.1982 <- merge(cnty80, cnty90, by=c("state","county"))

cnty80.1982$taxpc <- cnty80.1982$tax82/(0.8*cnty80.1982$pop80 + 0.2*cnty80.1982$pop90)

cnty80.1982$revpc <- cnty80.1982$rev82/(0.8*cnty80.1982$pop80 + 0.2*cnty80.1982$pop90)

## 1987 data: CCDB 1994

census.county[[14]]$rev87 <- census.county[[14]]$var209*10^3 # thousand $

census.county[[14]]$tax87 <- census.county[[14]]$var212*10^3 # thousand $

# weight 1980 + 1990
census.county[[14]]$pop87 <- 0.3*census.county[[14]]$var006 + 0.7*census.county[[14]]$var005

census.county[[14]]$taxpc <- census.county[[14]]$tax87/census.county[[14]]$pop87
census.county[[14]]$revpc <- census.county[[14]]$rev87/census.county[[14]]$pop87

## 1992 data: CCDB 1998

census.county[[16]]$rev92 <- census.county[[16]]$gl61092d*10^3 # thousand $

census.county[[16]]$tax92 <- census.county[[16]]$gl64092d*10^3 # thousand $

cnty92 <- merge(census.county[[16]], census.county[[15]], by=c("fips"))

cnty92$taxpc <- cnty92$tax92/cnty92$aa98p02d
cnty92$revpc <- cnty92$rev92/cnty92$aa98p02d

cnty92 <- subset(cnty92, select= c('taxpc','revpc','fips'))

## Create time-series: 1890,1902,1913,1932,1942 : revenues— Rev1;

rev1 <- subset(cntynames103.1890, select=c('fips','grpc'))

rev1 <- merge(rev1,subset(cntynames103.1902, select=c('grpc','fips')), by='fips',all.x=TRUE, all.y=TRUE) # 1902

rev1 <- merge(rev1,subset(cntynames103.1913, select=c('grpc','fips')), by='fips',all.x=TRUE, all.y=TRUE) # 1913

rev1 <- merge(rev1,subset(cntynames103.1932, select=c('revpc1','fips')), by='fips',all.x=TRUE, all.y=TRUE) # 1932

rev1 <- merge(rev1,subset(cntynames103.1942, select=c('revpc','fips')), by='fips',all.x=TRUE, all.y=TRUE) # 1942

colnames(rev1) <- c('fips','revpc1.1890', 'revpc1.1902','revpc1.1913','revpc1.1932','revpc1.1942')

rev1 <- rev1[!duplicated(rev1$fips) & !is.na(rev1$fips),] 

rev1$state.fips <- round(rev1$fips/1000)

## Create time-series: rev2

rev2 <- subset(cntynames103.1932, select=c('fips','revpc2')) # 1932

colnames(rev2) <- c('fips','revpc2')

rev2 <- merge(rev2,subset(cnty62.1957, select=c('revpc','fips')), by=c('fips'),all.x=TRUE, all.y=TRUE) # 1957

rev2 <- merge(rev2,subset(census.county.1962, select=c('revpc','fips')), by=c('fips'),all.x=TRUE, all.y=TRUE) # 1962

rev2 <- merge(rev2,subset(cnty72.1967, select=c('revpc','fips')), by=c('fips'),all.x=TRUE, all.y=TRUE) # 1967

cnty77.1972$fips <- cnty77.1972$state*1000 +cnty77.1972$county
rev2 <- merge(rev2,subset(cnty77.1972, select=c('revpc','fips')),by=c('fips'),all.x=TRUE, all.y=TRUE) # 1972

rev2 <- merge(rev2,subset(cnty83, select=c('revpc','fips')),by=c('fips'),all.x=TRUE, all.y=TRUE) # 1977

cnty80.1982$fips <- cnty80.1982$state*1000 + cnty80.1982$county
rev2 <- merge(rev2,subset(cnty80.1982, select=c('revpc','fips')),by=c('fips'),all.x=TRUE, all.y=TRUE) # 1982

rev2 <- merge(rev2,subset(census.county[[14]], select=c('revpc','fips')),by=c('fips'),all.x=TRUE, all.y=TRUE) # 1987

rev2 <- merge(rev2,subset(cnty92, select=c('revpc','fips')), by=c('fips'),all.x=TRUE, all.y=TRUE) # 1992

colnames(rev2) <- c('fips','revpc2.1932','revpc2.1957','revpc2.1962','revpc2.1967','revpc2.1972','revpc2.1977','revpc2.1982','revpc2.1987','revpc2.1992')

rev2 <- rev2[!duplicated(rev2$fips) & ! is.na(rev2$fips),] # rm dup & NA

rev2$state.fips <- round(rev2$fips/1000)

## Create time-series: tax2

tax2 <- subset(census.county[[1]], select=c('fips','taxpc2')) # 1870

tax2 <- merge(tax2, subset(census.county[[2]], select=c('fips','taxpc2')), by='fips', all.x=TRUE, all.y=TRUE) # 1880

tax2 <- merge(tax2, subset(cntynames103.1932, select=c('fips','taxpc2')), by='fips', all.x=TRUE, all.y=TRUE) # 1932

tax2 <- merge(tax2,subset(census.county.1962, select=c('fips','taxpc')), by=c('fips'),all.x=TRUE, all.y=TRUE) # 1962

tax2 <- merge(tax2,subset(cnty72.1967, select=c('fips','taxpc')), by=c('fips'),all.x=TRUE, all.y=TRUE) # 1967

cnty77.1972$fips <- cnty77.1972$state*1000 +cnty77.1972$county
tax2 <- merge(tax2,subset(cnty77.1972, select=c('fips','taxpc')),by=c('fips'),all.x=TRUE, all.y=TRUE) # 1972

tax2 <- merge(tax2,subset(cnty83, select=c('fips','taxpc')),by=c('fips'),all.x=TRUE, all.y=TRUE) # 1977

cnty80.1982$fips <- cnty80.1982$state*1000 + cnty80.1982$county
tax2 <- merge(tax2,subset(cnty80.1982, select=c('fips','taxpc')),by=c('fips'),all.x=TRUE, all.y=TRUE) # 1982

tax2 <- merge(tax2,subset(census.county[[14]], select=c('fips','taxpc')),by=c('fips'),all.x=TRUE, all.y=TRUE) # 1987

tax2 <- merge(tax2,subset(cnty92, select=c('fips','taxpc')), by=c('fips'),all.x=TRUE, all.y=TRUE) # 1992

tax2 <- tax2[!duplicated(tax2$fips) & ! is.na(tax2$fips),] # rm dup & NA

colnames(tax2) <- c('fips','taxpc2.1870','taxpc2.1880','taxpc2.1932','taxpc2.1962','taxpc2.1967','taxpc2.1972','taxpc2.1977','taxpc2.1982','taxpc2.1987','taxpc2.1992')

tax2$state.fips <- round(tax2$fips/1000)

## Create time-series: tax1

tax1 <- subset(census.county[[1]], select=c('fips','taxpc1')) # 1870

tax1 <- merge(tax1, subset(census.county[[2]], select=c('fips','taxpc1')), by='fips', all.x=TRUE, all.y=TRUE) # 1880

tax1 <- merge(tax1, subset(cntynames103.1922, select=c('fips','taxpc1')), by='fips', all.x=TRUE, all.y=TRUE) # 1922

tax1 <- merge(tax1, subset(cntynames103.1932, select=c('fips','taxpc1')), by='fips', all.x=TRUE, all.y=TRUE) # 1932

tax1 <- merge(tax1,subset(cntynames103.1942, select=c('fips','taxpc')), by=c('fips'),all.x=TRUE, all.y=TRUE) # 1942

tax1 <- tax1[!duplicated(tax1$fips) & ! is.na(tax1$fips),] # rm dup & NA

colnames(tax1) <- c('fips','taxpc1.1870','taxpc1.1880','taxpc1.1922','taxpc1.1932','taxpc1.1942')

tax1$state.fips <- round(tax1$fips/1000)

## Merge county/state info

data.directory <-"~/Dropbox/github/land-reform/data/"

fips.codes <- data.frame(read_excel(paste0(data.directory,'US_FIPS_Codes.xls'), skip = 1))

fips.codes$fips <- as.numeric(fips.codes$FIPS.State)*1000 + as.numeric(fips.codes$FIPS.County)

tax1 <- merge(tax1, fips.codes, by="fips")

tax2 <- merge(tax2, fips.codes, by="fips")

# state abbreviations

tax1$state.abb <- setNames(state.abb, state.name)[tax1$State]

tax2$state.abb <- setNames(state.abb, state.name)[tax2$State]

# reshape to cross-sectional time-series

tax1 <- reshape(tax1, direction="long", varying=list(names(tax1)[2:6]), v.names="taxpc1", 
                  idvar=c("fips"), timevar="year", times=c(1870,1880,1922,1932,1942))

tax2 <- reshape(tax2, direction="long", varying=list(names(tax2)[2:11]), v.names="taxpc2", 
                idvar=c("fips"), timevar="year", times=c(1870,1880,1932,1962,1967,1972,1977,1982,1987,1992))

# merge both

taxpc <- merge(tax2, tax1, by=c("fips","state.fips","State","County.Name","FIPS.State","FIPS.County","state.abb","year"), all=TRUE)

taxpc <- taxpc[c("fips","State","County.Name","state.abb", "year", "taxpc2", "taxpc1")]

# clean

taxpc$taxpc2[is.infinite(taxpc$taxpc2)] <- NA

taxpc$taxpc1 <- log(taxpc$taxpc1+ .Machine$double.eps) # take logs of tax measures
taxpc$taxpc2 <- log(taxpc$taxpc2+ .Machine$double.eps)
