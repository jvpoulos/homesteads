###################################
# Prepare patents data            #
###################################

require(doParallel)
require(data.table)
require(reshape2)
require(stringr)
library(dplyr)
library(weights)
library(caret)
library(zoo)

# Set location of files
setwd(paste0(data.directory, "patents"))

# Import glo files 

patent.files <- list.files(pattern = "*_Patent.csv", recursive = TRUE)

county.files <- list.files(pattern = "*County.csv", recursive = TRUE)

patents <- do.call(rbind,lapply(patent.files,read.csv, 
                                header=T,
                                sep = ",",
                                stringsAsFactors=FALSE))

counties <- do.call(rbind,lapply(county.files,read.csv, 
                                       header=T,
                                       sep = ",",
                                       stringsAsFactors=FALSE))


# Merge patents with counties
counties <- counties[!duplicated(counties$accession_nr),] # keep 1 county record/accession

patents <- merge(patents, counties[c("accession_nr","county_code")], by= c("accession_nr"),all.x=TRUE)

# Fix county code
patents$county_code <- as.numeric(patents$county_code)

patents <- patents[!is.na(patents$county_code),] # rm obs w missing county codes

patents$county_code <- patents$county_code*10 # make county code consistent with ICPSR

# Codes, etc. to numeric

patents$sales <- 0
patents$sales[patents$authority_code==272002] <- 1

patents$homesteads <- 0
patents$homesteads[patents$authority_code==251101] <- 1

# Date to time

patents$date <- as.yearmon(patents$signature_date, "%m/%d/%Y",tz="UTC") # convert to monthly data
patents <- patents[!is.na(patents$date) & !patents$date>2017,] # subset to non-missing and valid dates ## SAVED HERE

# Summarize by date/county/state

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") #,30,public,land,states

patents.sum <- patents %>%
  filter(state_code %in% pub.states) %>% # only public land states
  group_by(date,county_code,state_code) %>%
  summarise_each(funs(sum),sales,homesteads)

# Category
southern.pub <- c("AL", "AR", "FL", "LA", "MS") # 5 southern public land states

patents.sum$cat <- ifelse(patents.sum$state_code %in% southern.pub, "S", "NS") 

# Create control and treated sums
cats.sums <- patents.sum %>% 
  group_by(date,cat) %>% 
  summarise_each(funs(sum),sales,homesteads) 

cats.sums.r <- reshape(data.frame(cats.sums.r), idvar = "date", timevar = "cat", direction = "wide")

# Reshape long to wide

patents.sum$id <- interaction(patents.sum$county_code,patents.sum$state_code)

patents.sum <- patents.sum[patents.sum$cat=="Control",] # discard treated since we have treated time-series

homesteads <- reshape(data.frame(patents.sum[c("date","id","homesteads")]), idvar = "date", timevar = "id", direction = "wide")

sales <- reshape(data.frame(patents.sum[c("date","id","sales")]), idvar = "date", timevar = "id", direction = "wide")

# Labels

homesteads.y <- cats.sums.r[c("date", "homesteads.Treated")]
homesteads.y <- homesteads.y[!is.na(homesteads.y$homesteads.Treated),]

sales.y <- cats.sums.r[c("date", "sales.Treated")]
sales.y <- sales.y[!is.na(sales.y$sales.Treated),]

# Splits

homesteads.x.train <- homesteads[homesteads$date %in% homesteads.y$date & homesteads$date < "Jul 1901",]
homesteads.x.test <- homesteads[homesteads$date %in% homesteads.y$date & homesteads$date >= "Jul 1901",]

homesteads.y.train <- homesteads.y[homesteads.y$date < "Jul 1901",]
homesteads.y.test <- homesteads.y[homesteads.y$date >= "Jul 1901",]

sales.x.train <- sales[sales$date %in% sales.y$date & sales$date < "Jul 1901",]
sales.x.test <- sales[sales$date %in% sales.y$date & sales$date >= "Jul 1901",]

sales.y.train <- sales.y[sales.y$date < "Jul 1901",]
sales.y.test <- sales.y[sales.y$date >= "Jul 1901",]

# Preprocess
homesteads.pre.train <- preProcess(homesteads.x.train[!colnames(homesteads.x.train) %in% c("date")], method = c("center", "scale","medianImpute"))
homesteads.x.train[!colnames(homesteads.x.train) %in% c("date")] <- predict(homesteads.pre.train, homesteads.x.train[!colnames(homesteads.x.train) %in% c("date")] )

homesteads.x.test[!colnames(homesteads.x.test) %in% c("date")] <- predict(homesteads.pre.train, homesteads.x.test[!colnames(homesteads.x.test) %in% c("date")] ) # use training values for test set 

sales.pre.train <- preProcess(sales.x.train[!colnames(sales.x.train) %in% c("date")], method = c("center", "scale","medianImpute"))
sales.x.train[!colnames(sales.x.train) %in% c("date")] <- predict(sales.pre.train, sales.x.train[!colnames(sales.x.train) %in% c("date")] )

sales.x.test[!colnames(sales.x.test) %in% c("date")] <- predict(sales.pre.train, sales.x.test[!colnames(sales.x.test) %in% c("date")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/patents/"

write.csv(homesteads.x.train[!colnames(homesteads.x.train) %in% c("date")], paste0(data.directory,"homesteads-x-train.csv"), row.names=FALSE) 
write.csv(homesteads.x.test[!colnames(homesteads.x.test) %in% c("date")], paste0(data.directory,"homesteads-x-test.csv"), row.names=FALSE) 
write.csv(homesteads.y.train[!colnames(homesteads.y.train) %in% c("date")], paste0(data.directory,"homesteads-y-train.csv"), row.names=FALSE) 
write.csv(homesteads.y.test[!colnames(homesteads.y.test) %in% c("date")], paste0(data.directory,"homesteads-y-test.csv"), row.names=FALSE) 

write.csv(sales.x.train[!colnames(sales.x.train) %in% c("date")], paste0(data.directory,"sales-x-train.csv"), row.names=FALSE) 
write.csv(sales.x.test[!colnames(sales.x.test) %in% c("date")] , paste0(data.directory,"sales-x-test.csv"), row.names=FALSE) 
write.csv(sales.y.train[!colnames(sales.y.train) %in% c("date")], paste0(data.directory,"sales-y-train.csv"), row.names=FALSE) 
write.csv(sales.y.test[!colnames(sales.y.test) %in% c("date")], paste0(data.directory,"sales-y-test.csv"), row.names=FALSE) 

## Placebo (set intervention 1 year earlier)

# Splits

homesteads.x.train.p <- homesteads[homesteads$date %in% homesteads.y$date & homesteads$date < "Jul 1898",]
homesteads.x.test.p <- homesteads[homesteads$date %in% homesteads.y$date & homesteads$date >= "Jul 1898",]

homesteads.y.train.p <- homesteads.y[homesteads.y$date < "Jul 1898",]
homesteads.y.test.p <- homesteads.y[homesteads.y$date >= "Jul 1898",]

sales.x.train.p <- sales[sales$date %in% sales.y$date & sales$date < "Jul 1898",]
sales.x.test.p <- sales[sales$date %in% sales.y$date & sales$date >= "Jul 1898",]

sales.y.train.p <- sales.y[sales.y$date < "Jul 1898",]
sales.y.test.p <- sales.y[sales.y$date >= "Jul 1898",]

# Preprocess
homesteads.pre.train.p <- preProcess(homesteads.x.train.p[!colnames(homesteads.x.train.p) %in% c("date")], method = c("center", "scale","medianImpute"))
homesteads.x.train.p[!colnames(homesteads.x.train.p) %in% c("date")] <- predict(homesteads.pre.train.p, homesteads.x.train.p[!colnames(homesteads.x.train.p) %in% c("date")] )

homesteads.x.test.p[!colnames(homesteads.x.test.p) %in% c("date")] <- predict(homesteads.pre.train.p, homesteads.x.test.p[!colnames(homesteads.x.test.p) %in% c("date")] ) # use training values for test set 

sales.pre.train.p <- preProcess(sales.x.train.p[!colnames(sales.x.train.p) %in% c("date")], method = c("center", "scale","medianImpute"))
sales.x.train.p[!colnames(sales.x.train.p) %in% c("date")] <- predict(sales.pre.train.p, sales.x.train.p[!colnames(sales.x.train.p) %in% c("date")] )

sales.x.test.p[!colnames(sales.x.test.p) %in% c("date")] <- predict(sales.pre.train.p, sales.x.test.p[!colnames(sales.x.test.p) %in% c("date")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/patents/"

write.csv(homesteads.x.train.p[!colnames(homesteads.x.train.p) %in% c("date")], paste0(data.directory,"homesteads-x-train-placebo.csv"), row.names=FALSE) 
write.csv(homesteads.x.test.p[!colnames(homesteads.x.test.p) %in% c("date")], paste0(data.directory,"homesteads-x-test-placebo.csv"), row.names=FALSE) 
write.csv(homesteads.y.train.p[!colnames(homesteads.y.train.p) %in% c("date")], paste0(data.directory,"homesteads-y-train-placebo.csv"), row.names=FALSE) 
write.csv(homesteads.y.test.p[!colnames(homesteads.y.test.p) %in% c("date")], paste0(data.directory,"homesteads-y-test-placebo.csv"), row.names=FALSE) 

write.csv(sales.x.train.p[!colnames(sales.x.train.p) %in% c("date")], paste0(data.directory,"sales-x-train-placebo.csv"), row.names=FALSE) 
write.csv(sales.x.test.p[!colnames(sales.x.test.p) %in% c("date")] , paste0(data.directory,"sales-x-test-placebo.csv"), row.names=FALSE) 
write.csv(sales.y.train.p[!colnames(sales.y.train.p) %in% c("date")], paste0(data.directory,"sales-y-train-placebo.csv"), row.names=FALSE) 
write.csv(sales.y.test.p[!colnames(sales.y.test.p) %in% c("date")], paste0(data.directory,"sales-y-test-placebo.csv"), row.names=FALSE) 

# Save workspace
save.image("/Users/jason/Dropbox/ok-lottery-local/data/patents/glo-patents-county.RData")