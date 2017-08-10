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
patents <- patents[!is.na(patents$date) & !patents$date>2017,] # subset to non-missing and valid dates

# Save workspace
save.image(data.directory, "patents/patents.RData")

#############################################

# Summarize by date/county/state

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") #,30,public,land,states

patents.sum <- patents %>%
  filter(state_code %in% pub.states) %>% # only public land states
  group_by(date,county_code,state_code) %>%
  summarise_each(funs(sum),sales,homesteads)

# Category
southern.pub <- c("AL", "AR", "FL", "LA", "MS") # 5 southern public land states

patents.sum$cat <- ifelse(patents.sum$state_code %in% southern.pub, "Treated", "Control") 

# Create control and treated sums
cats.sums <- patents.sum %>% 
  group_by(date,cat) %>% 
  summarise_each(funs(sum),sales,homesteads) 

cats.sums.r <- reshape(data.frame(cats.sums), idvar = "date", timevar = "cat", direction = "wide")

## Analysis 1: Effect of SHA on treated (south) sales, intervention: June 1866-June 1876; period: - Feb 1889 (restrictions imposted March 1889)

# controls are non-south

patents.sum$id <- interaction(patents.sum$county_code,patents.sum$state_code)

patents.sum.control <- patents.sum[patents.sum$cat=="Control",] # discard treated since we have treated time-series

sales <- reshape(data.frame(patents.sum.control[c("date","id","sales")]), idvar = "date", timevar = "id", direction = "wide")

#Labels

sales.y <- cats.sums.r[c("date", "sales.Treated")]
sales.y <- sales.y[!is.na(sales.y$sales.Treated),]

# Splits

sales.dates <- intersect(sales$date,sales.y$date) # common sales dates in treated and control

sales.x.train <- sales[sales$date %in% sales.dates & sales$date <= "May 1866",]
sales.x.test <- sales[sales$date %in% sales.dates &sales$date >= "Jun 1866" & sales$date <= "Feb 1889",]

sales.y.train <- sales.y[sales.y$date %in% sales.dates & sales.y$date <= "May 1866",]
sales.y.test <- sales.y[sales.y$date %in% sales.dates &sales.y$date >= "Jun 1866" & sales.y$date <= "Feb 1889",]

# Preprocess
sales.pre.train <- preProcess(sales.x.train[!colnames(sales.x.train) %in% c("date")], method = c("center", "scale","medianImpute"))
sales.x.train[!colnames(sales.x.train) %in% c("date")] <- predict(sales.pre.train, sales.x.train[!colnames(sales.x.train) %in% c("date")] )

sales.x.test[!colnames(sales.x.test) %in% c("date")] <- predict(sales.pre.train, sales.x.test[!colnames(sales.x.test) %in% c("date")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/patents-public/analysis-12/treated/"

write.csv(sales.x.train[!colnames(sales.x.train) %in% c("date")], paste0(data.directory,"sales-x-train.csv"), row.names=FALSE) 
write.csv(sales.x.test[!colnames(sales.x.test) %in% c("date")] , paste0(data.directory,"sales-x-test.csv"), row.names=FALSE) 
write.csv(sales.y.train[!colnames(sales.y.train) %in% c("date")], paste0(data.directory,"sales-y-train.csv"), row.names=FALSE) 
write.csv(sales.y.test[!colnames(sales.y.test) %in% c("date")], paste0(data.directory,"sales-y-test.csv"), row.names=FALSE) 

# ## Analysis 2: Effect on controls (non-south), intervention: June 1866-June 1876; period: - Feb 1889 (restrictions imposted March 1889)
# 
# # treated is south
# 
# patents.sum.treated <- patents.sum[patents.sum$cat=="Treated",] # discard controls since we have controls time-series
# 
# sales <- reshape(data.frame(patents.sum.treated[c("date","id","sales")]), idvar = "date", timevar = "id", direction = "wide")
# 
# # Labels
# 
# sales.y <- cats.sums.r[c("date", "sales.Control")]
# sales.y <- sales.y[!is.na(sales.y$sales.Control),]
# 
# # Splits 
# 
# sales.dates <- intersect(sales$date,sales.y$date) # common sales dates in treated and control
# 
# sales.x.train <- sales[sales$date %in% sales.dates & sales$date <= "May 1866",]
# sales.x.test <- sales[sales$date %in% sales.dates &sales$date >= "Jun 1866" & sales$date <= "Feb 1889",]
# 
# sales.y.train <- sales.y[sales.y$date %in% sales.dates & sales.y$date <= "May 1866",]
# sales.y.test <- sales.y[sales.y$date %in% sales.dates &sales.y$date >= "Jun 1866" & sales.y$date <= "Feb 1889",]
# 
# # Preprocess
# 
# sales.pre.train <- preProcess(sales.x.train[!colnames(sales.x.train) %in% c("date")], method = c("center", "scale","medianImpute"))
# sales.x.train[!colnames(sales.x.train) %in% c("date")] <- predict(sales.pre.train, sales.x.train[!colnames(sales.x.train) %in% c("date")] )
# 
# sales.x.test[!colnames(sales.x.test) %in% c("date")] <- predict(sales.pre.train, sales.x.test[!colnames(sales.x.test) %in% c("date")] ) # use training values for test set 
# 
# # Export each as csv (labels, features)
# data.directory <- "~/Dropbox/github/drnns-prediction/data/patents-public/analysis-12/control/"
# 
# write.csv(sales.x.train[!colnames(sales.x.train) %in% c("date")], paste0(data.directory,"sales-x-train.csv"), row.names=FALSE) 
# write.csv(sales.x.test[!colnames(sales.x.test) %in% c("date")] , paste0(data.directory,"sales-x-test.csv"), row.names=FALSE) 
# write.csv(sales.y.train[!colnames(sales.y.train) %in% c("date")], paste0(data.directory,"sales-y-train.csv"), row.names=FALSE) 
# write.csv(sales.y.test[!colnames(sales.y.test) %in% c("date")], paste0(data.directory,"sales-y-test.csv"), row.names=FALSE) 

## Analysis 3: Effect on treated (south), intervention: Mar 1889; period: Jul 1876 - Oct 1976 (HSA repeal)

# Controls are MO counties

patents.sum$id <- interaction(patents.sum$county_code,patents.sum$state_code)

patents.sum.control <- patents.sum[patents.sum$state_code=="MO",] # discard treated since we have treated time-series

homesteads <- reshape(data.frame(patents.sum.control[c("date","id","homesteads")]), idvar = "date", timevar = "id", direction = "wide")

sales <- reshape(data.frame(patents.sum.control[c("date","id","sales")]), idvar = "date", timevar = "id", direction = "wide")

# Labels

homesteads.y <- cats.sums.r[c("date", "homesteads.Treated")]
homesteads.y <- homesteads.y[!is.na(homesteads.y$homesteads.Treated),]

sales.y <- cats.sums.r[c("date", "sales.Treated")]
sales.y <- sales.y[!is.na(sales.y$sales.Treated),]

# Splits

sales.dates <- intersect(sales$date,sales.y$date) # common sales dates in treated and control

sales.x.train <- sales[sales$date %in% sales.dates & sales$date >= "Jul 1876" & sales$date <= "Feb 1889",]
sales.x.test <- sales[sales$date %in% sales.dates & sales$date >= "Mar 1889" & sales$date <= "Oct 1976",]

sales.y.train <- sales.y[sales.y$date %in% sales.dates & sales.y$date >= "Jul 1876" & sales.y$date <= "Feb 1889",]
sales.y.test <- sales.y[sales.y$date %in% sales.dates & sales.y$date >= "Mar 1889" & sales.y$date <= "Oct 1976",]

homesteads.dates <- intersect(homesteads$date,homesteads.y$date) # common homesteads dates in treated and control

homesteads.x.train <- homesteads[homesteads$date %in% homesteads.dates & homesteads$date >= "Jul 1876" & homesteads$date <= "Feb 1889",]
homesteads.x.test <- homesteads[homesteads$date %in% homesteads.dates & homesteads$date >= "Mar 1889" & homesteads$date <= "Oct 1976",]

homesteads.y.train <- homesteads.y[homesteads.y$date %in% homesteads.dates & homesteads.y$date >= "Jul 1876" & homesteads.y$date <= "Feb 1889",]
homesteads.y.test <- homesteads.y[homesteads.y$date %in% homesteads.dates & homesteads.y$date >= "Mar 1889" & homesteads.y$date <= "Oct 1976",]

# Preprocess
homesteads.pre.train <- preProcess(homesteads.x.train[!colnames(homesteads.x.train) %in% c("date")], method = c("center", "scale","medianImpute"))
homesteads.x.train[!colnames(homesteads.x.train) %in% c("date")] <- predict(homesteads.pre.train, homesteads.x.train[!colnames(homesteads.x.train) %in% c("date")] )

homesteads.x.test[!colnames(homesteads.x.test) %in% c("date")] <- predict(homesteads.pre.train, homesteads.x.test[!colnames(homesteads.x.test) %in% c("date")] ) # use training values for test set 

sales.pre.train <- preProcess(sales.x.train[!colnames(sales.x.train) %in% c("date")], method = c("center", "scale","medianImpute"))
sales.x.train[!colnames(sales.x.train) %in% c("date")] <- predict(sales.pre.train, sales.x.train[!colnames(sales.x.train) %in% c("date")] )

sales.x.test[!colnames(sales.x.test) %in% c("date")] <- predict(sales.pre.train, sales.x.test[!colnames(sales.x.test) %in% c("date")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/patents-public/analysis-34/treated/"

write.csv(homesteads.x.train[!colnames(homesteads.x.train) %in% c("date")], paste0(data.directory,"homesteads-x-train.csv"), row.names=FALSE) 
write.csv(homesteads.x.test[!colnames(homesteads.x.test) %in% c("date")], paste0(data.directory,"homesteads-x-test.csv"), row.names=FALSE) 
write.csv(homesteads.y.train[!colnames(homesteads.y.train) %in% c("date")], paste0(data.directory,"homesteads-y-train.csv"), row.names=FALSE) 
write.csv(homesteads.y.test[!colnames(homesteads.y.test) %in% c("date")], paste0(data.directory,"homesteads-y-test.csv"), row.names=FALSE) 

write.csv(sales.x.train[!colnames(sales.x.train) %in% c("date")], paste0(data.directory,"sales-x-train.csv"), row.names=FALSE) 
write.csv(sales.x.test[!colnames(sales.x.test) %in% c("date")] , paste0(data.directory,"sales-x-test.csv"), row.names=FALSE) 
write.csv(sales.y.train[!colnames(sales.y.train) %in% c("date")], paste0(data.directory,"sales-y-train.csv"), row.names=FALSE) 
write.csv(sales.y.test[!colnames(sales.y.test) %in% c("date")], paste0(data.directory,"sales-y-test.csv"), row.names=FALSE) 

# ## Analysis 4: Effect on controls (missouri), intervention: Mar 1889; period: Jul 1876 - Oct 1976 (HSA repeal)
# 
# # treated is south
# 
# patents.sum.treated <- patents.sum[patents.sum$cat=="Treated",] # discard controls since we have controls time-series
# 
# homesteads <- reshape(data.frame(patents.sum.treated[c("date","id","homesteads")]), idvar = "date", timevar = "id", direction = "wide")
# 
# sales <- reshape(data.frame(patents.sum.treated[c("date","id","sales")]), idvar = "date", timevar = "id", direction = "wide")
# 
# # Labels
# 
# state.sums <- patents.sum %>% 
#   group_by(date,state_code) %>% 
#   summarise_each(funs(sum),sales,homesteads) 
# 
# state.sums.r <- reshape(data.frame(state.sums), idvar = "date", timevar = "cat", direction = "wide")
# 
# homesteads.y <- state.sums.r[c("date", "homesteads.MO")]
# homesteads.y <- homesteads.y[!is.na(homesteads.y$homesteads.MO),]
# 
# sales.y <- state.sums.r[c("date", "sales.MO")]
# sales.y <- sales.y[!is.na(sales.y$sales.MO),]
# 
# # Splits
# 
# sales.dates <- intersect(sales$date,sales.y$date) # common sales dates in treated and control
# 
# sales.x.train <- sales[sales$date %in% sales.dates & sales$date <= "May 1866",]
# sales.x.test <- sales[sales$date %in% sales.dates &sales$date >= "Jun 1866" & sales$date <= "Feb 1889",]
# 
# sales.y.train <- sales.y[sales.y$date %in% sales.dates & sales.y$date <= "May 1866",]
# sales.y.test <- sales.y[sales.y$date %in% sales.dates &sales.y$date >= "Jun 1866" & sales.y$date <= "Feb 1889",]
# 
# homesteads.dates <- intersect(homesteads$date,homesteads.y$date) # common homesteads dates in treated and control
# 
# homesteads.x.train <- homesteads[homesteads$date %in% homesteads.dates & homesteads$date <= "May 1866",]
# homesteads.x.test <- homesteads[homesteads$date %in% homesteads.dates &homesteads$date >= "Jun 1866" & homesteads$date <= "Feb 1889",]
# 
# homesteads.y.train <- homesteads.y[homesteads.y$date %in% homesteads.dates & homesteads.y$date <= "May 1866",]
# homesteads.y.test <- homesteads.y[homesteads.y$date %in% homesteads.dates &homesteads.y$date >= "Jun 1866" & homesteads.y$date <= "Feb 1889",]
# 
# # Preprocess
# sales.dates <- intersect(sales$date,sales.y$date) # common sales dates in treated and control
# 
# sales.x.train <- sales[sales$date %in% sales.dates & sales$date >= "Jul 1876" & sales$date <= "Feb 1889",]
# sales.x.test <- sales[sales$date %in% sales.dates & sales$date >= "Mar 1889" & sales$date <= "Oct 1976",]
# 
# sales.y.train <- sales.y[sales.y$date %in% sales.dates & sales.y$date >= "Jul 1876" & sales.y$date <= "Feb 1889",]
# sales.y.test <- sales.y[sales.y$date %in% sales.dates & sales.y$date >= "Mar 1889" & sales.y$date <= "Oct 1976",]
# 
# homesteads.dates <- intersect(homesteads$date,homesteads.y$date) # common homesteads dates in treated and control
# 
# homesteads.x.train <- homesteads[homesteads$date %in% homesteads.dates & homesteads$date >= "Jul 1876" & homesteads$date <= "Feb 1889",]
# homesteads.x.test <- homesteads[homesteads$date %in% homesteads.dates & homesteads$date >= "Mar 1889" & homesteads$date <= "Oct 1976",]
# 
# homesteads.y.train <- homesteads.y[homesteads.y$date %in% homesteads.dates & homesteads.y$date >= "Jul 1876" & homesteads.y$date <= "Feb 1889",]
# homesteads.y.test <- homesteads.y[homesteads.y$date %in% homesteads.dates & homesteads.y$date >= "Mar 1889" & homesteads.y$date <= "Oct 1976",]
# 
# # Export each as csv (labels, features)
# data.directory <- "~/Dropbox/github/drnns-prediction/data/patents-public/analysis-34/control/"
# 
# write.csv(homesteads.x.train[!colnames(homesteads.x.train) %in% c("date")], paste0(data.directory,"homesteads-x-train.csv"), row.names=FALSE) 
# write.csv(homesteads.x.test[!colnames(homesteads.x.test) %in% c("date")], paste0(data.directory,"homesteads-x-test.csv"), row.names=FALSE) 
# write.csv(homesteads.y.train[!colnames(homesteads.y.train) %in% c("date")], paste0(data.directory,"homesteads-y-train.csv"), row.names=FALSE) 
# write.csv(homesteads.y.test[!colnames(homesteads.y.test) %in% c("date")], paste0(data.directory,"homesteads-y-test.csv"), row.names=FALSE) 
# 
# write.csv(sales.x.train[!colnames(sales.x.train) %in% c("date")], paste0(data.directory,"sales-x-train.csv"), row.names=FALSE) 
# write.csv(sales.x.test[!colnames(sales.x.test) %in% c("date")] , paste0(data.directory,"sales-x-test.csv"), row.names=FALSE) 
# write.csv(sales.y.train[!colnames(sales.y.train) %in% c("date")], paste0(data.directory,"sales-y-train.csv"), row.names=FALSE) 
# write.csv(sales.y.test[!colnames(sales.y.test) %in% c("date")], paste0(data.directory,"sales-y-test.csv"), row.names=FALSE) 