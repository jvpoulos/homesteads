## Analysis 0: Effect of HSA on treated (western public land states), intervention: May 1862
# controls are northeastern state land states

# Summarize by category

rr.0 <- rr.inter.m

rr.0$cat <- NA
rr.0$cat[rr.0$state %in% setdiff(pub.states,southern.pub)] <- "Treated"
rr.0$cat[rr.0$state %in% setdiff(state.land.states,southern.state)] <- "Control"

rr.0$year <- rr.0$InOpBy

# Create control and treated sums
cats.rr.0 <- rr.0 %>% 
  select(year, cat, access)  %>%
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

cats.rr.0 <- select(cats.rr.0, -ID_NUM)

cats.rr.0.r <- reshape(data.frame(cats.rr.0), idvar = "year", timevar = "cat", direction = "wide")

rr.0.control <- rr.0[!is.na(rr.0$cat) & rr.0$cat=="Control",] # discard treated since we have treated time-series

access.0 <- reshape(data.frame(rr.0.control)[c("access","ID_NUM","year")], idvar = "year", timevar = "ID_NUM", direction = "wide") # county-level analysis 

#Labels

access.0.y <- cats.rr.0.r[c("year", "access.Treated")]
access.0.y <- access.0.y[!is.na(access.0.y$access.Treated),]

# Splits

access.0.years <- intersect(access.0$year,access.0.y$year) # common access.0 years in treated and control

access.0.x.train <- access.0[access.0$year %in% access.0.years & access.0$year < 1859,]
access.0.x.val <- access.0[access.0$year %in% access.0.years & (access.0$year >= 1859   & access.0$year < 1862),]
access.0.x.test <- access.0[access.0$year %in% access.0.years & access.0$year >= 1862,]

access.0.y.train <- access.0.y[access.0.y$year %in% access.0.years & access.0.y$year < 1859,]
access.0.y.val <- access.0.y[access.0.y$year %in% access.0.years & (access.0.y$year >= 1859  & access.0.y$year < 1862),]
access.0.y.test <- access.0.y[access.0.y$year %in% access.0.years &access.0.y$year >= 1862,]

# Preprocess
access.0.x.train <- data.frame(sapply(access.0.x.train, as.numeric))
access.0.x.train[is.na(access.0.x.train)] <- 0 # fill NA with 0 before scale
access.0.pre.train <- preProcess(access.0.x.train[!colnames(access.0.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
access.0.x.train[!colnames(access.0.x.train) %in% c("year")] <- predict(access.0.pre.train, access.0.x.train[!colnames(access.0.x.train) %in% c("year")] )

access.0.x.val <- data.frame(sapply(access.0.x.val, as.numeric))
access.0.x.val[!colnames(access.0.x.val) %in% c("year")] <- predict(access.0.pre.train, access.0.x.val[!colnames(access.0.x.val) %in% c("year")] ) # use training values for val set 

access.0.x.test <- data.frame(sapply(access.0.x.test, as.numeric))
access.0.x.test[!colnames(access.0.x.test) %in% c("year")] <- predict(access.0.pre.train, access.0.x.test[!colnames(access.0.x.test) %in% c("year")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/railroads/analysis-01/treated/"

write.csv(access.0.x.train[!colnames(access.0.x.train) %in% c("year")], paste0(data.directory,"access-x-train.csv"), row.names=FALSE) 
write.csv(access.0.x.val[!colnames(access.0.x.val) %in% c("year")] , paste0(data.directory,"access-x-val.csv"), row.names=FALSE) 
write.csv(access.0.x.test[!colnames(access.0.x.test) %in% c("year")] , paste0(data.directory,"access-x-test.csv"), row.names=FALSE) 
write.csv(access.0.y.train[!colnames(access.0.y.train) %in% c("year")], paste0(data.directory,"access-y-train.csv"), row.names=FALSE) 
write.csv(access.0.y.val[!colnames(access.0.y.val) %in% c("year")], paste0(data.directory,"access-y-val.csv"), row.names=FALSE) 
write.csv(access.0.y.test[!colnames(access.0.y.test) %in% c("year")], paste0(data.directory,"access-y-test.csv"), row.names=FALSE) 

## Analysis 1: Effect of SHA on treated (southern public land states), intervention: June 1866-June 1876-March 1889
# controls are southern state land states

# Summarize by category

rr.1 <- rr.inter.m

rr.1$cat <- NA
rr.1$cat[rr.1$state %in% southern.pub] <- "Treated"
rr.1$cat[rr.1$state %in% southern.state] <- "Control"

rr.1$year <- rr.1$InOpBy

# Create control and treated sums
cats.rr.1 <- rr.1 %>% 
  select(year, cat, access)  %>%
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

cats.rr.1 <- select(cats.rr.1, -ID_NUM)

cats.rr.1.r <- reshape(data.frame(cats.rr.1), idvar = "year", timevar = "cat", direction = "wide")

rr.1.control <- rr.1[!is.na(rr.1$cat) & rr.1$cat=="Control",] # discard treated since we have treated time-series

access.1 <- reshape(data.frame(rr.1.control)[c("access","ID_NUM","year")], idvar = "year", timevar = "ID_NUM", direction = "wide") # county-level analysis 

#Labels

access.1.y <- cats.rr.1.r[c("year", "access.Treated")]
access.1.y <- access.1.y[!is.na(access.1.y$access.Treated),]

# Splits

access.1.years <- intersect(access.1$year,access.1.y$year) # common access.1 years in treated and control

access.1.x.train <- access.1[access.1$year %in% access.1.years & access.1$year < 1859,]
access.1.x.val <- access.1[access.1$year %in% access.1.years & (access.1$year >= 1859 & access.1$year < 1866),]
access.1.x.test <- access.1[access.1$year %in% access.1.years & access.1$year >= 1866,]

access.1.y.train <- access.1.y[access.1.y$year %in% access.1.years & access.1.y$year < 1859,]
access.1.y.val <- access.1.y[access.1.y$year %in% access.1.years & (access.1.y$year >= 1859 & access.1.y$year < 1866),]
access.1.y.test <- access.1.y[access.1.y$year %in% access.1.years &access.1.y$year >= 1866,]

# Preprocess
access.1.x.train <- data.frame(sapply(access.1.x.train, as.numeric))
access.1.x.train[is.na(access.1.x.train)] <- 0 # fill NA with 0 before scale
access.1.pre.train <- preProcess(access.1.x.train[!colnames(access.1.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
access.1.x.train[!colnames(access.1.x.train) %in% c("year")] <- predict(access.1.pre.train, access.1.x.train[!colnames(access.1.x.train) %in% c("year")] )

access.1.x.val <- data.frame(sapply(access.1.x.val, as.numeric))
access.1.x.val[!colnames(access.1.x.val) %in% c("year")] <- predict(access.1.pre.train, access.1.x.val[!colnames(access.1.x.val) %in% c("year")] ) # use training values for val set 

access.1.x.test <- data.frame(sapply(access.1.x.test, as.numeric))
access.1.x.test[!colnames(access.1.x.test) %in% c("year")] <- predict(access.1.pre.train, access.1.x.test[!colnames(access.1.x.test) %in% c("year")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/railroads/analysis-12/treated/"

write.csv(access.1.x.train[!colnames(access.1.x.train) %in% c("year")], paste0(data.directory,"access-x-train.csv"), row.names=FALSE) 
write.csv(access.1.x.val[!colnames(access.1.x.val) %in% c("year")] , paste0(data.directory,"access-x-val.csv"), row.names=FALSE) 
write.csv(access.1.x.test[!colnames(access.1.x.test) %in% c("year")] , paste0(data.directory,"access-x-test.csv"), row.names=FALSE) 
write.csv(access.1.y.train[!colnames(access.1.y.train) %in% c("year")], paste0(data.directory,"access-y-train.csv"), row.names=FALSE) 
write.csv(access.1.y.val[!colnames(access.1.y.val) %in% c("year")], paste0(data.directory,"access-y-val.csv"), row.names=FALSE) 
write.csv(access.1.y.test[!colnames(access.1.y.test) %in% c("year")], paste0(data.directory,"access-y-test.csv"), row.names=FALSE) 

## Analysis 3: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is southern public land states (not MO)
# Controls are MO, state land states

rr.3 <- rr.inter.m

# Summarize by category

rr.3$cat <- NA
rr.3$cat[rr.3$state %in% southern.pub] <- "Treated"
rr.3$cat[rr.3$state %in% c("MO",state.land.states)] <- "Control"

rr.3$year <- rr.3$InOpBy

# Create control and treated sums
cats.rr.3 <- rr.3 %>% 
  select(year, cat, access)  %>%
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

cats.rr.3 <- select(cats.rr.3, -ID_NUM)

cats.rr.3.r <- reshape(data.frame(cats.rr.3), idvar = "year", timevar = "cat", direction = "wide")

rr.3.control <- rr.3[!is.na(rr.3$cat) & rr.3$cat=="Control",] # discard treated since we have treated time-series

access.3 <- reshape(data.frame(rr.3.control)[c("access","ID_NUM","year")], idvar = "year", timevar = "ID_NUM", direction = "wide") # county-level analysis

# Labels

access.3.y <- cats.rr.3.r[c("year", "access.Treated")]
access.3.y <- access.3.y[!is.na(access.3.y$access.Treated),]

# Splits

access.3.years <- intersect(access.3$year,access.3.y$year) # common access.3 years in treated and control

access.3.x.train <- access.3[access.3$year %in% access.3.years & access.3$year < 1886,]
access.3.x.val <- access.3[access.3$year %in% access.3.years & (access.3$year >= 1886 & access.3$year < 1889),]
access.3.x.test <- access.3[access.3$year %in% access.3.years & access.3$year >= 1889,]

access.3.y.train <- access.3.y[access.3.y$year %in% access.3.years & access.3.y$year < 1886,]
access.3.y.val <- access.3.y[access.3.y$year %in% access.3.years & (access.3.y$year >= 1886 & access.3.y$year < 1889),]
access.3.y.test <- access.3.y[access.3.y$year %in% access.3.years & access.3.y$year >= 1889,]

# Preprocess

access.3.x.train <- data.frame(sapply(access.3.x.train, as.numeric))
access.3.x.train[is.na(access.3.x.train)] <- 0 # fill NA with 0 before scale
access.3.pre.train <- preProcess(access.3.x.train[!colnames(access.3.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
access.3.x.train[!colnames(access.3.x.train) %in% c("year")] <- predict(access.3.pre.train, access.3.x.train[!colnames(access.3.x.train) %in% c("year")] )

access.3.x.val <- data.frame(sapply(access.3.x.val, as.numeric))
access.3.x.val[!colnames(access.3.x.val) %in% c("year")] <- predict(access.3.pre.train, access.3.x.val[!colnames(access.3.x.val) %in% c("year")] ) # use training values for val set 

access.3.x.test <- data.frame(sapply(access.3.x.test, as.numeric))
access.3.x.test[!colnames(access.3.x.test) %in% c("year")] <- predict(access.3.pre.train, access.3.x.test[!colnames(access.3.x.test) %in% c("year")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/railroads/analysis-34/treated/"

write.csv(access.3.x.train[!colnames(access.3.x.train) %in% c("year")], paste0(data.directory,"access-x-train.csv"), row.names=FALSE) 
write.csv(access.3.x.val[!colnames(access.3.x.val) %in% c("year")] , paste0(data.directory,"access-x-val.csv"), row.names=FALSE) 
write.csv(access.3.x.test[!colnames(access.3.x.test) %in% c("year")] , paste0(data.directory,"access-x-test.csv"), row.names=FALSE) 
write.csv(access.3.y.train[!colnames(access.3.y.train) %in% c("year")], paste0(data.directory,"access-y-train.csv"), row.names=FALSE) 
write.csv(access.3.y.val[!colnames(access.3.y.val) %in% c("year")], paste0(data.directory,"access-y-val.csv"), row.names=FALSE) 
write.csv(access.3.y.test[!colnames(access.3.y.test) %in% c("year")], paste0(data.directory,"access-y-test.csv"), row.names=FALSE) 

## Analysis 4: Effect of HSA restriction on treated, intervention: Mar 1889
# Treated is western public land states (not MO)
# Controls are MO, state land states

rr.4 <- rr.inter.m

# Summarize by category

rr.4$cat <- NA
rr.4$cat[rr.4$state %in% setdiff(setdiff(pub.states,southern.pub), "MO")] <- "Treated"
rr.4$cat[rr.4$state %in% c("MO",state.land.states)] <- "Control"

rr.4$year <- rr.4$InOpBy

# Create control and treated sums
cats.rr.4 <- rr.4 %>% 
  select(year, cat, access)  %>%
  filter(!is.na(cat)) %>% # rm non-southern state land states
  group_by(year,cat) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

cats.rr.4 <- select(cats.rr.4, -ID_NUM)

cats.rr.4.r <- reshape(data.frame(cats.rr.4), idvar = "year", timevar = "cat", direction = "wide")

rr.4.control <- rr.4[!is.na(rr.4$cat) & rr.4$cat=="Control",] # discard treated since we have treated time-series

access.4 <- reshape(data.frame(rr.4.control)[c("access","ID_NUM","year")], idvar = "year", timevar = "ID_NUM", direction = "wide") # county-level analysis

# Labels

access.4.y <- cats.rr.4.r[c("year", "access.Treated")]
access.4.y <- access.4.y[!is.na(access.4.y$access.Treated),]

# Splits

access.4.years <- intersect(access.4$year,access.4.y$year) # common access.4 years in treated and control

access.4.x.train <- access.4[access.4$year %in% access.4.years & access.4$year < 1886,]
access.4.x.val <- access.4[access.4$year %in% access.4.years & (access.4$year >= 1886 & access.4$year < 1889),]
access.4.x.test <- access.4[access.4$year %in% access.4.years & access.4$year >= 1889,]

access.4.y.train <- access.4.y[access.4.y$year %in% access.4.years & access.4.y$year < 1886,]
access.4.y.val <- access.4.y[access.4.y$year %in% access.4.years & (access.4.y$year >= 1886 & access.4.y$year < 1889),]
access.4.y.test <- access.4.y[access.4.y$year %in% access.4.years & access.4.y$year >= 1889,]

# Preprocess

access.4.x.train <- data.frame(sapply(access.4.x.train, as.numeric))
access.4.x.train[is.na(access.4.x.train)] <- 0 # fill NA with 0 before scale
access.4.pre.train <- preProcess(access.4.x.train[!colnames(access.4.x.train) %in% c("year")], method = c("center", "scale","medianImpute"))
access.4.x.train[!colnames(access.4.x.train) %in% c("year")] <- predict(access.4.pre.train, access.4.x.train[!colnames(access.4.x.train) %in% c("year")] )

access.4.x.val <- data.frame(sapply(access.4.x.val, as.numeric))
access.4.x.val[!colnames(access.4.x.val) %in% c("year")] <- predict(access.4.pre.train, access.4.x.val[!colnames(access.4.x.val) %in% c("year")] ) # use training values for val set 

access.4.x.test <- data.frame(sapply(access.4.x.test, as.numeric))
access.4.x.test[!colnames(access.4.x.test) %in% c("year")] <- predict(access.4.pre.train, access.4.x.test[!colnames(access.4.x.test) %in% c("year")] ) # use training values for test set 

# Export each as csv (labels, features)
data.directory <- "~/Dropbox/github/drnns-prediction/data/railroads/analysis-41/treated/"

write.csv(access.4.x.train[!colnames(access.4.x.train) %in% c("year")], paste0(data.directory,"access-x-train.csv"), row.names=FALSE) 
write.csv(access.4.x.val[!colnames(access.4.x.val) %in% c("year")] , paste0(data.directory,"access-x-val.csv"), row.names=FALSE) 
write.csv(access.4.x.test[!colnames(access.4.x.test) %in% c("year")] , paste0(data.directory,"access-x-test.csv"), row.names=FALSE) 
write.csv(access.4.y.train[!colnames(access.4.y.train) %in% c("year")], paste0(data.directory,"access-y-train.csv"), row.names=FALSE) 
write.csv(access.4.y.val[!colnames(access.4.y.val) %in% c("year")], paste0(data.directory,"access-y-val.csv"), row.names=FALSE) 
write.csv(access.4.y.test[!colnames(access.4.y.test) %in% c("year")], paste0(data.directory,"access-y-test.csv"), row.names=FALSE) 