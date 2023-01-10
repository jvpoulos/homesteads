#############################################
# Propensity score diagnostic plot         #
#############################################

library(MCPanel)
library(data.table)
library(reshape)
library(reshape2)
library(dplyr)
library(glmnet)
library(ggplot2)

source('code/utils.R')

# Load data
outcomes.missing <- readRDS("data/capacity-outcomes-none.rds")
outcomes.imputed <- readRDS(paste0("data/capacity-outcomes-mice-pmm.rds")) 
capacity.outcomes.linear <- readRDS("data/capacity-outcomes-linear.rds") # for covariates

outcomes.labels <- data.frame('rev.pc'="Log per-capita state government revenue (1982$)",
                              'exp.pc'= "Log per-capita state government expenditure (1982$)")

for(d in c('rev.pc','exp.pc')){
  
  t0 <- which(colnames(outcomes.missing[[d]]$M)=="1869")
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")][sort(rownames(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")])),], 
                           capacity.outcomes.linear[[d]]$farmsize[,c("1860")][sort(names(capacity.outcomes.linear[[d]]$farmsize[,c("1860")]))],
                           c("AZ"=0, "NM"=0, capacity.outcomes.linear[[d]]$access[,c("1860")])[sort(c(names(capacity.outcomes.linear[[d]]$access[,c("1860")]),"AZ","NM"))]) # AZ and NM not in dataset
  
  colnames(capacity.covars) <- c("faval.1850","faval.1860","farmsize.1860", "access.1860")
  
  capacity.covars <-capacity.covars[match(rownames(outcomes.imputed[[d]]$M), rownames(capacity.covars)), ] # same order
  capacity.covars[is.na(capacity.covars)] <- 0
  
  covars.x <- scale(matrix(capacity.covars[,1],N,T) + matrix(capacity.covars[,2],N,T) + matrix(capacity.covars[,3],N,T) +matrix(capacity.covars[,4],N,T)) # need to combine covars in NxT matrix
  
  # capacity.covars <- apply(capacity.covars, 2, scale) # scale
  capacity.covars <- as.data.frame(capacity.covars)
  capacity.covars$state <- rownames(outcomes.imputed[[d]]$M)
  
  pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
  treat_indices_order <- row.names(outcomes.imputed[[d]]$M)[row.names(outcomes.imputed[[d]]$M)%in% pub.states]
  
  # specify outcome and treatment matrix
  Y.missing <- outcomes.missing[[d]]$M  # to train on: 0s = missing
  Y <- outcomes.imputed[[d]]$M # NxT  # imputed outcomes
  missing.mat <- outcomes.imputed[[d]]$M.missing
  missing.mat[is.na(missing.mat)] <- 0 # 0s are missing/imputed
  treat <- outcomes.imputed[[d]]$mask# NxT masked matrix: 0 for control units and treated units before treatment and 1 for treated units after treatment
  
  N <- dim(treat)[1]
  T <- dim(treat)[2]
  
  mask <- 1-treat # masked matrix, 1= control units and treated units before treatment and 0 = treated units after treatment
  rownames(mask) <- rownames(treat)
  colnames(mask) <- colnames(treat)
  
  Y_obs <- Y * mask * missing.mat
  
  fr_obs <- sum(mask)/(N*T) # store fraction observed entries
  print(paste0("fraction observed: ", fr_obs))
  
  # get vector of initial treatment periods for N_t treated units
  
  A <- aggregate(col ~ row,
                 data = which(mask == 0, arr.ind = TRUE),
                 FUN = function(x) x[1])$col # gives the intial T0s for treated units
  
  ST <- aggregate(col ~ row,
                  data = which(mask == 0, arr.ind = TRUE),
                  FUN = function(x) x[1])$row  # switch treated indices
  NT <- setdiff(1:N, ST) # control indices
  
  ## Estimate propensity scores
  
  p.mod <- cv.glmnet(x=cbind(covars.x,Y_obs[,1:(t0-1)]), y=(1-mask*missing.mat), family="mgaussian", alpha=1,nfolds=10,intercept=FALSE)
  W <- predict(p.mod, cbind(covars.x,Y_obs[,1:(t0-1)]))[,,1]
  
  p.weights <- matrix(NA, nrow=nrow(mask), ncol=ncol(mask), dimnames = list(rownames(mask), colnames(mask)))
  p.weights <- boundProbs(W)/(1-boundProbs(W))
  
  ## -----
  ## Diagnostic plot
  ## -----
  
  prop.dat <- melt(Y)
  colnames(prop.dat) <- c("state","year","Y")
  prop.dat$treat <- ifelse(prop.dat$state%in%pub.states,1,0)
  prop.dat$exp <- melt(1-mask)$value
  prop.dat$pscore <- melt(boundProbs(W))$value
  prop.dat <- merge(prop.dat,capacity.covars,by="state",all.x = TRUE)
  
  treat.labs <- paste("Treatment status:", c("Treated", "Control"))
  exp.labs <- paste("Experimental period:", c("Post-", "Pre-"))
  
  prop.plot <- prop.dat %>%
    mutate(treat = ifelse(treat == 1, treat.labs[1], treat.labs[2]),
           exp = ifelse(exp == 1, exp.labs[1], exp.labs[2])) %>%
    ggplot(aes(x = pscore)) +
    geom_histogram(color = "white") +
    facet_wrap(treat~exp) +
    xlab("Estimated probabiltiy of treatment") +
    theme_bw()
  
  ggsave(paste0(plots.directory,"/propensity-plot-",d,".png"), prop.plot, scale=1.25)
}