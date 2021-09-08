###################################
# MC placebo estimates #
###################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(ggplot2)
library(latex2exp)
library(Matrix)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- detectCores()

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Load data
capacity.outcomes.mice <-readRDS("data/capacity-outcomes-mice.rds")
capacity.outcomes.linear <- readRDS("data/capacity-outcomes-linear.rds") # for covariates

capacity.outcomes <- list()
# Discard post-treatment 

for(d in c('rev.pc','exp.pc')){
  
  t0.loc <- which(colnames(capacity.outcomes.mice[[d]]$M)=="1869")
  capacity.outcomes[[d]]$M <- capacity.outcomes.mice[[d]]$M[,1:(t0.loc-1)] 
  capacity.outcomes[[d]]$M.missing <- capacity.outcomes.mice[[d]]$M.missing[,1:(t0.loc-1)]
  capacity.outcomes[[d]]$mask <- capacity.outcomes.mice[[d]]$mask[,1:(t0.loc-1)] 
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")][sort(rownames(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")])),], 
                           capacity.outcomes.linear[[d]]$farmsize[,c("1860")][sort(names(capacity.outcomes.linear[[d]]$farmsize[,c("1860")]))],
                           c("AZ"=0, "NM"=0, capacity.outcomes.linear[[d]]$access[,c("1860")])[sort(c(names(capacity.outcomes.linear[[d]]$access[,c("1860")]),"AZ","NM"))]) # AZ and NM not in dataset
  
  colnames(capacity.covars) <- c("faval.1850","faval.1860","farmsize.1860", "access.1860")
  
  capacity.covars <-capacity.covars[match(rownames(capacity.outcomes[[d]]$M), rownames(capacity.covars)), ] # same order
  capacity.covars[is.na(capacity.covars)] <- 0
  
  ## Estimate propensity scores
  
  treat_mat <- 1-capacity.outcomes.mice[[d]]$mask
  
  p.mod <- cv.glmnet(x=cbind(capacity.covars,capacity.outcomes.mice[[d]]$M[,1:(t0.loc-1)]), y=(1-treat_mat), family="mgaussian", alpha=1, parallel=TRUE,intercept=FALSE,type.multinomial="grouped",nfolds=5) 
  W <- predict(p.mod, cbind(capacity.covars,capacity.outcomes.mice[[d]]$M[,1:(t0.loc-1)]))[,,1]
  
  boundProbs <- function(x,bounds=c(0.01,0.99)){
    x[x>max(bounds)] <- max(bounds)
    x[x<min(bounds)] <- min(bounds)
    return(x)
  }
  
  p.weights <- matrix(NA, nrow=nrow(treat_mat), ncol=ncol(treat_mat), dimnames = list(rownames(treat_mat), colnames(treat_mat)))
  capacity.outcomes[[d]]$p.weights <- (1-treat_mat)*(1-boundProbs(W)) + (treat_mat)*(boundProbs(W))
}

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]])

source("code/MCEst.R")

# Get NxT matrix of point estimates

t_final_placebo <- ncol(capacity.outcomes[["rev.pc"]]$M) # all periods - same for each outcome

taus <- c(1,10,25)

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
treat_indices_order <- row.names(capacity.outcomes.mice$rev.pc$M)[row.names(capacity.outcomes.mice$rev.pc$M)%in% pub.states] # same for each outcome

# Get p-values
source("code/ChernoTest.R")

moving.block.placebo <- foreach(tau = taus) %dopar% {
  t0_placebo <- t_final_placebo-tau # n pre-treatment periods
  mclapply(capacity.outcomes.list,
           ChernoTest, t0=t0_placebo, ns=1000, treat_indices_order=treat_indices_order, permtype="moving.block",imputed=FALSE,mc.cores=cores)}
saveRDS(moving.block.placebo,"moving_block_placebo.rds")

iid.block.placebo <- foreach(tau = taus) %dopar% {
  t0_placebo <- t_final_placebo-tau # n pre-treatment periods
  mclapply(capacity.outcomes.list,
           ChernoTest, t0=t0_placebo,ns=1000, treat_indices_order=treat_indices_order, permtype="iid.block",imputed=FALSE,mc.cores=cores)}
saveRDS(iid.block.placebo,"iid_block_placebo.rds")

iid.placebo <- foreach(tau = taus) %dopar% {
  t0_placebo <- t_final_placebo-tau # n pre-treatment periods
  mclapply(capacity.outcomes.list,
           ChernoTest,t0=t0_placebo, ns=1000, treat_indices_order=treat_indices_order, permtype="iid",imputed=FALSE,mc.cores=cores)}
saveRDS(iid.block.placebo,"iid_placebo.rds")