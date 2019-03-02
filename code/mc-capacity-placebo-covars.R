###################################
# MC estimates #
###################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(ggplot2)
library(latex2exp)
library(missMDA)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- 28#detectCores()

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Load data
capacity.outcomes <- readRDS("capacity-outcomes.rds")
capacity.covars.placebo <- readRDS("capacity-covariates-placebo.rds")

# Discard post-treatment 

capacity.outcomes$educ.pc$M <- capacity.outcomes$educ.pc$M[,1:(ncol(capacity.outcomes$educ.pc$M)-70)] 
capacity.outcomes$educ.pc$M.missing <- capacity.outcomes$educ.pc$M.missing[,1:(ncol(capacity.outcomes$educ.pc$M.missing)-70)]
capacity.outcomes$educ.pc$mask <- capacity.outcomes$educ.pc$mask[,1:(ncol(capacity.outcomes$educ.pc$mask)-70)] 

capacity.outcomes$rev.pc$M <- capacity.outcomes$rev.pc$M[,1:(ncol(capacity.outcomes$rev.pc$M)-72)] 
capacity.outcomes$rev.pc$M.missing <- capacity.outcomes$rev.pc$M.missing[,1:(ncol(capacity.outcomes$rev.pc$M.missing)-72)]
capacity.outcomes$rev.pc$mask <- capacity.outcomes$rev.pc$mask[,1:(ncol(capacity.outcomes$rev.pc$mask)-72)] 

capacity.outcomes$exp.pc$M <- capacity.outcomes$exp.pc$M[,1:(ncol(capacity.outcomes$exp.pc$M)-73)] 
capacity.outcomes$exp.pc$M.missing <- capacity.outcomes$exp.pc$M.missing[,1:(ncol(capacity.outcomes$exp.pc$M.missing)-73)]
capacity.outcomes$exp.pc$mask <- capacity.outcomes$exp.pc$mask[,1:(ncol(capacity.outcomes$exp.pc$mask)-73)] 

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]],"educ.pc"=capacity.outcomes[["educ.pc"]])

source("MCEst.R")

# Get NxT matrix of point estimates

t_final_placebo <- ncol(capacity.outcomes[["rev.pc"]]$M) # all periods - same for each outcome

taus <- c(5,10,15)

treat_indices_order <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")

mc.est.placebo.w <- foreach(tau = taus) %dopar% {
      t0_placebo <- t_final_placebo-(tau) # n pre-treatment periods
      mclapply(capacity.outcomes.list,
                              MCEst,t0=t0_placebo,treat_indices_order,sim=FALSE, covars=capacity.covars.placebo,pca=FALSE,mc.cores=cores)}
saveRDS(mc.est.placebo.w,"mc_est_placebo.rds")

# Get p-values
source("ChernoTest.R")

moving.block.placebo.w <- foreach(tau = taus) %dopar% {
  t0_placebo <- t_final_placebo-tau # n pre-treatment periods
  mclapply(capacity.outcomes.list,
           ChernoTest, ns=1000, treat_indices_order=treat_indices_order, permtype="moving.block",t0=t0_placebo,imputed=FALSE,sim=FALSE,covars=capacity.covars.placebo,pca=FALSE,mc.cores=cores)}
saveRDS(moving.block.placebo.w,"moving_block_placebo_w.rds")

iid.block.placebo.w <- foreach(tau = taus) %dopar% {
  t0_placebo <- t_final_placebo-tau # n pre-treatment periods
  mclapply(capacity.outcomes.list,
           ChernoTest, ns=1000, treat_indices_order=treat_indices_order, permtype="iid.block",t0=t0_placebo,imputed=FALSE,sim=FALSE,covars=capacity.covars.placebo,pca=FALSE,mc.cores=cores)}
saveRDS(iid.block.placebo,"iid_block_placebo_w.rds")