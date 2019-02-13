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

# Discard post-treatment 

t0 <- which(colnames(capacity.outcomes[["rev.pc"]]$M)=="1869") # first treatment 
t_final <- ncol(capacity.outcomes[["rev.pc"]]$M) # all periods
t_star <- t_final-t0

capacity.outcomes$educ.pc$M <- capacity.outcomes$educ.pc$M[,1:(ncol(capacity.outcomes$educ.pc$M)-t_star)] 
capacity.outcomes$educ.pc$M.missing <- capacity.outcomes$educ.pc$M.missing[,1:(ncol(capacity.outcomes$educ.pc$M.missing)-t_star)]
capacity.outcomes$educ.pc$mask <- capacity.outcomes$educ.pc$mask[,1:(ncol(capacity.outcomes$educ.pc$mask)-t_star)] 

capacity.outcomes$rev.pc$M <- capacity.outcomes$rev.pc$M[,1:(ncol(capacity.outcomes$rev.pc$M)-t_star)] 
capacity.outcomes$rev.pc$M.missing <- capacity.outcomes$rev.pc$M.missing[,1:(ncol(capacity.outcomes$rev.pc$M.missing)-t_star)]
capacity.outcomes$rev.pc$mask <- capacity.outcomes$rev.pc$mask[,1:(ncol(capacity.outcomes$rev.pc$mask)-t_star)] 

capacity.outcomes$exp.pc$M <- capacity.outcomes$exp.pc$M[,1:(ncol(capacity.outcomes$exp.pc$M)-t_star)] 
capacity.outcomes$exp.pc$M.missing <- capacity.outcomes$exp.pc$M.missing[,1:(ncol(capacity.outcomes$exp.pc$M.missing)-t_star)]
capacity.outcomes$exp.pc$mask <- capacity.outcomes$exp.pc$mask[,1:(ncol(capacity.outcomes$exp.pc$mask)-t_star)] 

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]],"educ.pc"=capacity.outcomes[["educ.pc"]])

source("MCEst.R")

# Get NxT matrix of point estimates

t_final_placebo <- ncol(capacity.outcomes[["rev.pc"]]$M) # all periods

taus <- c(10,20,30)

mc.est.placebo <- foreach(tau = taus) %dopar% {
      t0_placebo <- t_final_placebo-(tau+1) # n pre-treatment periods
      mclapply(capacity.outcomes.list,
                              MCEst,t0=t0_placebo,sim=FALSE, covars=NULL,pca=FALSE,mc.cores=cores)}
saveRDS(mc.est.placebo,"mc_est_placebo.rds")

# Get p-values
source("ChernoTest.R")

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states

iid.placebo <- foreach(tau = taus) %dopar% {
       t0_placebo <- t_final_placebo-(tau+1) # n pre-treatment periods
       t_star_placebo <- t_final_placebo-t0_placebo
       mclapply(capacity.outcomes.list, 
                ChernoCI, t_star=t_star_placebo, c.range=c(-6,6), sd=0.5, alpha=0.025, l=5000, prec=1e-02, ns=100, treated.indices=pub.states, permtype="iid",t0=t0_placebo,sim=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)}
saveRDS(iid.placebo,"iid_placebo.rds")

moving.block.placebo <- foreach(tau = taus) %dopar% {
  t0_placebo <- t_final_placebo-(tau+1) # n pre-treatment periods
  t_star_placebo <- t_final_placebo-t0_placebo
  mclapply(capacity.outcomes.list, 
           ChernoCI, t_star=t_star_placebo, c.range=c(-6,6), sd=0.5, alpha=0.025, l=5000, prec=1e-02, ns=100, treated.indices=pub.states, permtype="moving.block",t0=t0_placebo,sim=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)}
saveRDS(moving.block.placebo,"moving_block_placebo.rds")

iid.block.placebo <- foreach(tau = taus) %dopar% {
  t0_placebo <- t_final_placebo-(tau+1) # n pre-treatment periods
  t_star_placebo <- t_final_placebo-t0_placebo
  mclapply(capacity.outcomes.list, 
           ChernoCI, t_star=t_star_placebo, c.range=c(-6,6), sd=0.5, alpha=0.025, l=5000, prec=1e-02, ns=100, treated.indices=pub.states, permtype="iid.block",t0=t0_placebo,sim=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)}
saveRDS(iid.block.placebo,"iid_block_placebo.rds")