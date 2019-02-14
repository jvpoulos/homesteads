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
#capacity.covars <- readRDS("capacity-covariates.rds")

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]],"educ.pc"=capacity.outcomes[["educ.pc"]])

source("MCEst.R")

t0 <- which(colnames(capacity.outcomes[["rev.pc"]]$M)=="1869") # first treatment time 
t_final <- ncol(capacity.outcomes[["rev.pc"]]$M) # all periods
t_star <- t_final-t0

# Get NxT matrix of point estimates

mc.est <- mclapply(capacity.outcomes.list,
                            MCEst,t0=t0,sim=FALSE, covars=NULL,pca=FALSE,mc.cores=cores)
saveRDS(mc.est,"mc_est.rds")

# Get NxT matrix of confidence intervals
source("ChernoTest.R")

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states

iid <- mclapply(capacity.outcomes.list, 
            ChernoCI, t_star=t_star, c.range=c(-6,6), sd=1, alpha=0.025, l=1000, prec=1e-02, ns=200, treated.indices=pub.states, permtype="iid",t0=t0,sim=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)
saveRDS(iid,"iid.rds")

moving.block <- mclapply(capacity.outcomes.list, 
           ChernoCI, t_star=t_star, c.range=c(-6,6), sd=1, alpha=0.025, l=1000, prec=1e-02, ns=200, treated.indices=pub.states, permtype="moving.block",t0=t0,sim=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)
saveRDS(moving.block,"moving_block.rds")

iid.block <- mclapply(capacity.outcomes.list, 
           ChernoCI, t_star=t_star, c.range=c(-6,6), sd=1, alpha=0.025, l=1000, prec=1e-02, ns=200, treated.indices=pub.states, permtype="iid.block",t0=t0,sim=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)
saveRDS(iid.block,"iid_block.rds")