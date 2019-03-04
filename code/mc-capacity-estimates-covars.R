###################################
# MC estimates #
###################################

## Loading Source files
library(MCPanel)
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
capacity.covars <- readRDS("capacity-covariates.rds")

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]],"educ.pc"=capacity.outcomes[["educ.pc"]])

source("MCEst.R")

t0 <- which(colnames(capacity.outcomes[["rev.pc"]]$M)=="1869") # first treatment time # same for all outcomes

# Get NxT matrix of point estimates

treat_indices_order <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")

mc.est.w <- mclapply(capacity.outcomes.list,
                   MCEst,t0=t0,treat_indices_order,imputed=FALSE,sim=FALSE,covars=capacity.covars,pca=FALSE,mc.cores=cores)
saveRDS(mc.est.w,"mc_est_w.rds")

# Get NxT matrix of confidence intervals
source("ChernoTest.R")

# p-values - covars

moving.block.w <- mclapply(capacity.outcomes.list,
                         ChernoTest, ns=1000, treat_indices_order=treat_indices_order, permtype="moving.block",t0=t0,imputed=FALSE,sim=FALSE,covars=capacity.covars,pca=FALSE,mc.cores=cores)
saveRDS(moving.block.w,"moving_block_w.rds")

iid.block.w <- mclapply(capacity.outcomes.list,
                      ChernoTest, ns=1000, treat_indices_order=treat_indices_order, permtype="iid.block",t0=t0,imputed=FALSE, sim=FALSE,covars=capacity.covars,pca=FALSE,mc.cores=cores)
saveRDS(iid.block.w,"iid_block_w.rds")

iid.w <- mclapply(capacity.outcomes.list,
                        ChernoTest, ns=1000, treat_indices_order=treat_indices_order, permtype="iid",t0=t0,imputed=FALSE, sim=FALSE,covars=capacity.covars,pca=FALSE,mc.cores=cores)
saveRDS(iid.w,"iid_w.rds")
