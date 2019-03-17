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

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]])

source("MCEst.R")

t0 <- which(colnames(capacity.outcomes[["rev.pc"]]$M)=="1869") # first treatment time # same for all outcomes

# Get NxT matrix of point estimates

treat_indices_order <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")

mc.est <- mclapply(capacity.outcomes.list,
                            MCEst,imputed=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)
saveRDS(mc.est,"mc_est.rds")

# Hypothesis testing
source("ChernoTest.R")

# P-values
moving.block <- mclapply(capacity.outcomes.list,
                         ChernoTest, ns=1000, treat_indices_order=treat_indices_order, permtype="moving.block",t0=t0,imputed=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)
saveRDS(moving.block,"moving_block.rds")

iid.block <- mclapply(capacity.outcomes.list,
                      ChernoTest, ns=1000, treat_indices_order=treat_indices_order, permtype="iid.block",t0=t0,imputed=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)
saveRDS(iid.block,"iid_block.rds")

iid <- mclapply(capacity.outcomes.list,
                      ChernoTest, ns=1000, treat_indices_order=treat_indices_order, permtype="iid",t0=t0,imputed=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)
saveRDS(iid.block,"iid.rds")