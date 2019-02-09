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

capacity.outcomes$educ.pc$M <- capacity.outcomes$educ.pc$M[,1:(ncol(capacity.outcomes$educ.pc$M)-3)] # discard last 3 years (no variance)
capacity.outcomes$educ.pc$M.missing <- capacity.outcomes$educ.pc$M.missing[,1:(ncol(capacity.outcomes$educ.pc$M.missing)-3)]
capacity.outcomes$educ.pc$mask <- capacity.outcomes$educ.pc$mask[,1:(ncol(capacity.outcomes$educ.pc$mask)-3)] 

capacity.outcomes$rev.pc$M <- capacity.outcomes$rev.pc$M[,-which(colnames(capacity.outcomes$rev.pc$M)=="1931")] # discard 1931 (no variance)
capacity.outcomes$rev.pc$M.missing <- capacity.outcomes$rev.pc$M.missing[,-which(colnames(capacity.outcomes$rev.pc$M.missing)=="1931")]
capacity.outcomes$rev.pc$mask <- capacity.outcomes$rev.pc$mask[,-which(colnames(capacity.outcomes$rev.pc$mask)=="1931")] 

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]],"educ.pc"=capacity.outcomes[["educ.pc"]])

source("MCEst.R")

t0 <- which(colnames(capacity.outcomes[["rev.pc"]]$M)=="1869")-1 # n pre-treatment periods
t_final <- ncol(capacity.outcomes[["rev.pc"]]$M) # all periods
t_star <- t_final-t0

# Get NxT matrix of point estimates

# mc.est <- mclapply(capacity.outcomes.list,
#                           MCEst,t0=t0,sim=FALSE, covars=NULL,pca=FALSE,mc.cores=cores)

# Get NxT matrix of confidence intervals
source("ChernoTest.R")

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states

# iid <- mclapply(capacity.outcomes.list, 
#            ChernoCI, t_star=t_star, c.range=c(-10,10), alpha=0.025, l=100, prec=1e-02, ns=100, treated.indices=pub.states, permtype="iid",t0=t0,sim=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)
# saveRDS(iid,"iid.rds")

moving.block <- mclapply(capacity.outcomes.list, 
           ChernoCI, t_star=t_star, c.range=c(-3,3), alpha=0.025, l=100, prec=1e-02, ns=100, treated.indices=pub.states, permtype="moving.block",t0=t0,sim=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)
saveRDS(moving.block,"moving_block.rds")

iid.block <- mclapply(capacity.outcomes.list, 
           ChernoCI, t_star=t_star, c.range=c(-3,3), alpha=0.025, l=100, prec=1e-02, ns=100, treated.indices=pub.states, permtype="iid.block",t0=t0,sim=FALSE,covars=NULL,pca=FALSE,mc.cores=cores)
saveRDS(iid.block,"iid_block.rds")