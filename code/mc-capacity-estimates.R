###################################
# MC estimates #
###################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(boot)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- detectCores()

cl <- parallel::makeForkCluster(cores)
doParallel::registerDoParallel(cores) # register cores (<p)
RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Set command line arguments
args <- as.numeric(commandArgs(trailingOnly = TRUE)) # command line arguments
settings <- c("mice","rf","ma","locf","linear")[args]

# Load data
capacity.outcomes.mice <- readRDS(paste0("data/capacity-outcomes-",settings,".rds"))
capacity.outcomes.linear <- readRDS("data/capacity-outcomes-linear.rds") # for covariates

t0 <- which(colnames(capacity.outcomes.mice[["rev.pc"]]$M)=="1869") # first treatment time # same for all outcomes

capacity.outcomes <- list()

for(d in c('rev.pc','exp.pc')){
  
  capacity.outcomes[[d]]$M <- capacity.outcomes.mice[[d]]$M
  capacity.outcomes[[d]]$M.missing <- capacity.outcomes.mice[[d]]$M.missing
  capacity.outcomes[[d]]$mask <- capacity.outcomes.mice[[d]]$mask
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")][sort(rownames(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")])),], 
                           capacity.outcomes.linear[[d]]$farmsize[,c("1860")][sort(names(capacity.outcomes.linear[[d]]$farmsize[,c("1860")]))],
                           c("AZ"=0, "NM"=0, capacity.outcomes.linear[[d]]$access[,c("1860")])[sort(c(names(capacity.outcomes.linear[[d]]$access[,c("1860")]),"AZ","NM"))]) # AZ and NM not in dataset
  
  colnames(capacity.covars) <- c("faval.1850","faval.1860","farmsize.1860", "access.1860")
  
  capacity.covars <-capacity.covars[match(rownames(capacity.outcomes[[d]]$M), rownames(capacity.covars)), ] # same order
  capacity.covars[is.na(capacity.covars)] <- 0
  
  # Estimate propensity scores
  
  treat_mat <- 1-capacity.outcomes[[d]]$mask
  
  p.mod <- cv.glmnet(x=cbind(capacity.covars,capacity.outcomes[[d]]$M[,1:(t0-1)]), y=(1-treat_mat), family="mgaussian", alpha=1, parallel=TRUE,intercept=FALSE,type.multinomial="grouped",nfolds=5) 
  W <- predict(p.mod, cbind(capacity.covars,capacity.outcomes[[d]]$M[,1:(t0-1)]))[,,1]
  W[,1:(t0-1)] <- W[,t0] # assume pre-treatment W same as t0
  
  source("code/boundProbs.R")
  
  p.weights <- matrix(NA, nrow=nrow(treat_mat), ncol=ncol(treat_mat), dimnames = list(rownames(treat_mat), colnames(treat_mat)))
  capacity.outcomes[[d]]$p.weights <- (1-treat_mat)*(1-boundProbs(W)) + (treat_mat)*(boundProbs(W)) # overlap weighting: treated are 0
}

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]])

# MC estimation

source("code/MCEst.R")

# Get NxT matrix of point estimates

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
treat_indices_order <- row.names(capacity.outcomes.mice$rev.pc$M)[row.names(capacity.outcomes.mice$rev.pc$M)%in% pub.states] # same for each outcome

mc.est <- mclapply(capacity.outcomes.list,
                            MCEst,mc.cores=cores)
saveRDS(mc.est,paste0("mc_est_",settings,".rds"))

# Hypothesis testing
source("code/ChernoTest.R")

# P-values
moving.block <- mclapply(capacity.outcomes.list,
                         ChernoTest, ns=1000, treat_indices_order=treat_indices_order, permtype="moving.block",t0=t0,imputed=FALSE,mc.cores=cores)
saveRDS(moving.block,paste0("moving_block_",settings,".rds"))

iid.block <- mclapply(capacity.outcomes.list,
                      ChernoTest, ns=1000, treat_indices_order=treat_indices_order, permtype="iid.block",t0=t0,imputed=FALSE,mc.cores=cores)
saveRDS(iid.block,paste0("iid_block",settings,".rds"))

# Bootstrap CIs

source("code/MCEstBoot.R")

source("code/PolitisWhite.R")

bopt.rev.pc <- b.star(t(capacity.outcomes[["rev.pc"]]$M),round=TRUE)[,1]  # get optimal stationary bootstrap lengths
bopt.exp.pc <- b.star(t(capacity.outcomes[["exp.pc"]]$M),round=TRUE)[,1] 

rev.pc.boot <- tsboot(ts(t(capacity.outcomes[["rev.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["rev.pc"]]$M.missing, mask=capacity.outcomes[["rev.pc"]]$mask, p.weights=capacity.outcomes[["rev.pc"]]$p.weights, imputed=FALSE, R= 1000, parallel = "multicore", l =bopt.rev.pc, 
                      sim = "fixed") # block resampling with fixed block lengths of length l)
saveRDS(rev.pc.boot, paste0("rev-pc-boot-",settings,".rds"))

exp.pc.boot <- tsboot(ts(t(capacity.outcomes[["exp.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["exp.pc"]]$M.missing, mask=capacity.outcomes[["exp.pc"]]$mask, p.weights=capacity.outcomes[["rev.pc"]]$p.weights, imputed=FALSE,R= 1000, parallel = "multicore", l =bopt.exp.pc, 
                      sim = "fixed") 
saveRDS(exp.pc.boot, paste0("exp-pc-boot-",settings,".rds"))