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

cores <- 4 #detectCores()

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Load data
capacity.outcomes <- readRDS(paste0(data.directory,"capacity-outcomes.rds"))
capacity.covars <- readRDS(paste0(data.directory,"capacity-covariates.rds"))

MCEst <- function(outcomes,sim=FALSE,covars=NULL,pca=FALSE) {
  Y <- outcomes$M # NxT 
  Y.missing <- outcomes$M.missing # NxT 
  
  if(!is.null(covars)){
    Z <- rbind(covars$Z,"AK"=rep(0,ncol(covars$Z))) # NxT # missing AK
    Z <- Z[row.names(Y),]  # reorder
  }
  
  treat <- outcomes$mask # NxT masked matrix 

  N <- nrow(treat)
  T <- ncol(treat)
  
  ## Treated indices
  indices <- cbind("id"=1:nrow(Y), "name"=rownames(Y))
  treat_indices_order <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")
  treat_indices <- as.numeric(indices[order(match(indices[,2], treat_indices_order))][1:length(treat_indices_order)]) # sort indices increasingly based on T0
  
  N_t <- length(treat_indices) # Number of treated units desired
  T0 <- which(colnames(Y)=="1869")# The first treatment time
  
  ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
  if(sim){
    treat_mat <- simul_adapt(Y, N_t, T0, treat_indices)
  }else{
    treat_mat <- stag_adapt(Y, N_t, T0, treat_indices)
  }
  
  Y_obs <- Y * treat_mat
  
  if(!is.null(covars)){
    ## ------
    ## MC-NNM-W
    ## ------
    
    est_model_MCPanel_w <- mcnnm_wc_cv(M=Y_obs, X = Z, Z=matrix(0L,0,0), mask=treat_mat, to_estimate_u = 1, to_estimate_v = 1, num_folds = 2, num_lam_L=10, num_lam_H=10)
    est_model_MCPanel_w$Mhat <- est_model_MCPanel_w$L + replicate(T,est_model_MCPanel_w$u) + t(replicate(N,est_model_MCPanel_w$v))
    est_model_MCPanel_w$impact <- (est_model_MCPanel_w$Mhat - Y*Y.missing)*(1-treat_mat)
    
    return(list("impact" = est_model_MCPanel_w$impact, "Mhat" = est_model_MCPanel_w$Mhat))
  } 
  
  if(pca){
    
    treat_mat_NA <- treat_mat
    treat_mat_NA[treat_mat==0] <- NA
    
    ## ------
    ## PCA
    ## ------
    
    nb <- estim_ncpPCA(data.frame(Y_obs*treat_mat_NA),ncp.max=5) # cv num components
    
    PCA_Mhat <- imputePCA(data.frame(Y_obs*treat_mat_NA), nb$ncp)$completeObs # regularized iterative PCA
    PCA_impact <- (PCA_Mhat - Y*Y.missing)*(1-treat_mat)
    
    return(list("impact" = PCA_impact, "Mhat" = PCA_Mhat))
    
  } else{
    ## ------
    ## MC-NNM
    ## ------
    
    est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, num_folds = 5)
    est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
    est_model_MCPanel$impact <- (est_model_MCPanel$Mhat - Y*Y.missing)*(1-treat_mat)
    
    return(list("impact" = est_model_MCPanel$impact, "Mhat" = est_model_MCPanel$Mhat))
  }
}

# Get NxT matrix of point estimates
mc.stag.est <- mclapply(list(capacity.outcomes[["rev.pc"]],capacity.outcomes[["exp.pc"]],capacity.outcomes[["educ.pc"]]),
                        MCEst, sim=FALSE,mc.cores=cores)

# Get NxT matrix of confidence intervals
source("ChernoTest.R")
pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
ChernoTest(outcomes,  ns=10, treated.indices=pub.states, permtype="iid.block") # move real t stat outside of fn

ChernoCI(t_star, l=10, outcomes, d, ns=10, q=1, permtype="iid.block")