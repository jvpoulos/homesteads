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

cores <- 28 #detectCores()

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Load data
capacity.outcomes <- readRDS("capacity-outcomes.rds")
capacity.covars <- readRDS("capacity-covariates.rds")

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]],"educ.pc"=capacity.outcomes[["educ.pc"]])

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
  T0 <- 87 #which(colnames(Y)=="1869")# The first treatment time
  
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
    est_model_MCPanel_w$impact <- (est_model_MCPanel_w$Mhat - Y)
    
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
    PCA_impact <- (PCA_Mhat - Y)
    
    return(list("impact" = PCA_impact, "Mhat" = PCA_Mhat))
    
  } else{
    ## ------
    ## MC-NNM
    ## ------
    
    est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, num_folds = 5)
    est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
    est_model_MCPanel$impact <- (est_model_MCPanel$Mhat - Y)
    
    return(list("impact" = est_model_MCPanel$impact, "Mhat" = est_model_MCPanel$Mhat))
  }
}

# Get NxT matrix of point estimates

#mc.est <- mclapply(capacity.outcomes.list,
#                        MCEst,sim=FALSE, covars=NULL,pca=FALSE,mc.cores=cores)

# Get NxT matrix of confidence intervals
source("ChernoTest.R")

t0 <- which(colnames(capacity.outcomes[["rev.pc"]]$M)=="1869")-1 # n pre-treatment periods
t_final <- ncol(capacity.outcomes[["rev.pc"]]$M) # all periods
t_star <- t_final-t0

pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states

mc.ci.stag <-  mclapply(c("rev.pc","exp.pc","educ.pc"),
                       function(x){
                         ChernoCI(t_star, c.range=c(-50,50), alpha=0.025, l=100, prec=1e-02, capacity.outcomes[[x]], treated.indices=pub.states, permtype="iid.block",sim=FALSE,covars=NULL,pca=FALSE)
                       }, mc.cores=cores) 

saveRDS(mc.ci.stag,"mc-ci-stag.rds")