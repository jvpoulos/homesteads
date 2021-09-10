####################################
# State gov't spending simulations #
####################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(ggplot2)
library(latex2exp)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- parallel::detectCores()
print(paste0('cores registered: ', cores))

cl <- makePSOCKcluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)
RNGkind("L'Ecuyer-CMRG") # ensure random number generation

## Reading data
CapacitySim <- function(outcomes,covars.x,d,sim,treated.indices){
  Y <- outcomes[[d]]$M # NxT 
  Y.missing <- outcomes[[d]]$M.missing # NxT 
  treat <- outcomes[[d]]$mask # NxT masked matrix 
  
  ## Treated 
  treat_y <- Y[rownames(Y)%in%treated.indices,] 
  
  ## Working with the rest of matrix
  treat <- treat[!rownames(treat)%in%rownames(treat_y),] 
  Y <- Y[!rownames(Y)%in%rownames(treat_y),] 
  Y.missing <- Y.missing[!rownames(Y.missing)%in%rownames(treat_y),] 
  covars.x <- covars.x[!rownames(covars.x)%in%c(rownames(treat_y)),]
  
  ## Setting up the configuration
  N <- nrow(treat)
  T <- ncol(treat)
  number_T0 <- 5
  T0 <- ceiling(T*((1:number_T0)*2-1)/(2*number_T0))
  N_t <- ceiling(N*0.5) # no. treated units desired <=N
  num_runs <- 1000
  is_simul <- sim ## Whether to simulate Simultaneus Adoption or Staggered Adoption
  to_save <- 1 ## Whether to save the plot or not
  
  ## Matrices for saving RMSE values
  
  MCPanel_RMSE_test <- matrix(0L,num_runs,length(T0))
  ENT_RMSE_test <- matrix(0L,num_runs,length(T0))
  DID_RMSE_test <- matrix(0L,num_runs,length(T0))
  ADH_RMSE_test <- matrix(0L,num_runs,length(T0))
  
  ## Run different methods
  
  for(i in c(1:num_runs)){
    print(paste0(paste0("Run number ", i)," started"))
    ## Fix the treated units in the whole run for a better comparison
    treat_indices <- sample(1:N, N_t)
    for (j in c(1:length(T0))){
      treat_mat <- matrix(1L, N, T) # masked matrix, 1= control units and treated units before treatment and 0 = treated units after treatment
      t0 <- T0[j]
      ## Simultaneuous (simul_adapt) or Staggered adoption (stag_adapt)
      if(is_simul == 1){
        treat_mat <- simul_adapt(Y, N_t, t0, treat_indices)
      }else{
        treat_mat <- stag_adapt(Y, N_t, t0, treat_indices)
      }
      treat_mat_NA <- treat_mat
      treat_mat_NA[treat_mat==0] <- NA
      
      Y_obs <- Y * treat_mat
      
      ## Estimate propensity scores
      
      p.mod <- cv.glmnet(x=cbind(covars.x,Y[,1:(t0-1)]), y=(1-treat_mat), family="mgaussian", alpha=1, parallel=TRUE,intercept=FALSE,type.multinomial="grouped",nfolds=5) 
      W <- predict(p.mod, cbind(covars.x,Y[,1:(t0-1)]))[,,1]
      W[,1:(t0-1)] <- W[,t0] # assume pre-treatment W same as t0 
      
      boundProbs <- function(x,bounds=c(0.01,0.99)){
        x[x>max(bounds)] <- max(bounds)
        x[x<min(bounds)] <- min(bounds)
        return(x)
      }
      
      p.weights <- matrix(NA, nrow=nrow(treat_mat), ncol=ncol(treat_mat), dimnames = list(rownames(treat_mat), colnames(treat_mat)))
      p.weights <- (1-treat_mat)*(1-boundProbs(W)) + (treat_mat)*(boundProbs(W)) # treated are 0
      
      ## ------
      ## MC-NNM
      ## ------
      
      est_model_MCPanel <- mcnnm_cv(Y_obs, mask=treat_mat, W=p.weights, to_estimate_u = 1, to_estimate_v = 1, num_lam_L = 30, num_folds = 5, niter = 200, rel_tol = 1e-05)
      est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
      est_model_MCPanel$msk_err <- (est_model_MCPanel$Mhat - Y)*(1-treat_mat) 
      est_model_MCPanel$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_MCPanel$msk_err^2, na.rm = TRUE))
      MCPanel_RMSE_test[i,j] <- est_model_MCPanel$test_RMSE
      print(paste("MC-NNM RMSE:", round(est_model_MCPanel$test_RMSE,3),"run",i))
      
      ## -----
      ## VT-EN 
      ## -----
      
      est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat), num_alpha = 1, num_lam = 30, num_folds = 5))
      est_model_ENT_msk_err <- (est_model_ENT - Y)*(1-treat_mat)
      est_model_ENT_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ENT_msk_err^2, na.rm = TRUE))
      ENT_RMSE_test[i,j] <- est_model_ENT_test_RMSE
      print(paste("VT-EN RMSE:", round(est_model_ENT_test_RMSE,3),"run",i))
      
      ## -----
      ## DID
      ## -----
      
      est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
      est_model_DID_msk_err <- (est_model_DID - Y)*(1-treat_mat)
      est_model_DID_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_DID_msk_err^2, na.rm = TRUE))
      DID_RMSE_test[i,j] <- est_model_DID_test_RMSE
      print(paste("DID RMSE:", round(est_model_DID_test_RMSE,3),"run",i))
      
      ## -----
      ## ADH
      ## -----
      est_model_ADH <- adh_mp_rows(Y_obs, treat_mat, niter = 200, rel_tol = 1e-05)
      est_model_ADH_msk_err <- (est_model_ADH - Y)*(1-treat_mat)
      est_model_ADH_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ADH_msk_err^2, na.rm = TRUE))
      ADH_RMSE_test[i,j] <- est_model_ADH_test_RMSE
    }
  }
  
  ## Computing means and standard errors
  MCPanel_avg_RMSE <- apply(MCPanel_RMSE_test,2,mean)
  MCPanel_std_error <- apply(MCPanel_RMSE_test,2,sd)/sqrt(num_runs)
  
  ENT_avg_RMSE <- apply(ENT_RMSE_test,2,mean)
  ENT_std_error <- apply(ENT_RMSE_test,2,sd)/sqrt(num_runs)
  
  DID_avg_RMSE <- apply(DID_RMSE_test,2,mean)
  DID_std_error <- apply(DID_RMSE_test,2,sd)/sqrt(num_runs)
  
  ADH_avg_RMSE <- apply(ADH_RMSE_test,2,mean)
  ADH_std_error <- apply(ADH_RMSE_test,2,sd)/sqrt(num_runs)
  
  ## Creating plots
  
  df1 <-
    data.frame(
      "y" =  c(DID_avg_RMSE, ENT_avg_RMSE, MCPanel_avg_RMSE, ADH_avg_RMSE),
      "lb" = c(DID_avg_RMSE - 1.96*DID_std_error, 
               ENT_avg_RMSE - 1.96*ENT_std_error,
               MCPanel_avg_RMSE - 1.96*MCPanel_std_error, 
               ADH_avg_RMSE - 1.96*ADH_std_error),
      "ub" = c(DID_avg_RMSE + 1.96*DID_std_error, 
               ENT_avg_RMSE + 1.96*ENT_std_error,
               MCPanel_avg_RMSE + 1.96*MCPanel_std_error, 
               ADH_avg_RMSE + 1.96*ADH_std_error),
      "x" = c(T0/T, T0/T ,T0/T, T0/T),
      "Method" = c(replicate(length(T0),"DID"), 
                   replicate(length(T0),"SCM-L1"),
                   replicate(length(T0),"MC"), 
                   replicate(length(T0),"SCM")))
  
  p <- ggplot(data = df1, aes(x, y, color = Method, shape = Method)) +
    geom_point(size = 4, position=position_dodge(width=0.2)) +
    geom_line(position=position_dodge(width=0.2), linetype = "dashed") +
    geom_errorbar(
      aes(ymin = lb, ymax = ub),
      width = 0.3,
      linetype = "solid",
      position=position_dodge(width=0.2)) +
    scale_shape_manual("Method",values=c(1:4)) +
    scale_color_discrete("Method")+
    theme_bw() +
    xlab(TeX('$T_0/T$')) +
    ylab("Average RMSE") +
    theme(axis.title=element_text(family="serif", size=16)) +
    theme(axis.text=element_text(family="serif", size=14)) +
    theme(legend.text=element_text(family="serif", size = 12)) +
    theme(legend.title=element_text(family="serif", size = 12)) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l =0))) +
    theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l =0))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))  # rm background
  print(p)
  
  ##
  if(to_save == 1){
    filename<-paste0(paste0(paste0(paste0(paste0(paste0(gsub("\\.", "_", d),"_N_", N),"_T_", T),"_numruns_", num_runs), "_num_treated_", N_t), "_simultaneuous_", is_simul),".png")
    ggsave(filename, plot = last_plot(), device="png", dpi=600)
    df2<-data.frame(N,T,N_t,is_simul, DID_RMSE_test, ENT_RMSE_test, MCPanel_RMSE_test, ADH_RMSE_test)
    colnames(df2)<-c("N", "T", "N_t", "is_simul", 
                     replicate(length(T0), "DID"), 
                     replicate(length(T0), "SCM-L1"), 
                     replicate(length(T0), "MC"), 
                     replicate(length(T0),"SCM"))
    
    filename<-paste0(paste0(paste0(paste0(paste0(paste0(gsub("\\.", "_", d),"_N_", N),"_T_", T),"_numruns_", num_runs), "_num_treated_", N_t), "_simultaneuous_", is_simul),".rds")
    save(df1, df2, file = filename)
  }
}


# Load data
capacity.outcomes.mice <- readRDS("data/capacity-outcomes-mice.rds")
capacity.outcomes.linear <- readRDS("data/capacity-outcomes-linear.rds") # for covariates

foreach(d = c('rev.pc','exp.pc')) %dopar% {
  
  print(paste0("N X T data dimension: ", dim(capacity.outcomes.mice[[d]]$M)))
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")][sort(rownames(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")])),], 
                           capacity.outcomes.linear[[d]]$farmsize[,c("1860")][sort(names(capacity.outcomes.linear[[d]]$farmsize[,c("1860")]))],
                           c("AZ"=0, "NM"=0, capacity.outcomes.linear[[d]]$access[,c("1860")])[sort(c(names(capacity.outcomes.linear[[d]]$access[,c("1860")]),"AZ","NM"))]) # AZ and NM not in dataset
  
  colnames(capacity.covars) <- c("faval.1850","faval.1860","farmsize.1860", "access.1860")
  
  capacity.covars <-capacity.covars[match(rownames(capacity.outcomes.mice[[d]]$M), rownames(capacity.covars)), ] # same order
  capacity.covars[is.na(capacity.covars)] <- 0
  
  pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
  treat_indices_order <- row.names(capacity.outcomes.mice[[d]]$M)[row.names(capacity.outcomes.mice[[d]]$M)%in% pub.states]
  
  CapacitySim(outcomes=capacity.outcomes.mice,covars.x=capacity.covars,d,sim=0,treated.indices=treat_indices_order)
}