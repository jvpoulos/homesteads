###################################
# MC Estimation  #
###################################

## Loading Source files
library(MCPanel)
library(glmnet)
library(ggplot2)
library(missForest)
library(latex2exp)
library(softImpute)
library(recommenderlab)

# Setup parallel processing 
library(parallel)
library(doParallel)

detectCores()

registerDoParallel(14) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Load data
dfList <- readRDS("synth-data")

## Reading data
for(d in c('basque','germany','california')){
  Y <- dfList[[d]]$M # NxT 
  treat <- dfList[[d]]$mask # NxT masked matrix 
  years <- colnames(Y) # T-length 
  
  ## Setting up the configuration
  N <- nrow(treat)
  T <- ncol(treat)
  number_T0 <-8
  T0 <- ceiling(T*((1:number_T0)*2-1)/(2*number_T0))
  N_t <- 5 # no. treated units desired <=N
  num_runs <- 10
  is_simul <- 1 ## Whether to simulate Simultaneus Adoption or Staggered Adoption
  to_save <- 1 ## Whether to save the plot or not
  
  ## Matrices for saving RMSE values
  
  MCPanel_RMSE_test <- matrix(0L,num_runs,length(T0))
  SVD_RMSE_test <- matrix(0L,num_runs,length(T0))
  RF_RMSE_test <- matrix(0L,num_runs,length(T0))
  EN_RMSE_test <- matrix(0L,num_runs,length(T0))
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

      ## ------
      ## MC-NNM
      ## ------

      est_model_MCPanel <- mcnnm_cv(Y_obs, treat_mat, to_estimate_u = 1, to_estimate_v = 1, num_folds = 5)
      est_model_MCPanel$Mhat <- est_model_MCPanel$L + replicate(T,est_model_MCPanel$u) + t(replicate(N,est_model_MCPanel$v))
      est_model_MCPanel$msk_err <- (est_model_MCPanel$Mhat - Y)*(1-treat_mat)
      est_model_MCPanel$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_MCPanel$msk_err^2, na.rm = TRUE))
      MCPanel_RMSE_test[i,j] <- est_model_MCPanel$test_RMSE

      ## ------
      ## SVD
      ## ------
    
      SVD_xc <- biScale(Y_obs*treat_mat_NA,col.scale=FALSE,row.scale=FALSE)
      SVD_lambda <- lambda0(SVD_xc) # compute the smallest lambda with a zero solution
        
      est_model_SVD <- softImpute(SVD_xc, rank.max=3,lambda=SVD_lambda, type="svd", maxit=500)
      est_model_SVD$Mhat <- complete(Y_obs*treat_mat_NA,est_model_SVD, unscale = TRUE)
      est_model_SVD$msk_err <- (est_model_SVD$Mhat - Y)*(1-treat_mat)
      est_model_SVD$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_SVD$msk_err^2, na.rm=TRUE))
      SVD_RMSE_test[i,j] <- est_model_SVD$test_RMSE

      # ## ------
      # ## ALS
      # ## ------
      # 
      # est_model_ALS <- softImpute(SVD_xc, rank.max=3,lambda=SVD_lambda, type="als", warm.start = est_model_SVD, maxit=500)
      # est_model_ALS$Mhat <- complete(Y_obs*treat_mat_NA,est_model_ALS, unscale = TRUE)
      # est_model_ALS$msk_err <- (est_model_ALS$Mhat - Y)*(1-treat_mat)
      # est_model_ALS$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ALS$msk_err^2, na.rm=TRUE))
      # ALS_RMSE_test[i,j] <- est_model_ALS$test_RMSE
      
      ## -----
      ## HR-EN : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
      ## -----

      est_model_EN <- en_mp_rows(Y_obs, treat_mat)
      est_model_EN_msk_err <- (est_model_EN - Y)*(1-treat_mat)
      est_model_EN_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_EN_msk_err^2, na.rm = TRUE))
      EN_RMSE_test[i,j] <- est_model_EN_test_RMSE
      
      ## -----
      ## VT-EN : It does Not cross validate on alpha (only on lambda) and keep alpha = 1 (LASSO).
      ## -----

      #source("EN.R")
      
      est_model_ENT <- t(en_mp_rows(t(Y_obs), t(treat_mat)))
      est_model_ENT_msk_err <- (est_model_ENT - Y)*(1-treat_mat)
      est_model_ENT_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ENT_msk_err^2, na.rm = TRUE))
      ENT_RMSE_test[i,j] <- est_model_ENT_test_RMSE
      
      ## ------
      ## RF
      ## ------
      
      est_model_RF <- missForest(Y_obs*treat_mat_NA, ntree=500, mtry=8, parallelize = "variables")
      est_model_RF$Mhat <- est_model_RF$ximp
      est_model_RF$msk_err <- (est_model_RF$Mhat - Y)*(1-treat_mat) 
      est_model_RF$test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_RF$msk_err^2, na.rm = TRUE))
      RF_RMSE_test[i,j] <- est_model_RF$test_RMSE

      ## -----
      ## DID
      ## -----

      est_model_DID <- t(DID(t(Y_obs), t(treat_mat)))
      est_model_DID_msk_err <- (est_model_DID - Y)*(1-treat_mat)
      est_model_DID_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_DID_msk_err^2, na.rm = TRUE))
      DID_RMSE_test[i,j] <- est_model_DID_test_RMSE

      ## -----
      ## ADH
      ## -----
      est_model_ADH <- adh_mp_rows(Y_obs, treat_mat)
      est_model_ADH_msk_err <- (est_model_ADH - Y)*(1-treat_mat)
      est_model_ADH_test_RMSE <- sqrt((1/sum(1-treat_mat)) * sum(est_model_ADH_msk_err^2, na.rm = TRUE))
      ADH_RMSE_test[i,j] <- est_model_ADH_test_RMSE
    }
  }
  
  ## Computing means and standard errors
  MCPanel_avg_RMSE <- apply(MCPanel_RMSE_test,2,mean)
  MCPanel_std_error <- apply(MCPanel_RMSE_test,2,sd)/sqrt(num_runs)
  
  SVD_avg_RMSE <- apply(SVD_RMSE_test,2,mean)
  SVD_std_error <- apply(SVD_RMSE_test,2,sd)/sqrt(num_runs)
  
  EN_avg_RMSE <- apply(ENT_RMSE_test,2,mean)
  EN_std_error <- apply(ENT_RMSE_test,2,sd)/sqrt(num_runs)
  
  ENT_avg_RMSE <- apply(ENT_RMSE_test,2,mean)
  ENT_std_error <- apply(ENT_RMSE_test,2,sd)/sqrt(num_runs)
  
  RF_avg_RMSE <- apply(RF_RMSE_test,2,mean)
  RF_std_error <- apply(RF_RMSE_test,2,sd)/sqrt(num_runs)
  
  DID_avg_RMSE <- apply(DID_RMSE_test,2,mean)
  DID_std_error <- apply(DID_RMSE_test,2,sd)/sqrt(num_runs)
  
  ADH_avg_RMSE <- apply(ADH_RMSE_test,2,mean)
  ADH_std_error <- apply(ADH_RMSE_test,2,sd)/sqrt(num_runs)
  
  ## Creating plots
  
  df1 <-
    data.frame(
      "y" =  c(DID_avg_RMSE, EN_avg_RMSE, ENT_avg_RMSE, RF_avg_RMSE, MCPanel_avg_RMSE, SVD_avg_RMSE, ADH_avg_RMSE),
      "lb" = c(DID_avg_RMSE - 1.96*DID_std_error, 
               EN_avg_RMSE - 1.96*ENT_std_error,
               ENT_avg_RMSE - 1.96*ENT_std_error,
               RF_avg_RMSE - 1.96*RF_std_error,
               MCPanel_avg_RMSE - 1.96*MCPanel_std_error, 
               SVD_avg_RMSE - 1.96*SVD_std_error, 
               ADH_avg_RMSE - 1.96*ADH_std_error),
      "ub" = c(DID_avg_RMSE + 1.96*DID_std_error, 
               EN_avg_RMSE + 1.96*ENT_std_error,
               ENT_avg_RMSE + 1.96*ENT_std_error,
               RF_avg_RMSE + 1.96*RF_std_error,
               MCPanel_avg_RMSE + 1.96*MCPanel_std_error, 
               SVD_avg_RMSE + 1.96*SVD_std_error, 
               ADH_avg_RMSE + 1.96*ADH_std_error),
      "x" = c(T0/T, T0/T ,T0/T, T0/T, T0/T, T0/T, T0/T),
      "Method" = c(replicate(length(T0),"DID"), 
                   replicate(length(T0),"HR-EN"),
                   replicate(length(T0),"VT-EN"),
                   replicate(length(T0),"RF"),
                   replicate(length(T0),"MC-NNM"), 
                   replicate(length(T0),"SOFT-IMPUTE"), 
                   replicate(length(T0),"SC-ADH")))
  
  p <- ggplot(data = df1, aes(x, y, color = Method, shape = Method)) +
    geom_point(size = 2, position=position_dodge(width=0.1)) +
    geom_errorbar(
      aes(ymin = lb, ymax = ub),
      width = 0.1,
      linetype = "solid",
      position=position_dodge(width=0.1)) +
    scale_shape_manual("Method",values=c(1:8)) +
    scale_color_discrete("Method")+
    theme_bw() +
    xlab(TeX('$T_0/T$')) +
    ylab("Average RMSE") +
    theme(axis.title=element_text(family="Times", size=14)) +
    theme(axis.text=element_text(family="Times", size=12)) +
    theme(legend.text=element_text(family="Times", size = 12)) +
    theme(legend.title=element_text(family="Times", size = 12))
  print(p)
  
  ##
  if(to_save == 1){
    filename<-paste0(paste0(paste0(paste0(paste0(paste0(gsub("\\.", "_", d),"_N_", N),"_T_", T),"_numruns_", num_runs), "_num_treated_", N_t), "_simultaneuous_", is_simul),".png")
    ggsave(filename, plot = last_plot(), device="png", dpi=600)
    df2<-data.frame(N,T,N_t,is_simul, DID_RMSE_test, EN_RMSE_test, ENT_RMSE_test, RF_RMSE_test, MCPanel_RMSE_test, SVD_RMSE_test, ADH_RMSE_test)
    colnames(df2)<-c("N", "T", "N_t", "is_simul", 
                     replicate(length(T0), "DID"), 
                     replicate(length(T0), "HR-EN"), 
                     replicate(length(T0), "VT-EN"), 
                     replicate(length(T0), "RF"), 
                     replicate(length(T0), "MC-NNM"), 
                     replicate(length(T0),"SOFT-IMPUTE"), 
                     replicate(length(T0),"SC-ADH"))
    
    filename<-paste0(paste0(paste0(paste0(paste0(paste0(gsub("\\.", "_", d),"_N_", N),"_T_", T),"_numruns_", num_runs), "_num_treated_", N_t), "_simultaneuous_", is_simul),".rds")
    save(df1, df2, file = filename)
  }
}