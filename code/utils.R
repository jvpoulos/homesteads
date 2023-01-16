##################################
# Utility fns.                  #
##################################

# function to bound probabilities to be used when making predictions
boundProbs <- function(x,bounds=c(0.001, 0.999)){
  x[x>max(bounds)] <- max(bounds)
  x[x<min(bounds)] <- min(bounds)
  return(x)
}

# mean exluding zero values (for calc. ATT)
nzmean <- function(x) {
  if (all(x==0)) 0 else mean(x[x!=0])
}

# convert NxT matrices to long panel

widetoLong <- function(Y,mask,X=NULL){
  # inputs numeric matrices Y, Mask, X
  # returns: data table with the columns

  data.long <- melt(Y)
  colnames(data.long) <- c("person_id","year","y_value")
  
  data.long <- cbind(data.long, "W"=melt(mask)$value)
  
  if(!is.null(X)){
    data.long <- cbind(data.long, "x_value"=melt(X)$value)
  }

  data.long <-  data.long[order(data.long$person_id, data.long$year), ]
  
  return(data.table(data.long))
}

# revert long panel back to NxT matrices
longtoWide <- function(data.long){
  # inputs data table and outputs matrices Y, mask, X
  Y.new <- as.matrix(reshape(data.long[,c("pid_boot","year_boot","y_value")], idvar = "pid_boot", timevar = "year_boot", direction = "wide")[,-1])
  mask.new <- as.matrix(reshape(data.long[,c("pid_boot","year_boot","W")], idvar = "pid_boot", timevar = "year_boot", direction = "wide")[,-1])
  if("x_value" %in% colnames(data.long)){
    X.new <- as.matrix(reshape(data.long[,c("pid_boot","year_boot","x_value")], idvar = "pid_boot", timevar = "year_boot", direction = "wide")[,-1])
  }else{
    X.new <- NULL
  }
  return(list("Y"=Y.new, "mask"=mask.new, "X"=X.new))
}

#  one bootstrap sample
one_boot <- function(sim_num, current_data_realized_long, N, T, T0, estimator, est_weights, return_per_period=FALSE, placebo=FALSE){
  boot_matrices <- list()
  boot_matrices$mask <- matrix(0, nrow=N, ncol=T) # treat matrix
  while(any(rowSums(boot_matrices$mask)<=1) || max(rowSums(boot_matrices$mask))<T){ # ensure that there are ST (switch after at least 1 period) and AT units
    num_units <- data.table::uniqueN(current_data_realized_long$person_id)
    sample_units <- data.table::data.table((table(sample(1:num_units, replace =  TRUE))))
    sample_units[, person_id := as.numeric(V1)]
    sample_units[, N := as.numeric(N)]
    
    boot_DT <- merge(current_data_realized_long, sample_units,
                     by = "person_id", all.x = TRUE)
    boot_DT <- boot_DT[!is.na(N),]
    boot_DT[, ID := .I]
    
    boot_DT <- boot_DT[rep(boot_DT$ID, boot_DT$N)]
    boot_DT$pid_boot <- rep(1:N, each = T)
    boot_DT$year_boot <- rep(1:T, times = N)
    
    boot_matrices <- longtoWide(data.long=boot_DT) 
  }
  
  # get vector of initial treatment periods
  
  A.boot <- aggregate(col ~ row,
                 data = which(boot_matrices$mask == 0, arr.ind = TRUE),
                 FUN = function(x) x[1])$col # gives the intial T0s for treated units
  
  ST.boot <- aggregate(col ~ row,
                       data = which(boot_matrices$mask == 0, arr.ind = TRUE),
                       FUN = function(x) x[1])$row  # switch treated indices
  NT.boot <- setdiff(1:N, ST.boot) # control indices
  
  # Observed outcome matrix 
  
  boot_matrices$obs_mat <- boot_matrices$Y * boot_matrices$mask
  
  if(est_weights){
    
    # Estimate propensity weights
    
    boot.p.mod <- cv.glmnet(x=cbind(boot_matrices$X,boot_matrices$obs_mat[,1:T0]), y=(1-boot_matrices$mask), family="mgaussian", alpha=1,nfolds=10,intercept=FALSE)
    W.boot <- predict(boot.p.mod, cbind(boot_matrices$X,boot_matrices$obs_mat[,1:T0]))[,,1]
    
    p.weights.boot <- matrix(NA, nrow=nrow(boot_matrices$mask), ncol=ncol(boot_matrices$mask), dimnames = list(rownames(boot_matrices$mask), colnames(boot_matrices$mask)))
    p.weights.boot <- boundProbs(W.boot)/(1-boundProbs(W.boot))
  }
  
  tau.boot <- c()
  # estimators
  if(estimator=="mc_plain"){
    boot_mc_plain <- mcnnm_cv(M = boot_matrices$obs_mat, mask = boot_matrices$mask, W = matrix(1, nrow(boot_matrices$mask),ncol(boot_matrices$mask)), to_estimate_u = 1, to_estimate_v = 1, rel_tol = 1e-05, is_quiet = 1)
    boot_mc_plain$Mhat <- boot_mc_plain$L + replicate(ncol(boot_matrices$obs_mat),boot_mc_plain$u) + t(replicate(nrow(boot_matrices$obs_mat),boot_mc_plain$v))
    tau.boot <- (boot_matrices$Y - boot_mc_plain$Mhat) # estimated treatment effect
  }
  
  if(estimator=="mc_weights"){
    boot_mc_weights <- mcnnm_cv(M = boot_matrices$obs_mat, mask = boot_matrices$mask, W = p.weights.boot, to_estimate_u = 1, to_estimate_v = 1, rel_tol = 1e-05, is_quiet = 1)
    boot_mc_weights$Mhat <- boot_mc_weights$L + replicate(ncol(boot_matrices$mask),boot_mc_weights$u) + t(replicate(nrow(boot_matrices$mask),boot_mc_weights$v))
    tau.boot <- (boot_matrices$Y-boot_mc_weights$Mhat) # estimated treatment effect
  }
  
  if(estimator=="ADH"){
    boot_model_ADH <- list()
    boot_model_ADH$Mhat <- adh_mp_rows(M=boot_matrices$obs_mat, mask=boot_matrices$mask)
    tau.boot <- (boot_matrices$Y-boot_model_ADH$Mhat) # estimated treatment effect
  }
  
  if(estimator=="ENT"){
    boot_model_ENT <- list()
    boot_model_ENT$Mhat <- t(en_mp_rows(M=t(boot_matrices$obs_mat), mask=t(boot_matrices$mask)))
    tau.boot <- (boot_matrices$Y-boot_model_ENT$Mhat) # estimated treatment effect
  }
  
  if(estimator=="DID"){
    boot_model_DID <- list()
    boot_model_DID$Mhat <- DID(M=boot_matrices$obs_mat, mask=boot_matrices$mask)
    tau.boot <- (boot_matrices$Y-boot_model_DID$Mhat) # estimated treatment effect
  }
  
  if(estimator=="IFE"){
    boot_model_IFE <- list()
    boot_model_IFE$Mhat <- IFE(M=boot_matrices$obs_mat, mask=boot_matrices$mask, k=2)
    tau.boot <- (boot_matrices$Y-boot_model_IFE$Mhat) # estimated treatment effect
  }
  
  # Calc. real ATT on the ST
  att.boot <- apply(tau.boot*(1-boot_matrices$mask),1,nzmean)[ST.boot]
  att.bar.boot <- mean(att.boot)
  if(return_per_period){
    return(list("att.bar.boot"=att.bar.boot, "att.boot"=att.boot))
  }else{
    return(att.bar.boot)
  }
}

# bootstap with cluster (return att.bar variance)
clustered_bootstrap <- function(current_data_realized_long, N,T,T0,estimator, B = 999, est_weights, return_per_period=FALSE, return_replicates=FALSE, ncores=NULL){
  if(return_per_period){
    if(!is.null(ncores)){
      boot_stats <- mclapply(c(1:B), one_boot, current_data_realized_long, N, T, T0, estimator, est_weights, return_per_period=TRUE, mc.cores=ncores)
    }else{
      boot_stats <- lapply(c(1:B), one_boot, current_data_realized_long, N, T, T0, estimator, est_weights, return_per_period=TRUE)
    }
    clustered_bootstrap_att_var <- colVars(as.matrix(plyr::ldply(sapply(boot_stats, function(x) rbind(x$att.boot)), rbind)), na.rm = TRUE)
    clustered_bootstrap_att_bar_var <- var(unlist(sapply(boot_stats, function(x) c(x$att.bar.boot))))
    return(list("att.bar.boot.var"=clustered_bootstrap_att_bar_var,"att.boot.var"=clustered_bootstrap_att_var))
  }
  if(return_replicates){
    boot_stats <- unlist(lapply(c(1:B), one_boot, current_data_realized_long, N, T, T0, estimator, est_weights, return_per_period=FALSE))
    return(boot_stats)
  }else{
    clustered_bootstrap_var <- var(unlist(lapply(c(1:B), one_boot, current_data_realized_long, N, T, T0, estimator, est_weights, return_per_period=FALSE)))
    return(clustered_bootstrap_var)
  }
}

# confidence interval
CI_test <- function(est_coefficent, real_coefficent, est_var,alpha=0.05){
  return(as.numeric(est_coefficent - qnorm(1 - alpha/2)*sqrt(est_var) <= real_coefficent &
               est_coefficent + qnorm(1 - alpha/2)*sqrt(est_var) >= real_coefficent ))
}

boot_CI <- function(est_coefficent,est_var,alpha=0.05){
  return(list("lb"=est_coefficent - qnorm(1 - alpha/2)*sqrt(est_var),
               "ub"=est_coefficent + qnorm(1 - alpha/2)*sqrt(est_var) ))
}

# Helper function for continuous DID regression bootstrap
RunDiD <- function(data, indices, f1) {
  d <- data[indices,]
  did.model <- glm(f1, data=d)
  return(coef(did.model)[['did']])
}

# For inequality descriptive plot
LmEq <- function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            p.val = format(summary(m)[[4]][2], digits = 1));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(p)~"="~p.val,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(p)~"="~p.val,l)    
  }
  
  as.character(as.expression(eq));                 
}

TLag <- function(x, n = 1L, time) { 
  index <- match(time - n, time, incomparables = NA)
  x[index]
}