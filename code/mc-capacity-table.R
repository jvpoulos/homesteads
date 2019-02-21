# Table for actual tests

mc_est <- readRDS(paste0(results.directory, "mc/mc_est.rds"))

iid <- readRDS(paste0(results.directory, "mc/iid.rds"))

iid_block <- readRDS(paste0(results.directory, "mc/iid_block.rds"))

moving_block <- readRDS(paste0(results.directory, "mc/moving_block.rds"))

MCCapacity <- function(x,permtype,q=2,t0){
  
  predicted <- mc_est[[x]]$Mhat
  
  pointwise <- mc_est[[x]]$impact 
  
  pointwise.ci <- t(readRDS(paste0(results.directory, "mc/", permtype,".rds"))[[x]])
  
  m <- ncol(predicted)
  n <- t0-1 # n pre-treatment periods
  
  cumulative <- sapply((n+1):m, function(t){
    (1/(t-n))*rowSums(pointwise[,n:t], na.rm=TRUE)
  })
  
  cumulative <- cbind(matrix(0, ncol=ncol(pointwise)-ncol(cumulative), nrow = nrow(pointwise)), cumulative)
  
  cumulative.ci <- t(sapply(1:nrow(pointwise.ci), function(i){
    t <- ((n+1):m)[i]
    if(i==1){
      pointwise.ci[1,]
    }else{
      (1/(t-n))*colSums(pointwise.ci[1:i,])   
    }
  }))
  
  # cumulative.ci <- cbind(matrix(0, ncol=ncol(cumulative)-ncol(cumulative.ci), nrow = nrow(pointwise.ci)), cumulative.ci)
  # 
  # pointwise.ci <- cbind(matrix(0, ncol=ncol(pointwise)-ncol(pointwise.ci), nrow = nrow(pointwise.ci)), pointwise.ci)
  
  ## Plot time series 
  
  treat.status <- matrix(rownames(pointwise), nrow=nrow(pointwise), ncol=1)
  treat.status[rownames(pointwise) %in% c(southern.pub,western.pub)] <- "PLS"
  treat.status[rownames(pointwise) %in% state.land.states] <- "SLS"
  treat.status <- matrix(treat.status, dimnames=list(NULL, "status"))
  
  predicted.mean <-  aggregate(predicted, list(treat.status), mean)[-1]
  pointwise.mean <- aggregate(pointwise, list(treat.status), mean, na.rm=TRUE)[-1]
  cumulative.mean <- aggregate(cumulative, list(treat.status), mean, na.rm=TRUE)[-1]
  
  pointwise.mean.post <- rowMeans(pointwise.mean[1,][(n+1):m])[[1]] # mean of pointwise effects in post-period for PLS
  
  real_teststat <- ((1/sqrt(m-n)) * sum(abs(pointwise.mean.post)^q))^(1/q)
  
  if(!is.numeric(pointwise.ci)){
    pointwise.mean.ci.post <-c(NA,NA)
  }else{
    pointwise.mean.ci.post <- rowMeans(pointwise.ci)
  }
  
  return(list("period"= range(as.numeric(colnames(pointwise.mean[(n+1):m]))),
              "t.stat"=real_teststat,
              "pointwise.ci"=pointwise.mean.ci.post))
}

mc.iid.placebo <- lapply(list("rev.pc", "exp.pc", "educ.pc"), MCCapacity, "iid",t0=t0)
mc.iid.block.placebo <- lapply(list("rev.pc", "exp.pc", "educ.pc"), MCCapacity, "iid_block",t0=t0)
mc.moving.block.placebo <- lapply(list("rev.pc", "exp.pc", "educ.pc"), MCCapacity, "moving_block",t0=t0)