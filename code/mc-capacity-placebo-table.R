# Table for placebo tests

mc_est_placebo <- readRDS(paste0(results.directory, "mc/mc_est_placebo.rds"))

iid_placebo <- readRDS(paste0(results.directory, "mc/iid_placebo.rds"))

iid_block_placebo <- readRDS(paste0(results.directory, "mc/iid_block_placebo.rds"))

moving_block_placebo <- readRDS(paste0(results.directory, "mc/moving_block_placebo.rds"))

MCCapacityPlacebo <- function(x,taus,permtype,q=2){
  ## Create time series data
  
  ts.means.tau <- lapply(1:length(taus), function(t){
    
    predicted <- mc_est_placebo[[t]][[x]]$Mhat
    
    pointwise <- mc_est_placebo[[t]][[x]]$impact 
    
    pointwise.ci <- t(readRDS(paste0(results.directory, "mc/", permtype,"_placebo.rds"))[[t]][[x]])
    
    m <- ncol(predicted)
    n <- m-(taus[t]+1) # n pre-treatment periods
    
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
    
 #    ts.means <- cbind(t(predicted.mean), t(pointwise.mean), t(cumulative.mean))
 #    colnames(ts.means) <- c("predicted.pls","predicted.sls","pointwise.pls","pointwise.sls","cumulative.pls","cumulative.sls")
 #    ts.means <- cbind(ts.means, "year"=as.numeric(colnames(pointwise)))
 #    
 #    ts.ci.means <- cbind(t(pointwise.ci), t(cumulative.ci))
 # #   colnames(ts.ci.means) <- c("pointwise.lo","pointwise.hi","cumulative.lo","cumulative.hi")
 #    ts.ci.means <- cbind(ts.ci.means, "year"=as.numeric(colnames(pointwise)))
 #    
 #    ts.means <- merge(ts.means, ts.ci.means, by=c("year"), all.x=TRUE) # bind cis
  })
  
  return(ts.means.tau)
}

taus <- c(10,20,30)
mc.iid.placebo <- lapply(list("rev.pc", "exp.pc", "educ.pc"), MCCapacityPlacebo, taus, "iid")
mc.iid.block.placebo <- lapply(list("rev.pc", "exp.pc", "educ.pc"), MCCapacityPlacebo, taus, "iid_block")
mc.moving.block.placebo <- lapply(list("rev.pc", "exp.pc", "educ.pc"), MCCapacityPlacebo, taus, "moving_block")