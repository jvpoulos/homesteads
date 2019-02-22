# Table for placebo tests

mc_est_placebo <- readRDS(paste0(results.directory, "mc/mc_est_placebo.rds"))

iid_placebo <- readRDS(paste0(results.directory, "mc/iid_placebo_p.rds"))

iid_block_placebo <- readRDS(paste0(results.directory, "mc/iid_block_placebo_p.rds"))

moving_block_placebo <- readRDS(paste0(results.directory, "mc/moving_block_placebo_p.rds"))

MCCapacityPlacebo <- function(x,taus,permtype,q=1){
  ## Create time series data
  
  ts.means.tau <- lapply(1:length(taus), function(t){
    
    predicted <- mc_est_placebo[[t]][[x]]$Mhat
    
    pointwise <- mc_est_placebo[[t]][[x]]$impact 
    
    p <- t(readRDS(paste0(results.directory, "mc/", permtype,"_placebo_p.rds"))[[t]][[x]])
    
    m <- ncol(predicted)
    n <- m-(taus[t]+1) # n pre-treatment periods
    
    ## Plot time series 
    
    treat.status <- matrix(rownames(pointwise), nrow=nrow(pointwise), ncol=1)
    treat.status[rownames(pointwise) %in% c(southern.pub,western.pub)] <- "PLS"
    treat.status[rownames(pointwise) %in% state.land.states] <- "SLS"
    treat.status <- matrix(treat.status, dimnames=list(NULL, "status"))
    
    predicted.mean <-  aggregate(predicted, list(treat.status), mean)[-1]
    pointwise.mean <- aggregate(pointwise, list(treat.status), mean, na.rm=TRUE)[-1]
    
    pointwise.mean.post <- rowMeans(pointwise.mean[1,][(n+1):m])[[1]] # mean of pointwise effects in post-period for PLS
    
    real_teststat <- ((1/sqrt(m-n)) * sum(abs(pointwise.mean.post)^q))^(1/q)
    
    return(list("period"= range(as.numeric(colnames(pointwise.mean[(n+1):m]))),
                "t.stat"=real_teststat,
                "p"=p))
  })
  
  return(ts.means.tau)
}

taus <- c(10,20,30)
mc.iid.placebo <- lapply(list("rev.pc", "exp.pc", "educ.pc"), MCCapacityPlacebo, taus, "iid")
mc.iid.block.placebo <- lapply(list("rev.pc", "exp.pc", "educ.pc"), MCCapacityPlacebo, taus, "iid_block")
mc.moving.block.placebo <- lapply(list("rev.pc", "exp.pc", "educ.pc"), MCCapacityPlacebo, taus, "moving_block")