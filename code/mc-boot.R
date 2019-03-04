###################################
# Boostraps for MC estimates #
###################################

## Loading Source files
library(MCPanel)
library(missMDA)
library(boot)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- 28#detectCores()

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Load data
capacity.outcomes <- readRDS("capacity-outcomes.rds")
capacity.covars <- readRDS("capacity-covariates.rds")

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]],"educ.pc"=capacity.outcomes[["educ.pc"]])

source("MCEstBoot.R")

t0 <- which(colnames(capacity.outcomes[["rev.pc"]]$M)=="1869") # first treatment time # same for all outcomes
treat_indices_order <- c("CA", "CO", "IA", "KS", "MI", "MN", "MO", "NE", "OH", "OR", "SD", "WA", "WI", "IL", "NV", "ID", "MT", "ND",  "UT", "AL", "MS", "AR", "FL", "LA", "IN", "NM", "WY", "AZ", "OK", "AK")

source("PolitisWhite.R")

bopt.rev.pc <- b.star(capacity.outcomes[["rev.pc"]]$M,round=TRUE)[[1]]  # get optimal bootstrap lengths
bopt.exp.pc <- b.star(capacity.outcomes[["exp.pc"]]$M,round=TRUE)[[1]]  
bopt.educ.pc <- b.star(capacity.outcomes[["educ.pc"]]$M,round=TRUE)[[1]]  

# No covars
rev.pc.boot <- tsboot(ts(t(capacity.outcomes[["rev.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["rev.pc"]]$M.missing, mask=capacity.outcomes[["rev.pc"]]$mask, t0=t0, treat_indices_order=treat_indices_order, imputed=FALSE,simul=FALSE,covars=NULL,pca=FALSE, R= 1000, parallel = "multicore", l =bopt.rev.pc, 
                      sim = "fixed") # block resampling with fixed block lengths of length l)
exp.pc.boot <- tsboot(ts(t(capacity.outcomes[["exp.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["exp.pc"]]$M.missing, mask=capacity.outcomes[["exp.pc"]]$mask, t0=t0, treat_indices_order=treat_indices_order, imputed=FALSE,simul=FALSE,covars=NULL,pca=FALSE, R= 1000, parallel = "multicore", l =bopt.exp.pc, 
                      sim = "fixed") 
educ.pc.boot <- tsboot(ts(t(capacity.outcomes[["educ.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["educ.pc"]]$M.missing, mask=capacity.outcomes[["educ.pc"]]$mask, t0=t0, treat_indices_order=treat_indices_order,imputed=FALSE,simul=FALSE,covars=NULL,pca=FALSE, R= 1000, parallel = "multicore", l =bopt.educ.pc, 
                       sim = "fixed") 

saveRDS(rev.pc.boot, "mc/rev-pc-boot.rds")
saveRDS(exp.pc.boot, "mc/exp-pc-boot.rds")
saveRDS(educ.pc.boot, "mc/educ-pc-boot.rds")

# Covars
rev.pc.boot.w <- tsboot(ts(t(capacity.outcomes[["rev.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["rev.pc"]]$M.missing, mask=capacity.outcomes[["rev.pc"]]$mask, t0=t0, treat_indices_order=treat_indices_order,imputed=FALSE,simul=FALSE,covars=capacity.covars,pca=FALSE, R= 1000, parallel = "multicore", l =bopt.rev.pc, 
                      sim = "fixed") # block resampling with fixed block lengths of length l)
exp.pc.boot.w <- tsboot(ts(t(capacity.outcomes[["exp.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["exp.pc"]]$M.missing, mask=capacity.outcomes[["exp.pc"]]$mask, t0=t0, treat_indices_order=treat_indices_order,imputed=FALSE,simul=FALSE,covars=capacity.covars,pca=FALSE, R= 1000, parallel = "multicore", l =bopt.exp.pc, 
                      sim = "fixed") 
educ.pc.boot.w <- tsboot(ts(t(capacity.outcomes[["educ.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["educ.pc"]]$M.missing, mask=capacity.outcomes[["educ.pc"]]$mask, t0=t0, treat_indices_order=treat_indices_order,imputed=FALSE,simul=FALSE,covars=capacity.covars,pca=FALSE, R= 1000, parallel = "multicore", l =bopt.educ.pc, 
                       sim = "fixed") 

saveRDS(rev.pc.boot.w, "mc/rev-pc-boot-w.rds")
saveRDS(exp.pc.boot.w, "mc/exp-pc-boot-w.rds")
saveRDS(educ.pc.boot.w, "mc/educ-pc-boot-w.rds")