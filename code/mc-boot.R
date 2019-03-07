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

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]],"educ.pc"=capacity.outcomes[["educ.pc"]])

source("MCEstBoot.R")

source("PolitisWhite.R")

bopt.rev.pc <- b.star(t(capacity.outcomes[["rev.pc"]]$M),round=TRUE)[,1]  # get optimal stationary bootstrap lengths
bopt.exp.pc <- b.star(t(capacity.outcomes[["exp.pc"]]$M),round=TRUE)[,1] 
bopt.educ.pc <- b.star(t(capacity.outcomes[["educ.pc"]]$M),round=TRUE)[,1] 

# No covars
rev.pc.boot <- tsboot(ts(t(capacity.outcomes[["rev.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["rev.pc"]]$M.missing, mask=capacity.outcomes[["rev.pc"]]$mask, imputed=FALSE,covars=NULL,pca=FALSE, R= 1000, parallel = "multicore", l =bopt.rev.pc, 
                      sim = "fixed") # block resampling with fixed block lengths of length l)
exp.pc.boot <- tsboot(ts(t(capacity.outcomes[["exp.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["exp.pc"]]$M.missing, mask=capacity.outcomes[["exp.pc"]]$mask, imputed=FALSE,covars=NULL,pca=FALSE, R= 1000, parallel = "multicore", l =bopt.exp.pc, 
                      sim = "fixed") 
educ.pc.boot <- tsboot(ts(t(capacity.outcomes[["educ.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["educ.pc"]]$M.missing, mask=capacity.outcomes[["educ.pc"]]$mask, imputed=FALSE,covars=NULL,pca=FALSE, R= 1000, parallel = "multicore", l =bopt.educ.pc, 
                       sim = "fixed") 

saveRDS(rev.pc.boot, "rev-pc-boot.rds")
saveRDS(exp.pc.boot, "exp-pc-boot.rds")
saveRDS(educ.pc.boot, "educ-pc-boot.rds")