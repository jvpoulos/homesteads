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

cores <- 2#detectCores()

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

# Load data
capacity.outcomes <- readRDS(paste0(data.directory,"capacity-outcomes.rds"))

capacity.outcomes.list <- list("rev.pc"=capacity.outcomes[["rev.pc"]],"exp.pc"=capacity.outcomes[["exp.pc"]],"educ.pc"=capacity.outcomes[["educ.pc"]])

source(paste0(code.directory,"MCEstBoot.R"))

t0 <- which(colnames(capacity.outcomes[["rev.pc"]]$M)=="1869") # first treatment time # same for all outcomes

source(paste0(code.directory,"PolitisWhite.R"))

bopt.rev.pc <- b.star(capacity.outcomes[["rev.pc"]]$M,round=TRUE)[[1]]  # get optimal bootstrap lengths
bopt.exp.pc <- b.star(capacity.outcomes[["exp.pc"]]$M,round=TRUE)[[1]]  
bopt.educ.pc <- b.star(capacity.outcomes[["educ.pc"]]$M,round=TRUE)[[1]]  

rev.pc.boot <- tsboot(ts(t(capacity.outcomes[["rev.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["rev.pc"]]$M.missing, mask=capacity.outcomes[["rev.pc"]]$mask, t0=t0, simul=FALSE, R= 1000, parallel = "multicore", l =bopt.rev.pc, 
                      sim = "fixed") # block resampling with fixed block lengths of length l)
exp.pc.boot <- tsboot(ts(t(capacity.outcomes[["exp.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["exp.pc"]]$M.missing, mask=capacity.outcomes[["exp.pc"]]$mask, t0=t0, simul=FALSE, R= 1000, parallel = "multicore", l =bopt.exp.pc, 
                      sim = "fixed") 
educ.pc.boot <- tsboot(ts(t(capacity.outcomes[["educ.pc"]]$M)), MCEstBoot, M.missing= capacity.outcomes[["educ.pc"]]$M.missing, mask=capacity.outcomes[["educ.pc"]]$mask, t0=t0, simul=FALSE, R= 1000, parallel = "multicore", l =bopt.educ.pc, 
                       sim = "fixed") 

saveRDS(rev.pc.boot, paste0(results.directory, "mc/rev-pc-boot.rds"))
saveRDS(exp.pc.boot, paste0(results.directory, "mc/exp-pc-boot.rds"))
saveRDS(educ.pc.boot, paste0(results.directory, "mc/educ-pc-boot.rds"))