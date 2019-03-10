###################################
# DD Estimation (state-level measures)   #
###################################
library(data.table)
library(dplyr)
library(boot)

# Setup parallel processing 
library(parallel)
library(doParallel)

cores <- 4#detectCores()

cl <- parallel::makeForkCluster(cores)

doParallel::registerDoParallel(cores) # register cores (<p)

RNGkind("L'Ecuyer-CMRG") # ensure random number generation

source(paste0(code.directory,"RunDiD.R"))

# Load capacity data
capacity.outcomes <- readRDS(paste0(data.directory,"capacity-outcomes.rds"))
capacity.covars <- readRDS(paste0(data.directory,"capacity-covariates.rds"))
capacity.covars <- data.frame(capacity.covars)
capacity.covars$state <- rownames(capacity.covars)

capacity.outcomes.M <- list("rev.pc"=capacity.outcomes[["rev.pc"]]$M,"exp.pc"=capacity.outcomes[["exp.pc"]]$M,"educ.pc"=capacity.outcomes[["educ.pc"]]$M)
capacity.outcomes.mask <- list("rev.pc"=capacity.outcomes[["rev.pc"]]$mask,"exp.pc"=capacity.outcomes[["exp.pc"]]$mask,"educ.pc"=capacity.outcomes[["educ.pc"]]$mask)

capacity.outcomes.panel <- lapply(capacity.outcomes.M, melt, value.name="outcome")
capacity.outcomes.mask <- lapply(capacity.outcomes.mask, melt, value.name="mask")

capacity.outcomes.panel <- lapply(capacity.outcomes.panel, function(x){
  colnames(x) <- c("state","year","outcome")
  return(x)
})
capacity.outcomes.mask <- lapply(capacity.outcomes.mask, function(x){
  colnames(x) <- c("state","year","mask")
  return(x)
})

capacity.outcomes.panel$rev.pc <- merge(x=capacity.outcomes.panel$rev.pc, y=capacity.outcomes.mask$rev.pc, by=c("state","year"), all.x=TRUE)
capacity.outcomes.panel$exp.pc <- merge(x=capacity.outcomes.panel$exp.pc, y=capacity.outcomes.mask$exp.pc, by=c("state","year"), all.x=TRUE)
capacity.outcomes.panel$educ.pc <- merge(x=capacity.outcomes.panel$educ.pc, y=capacity.outcomes.mask$educ.pc, by=c("state","year"), all.x=TRUE)

capacity.outcomes.panel <- lapply(capacity.outcomes.panel, merge, y=capacity.covars, by="state",all.x=TRUE)

# Log per-capita total number of patents issued under the HSA 

homesteads.state <- patents.sum %>%
  group_by(state_code,year) %>% #group counties by state and year
  dplyr::mutate(homesteads.sum=sum(homesteads)) %>%
  distinct(state_code,year,.keep_all = TRUE) %>%
  arrange(state_code,year) %>%
  select(state_code,year, homesteads.sum, ns.pop)

homesteads.state <- homesteads.state  %>%
  group_by(state_code) %>%
  dplyr::mutate(homesteads.pc = log((homesteads.sum/ns.pop) + .Machine
                $double.eps)) %>%
  select(state_code,year, homesteads.pc) %>%
  arrange(state_code,year)

capacity.outcomes.panel <- lapply(capacity.outcomes.panel, merge, y=homesteads.state, by.x=c("state","year"),by.y=c("state_code","year"), all.x=TRUE)

# NAs are 0
capacity.outcomes.panel$rev.pc$homesteads.pc[is.na(capacity.outcomes.panel$rev.pc$homesteads.pc)] <- log(.Machine
                                                                                                         $double.eps)
capacity.outcomes.panel$exp.pc$homesteads.pc[is.na(capacity.outcomes.panel$exp.pc$homesteads.pc)] <- log(.Machine
                                                                                                         $double.eps)
capacity.outcomes.panel$educ.pc$homesteads.pc[is.na(capacity.outcomes.panel$educ.pc$homesteads.pc)] <- log(.Machine
                                                                                                         $double.eps)

# create interaction term

capacity.outcomes.panel <- lapply(capacity.outcomes.panel, function(x){
  x$did <- NA
  x$did <- x$mask*x$homesteads.pc
  return(x)
})

# rev.pc
f1 <- formula(outcome ~ factor(state) + 
                factor(year) + mask + did)

f2 <- formula(outcome ~ factor(state) + 
                factor(year) + mask + did + faval.1850+faval.1860+farmsize.1860+track2.1850+track2.1860)

# DD Estimates

# Covars
rev.pc.all.robust.did <- boot(data=capacity.outcomes.panel$rev.pc,
                              statistic=RunDiD,
                              f1=f2,
                              R=1000,
                              strata=as.factor(capacity.outcomes.panel$rev.pc$state), # stratify by state
                              parallel="multicore", ncpus = cores)

rev.pc.all.robust.did.delta <- rev.pc.all.robust.did$t0
rev.pc.all.robust.did.delta

rev.pc.all.robust.did.CI <- boot.ci(rev.pc.all.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
rev.pc.all.robust.did.CI

rev.pc.all.robust.did.delta
rev.pc.all.robust.did.CI
nobs(lm(f2, data=capacity.outcomes.panel$rev.pc))

# Covars
exp.pc.all.robust.did <- boot(data=capacity.outcomes.panel$exp.pc,
                              statistic=RunDiD,
                              f1=f2,
                              R=1000,
                              strata=as.factor(capacity.outcomes.panel$exp.pc$state), # stratify by state
                              parallel="multicore", ncpus = cores)

exp.pc.all.robust.did.delta <- exp.pc.all.robust.did$t0
exp.pc.all.robust.did.delta

exp.pc.all.robust.did.CI <- boot.ci(exp.pc.all.robust.did, conf=0.95, index=1, type="norm")$normal[2:3] # 95% nonparametric bootstrap CIs
exp.pc.all.robust.did.CI

exp.pc.all.robust.did.delta
exp.pc.all.robust.did.CI
nobs(lm(f2, data=capacity.outcomes.panel$exp.pc))
