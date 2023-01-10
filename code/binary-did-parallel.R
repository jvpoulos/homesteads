############################################
# Binary treatment DID parallel trends test #
############################################

library(MCPanel)
library(matrixStats)
library(Matrix)
library(data.table)
library(reshape)
library(reshape2)
library(ggplot2)
library(ParallelTrendsPlot)
library(wesanderson)
library(stringr)
library(grid)
library(ParallelTrendsPlot)

source('code/utils.R')

# Load data
outcomes.missing <- readRDS("data/capacity-outcomes-none.rds")
outcomes.imputed <- readRDS(paste0("data/capacity-outcomes-mice-pmm.rds")) 
capacity.outcomes.linear <- readRDS("data/capacity-outcomes-linear.rds") # for covariates

outcomes.labels <- data.frame('rev.pc'="Log per-capita state government revenue (1982$)",
                              'exp.pc'= "Log per-capita state government expenditure (1982$)")

for(d in c('rev.pc','exp.pc')){
  
  t0 <- which(colnames(outcomes.missing[[d]]$M)=="1869")
  
  # Transform covars to unit and time-specific inputs
  capacity.covars <- cbind(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")][sort(rownames(capacity.outcomes.linear[[d]]$faval[,c("1850","1860")])),], 
                           capacity.outcomes.linear[[d]]$farmsize[,c("1860")][sort(names(capacity.outcomes.linear[[d]]$farmsize[,c("1860")]))],
                           c("AZ"=0, "NM"=0, capacity.outcomes.linear[[d]]$access[,c("1860")])[sort(c(names(capacity.outcomes.linear[[d]]$access[,c("1860")]),"AZ","NM"))]) # AZ and NM not in dataset
  
  colnames(capacity.covars) <- c("faval.1850","faval.1860","farmsize.1860", "access.1860")
  
  capacity.covars <-capacity.covars[match(rownames(outcomes.imputed[[d]]$M), rownames(capacity.covars)), ] # same order
  capacity.covars[is.na(capacity.covars)] <- 0
  
  capacity.covars <- apply(capacity.covars, 2, scale) # scale
  capacity.covars <- as.data.frame(capacity.covars)
  capacity.covars$state <- rownames(outcomes.imputed[[d]]$M)
  
  pub.states <- c("AK","AL","AR","AZ","CA","CO","FL","IA","ID","IL","IN","KS","LA","MI","MN","MO","MS","MT","ND","NE","NM","NV","OH","OK","OR","SD","UT","WA","WI","WY") # 30 public land states
  treat_indices_order <- row.names(outcomes.imputed[[d]]$M)[row.names(outcomes.imputed[[d]]$M)%in% pub.states]
  
  # specify outcome and treatment matrix
  Y.missing <- outcomes.missing[[d]]$M  # to train on: 0s = missing
  Y <- outcomes.imputed[[d]]$M # NxT  # imputed outcomes
  missing.mat <- outcomes.imputed[[d]]$M.missing
  missing.mat[is.na(missing.mat)] <- 0 # 0s are missing/imputed
  treat <- outcomes.imputed[[d]]$mask# NxT masked matrix: 0 for control units and treated units before treatment and 1 for treated units after treatment
  
  N <- dim(treat)[1]
  T <- dim(treat)[2]
  
  mask <- 1-treat # masked matrix, 1= control units and treated units before treatment and 0 = treated units after treatment
  rownames(mask) <- rownames(treat)
  colnames(mask) <- colnames(treat)
  
  Y_obs <- Y * mask * missing.mat
  
  fr_obs <- sum(mask)/(N*T) # store fraction observed entries
  print(paste0("fraction observed: ", fr_obs))
  
  # get vector of initial treatment periods for N_t treated units
  
  A <- aggregate(col ~ row,
                 data = which(mask == 0, arr.ind = TRUE),
                 FUN = function(x) x[1])$col # gives the intial T0s for treated units
  
  ST <- aggregate(col ~ row,
                  data = which(mask == 0, arr.ind = TRUE),
                  FUN = function(x) x[1])$row  # switch treated indices
  NT <- setdiff(1:N, ST) # control indices
  
  ## -----
  ## DID estimates
  ## -----
  est_model_DID <- list()
  est_model_DID$Mhat <- DID(Y_obs, mask)
  est_model_DID$tau <- (Y-est_model_DID$Mhat) # estimated treatment effect
  
  est_model_DID$att <- apply(est_model_DID$tau*(1-mask),1,nzmean)[ST]
  est_model_DID$att.bar <- mean(est_model_DID$att)
  
  ## -----
  ## Parallel trends plot
  ## -----
  
  pt.dat <- melt(Y)
  colnames(pt.dat) <- c("state","year","Y")
  pt.dat$treat <- ifelse(pt.dat$state%in%pub.states,1,0)
  pt.dat$exp <- melt(1-mask)$value
  pt.dat <- merge(pt.dat,capacity.covars,by="state",all.x = TRUE)
  
  pal <- wes_palette("Zissou1",5, type = "discrete")

  year.labels <- c(colnames(Y.missing)[seq(1,ncol(Y.missing),15)])
  
  pt.dat <- parallel.trends.data(pt.dat,timevar = "year",yvar="Y",treatdummy = "treat",expdummy = "exp",cvars=colnames(capacity.covars)[-5])
  pt.plot <- parallel.trends.plot(pt.dat, facet.mode = FALSE, add.exp.line = "none") + 
    theme_grey(base_size=9) + labs(x="Year", y=outcomes.labels[[d]]) + 
    theme_bw() + 
    geom_vline(xintercept=1869, linetype=2) +
    scale_color_manual(name="Treatment status",
                       labels=c("Control","Treated"),
                       values=c(wes_palette("Darjeeling1")[1], wes_palette("Darjeeling1")[5])) +
    theme(legend.position = "none") +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(), legend.title = element_text(size = 9, face="bold"), 
          axis.text.x = element_text(size=9),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          axis.ticks=element_blank()) 
  
  ggsave(paste0(plots.directory,"/parallel-plot-",d,".png"), pt.plot, scale=1.25)
}