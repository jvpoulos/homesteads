
### Chernozhukov method

#' @param outcomes Tidy dataframe with the outcomes and meta data
#' @param metadata Dataframe of metadata
#' @param ns Number of samples from the permutation distribution to draw
#' @param trt_unit Treated unit
#' @param cols Column names corresponding to the units,
#'             time variable, outcome, and treated indicator
#' @param order of test statistic
#'
#' @return outcomes with additional synthetic control added and weights
#' @export
cherno_test <- function(outcomes, metadata, ns=1000,  q=c(2,1), trt_unit=1, 
                        cols=list(unit="unit", time="time",
                                  outcome="outcome", treated="treated")) {
  
  
  ## format data for synth
  syn_data <- format_data(outcomes, metadata, trt_unit, cols=cols)
  
  ## pre and post outcomes
  
  
  trtmat <- syn_data$synth_data$Y0plot
  ctrlmat <- syn_data$synth_data$Y1plot
  
  t0 <- nrow(syn_data$synth_data$Z0)
  t_final <- nrow(syn_data$synth_data$Y0plot)
  
  teststats <- matrix(0, nrow=ns, ncol=length(q))
  
  for(i in 1:ns) {
    
    ## sample from permutation distribution
    reorder <- sample(1:t_final, t_final)
    
    ## fit synth with reordered time periods
    
    new_synth_data <- syn_data
    
    new_synth_data$synth_data$X0 <- syn_data$synth_data$Y0plot[reorder,,drop=FALSE][1:t0,]
    new_synth_data$synth_data$X1 <- syn_data$synth_data$Y1plot[reorder,,drop=FALSE][1:t0,]
    
    syn <- fit_synth_formatted(new_synth_data)
    
    ## get treatment effect estimates
    
    att <- syn_data$synth_data$Y1plot[reorder,,drop=FALSE][(t0+1):t_final,,drop=FALSE] -
      syn_data$synth_data$Y0plot[reorder,,drop=FALSE][(t0+1):t_final,,drop=FALSE] %*% syn$weights
    
    teststats[i,] <- sapply(1:length(q),
                            function(j) mean(abs(att)^q[j])^(1/q[j]))
  }
  
  ## compute test stat for actual data
  syn <- fit_synth_formatted(syn_data)
  real_att <-  syn_data$synth_data$Y1plot[(t0+1):t_final,,drop=FALSE] -
    syn_data$synth_data$Y0plot[(t0+1):t_final,,drop=FALSE] %*% syn$weights
  real_teststat <- sapply(1:length(q),
                          function(j) mean(abs(real_att)^q[j])^(1/q[j]))
  pvals <- sapply(1:length(q), function(i) mean(teststats[,i] >= real_teststat[i]))
  
  return(pvals)
}


########### Ex


library(dplyr)
library(Synth)
library(tidyr)
library(augsynth)

data(basque)
basque <- basque %>% mutate(trt = case_when(year < 1975 ~ 0,
                                            regionno != 17 ~0,
                                            regionno == 17 ~ 1)) %>%
  filter(regionno != 1)

test_that("format_data creates matrices with the right dimensions", {
  
  dat <- format_data(quo(gdpcap), quo(trt), quo(regionno), quo(year),1975, basque)
  
  test_dim <- function(obj, d) {
    expect_equivalent(dim(obj), d)
  }
  
  test_dim(dat$X, c(17, 20))
  expect_equivalent(length(dat$trt), 17)
  test_dim(dat$y, c(17, 23))
}
)


test_that("format_synth creates matrices with the right dimensions", {
  
  dat <- format_data(quo(gdpcap), quo(trt), quo(regionno), quo(year),1975, basque)
  syn_dat <- format_synth(dat$X, dat$trt, dat$y)
  test_dim <- function(obj, d) {
    expect_equivalent(dim(obj), d)
  }
  
  test_dim(syn_dat$Z0, c(20, 16))
  test_dim(syn_dat$Z1, c(20, 1))
  
  test_dim(syn_dat$Y0plot, c(43, 16))
  test_dim(syn_dat$Y1plot, c(43, 1))
  
  expect_equivalent(syn_dat$Z1, syn_dat$X1)
  expect_equivalent(syn_dat$Z0, syn_dat$X0)
}
)