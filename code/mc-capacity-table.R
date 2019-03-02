# Table for actual tests
library(matrixStats)

mc_est <- readRDS(paste0(results.directory, "mc/mc_est.rds"))

# P-values
iid <- readRDS(paste0(results.directory, "mc/iid.rds"))

iid_block <- readRDS(paste0(results.directory, "mc/iid_block.rds"))

moving_block <- readRDS(paste0(results.directory, "mc/moving_block.rds"))