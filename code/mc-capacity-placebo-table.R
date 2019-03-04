# Table for placebo tests

# No covariates
mc_est_placebo <- readRDS(paste0(results.directory, "mc/mc_est_placebo.rds"))

iid_block_placebo <- readRDS(paste0(results.directory, "mc/iid_block_placebo.rds"))

moving_block_placebo <- readRDS(paste0(results.directory, "mc/moving_block_placebo.rds"))

# Covariates
mc_est_placebo_w <- readRDS(paste0(results.directory, "mc/mc_est_placebo_w.rds"))

iid_block_placebo_w <- readRDS(paste0(results.directory, "mc/iid_block_placebo_w.rds"))

moving_block_placebo_w <- readRDS(paste0(results.directory, "mc/moving_block_placebo_w.rds"))