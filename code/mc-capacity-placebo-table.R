# Table for placebo tests

# No covariates

iid_placebo <- readRDS(paste0(results.directory, "mc/iid_placebo.rds"))

iid_block_placebo <- readRDS(paste0(results.directory, "mc/iid_block_placebo.rds"))

moving_block_placebo <- readRDS(paste0(results.directory, "mc/moving_block_placebo.rds"))

# Covariates

iid_placebo_w <- readRDS(paste0(results.directory, "mc/iid_placebo_w.rds"))

iid_block_placebo_w <- readRDS(paste0(results.directory, "mc/iid_block_placebo_w.rds"))

moving_block_placebo_w <- readRDS(paste0(results.directory, "mc/moving_block_placebo_w.rds"))