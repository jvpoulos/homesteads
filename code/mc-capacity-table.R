
## No covariates
mc_est <- readRDS(paste0(results.directory, "mc/mc_est.rds"))

# P-values

iid <- readRDS(paste0(results.directory, "mc/iid.rds"))

iid_block <- readRDS(paste0(results.directory, "mc/iid_block.rds"))

moving_block <- readRDS(paste0(results.directory, "mc/moving_block.rds"))

## Covariates
mc_est_w <- readRDS(paste0(results.directory, "mc/mc_est_w.rds"))

# P-values

iid_w <- readRDS(paste0(results.directory, "mc/iid_w.rds"))

iid_block_w <- readRDS(paste0(results.directory, "mc/iid_block_w.rds"))

moving_block_w <- readRDS(paste0(results.directory, "mc/moving_block_w.rds"))