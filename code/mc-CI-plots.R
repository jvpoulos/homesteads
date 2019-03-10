# CIs

iid_block_ci <- readRDS(paste0(results.directory, "mc/iid_block_ci.rds"))
iid_block_ci <- lapply(1:3, function(x) lapply(iid_block_ci[[x]],rowRanges, na.rm=TRUE))

moving_block_ci <- readRDS(paste0(results.directory, "mc/moving_block_ci.rds"))
moving_block_ci <- lapply(1:3, function(x) lapply(moving_block_ci[[x]],rowRanges, na.rm=TRUE))
