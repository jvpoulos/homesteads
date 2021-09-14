#!/bin/bash
#----------------------------------------------------

Rscript code/mc-capacity-estimates.R ${SLURM_ARRAY_TASK_ID}
