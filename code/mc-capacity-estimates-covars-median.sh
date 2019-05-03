#!/bin/bash
#----------------------------------------------------
#SBATCH -J mc_capacity_estimates_covars_median           # Job name
#SBATCH -o mc_capacity_estimates_covars_median.o%j       # Name of stdout output file
#SBATCH -e mc_capacity_estimates_covars_median.e%j       # Name of stderr error file
#SBATCH -p normal		        # Queue (partition) name
#SBATCH -N 1              		# Total # of nodes (must be 1 for serial)
#SBATCH -n 1              	 	# Total # of mpi tasks (should be 1 for serial)
#SBATCH -t 48:00:00        		# Run time (hh:mm:ss)
#SBATCH --mail-user=poulos@berkeley.edu
#SBATCH --mail-type=all    		# Send email at begin and end of job

R --no-save < mc-capacity-estimates-covars-median.R
