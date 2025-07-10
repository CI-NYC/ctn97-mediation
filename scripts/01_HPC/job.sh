#!/bin/bash
#SBATCH --account=msph
#SBATCH --job-name=m00
#SBATCH --mem-per-cpu=5G
#SBATCH --time=4-00:00
#SBATCH --array=12-13
#SBATCH -N 1                     # The number of nodes to request
#SBATCH -c 4
#SBATCH --mail-type=ALL
#SBATCH --mail-user=si2426@cumc.columbia.edu
#SBATCH -t 4-00:00
x=0
y=0

Rscript main.R $x $y 

exit 0
