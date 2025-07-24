#!/bin/bash
#SBATCH --account=msph
#SBATCH --job-name=m00
#SBATCH --mem-per-cpu=5G
#SBATCH --time=5-00:00
#SBATCH --array=14
#SBATCH -N 1                     
#SBATCH -c 8
#SBATCH --mail-type=ALL
#SBATCH --mail-user=X@cumc.columbia.edu

x=0 # aprime
y=0 # astar

Rscript main.R $x $y 

exit 0
