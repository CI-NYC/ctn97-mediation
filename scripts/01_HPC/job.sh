#!/bin/bash
#SBATCH --account=msph
#SBATCH --job-name=m11
#SBATCH --mem-per-cpu=5G
#SBATCH --time=4-00:00
#SBATCH --array=5-14
#SBATCH -N 1                     
#SBATCH -c 4
#SBATCH --mail-type=ALL
#SBATCH --mail-user=X@cumc.columbia.edu

x=1 # aprime
y=1 # astar

Rscript main.R $x $y 

exit 0
