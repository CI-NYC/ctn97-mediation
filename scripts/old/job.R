#!/bin/bash
#SBATCH --account=msph
#SBATCH --job-name=mediation_11
#SBATCH --mem=32G
#SBATCH --time=3:00:00:00
#SBATCH --array=5-7
#$ -pe smp 14

x=$1
y=$1

Rscript test.R $x $y 

exit 0