#!/bin/bash
#SBATCH -p shared
#SBATCH -n 1
#SBATCH --mem=18000
#SBATCH -t 1280
#SBATCH -e /n/holyscratch01/koutrakis_lab/Users/loli/log/log_error/Rn_%A_%a.err
#SBATCH -o /n/holyscratch01/koutrakis_lab/Users/loli/log/log_pred/Model_%A_%a.out # Standard output
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER

module load R/3.6.1-fasrc02

Sim=${SLURM_ARRAY_TASK_ID}

export Sim

Rscript --quiet --no-restore --no-save /n/koutrakis_lab/lab/Radon_Mortality/Code/09_Final_Prediction.R

sleep 3 # pause to be kind to the scheduler