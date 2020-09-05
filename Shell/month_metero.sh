#!/bin/bash
#SBATCH -p shared
#SBATCH -n 1
#SBATCH --mem=18000
#SBATCH -t 60
#SBATCH -e /n/holyscratch01/koutrakis_lab/Users/loli/log/log_error/Info_%A_%a.err
#SBATCH -o /n/holyscratch01/koutrakis_lab/Users/loli/log/log_mete/Mete_%A_%a.out # Standard output
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER

module load R/3.6.1-fasrc02

Sim=${SLURM_ARRAY_TASK_ID}

export Sim

Rscript --quiet --no-restore --no-save /n/koutrakis_lab/lab/Radon_Mortality/Code/01_Process_Meterological_Data.R

sleep 3 # pause to be kind to the scheduler