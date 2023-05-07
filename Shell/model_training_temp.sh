#!/bin/bash
#SBATCH -p serial_requeue
#SBATCH -n 1
#SBATCH --mem=30000
#SBATCH --account=koutrakis_lab
#SBATCH -t 00-03:00
#SBATCH -e /n/holyscratch01/koutrakis_lab/Users/loli/log/log_error/Rn_%A_%a.err
#SBATCH -o /n/holyscratch01/koutrakis_lab/Users/loli/log/log_rn_model/Model_%A_%a.out # Standard output
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER

module load R/3.6.1-fasrc02

Sim=${SLURM_ARRAY_TASK_ID}

export Sim

Rscript --quiet --no-restore --no-save /n/holyscratch01/koutrakis_lab/Users/loli/code/34_Local_Radon_Model_Training.R

sleep 3 # pause to be kind to the scheduler