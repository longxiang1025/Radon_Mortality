#!/bin/bash
#SBATCH -p serial_requeue
#SBATCH -n 1
#SBATCH --mem=18000
#SBATCH -t 1280
#SBATCH -o /n/holyscratch01/koutrakis_lab/longxiang/log/slurm_%A_%a.out
export R_LIBS_USER=$HOME/apps/R:$R_LIBS_USER

source /n/home02/loli/load_modules.sh

Sim=${SLURM_ARRAY_TASK_ID}

export Sim

Rscript --quiet --no-restore --no-save /n/koutrakis_lab/lab/Fracking_Radon/Code/01_Process_Meterological_Data.R

sleep 3 # pause to be kind to the scheduler