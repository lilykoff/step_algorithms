#!/bin/bash
#SBATCH --job-name=snakemake_test
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=100G
#SBATCH --time=05:00:00
#SBATCH --output=snakemake_%A.out
#SBATCH --error=snakemake_%A.err

# Load any necessary modules or activate your virtual environment
# module load your_module

# Navigate to the directory containing your Snakefile
cd /users/lkoffman/step_algos_test

# Activate your conda environment or set up your environment
# conda activate your_environment

# Run Snakemake
snakemake --cores 1