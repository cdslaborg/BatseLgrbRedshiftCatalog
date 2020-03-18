#!/bin/bash
#----------------------------------------------------
# Sample Slurm job script
#   for TACC Stampede2 SKX nodes
#
#   *** MPI Job on SKX Normal Queue ***
# 
# Last revised: 20 Oct 2017
#
# Notes:
#
#   -- Launch this script by executing
#      "sbatch skx.mpi.slurm" on Stampede2 login node.
#
#   -- Use ibrun to launch MPI codes on TACC systems.
#      Do not use mpirun or mpiexec.
#
#   -- Max recommended MPI ranks per SKX node: 48
#      (start small, increase gradually).
#
#   -- If you're running out of memory, try running
#      fewer tasks per node to give each task more memory.
#
#----------------------------------------------------

#SBATCH -J cosmicRate        # Job name
#SBATCH -o cosmicRate.%j.out # Name of stdout output file
#SBATCH -e cosmicRate.%j.err # Name of stderr error file
#SBATCH -p skx-normal           # Queue (partition) name
#SBATCH -N 1                    # Total # of nodes 
#SBATCH -n 48                   # Total # of mpi tasks
#SBATCH -t 12:00:00             # Run time (hh:mm:ss)
#SBATCH --mail-user=shahmoradi@utexas.edu
#SBATCH --mail-type=all         # Send email at begin and end of job
#SBATCH -A TGM-startup          # Allocation name (req'd if you have more than 1)

# Other commands must follow all #SBATCH directives...

module list
pwd
date

# Launch MPI code... 
# Use ibrun instead of mpirun or mpiexec

./run.sh -n 48

# ---------------------------------------------------