#!/bin/sh

#SBATCH --partition=t1standard
#SBATCH --ntasks=24
#SBATCH --tasks-per-node=24
#SBATCH --mail-user=agmccarthy@alaska.edu
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
#SBATCH --output=test_run.%j

ulimit -s unlimited
ulimit -l unlimited

# Load any desired modules, usually the same as loaded to compile
. /etc/profile.d/modules.sh
module purge
module load slurm
module load GCC/11.3.0
module load OpenMPI/4.1.4
module load R/4.2.2



cd slurm_test_output
# Launch the MPI application
srun Rscript /center1/OCSOPLRM/agmccarthy/Ordinal_binary/Script/test_run.R
