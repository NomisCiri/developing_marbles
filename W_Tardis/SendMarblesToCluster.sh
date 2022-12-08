#here you need to specify how many instances you want to run simulations on. 
#currently it si set up as such that for any "real" collected data from our pilot there will be Simulations.
#this is not to fit a model yet. this makes extensive Simulations. I fit the model back at another place.
HowManyGroups=5
HowManyModels=6
#j=2;
#i=1;
#k=5;
#NJobs=100
#j=2;
#i=1;
#k=5;
PATH_LOG_E="./logsE/$(date '+%Y%m%d_%H%M%S')"
PATH_LOG_O="./logsY/$(date '+%Y%m%d_%H%M%S')"
# path to the text file with all subject ids:
PATH_SUB_LIST="${PATH_SCRIPT}/subs.txt"
# CREATE RELEVANT DIRECTORIES:
# ==============================================================================
# create output directory:
if [ ! -d ${PATH_LOG_E} ]; then
	mkdir -p ${PATH_LOG_E}
fi
# create directory for log files:
if [ ! -d ${PATH_LOG_O} ]; then
	mkdir -p ${PATH_LOG_O}
fi

for i in `seq 1 $HowManyGroups`;
do
	for j in `seq 2 2`;
	do
		echo "$i  $Subjects";
		#here i specify the job for the cluster.
		#for input_file in INPUT/* ; do
			#echo "#PBS -m n"                         > job.pbs
		  echo '#!/bin/bash'                                > job.slurm
      echo "#SBATCH --job-name Marble${i}_${j}"     >> job.slurm
      echo "#SBATCH --partition long"                   >> job.slurm
      echo "#SBATCH --mem 10GB"                          >> job.slurm
      echo "#SBATCH --cpus 3"                           >> job.slurm
      echo "#SBATCH --time 700:0:0"                     >> job.slurm
      echo "#SBATCH --workdir ."                        >> job.slurm
      echo "#SBATCH --error ${PATH_LOG_E}"              >> job.slurm
      echo "#SBATCH --output ${PATH_LOG_O}"             >> job.slurm
			echo "module load R/4; Rscript stanScriptNew.R $i $j"	>>  job.slurm
    sbatch job.slurm
    rm -f job.slurm
		done
	done
