#!/bin/sh
#PBS -l ncpus=%%%4,mpiprocs=%%%4,mem=%%%5gb,host=hpc_c%%%1
#PBS -M %%%6
#PBS -m abe
#PBS -N Val-%%%2
#PBS -e ../output/jobreport
#PBS -o ../output/jobreport
export OMP_NUM_THREADS=32      # 環境変数の設定
. ~/.bashrc
echo Working Directory is $PBS_O_WORKDIR
cd $PBS_O_WORKDIR
echo `pwd`
timestamp=`date +"%m-%d-%y-%H-%M-%S"`
echo $timestamp > ../output/jobreport/psublog_${PBS_JOBID}.txt 
cat ./settings/%%%2.sh >> ../output/jobreport/psublog_${PBS_JOBID}.txt
bash Validation.sh %%%2 2 >> ../output/jobreport/psublog_${PBS_JOBID}.txt
