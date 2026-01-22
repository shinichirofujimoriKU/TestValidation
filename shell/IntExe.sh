#!/bin/bash
#$1: node number (if $1 is "0", then it is run directly with bash
#$2: shell program parameter setting file name
arg1=$1
arg2=$2
arg3=$3
ModExe=Validation_exe
UserEmail=fujimori.shinichiro.8a@kyoto-u.ac.jp

#Get CPU number allocation
while read -r line; do
    if [ "$line" == *CPU* ]; then
        eval "$line"
    fi
done < ./settings/$2.sh
if [ -z "${PBSCPU}" ]; then
  CoreN=${NCPU}
else
  CoreN=${PBSCPU}
fi
#Phisical memory allocation
PhsMem=$(expr ${NCPU} \* 4)

mkdir -m 777 -p ../../output/jobsche_tmp ../../output/jobreport

if [ "$3" == "alpha" ]; then
  host=alpha
else
  host=hpc
fi

if [ $1 == 0 ]; then
  bash ./validation_exe.sh -e ./settings/$2.sh
else
  sed "s/%%%1/${arg1}/g; s/%%%2/${arg2}/g; s/%%%3/${ModExe}/g; s/%%%4/${CoreN}/g; s/%%%5/${PhsMem}/g; s/%%%6/${UserEmail}/g" ./jobsche/psubjob_template_${host}.sh  >  ../../output/jobsche_tmp/psubjob_$1_$2.sh
  qsub ../../output/jobsche_tmp/psubjob_$1_$2.sh
fi
