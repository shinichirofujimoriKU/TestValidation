

#argument(exefile and revision_validation)--------------------------------------
if [ "$1" = "" ]; then
  exefile="execution01"
else
  exefile=$1
fi
#parameter settings by using ${exefile}-----------------------------------------
. ./settings/execution01.sh
if [ ! -e "./settings/${exefile}.sh" ];then
  echo "./settings/${exefile}.sh is not exist."
  exit
else
  . "./settings/${exefile}.sh"
fi
flagpbs=${2:-000}
if [ $flagpbs != 000 ]; then
  pausemode=off
fi

#Make directories
listmkdir="output output/jobreport output/temp "
for j in ${listmkdir}; do
  mkdir -m 777 -p ../${j} 2>/dev/null
done

read -p "${NCPU}"
cd ../prog
if [ "$train" = "on" ]; then
  R --vanilla --slave -f train_flags.R --args MODEL_TYPE=${ModelT} N_CORES=${NCPU}
fi
if [ "$score" = "on" ]; then
  R --vanilla --slave -f score_flags.R --args MODEL_TYPE=${ModelT} N_CORES=${NCPU}
fi

