

set -euo pipefail

# 1) 個人設定を読む（無ければテンプレ値）
CONFIG_FILE="${CONFIG_FILE:-$(dirname "$0")/../config/config.sh}"
if [ -f "$CONFIG_FILE" ]; then
  source "$CONFIG_FILE"
else
  echo "WARN: config not found: $CONFIG_FILE (using defaults)" >&2
fi

# 2) デフォルト（個人設定で上書き可）
#: "${PY_PREFIX:=$HOME/python-3.10}"
#: "${VENV_PY:=$HOME/venv/r-tf/bin/python}"
#: "${FFI_LIBDIR:=$HOME/local/lib64}"

#export LD_LIBRARY_PATH="$PY_PREFIX/lib:$FFI_LIBDIR:${LD_LIBRARY_PATH:-}"
#export RETICULATE_PYTHON="$VENV_PY"

#echo "CONDA_PREFIX=$CONDA_PREFIX"
#echo "RETICULATE_PYTHON=${RETICULATE_PYTHON}"
which python || true
which R || true
#read -p "Press [Enter] key to continue..."

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

echo "Core number is ${NCPU}"
cd ../prog
trainf=false
scoref=false
if [ "$train" = "on" ]; then trainf=true; fi
if [ "$score" = "on" ]; then scoref=true; fi
if [ "$allrun" = "on" ]; then
  trainf=true
  scoref=true
fi
#  R --vanilla --slave -f train_flags.R --args MODEL_TYPE=${ModelT} N_CORES=${NCPU}
#  R --vanilla --slave -f score_flags.R --args MODEL_TYPE=${ModelT} N_CORES=${NCPU}

  R --vanilla --slave -f tran_and_score_flags2.R --args MODEL_TYPE=${ModelT} N_CORES=${NCPU} \
    RUN_TRAIN=${trainf} RUN_SCORE=${scoref}
