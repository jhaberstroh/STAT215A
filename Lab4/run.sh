#!/bin/bash

if [ -z $1 ]; then
   echo "run.sh must be passed one argument: /absolute/path/to/data"
   exit
fi

echo "config:" > scripts/cfg.yml
echo "  dir: $(pwd)" >> scripts/cfg.yml
echo "  data: $1" >> scripts/cfg.yml

cd scripts
Rscript exploration.R -c cfg.yml
#for rfile in $(ls *.R); do
#    Rscript rfile -c cfg.yml
#done

cd ../writeup
latex Lab4_Ruoxi.tex
