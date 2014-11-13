#!/bin/bash

if [ -z $1 ]; then
   echo "run.sh must be passed one argument: /absolute/path/to/data"
   exit
fi

echo "config:" > scripts/cfg.yml
echo "  dir: $(pwd)" >> scripts/cfg.yml
echo "  data: $1" >> scripts/cfg.yml

cd scripts
echo "Generating EDA images..."
Rscript exploration.R -c cfg.yml
echo "Generating CRF images..."
Rscript crf_example.R -c cfg.yml
echo "Permutation analysis..."
Rscript feature_selection_example.R -c cfg.yml
echo "Cross-Validation results"
Rscript Validation.R -c cfg.yml

echo "To complete document generation, run pdfLaTeX on writeup/Lab4_writeup.tex"
