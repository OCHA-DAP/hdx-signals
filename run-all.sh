#!/bin/bash

shopt -s globstar
set -e
for script in src/indicators/**/update_*; do
    Rscript $script
done