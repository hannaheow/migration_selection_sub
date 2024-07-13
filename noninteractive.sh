#!/bin/bash

Rscript ksim_scriptforchtc_withreltol.R $@ 
echo "listing files in current wd" 
ls -R 
echo "looking for bickij_int" 
find . -name '*bickij_int*' 

