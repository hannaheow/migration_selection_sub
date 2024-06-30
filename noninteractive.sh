#!/bin/bash

Rscript ksim_scriptforchtc_withreltol.R $@ 
echo "listing files in current wd" 
ls -R 
echo "looking for bick" 
find . -name '*bick*' 

