#!/bin/bash

Rscript ksim_nourb.R $@ 
echo "listing files in current wd" 
ls -R 
echo "looking for bick_nourb" 
find . -name '*bick_nourb*' 

