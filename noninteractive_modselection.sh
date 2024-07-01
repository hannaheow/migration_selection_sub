#!/bin/bash

Rscript spatial_mod_selection_with_urb_corrected.R $@ 
echo "listing files in current wd" 
ls -R 
echo "looking for bic" 
find . -name '*bic*' 

