#!/bin/bash

Rscript spatial_mod_selection_with_urb_natality.R $@ 
echo "listing files in current wd" 
ls -R 
echo "looking for modselection_nat" 
find . -name '*modselection_nat*' 

