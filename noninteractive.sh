#!/bin/bash

Rscript ksim_scriptforchtc.R $@ 
echo "listing files in current wd" 
ls -R 
echo "looking for tempoutput" 
find . -name '*tempoutput*' 

