#!/bin/bash

folders=`ls -d */`
beast_path='/Users/sebastianducheneAIr/Desktop/temp_beast2/beast2/BEAST2/lib/beast.jar'

echo $folders

for i in $folders; do
    cd $i
    rm *log
    java -jar $beast_path -beagle true_dat.xml
    java -jar $beast_path -beagle rand_dat_1.xml
    java -jar $beast_path -beagle rand_dat_2.xml
    java -jar $beast_path -beagle rand_dat_3.xml
    Rscript ../../get_results.R
    rm *log
    cd ..
done