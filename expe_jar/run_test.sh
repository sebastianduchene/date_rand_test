#!/bin/bash

folders=`ls -d */`
beast_path='/Users/sebastianduchene/Desktop/progs/BEAST2.1.3/lib/beast.jar'

echo $folders

for i in $folders; do
    cd $i
    rm *log
    java -jar $beast_path -beagle test1.xml
    java -jar $beast_path -beagle rand_1.xml
    java -jar $beast_path -beagle rand_2.xml
    java -jar $beast_path -beagle rand_3.xml
    Rscript ../../get_results.R
    rm *log
    cd ..
done