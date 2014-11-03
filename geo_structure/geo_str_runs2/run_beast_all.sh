#!/bin/bash

xml_files=`ls | awk '/xml$/'`

for run_file in $xml_files; do

    echo $run_file
#    sed -i.bak 's/15000000/50000000/g' $run_file
    java -jar ~/Desktop/progs/BEAST2.1.3/lib/beast.jar $run_file

done

