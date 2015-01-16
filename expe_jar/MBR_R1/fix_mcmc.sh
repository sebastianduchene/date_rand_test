#!/bin/bash

fils=`ls *xml`
for i in $fils; do
    echo I am fixing $i in
    echo `pwd`
    sed -i.bak 's/50000000/25000000/g' $i
done

rm *bak