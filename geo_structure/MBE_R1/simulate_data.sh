#!/bin/bash

Rscript sim_new_dat_r1.R
~/Downloads/BEAST2/bin/beast -overwrite out_test.xml
cat end.txt >> geo_structure.trees
mv geo_structure.trees sim_trees_1.trees
 
Rscript sim_new_dat_r1.R
~/Downloads/BEAST2/bin/beast -overwrite out_test.xml
cat end.txt >> geo_structure.trees
mv geo_structure.trees sim_trees_2.trees

Rscript sim_new_dat_r1.R
~/Downloads/BEAST2/bin/beast -overwrite out_test.xml
cat end.txt >> geo_structure.trees
mv geo_structure.trees sim_trees_3.trees

Rscript sim_new_dat_r1.R
~/Downloads/BEAST2/bin/beast -overwrite out_test.xml
cat end.txt >> geo_structure.trees
mv geo_structure.trees sim_trees_4.trees

Rscript sim_new_dat_r1.R
~/Downloads/BEAST2/bin/beast -overwrite out_test.xml
cat end.txt >> geo_structure.trees
mv geo_structure.trees sim_trees_5.trees

Rscript sim_new_dat_r1.R
~/Downloads/BEAST2/bin/beast -overwrite out_test.xml
cat end.txt >> geo_structure.trees
mv geo_structure.trees sim_trees_6.trees

Rscript sim_new_dat_r1.R
~/Downloads/BEAST2/bin/beast -overwrite out_test.xml
cat end.txt >> geo_structure.trees
mv geo_structure.trees sim_trees_7.trees

Rscript sim_new_dat_r1.R
~/Downloads/BEAST2/bin/beast -overwrite out_test.xml
cat end.txt >> geo_structure.trees
mv geo_structure.trees sim_trees_8.trees

Rscript sim_new_dat_r1.R
~/Downloads/BEAST2/bin/beast -overwrite out_test.xml
cat end.txt >> geo_structure.trees
mv geo_structure.trees sim_trees_9.trees

Rscript sim_new_dat_r1.R
~/Downloads/BEAST2/bin/beast -overwrite out_test.xml
cat end.txt >> geo_structure.trees
mv geo_structure.trees sim_trees_10.trees


Rscript get_data_sets.R
