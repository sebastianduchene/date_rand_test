# Simulation of uniform sampling (non-clustering)

To generate trees with different calibration windows open the make_chronograms.R script and specify the range of sampling times. 
Run this script using source in R. This will produce an xml file called simulation_1.xml as shown in the example here. This is a beast file with no data, which is equivalent to sampling from the prior distribution. 

Run simulation_1.xml (or use the name that you specified for the file) in beast2. This will produce a file with trees called unif_sampling.trees. 

Then use source to run the script called get_data_sets.R. This will produce some folders with xml files. Those with names that start with rand have randomised sampling times.

