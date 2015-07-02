# Examples of the date-randomisation test

This folder contains some examples of the different methods. 'uniform sampling' is for non-layered sampling with variable sampling times, which can be specified in the scripts attached, and following the instructions in the readme in the corresponding folder. 'layered_non_monophyletic' and 'layered_monophyletic' are described in our paper Duchêne, S., Duchêne, D., Holmes, E. C., & Ho, S. Y. (2015). The performance of the date-randomisation test in phylogenetic analyses of time-structured virus data. Molecular biology and evolution, msv056. Note that each of folder has a number of subfolders r1 through r5, which are different replicates. Each replicate consists of 5 'regular' randomisations and 5 cluster randomisations (for those with layered sampling). In the paper, however, we suggest using around 20. The python script randomise_blocks.py can do cluster randomisations, while standard randomisations are done through R in this study, using the instructions in 'uniform_sampling'. For beast 1 I also made a python script available here https://github.com/sebastianduchene/phylo_xml_tools.

Please email me at sebastian.duchene AT sydney.edu.au

