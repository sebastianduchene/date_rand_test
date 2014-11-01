import re, os


raw_lines = open('true_dat.xml', 'r').readlines()

dates_lim = [[i for i in range(len(raw_lines)) if 'dateTrait' in raw_lines[i]][0], [i for i in range(len(raw_lines)) if 'spec=\"TaxonSet\"' in raw_lines[i]][0]]



