import re, os

tax_names = []

for loc, date in zip(['A', 'B', 'C', 'D', 'E'], [0, 1, 2, 3, 4]):
    for tax in range(10):
        tax_names.append('>'+loc+str(tax)+'_'+str(date)+'\n')
        tax_names.append('NN\n')

open('geo_structure.fasta', 'w').writelines(tax_names)

