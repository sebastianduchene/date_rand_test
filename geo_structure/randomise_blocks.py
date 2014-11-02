import re, os, copy


raw_lines = open('true_dat.xml', 'r').readlines()

dates_lim = [[i for i in range(len(raw_lines)) if 'dateTrait' in raw_lines[i]][0], [i for i in range(len(raw_lines)) if 'spec=\"TaxonSet\"' in raw_lines[i]][0]]


dates_init_line = re.split(r' ', raw_lines[dates_lim[0]])
dates_first = dates_init_line[-1]
dates_init_line = ' '.join(dates_init_line[0:(len(dates_init_line) - 1)])


dates_last_line = re.split(r' ', raw_lines[dates_lim[1]])
dates_last = dates_last_line[0]
dates_last_line = ' '.join(dates_last_line[1:len(dates_last_line)])


dates_data = raw_lines[(dates_lim[0] + 1):(dates_lim[-1] - 1)]
dates_data.insert(0, dates_first)
dates_data.insert(len(dates_data), dates_last)

dates_data = [re.sub(',|\n', '', i) for i in dates_data]


dates_dict = {}

for i in dates_data:
    split_dat = re.split('=', i)
    dates_dict[split_dat[0]] = split_dat[1]

block_names = list(set([i[0] for i in dates_dict.keys()]))



