import re, os, copy, random, sys

# To use open terminal and type. python path/to/this/script path/to/the/folder/with/true/dat number_of_randomisations

#raw_lines = open('true_dat_test.xml', 'r').readlines()
#n_rand = 5
raw_lines = open(sys.argv[1], 'r').readlines()
n_rand = int(sys.argv[2])

dates_lim = [[i for i in range(len(raw_lines)) if 'dateTrait' in raw_lines[i]][0], [i for i in range(len(raw_lines)) if 'spec=\"TaxonSet\"' in raw_lines[i]][0]]

dates_init_line = re.split(r' ', raw_lines[dates_lim[0]])
dates_first = dates_init_line[-1]
dates_init_line = ' '.join(dates_init_line[0:(len(dates_init_line) - 1)])

dates_last_line = re.split(r' ', raw_lines[dates_lim[1]])
dates_last = dates_last_line[0]
dates_last_line = ' '.join(dates_last_line[1:len(dates_last_line)])

dates_data = raw_lines[(dates_lim[0] + 1):(dates_lim[-1])]
dates_data.insert(0, dates_first)
dates_data.insert(len(dates_data), dates_last)

dates_data = [re.sub(',|\n', '', i) for i in dates_data]
block_dates_set = set([re.findall('^([A-Z]|[a-z])+', i)[0]+re.findall('=.+$', i)[0] for i in dates_data])

block_dates = {}
for i in block_dates_set:
    split_temp = re.split('=', i)
    block_dates[split_temp[0]] = split_temp[1]


dates_dict = {}
for i in dates_data:
    split_dat = re.split('=', i)
    dates_dict[split_dat[0]] = split_dat[1]

dates_rand = [block_dates[i] for i in block_dates]
for i in range(n_rand):
    random.shuffle(dates_rand)
    block_rand_dict = {b_name:date for b_name, date in zip(block_dates.keys(), dates_rand)}

    dates_dict_rand = copy.copy(dates_dict)
    for taxa in dates_dict_rand:
        dates_dict_rand[taxa] = block_rand_dict[re.findall('^([A-Z]|[a-z])+', taxa)[0]]
    
    new_dates_list = [date_key+'=' + dates_dict_rand[date_key] + ',\n' for date_key in dates_dict_rand]
    new_dates_list[0] = dates_init_line + ' ' + new_dates_list[0]
    new_dates_list[-1] = re.sub(',|\n', '', new_dates_list[-1])[0] + ' ' + dates_last_line
    print block_rand_dict
    new_rand_lines = copy.copy(raw_lines)
    new_rand_lines[(dates_lim[0] + 1):dates_lim[-1]] = new_dates_list[1:-1]
    new_rand_lines = ''.join(new_rand_lines)
    new_file_name = re.sub('[.]xml', '_rand_cluster_', sys.argv[1]) + str(i + 1)
    new_rand_lines = re.sub(re.sub('[.]xml', '', sys.argv[1]), new_file_name, new_rand_lines) 
    open(new_file_name + '.xml', 'w').write(new_rand_lines)
