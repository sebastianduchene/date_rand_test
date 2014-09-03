
# coding: utf-8

# In[2]:

import pandas as pd
import numpy as np
import subprocess, sys, os, re


# In[323]:

true_log = list()
rand_log = list()
for i in os.listdir('.'):
    true_find = re.findall(r'.+true[.]log', i)
    rand_find = re.findall(r'.+rand.+[.]log', i)
    if len(true_find) > 0:
        true_log.append(true_find[0])
    if len(rand_find) > 0:
        rand_log.append(rand_find[0])


# In[345]:

def read_log_file(file_name):
    lines_text = np.array(open(file_name, 'r').readlines())
    lines_index = np.array([None is re.match(r'#', i) for i in lines_text])
    lines_text = lines_text[lines_index]
    lines_break = np.array([re.split('\t', i) for i in lines_text])
    lines_df = pd.DataFrame(data = lines_break[1:], columns = lines_break[0])
    lines_df = lines_df.ix[:, 1:(np.shape(lines_df)[1] - 1)]
    rate_data = [float(i) for i in lines_df.ix[10:, 'ucldMean']]
    mean_rate = np.mean(rate_data)
    perc_rate = np.percentile(rate_data, [2.5, 97.5])
    perc_rate.insert(0, mean_rate)
    return np.array(perc_rate)


# In[346]:

res_data = read_log_file(true_log[0])


# In[347]:

for i in rand_log:
    res_data = np.vstack([res_data, read_log_file(i)])


# In[348]:

res_data = pd.DataFrame(res_data, columns=['meanRate', 'lowHPD', 'highHPD'], index = ['true', 'rand1', 'rand2', 'rand3', 'rand4', 'rand5'])


# In[354]:

res_data.to_csv('res_out.txt', sep = '\t')


# In[354]:




# In[241]:




# In[ ]:



