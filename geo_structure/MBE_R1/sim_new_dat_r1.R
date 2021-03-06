# read the xml template
# Find all taxon names, which are in the tag sequence and the parameter 'taxon'
# Make a data frame with three columns: the old taxon names, new taxon names, and sampling times
# Substitute each value in the text file
# set the sampling times in the corresponding section of the xml

raw_lines <- readLines('sim_data_template.xml')

taxon_names_raw <- grep('sequence.+taxon', raw_lines, value = T)
taxon_names_loc <- gregexpr('taxon=\"([A-Z]|[0-9]|_)+', taxon_names_raw)

taxon_names <- vector()

for(i in 1:length(taxon_names_loc)){
  taxon_names[i] <- substr(taxon_names_raw[i], taxon_names_loc[[i]][1], taxon_names_loc[[i]][1] + attr(taxon_names_loc[[i]], 'match.length'))
  taxon_names[i] <- gsub('taxon=|\"', '', taxon_names[i])
}

taxa_replace <- data.frame(taxon_names, new_taxon_names = NA, sampling_time = NA, old_dates = NA, new_dates = NA)

# This is the sampling time
taxa_replace$sampling_time <- round(runif(nrow(taxa_replace), 0, 1), 2)

taxa_replace$sampling_time[c(1, nrow(taxa_replace))] <- c(0, 1)

for(i in 1:nrow(taxa_replace)){
  taxa_replace$new_taxon_names[i] <- gsub('_.+', paste0('_', taxa_replace$sampling_time[i]), taxa_replace$taxon_names[i])
}

for(i in 1:nrow(taxa_replace)){
      taxa_replace$old_dates[i] <- grep(paste0(taxa_replace$taxon_names[i], '='),  raw_lines, value = T)
      taxa_replace$new_dates[i] <- paste0(taxa_replace$new_taxon_names[i], '=', taxa_replace$sampling_time[i])
}

# substitute each at a time the , should not be added to the last instance
taxa_replace$new_dates[1:(nrow(taxa_replace) - 1)] <- paste0(taxa_replace$new_dates[1:(nrow(taxa_replace) - 1)], ',')

# Note that we use the \" as a tag to replace the taxon names only
for(i in 1:nrow(taxa_replace)){
      raw_lines <- gsub(taxa_replace$old_dates[i], taxa_replace$new_dates[i], raw_lines)
      raw_lines <- gsub(paste0(taxa_replace$taxon_names[i], '\"'), paste0(taxa_replace$new_taxon_names[i], '\"' ), raw_lines)      

}

cat(raw_lines, sep = '\n', file = 'out_test.xml')

