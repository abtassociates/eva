print("updating test good for FSA")

# Incorrect Date Format, High-Priority (check 11)
reduced_data_fsa$Client$DOB <- format(reduced_data_fsa$Client$DOB, "%d-%m-%y")

# Incorrect Date Format, Error (check 47)
reduced_data_fsa$Client$DateUpdated <- format(reduced_data_fsa$Client$DateUpdated, "%d-%m-%y")

# incorrect/extra column (check 82)
reduced_data_fsa$Client$ExtraCol <- 1

# incorrect/missing column, error (check 82)
reduced_data_fsa$Client <- reduced_data_fsa$Client %>% select(-DateCreated)

# incorrect/missing column, high-priority (check 12)
reduced_data_fsa$Client <- reduced_data_fsa$Client %>% select(-BlackAfAmerican)

# incorrect/misspelled column (check 12)
reduced_data_fsa$Client <- reduced_data_fsa$Client %>% rename(Transgenderr = Transgender)

## add more checks here