# Incorrect Date Format, High-Priority (check 11)
Client$DOB <- format(Client$DOB, "%d-%m-%y")

# Incorrect Date Format, Error (check 47)
Client$DateUpdated <- format(Client$DateUpdated, "%d-%m-%y")

# incorrect/extra column (check 82)
Client$ExtraCol <- 1

# incorrect/missing column, error (check 82)
Client <- Client %>% select(-DateCreated)

# incorrect/missing column, high-priority (check 12)
Client <- Client %>% select(-BlackAfAmerican)

# incorrect/misspelled column (check 12)
Client <- Client %>% rename(Transgenderr = Transgender)

## add more checks here