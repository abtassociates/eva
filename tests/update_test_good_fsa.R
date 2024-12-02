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

# impermissible characters
reduced_data_fsa$Project$ProjectName <- paste0(reduced_data_fsa$Project$ProjectName, "[")
reduced_data_fsa$Project$ProjectCommonName <- paste0("“", reduced_data_fsa$Project$ProjectCommonName, "”")
reduced_data_fsa$Organization$OrganizationCommonName <- replace_char_at(reduced_data_fsa$Organization$OrganizationName, 4, "ñ")
reduced_data_fsa$Organization$OrganizationCommonName <- replace_char_at(reduced_data_fsa$Organization$OrganizationCommonName, 5, "😊")
reduced_data_fsa$Organization$OrganizationName <- paste0(reduced_data_fsa$Organization$OrganizationName, "ع")

## add more checks here