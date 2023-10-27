select_random_rows <- function(cond) {
  return(
    split(
      sample(which(cond), 6), 
      rep(1:3, each = 2)
    )
  )
}
# Destination Subsidy Type ------------------------------------------------

# pull 6 random rows to change (that fit criteria)
# we split up the 6 rows into 3 groups of 2, to test 3 issues
split_rows <- select_random_rows(Exit$Destination == 435)
Exit <- Exit %>%
  mutate(DestinationSubsidyType = 
           case_when(
             row_number() %in% split_rows[[1]] ~ NA, # null values
             row_number() %in% split_rows[[2]] ~ 99, # 99 (Data not collected, generally) values
             row_number() %in% split_rows[[3]] ~ 238, # nonsense values
             TRUE ~ DestinationSubsidyType
           )
  )


# CLS Subsidy Type --------------------------------------------------------
split_rows <- select_random_rows(CurrentLivingSituation$CurrentLivingSituation == 435)
CurrentLivingSituation <- CurrentLivingSituation %>%
  mutate(CLSSubsidyType = 
           case_when(
             row_number() %in% split_rows[[1]] ~ NA, # null values
             row_number() %in% split_rows[[2]] ~ 99, # 99 (Data not collected, generally) values
             row_number() %in% split_rows[[3]] ~ 234, # nonsense values
             TRUE ~ CLSSubsidyType
           )
  )

# Prior Living Situation Subsidy ------------------------------------------
split_rows <- select_random_rows(Enrollment$LivingSituation == 435)
Enrollment <- Enrollment %>%
  mutate(RentalSubsidyType = 
           case_when(
             row_number() %in% split_rows[[1]] ~ NA, # null values
             row_number() %in% split_rows[[2]] ~ 99, # 99 (Data not collected, generally) values
             row_number() %in% split_rows[[3]] ~ 234, # nonsense values
             TRUE ~ RentalSubsidyType
           )
  )



HMISParticipation <- HMISParticipation %>%
  mutate(
    HMISParticipationStatusStartDate = 
      case_when(
        row_number() %in% sample(
          which(
            HMISParticipationType == 1 & !is.na(HMISParticipationStatusEndDate)
          ), 
          1
        ),
        HMISParticipationStatusEndDate + days(6),
        HMISParticipationStatusStartDate
      ),
    HMISParticipationStatusEndDate = NA,
  )

