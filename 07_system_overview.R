
system_df_prep <- EnrollmentAdjust %>%
  left_join(Project0 %>%
              select(ProjectID, ProjectName, OrganizationID, RRHSubType),
            by = "ProjectID") %>%
  left_join(Organization %>% select(OrganizationID, OrganizationName),
            by = "OrganizationID") %>%
  left_join(Client %>%
              select(PersonalID, VeteranStatus, Woman, Man, NonBinary, Transgender, CulturallySpecific,
          DifferentIdentity, Questioning, GenderNone), by = "PersonalID") %>%
  left_join(HealthAndDV %>%
              filter(DataCollectionStage == 1) %>%
              select(EnrollmentID, DomesticViolenceSurvivor, CurrentlyFleeing),
            by = "EnrollmentID") %>%
  select(
    EnrollmentID,
    PersonalID,
    VeteranStatus,
    OrganizationName,
    ProjectID,
    ProjectName,
    ProjectType,
    RRHSubType,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    DomesticViolenceSurvivor,
    CurrentlyFleeing,
    LivingSituation,
    RentalSubsidyType,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    MonthsHomelessPastThreeYears,
    DisablingCondition,
    AgeAtEntry,
    Destination,
    DestinationSubsidyType
  )

# corrected hohs ----------------------------------------------------------

hh_adjustments <- system_df_prep %>%
  mutate(VeteranStatus = if_else(VeteranStatus == 1 &
                                   !is.na(VeteranStatus), 1, 0),
         HoHAlready = if_else(RelationshipToHoH == 1 &
                                AgeAtEntry > 17, 1, 0)) %>%
  group_by(HouseholdID, ProjectID) %>%
  arrange(desc(HoHAlready), desc(VeteranStatus), desc(AgeAtEntry), PersonalID) %>%
  mutate(Sequence = seq(n()),
         CorrectedHoH = if_else(Sequence == 1, 1, 0)) %>%
  ungroup() %>%
  select(EnrollmentID, CorrectedHoH)

# keeps original HoH unless the HoH is younger than 18 or if there are mult hohs
# if they are younger than 18, or if there are mult hohs, it will take the
# veteran in the hh. if there are none or multiple veterans, it will take the
# oldest. if they're the same veteran status and age, it will name the person
# with the first PersonalID.
# if the hh looks like: 
# age 8 (hoh), age 40 (non-veteran), age 38 (veteran) ->  age 38 (veteran)
# age 16 (hoh), age 8, age 3 -> hoh stays age 16 (bc they're the oldest)
# age 42 (hoh), age 52 (veteran), age 5 -> hoh stays age 42 (hoh) (trusting the
# decisions of the case manager, maybe the 42 year old was the eligible person)
# age 53 (hoh), age 46 (hoh), age 17 -> only the age 53 and not the age 46

# adding corrected hoh ----------------------------------------------------

system_df <- system_df_prep %>%
  left_join(hh_adjustments, join_by(EnrollmentID)) %>%
  relocate(CorrectedHoH, .after = RelationshipToHoH)