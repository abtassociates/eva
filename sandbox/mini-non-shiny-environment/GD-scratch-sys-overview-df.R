
system_df_prep <- EnrollmentAdjust %>%
  left_join(Project0 %>%
              select(ProjectID, ProjectName, OrganizationID, RRHSubType),
            by = "ProjectID") %>%
  left_join(Organization %>% select(OrganizationID, OrganizationName),
            by = "OrganizationID") %>%
  left_join(Client %>%
              select(PersonalID, VeteranStatus), by = "PersonalID") %>%
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
  mutate(VeteranStatus = if_else(VeteranStatus == 1, 1, 0)) %>%
  group_by(HouseholdID) %>%
  arrange(desc(VeteranStatus), desc(AgeAtEntry)) %>%
  mutate(Sequence = seq(n()),
         CorrectedHoH = if_else(Sequence == 1, 1, 0)) %>%
  ungroup() %>%
  select(EnrollmentID, CorrectedHoH)

# adding corrected hoh ----------------------------------------------------

system_df <- system_df_prep %>%
  left_join(hh_adjustments, join_by(EnrollmentID))

