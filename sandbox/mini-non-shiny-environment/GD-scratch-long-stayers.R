

# The Variables That We Want ----------------------------------------------

vars_prep <- c(
  "HouseholdID",
  "PersonalID",
  "OrganizationName",
  "ProjectID",
  "ProjectName",
  "ProjectType",
  "EntryDate",
  "MoveInDateAdjust",
  "ExitDate"
)

vars_we_want <- c(vars_prep,
                  "Issue",
                  "Type",
                  "Guidance")

cls_df <- validation %>%
  left_join(CurrentLivingSituation %>%
              select(CurrentLivingSitID,
                     EnrollmentID,
                     InformationDate), by = "EnrollmentID") %>%
  group_by(EnrollmentID) %>%
  slice_max(InformationDate) %>%
  ungroup() %>%
  select(EnrollmentID, "MaxCLSInformationDate" = InformationDate)

long_stayers <- validation %>%
  left_join(cls_df, by = "EnrollmentID") %>%
  select(all_of(vars_prep), ProjectID, MaxCLSInformationDate) %>%
  filter(is.na(ExitDate) &
           ((ProjectType %in% c(project_types_w_cls) &
           !is.na(MaxCLSInformationDate)) |
           (!ProjectType %in% c(project_types_w_cls)))) %>%
  mutate(
    Days = 
      as.numeric(difftime(
        as.Date(meta_HUDCSV_Export_Date()),
        if_else(ProjectType %in% c(project_types_w_cls),
                MaxCLSInformationDate, # most recent CLS
                EntryDate), # project entry
        units = "days"
    )),
    Issue = "Days Enrollment Active Exceeds Local Settings",
    Type = "Warning",
    Guidance = str_squish(
      "You have at least one active enrollment that has been
         active for longer than the days set for this Project Type in your
         Referral settings on the Edit Local Settings tab. Please check that "
    )
  ) %>%
  filter(# ProjectType == projecttype &
           60 < Days) %>%
  select(all_of(vars_we_want))
