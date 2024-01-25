universe <- system_df_enrl_filtered() %>%
  inner_join(system_df_people_filtered(), by = "PersonalID") %>%
  inner_join(enrollments_crossing_report()$eecr, by = "PersonalID") %>%
  inner_join(enrollments_crossing_report()$lecr, by = "PersonalID") %>%
  # remove enrollments where the exit is over 2 years prior to report start
  filter(as.numeric(difftime(ExitDate, input$syso_date_range[1],
                             unit = "days")) / 365 <= 2) %>%
  mutate(is_before_eecr = EntryDate < EntryDate_eecr) %>%
  # create enrollment-level variables/flags that will be used to 
  # label people to be counted in the system activity charts
  group_by(PersonalID) %>%
  mutate(
    lookback_stay_in_lh = any(ProjectType %in% lh_project_types & is_before_eecr == TRUE),
    lookback_entered_as_homeless = any(lh_prior_livingsituation & is_before_eecr),
    NoEnrollmentsToLHFor14DaysFromLECR = !any(
      as.numeric(difftime(ExitDate_lecr, EntryDate, "days")) <= -14 & 
        ProjectType %in% lh_project_types
    ),
    return_from_permanent = any(lh_at_entry == TRUE & 
                                  as.numeric(difftime(EntryDate_eecr, ExitDate, unit = "days")) >= 14 &
                                  Destination %in% perm_destinations),
    reengaged_from_temporary = any(lh_at_entry == TRUE &
                                     as.numeric(difftime(EntryDate_eecr, ExitDate, unit = "days")) >= 14 &
                                     !(Destination %in% perm_destinations))
  ) %>%
  ungroup() %>%
  # consider dropping all the enrollment-level vars here
  unique() %>%
  mutate(
    InflowType = case_when(
      #1) If project type is in (lh_project_types), then client is not newly homeless (0)
      #2) If LivingSituation is in (hs_living_situation), then client is not newly homeless (0)
      #3) If LivingSituation is in (non_hs_living_sit) and both LOSUnderThreshold and PreviousStreetESSH == 1, then client is not newly homeless (0)
      EnrolledHomeless_eecr ~ "Enrolled: Homeless",
      EnrolledHoused_eecr ~ "Enrolled: Housed",
      !lookback_stay_in_lh & !lookback_entered_as_homeless ~ "Newly Homeless",
      return_from_permanent ~ "Returned from \nPermanent",
      reengaged_from_temporary ~ "Re-engaged from \nTemporary/Unknown"
    ),
    OutflowType = case_when(
      # The client has exited from an enrollment with a permanent destination
      # and does not have any other enrollments (aside from RRH/PSH with a move-in date?)
      # in emergency shelter, transitional housing, safe haven, or street outreach for at least 14 days following.
      Destination_lecr %in% perm_destinations &
        !is.na(ExitDate_lecr)# &
      # NoEnrollmentsToLHFor14DaysFromLECR == TRUE
      ~ "Permanent Destination",
      
      # The client has exited from an enrollment with a temporary/unknown destination
      # and does not have any other enrollments (aside from RRH/PSH with a move-in date?)
      # in emergency shelter, transitional housing, safe haven, or street outreach for at least 14 days following.!(Destination_lecr %in% perm_destinations) &
      !is.na(ExitDate_lecr) &
        !Destination_lecr %in% perm_destinations
      ~ "Non-Permanent \nDestination",
      
      EnrolledHomeless_lecr == TRUE & is.na(ExitDate_lecr) &
        HouseholdID == HouseholdID_lecr
      ~ "Enrolled: Homeless",
      
      EnrolledHoused_lecr == TRUE & is.na(ExitDate_lecr) &
        HouseholdID == HouseholdID_lecr
      ~ "Enrolled: Housed",
      TRUE ~ "enrollment isn't an lecr"
    )
  )

null_inflow <- universe %>% filter(CorrectedHoH == 1 & is.na(InflowType))
null_outflow <- universe %>% filter(CorrectedHoH == 1 & OutflowType == "something's wrong")
# the problem is with EnrolledHoused/EnrolledHomeless

null_outflow %>% filter(HouseholdID != HouseholdID_lecr)
# it's assigning In/Outflow Types to non-eecr/non-lecr enrollments

universe %>%
  filter(HouseholdID == HouseholdID_eecr | HouseholdID == HouseholdID_lecr)


