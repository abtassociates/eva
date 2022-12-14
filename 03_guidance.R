
guidance_conflicting_hi <-
  str_squish("There is a discrepancy between the data element indicating that the
             client is receiving health insurance and the data elements regarding
             the health insurance sources. Please verify this client's health
             insurance to correct this error.")

guidance_conflicting_income <-
  str_squish("There is a discrepancy between the data element indicating that the client
is receiving income and the data elements regarding the income sources. Please 
verify this client's income to correct this error.")

guidance_conflicting_ncbs <-
  str_squish("There is a discrepancy between the data element indicating that the client
is receiving non-cash benefits and the data elements regarding the non-cash 
benefits sources. Please verify this client's non-cash benefits to correct
this error.")

guidance_missing_at_exit <- 
  str_squish("This data element is required to be collected at Project Exit. Please
  go to the client's assessment at Project Exit to enter this data to HMIS.")

guidance_missing_at_entry <- 
  str_squish("This data element is required to be collected at Project Start.
             Please go to the client's assessment at Project Start to enter this
             data to HMIS.")

guidance_missing_pii <- 
  str_squish("Please correct by providing the missing information in the client's
             record.")

guidance_dkr_data <- 
  str_squish("This record is being flagged because it is incomplete. Some
             clients may decline or be unable to provide the answer to one or
             more questions during an assessment. Please review the HMIS Data
             Manual, pages 12-13, for further guidance on data completeness. If
             possible to complete this data, please update the record. Note that
             high levels of incomplete data for a given element may indicate the
             need for additional training.")

guidance_enrl_active_outside_op <-
  str_squish("This enrollment is active outside of the project's operating start and 
end dates. Please reconcile this by either correcting the enrollment
dates or the project's operating dates.")

guidance_exit_before_start <-
  str_squish("This enrollment's exit date is before the enrollment's project start date. 
The exit date must be after the project start date. Please go to this enrollment 
and ensure that the correct project start date and exit date are entered.")

guidance_hoh_issues <- str_squish(
  "Every household must have exactly one head of household. Please be sure:
      1. All members of the household are included in the program stay;
      2. Each household member's birthdate is correct; 
      3. Every household member has a Relationship to Head of Household; 
      4. One member is designated as the head of household for the entire duration
      of the enrollment.")

overlapNEW_entry_and_exit_guidance <-
"This error indicates a residential project enrollment overlaps with another 
residential project enrollment. Please verify this client's enrollment dates 
with both projects to correct this overlap."

overlapNEW_entry_and_exit_bn_guidance <-
  str_squish("This error indicates that a bed night in a shelter enrollment overlaps with
a residential project's enrollment. Please verify the enrollment dates and bed
night dates of both projects to correct this overlap.")

overlapNEW_entry_and_exit_bn2_guidance <-
  str_squish("This error indicates that a bed night in a shelter enrollment overlaps with a
permanent housing enrollment after that enrollment's housing move-in date.
Please verify the bed night date, enrollment dates, and housing move-in date
of both projects to correct this overlap.")

overlapNEW_movein_and_exit_guidance <-
  str_squish("This error indicates that a stay in a permanent housing enrollment (indicated
by the housing move-in date) overlaps with another permanent housing 
enrollment (indicated by the latter enrollment's housing move-in date). Please
verify the enrollment dates and housing move-in dates of both projects to 
correct this overlap.")
