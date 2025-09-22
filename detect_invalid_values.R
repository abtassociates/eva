# FY26 vlaidation specs
validation_specs_bk <- "/media/sdrive/projects/CE_Data_Toolkit/FY26 HMIS-CSV-Machine-Readable-Specifications.xlsx"

# Read in Variable-List xwalk
# We will handle the nuanced ones later
var_list_xwalk <- read_excel(validation_specs_bk, sheet = "CSV Lists Data Dict FY2026") %>%
  fmutate(
    Type = gsub("\u00A0", "", Type),
    List = str_trim(ifelse(List == "1.1000000000000001", "1.1", List)),
    Name = gsub("\u00A0", "", Name),
    Name = ifelse(Name == "SSN[1]", "SSN", ifelse(Name == "Geocode[1]", "Geocode", Name))
  ) %>%
  fselect(CSV, Name, Type, List, Null, Notes) %>%
  funique() %>% #Dedup since some variables can appear multiple times, one for each Data Element, e.g. LOSUnderThreshold
  qDT()

# Read in CSV Lists
validation_lists <- read_excel(validation_specs_bk, sheet = "CSV Lists FY2026") %>%
  fmutate(
    Value = gsub("\u00A0", "", Value),
    List = ifelse(
      List == "1.1000000000000001", "1.1", 
      ifelse(
        List == "4.1399999999999997", "4.14",
        List
      )
    )
  )


# Convert to a named list, where the names are the unique values of `List` and the values are the vector of `Value`s.
result <- split(validation_lists$Value, validation_lists$List)


# Now loop through each variable in each CSV and make sure it has the right Type and List
validation_summary <- list()  # store flagged rows for each dataset

for(csv_name in intersect(unique(cols_and_data_types$File), var_list_xwalk$CSV)) {
  # 1. Get dataset
  dt <- get(csv_name) %>% qDT()
  
  # 2. Columns for this CSV
  all_cols <- var_list_xwalk[CSV == csv_name, Name]
  list_cols <- var_list_xwalk[CSV == csv_name & !is.na(List), Name]
  
  # 3. Build allowed values for each column
  # Handle nuanced columns separately
  allowed_values <- setNames(
    lapply(var_list_xwalk[CSV == csv_name & !is.na(List), List], function(l) result[[as.character(l)]]),
    list_cols
  )

  # Allow Nulls, if
  for(col in list_cols) {
    nulls_allowed <- var_list_xwalk[CSV == csv_name & Name == col, toupper(Null)]
    if(fcoalesce(nulls_allowed, "N") == "Y") {
      allowed_values[[col]] <- c(allowed_values[[col]], NA)
    }
  }
  
  hashed_cols <- c("FirstName","MiddleName","LastName","NameSuffix","SSN")
  invalid_counts <- sapply(all_cols, function(col) {
    if (col %in% names(dt)) {
      col_type <- ifelse(
        col %in% hashed_cols,
        "S32",
        var_list_xwalk[CSV == csv_name & Name == col, Type]
      )
      
      valid_list <- var_list_xwalk[CSV == csv_name & Name == col, List]
      
      conditional_nulls <- var_list_xwalk[CSV == csv_name & Name == col, Notes]
      if(col_type %in% c("S","T","D", "M", "M+") || (col_type == "I" & is.na(valid_list))) return(0)
      
      sum(
        if(substr(col_type, 1, 1) == "S") {
          str_length(dt[[col]]) > rep(as.integer(gsub("S", "", col_type)), nrow(dt))
        } else if(csv_name == "Services" && col == "TypeProvided") {
          # For RecordType · 141 – list P1.2 · 142 – list R14.2 · 143 – list W1.2 · 144 – list V2.2 · 151 – list W2.2 · 152 – list V3.3 · 161 – list P2.2 · 200 – list 4.14 · 210 – list V8.2 300 – list C2.2
          new_dt <- dt %>%
            fmutate(
              allowed_list = fcase(
                RecordType == 141, list(result[["P1.2"]]),
                RecordType == 142, list(result[["R14.2"]]),
                RecordType == 143, list(result[["W1.2"]]),
                RecordType == 144, list(result[["V2.2"]]),
                RecordType == 151, list(result[["W2.2"]]),
                RecordType == 152, list(result[["V3.3"]]),
                RecordType == 161, list(result[["P2.2"]]),
                RecordType == 200, list(result[["4.14"]]),
                RecordType == 210, list(result[["V8.2"]]),
                RecordType == 300, list(result[["C2.2"]])
              ),
              invalid = !(get(col) %in% unlist(allowed_list))
            )
          new_dt$invalid
        } else if(csv_name == "Services" && col == "SubTypeProvided") {
          # Null unless RecordType = 144 and TypeProvided = 3, 4, or 5. For TypeProvided: · 3 – list V2.A · 4 – list V2.B · 5 – list V2.C
          new_dt <- dt %>%
            fmutate(
              allowed_list = fcase(
                RecordType == 144 & TypeProvided == 3, list(c(result[["V2.A"]], NA)),
                RecordType == 144 & TypeProvided == 4, list(c(result[["V2.B"]], NA)),
                RecordType == 144 & TypeProvided == 5, list(c(result[["V2.C"]], NA))
              ),
              invalid = fifelse(RecordType == 144, !(get(col) %in% unlist(allowed_list)), FALSE)
            )
          new_dt$invalid
        } else if(grepl("Null unless|required if", conditional_nulls) > 0) {
          # CSV	DE#	Name	Type	List	Null	Notes
          # Inventory	2.07.6	Availability	I	2.07.6	Y	Null unless Project.csv ProjectType = 0 or 1
          # Inventory	2.07.5	ESBedType	I	2.07.5	Y	Null unless Project.csv ProjectType = 0 or 1
          # Enrollment	3.917.A	RentalSubsidyType	I	3.12.A	Y	Null unless 3.917.1 = 435
          # Enrollment	P3.A 	ReasonNotEnrolled	I	P3.A	Y	Null unless P3.2 = 0 
          # Enrollment	R2.A	ReasonNoServices	I	R2.A	Y	Null unless R2.2 = 0
          # Enrollment	R2.B	RunawayYouth	I	1.8	Y	Null unless R2.2 = 1
          # Enrollment	C4.B	PreferredLanguageDifferent	S100		Y	Null unless C4.A = 21
          # Exit	3.12.A	DestinationSubsidyType	I	3.12.A	Y	Null unless Destination = 435
          # Exit	3.12.B	OtherDestination	S50		Y	Null unless Destination = 17
          # Exit	R17.A	EarlyExitReason	I	R17.A	Y	Null unless R17.1 = 3
          # Exit	R15.A	ExchangeForSexPastThreeMonths	I	1.8	Y	Null unless R15.1 = 1
          # Exit	R15.B	CountOfExchangeForSex	I	R15.B	Y	Null unless R15.1 = 1
          # Exit	R15.C	AskedOrForcedToExchangeForSex	I	1.8	Y	Null unless R15.1 = 1
          # Exit	R15.D	AskedOrForcedToExchangeForSexPastThreeMonths	I	1.8	Y	Null unless R15.C = 1
          # Exit	R16.A	CoercedToContinueWork	I	1.8	Y	Null unless R16.1 or R16.2 = 1
          # Exit	R16.B	LaborExploitPastThreeMonths	I	1.8	Y	Null unless R16.1 or R16.2 = 1
          # Exit	R18.A	IndividualCounseling	I	1.10	Y	Null unless R18.1 = 1
          # Exit	R18.A	FamilyCounseling	I	1.10	Y	Null unless R18.1 = 1
          # Exit	R18.A	GroupCounseling	I	1.10	Y	Null unless R18.1 = 1
          # Exit	R18.B 	SessionCountAtExit	I		Y	Null unless R18.1 = 1 Integer >0
          # Exit	R20.1 	AftercareDate	D		Y	Null unless date is between ExitDate and ExitDate + 180 days AND AftercareProvided is not null
          # Exit	R20.2	AftercareProvided	I	R20.2	Y	Null unless AftercareDate is between ExitDate and ExitDate + 180 days
          # Exit	R20.A	EmailSocialMedia	I	1.10	Y	Null unless R20.2 = 1
          # Exit	R20.A	Telephone	I	1.10	Y	Null unless R20.2 = 1
          # Exit	R20.A	InPersonIndividual	I	1.10	Y	Null unless R20.2 = 1
          # Exit	R20.A	InPersonGroup	I	1.10	Y	Null unless R20.2 = 1
          # IncomeBenefits	4.02.P	OtherIncomeSourceIdentify	S50		Y	Null unless 4.02.17 = 1
          # IncomeBenefits	4.03.A	OtherBenefitsSourceIdentify	S50		Y	Null unless 4.03.8 = 1
          # IncomeBenefits	4.04.3A	NoMedicaidReason	I	4.04.A	Y	Null unless 4.04.3 = 0
          # IncomeBenefits	4.04.4A	NoMedicareReason	I	4.04.A	Y	Null unless 4.04.4 = 0
          # IncomeBenefits	4.04.5A	NoSCHIPReason	I	4.04.A	Y	Null unless 4.04.5 = 0
          # IncomeBenefits	4.04.6A	NoVHAReason	I	4.04.A	Y	Null unless 4.04.6 = 0
          # IncomeBenefits	4.04.7A	NoEmployerProvidedReason	I	4.04.A	Y	Null unless 4.04.7 = 0
          # IncomeBenefits	4.04.8A	NoCOBRAReason	I	4.04.A	Y	Null unless 4.04.8 = 0
          # IncomeBenefits	4.04.9A	NoPrivatePayReason	I	4.04.A	Y	Null unless 4.04.9 = 0
          # IncomeBenefits	4.04.10A	NoStateHealthInsReason	I	4.04.A	Y	Null unless 4.04.10 = 0
          # IncomeBenefits	4.04.11A	NoIndianHealthServicesReason	I	4.04.A	Y	Null unless 4.04.11 = 0
          # IncomeBenefits	4.04.12A	OtherInsuranceIdentify	S50		Y	Null unless 4.04.12 = 1
          # IncomeBenefits	W3.B	NoADAPReason	I	W3	Y	Null unless W3.3 = 0
          # IncomeBenefits	W3.C	NoRyanWhiteReason	I	W3	Y	Null unless W3.4 = 0
          # HealthAndDV	4.11.A	WhenOccurred	I	4.11.A	Y	Null unless 4.11.2 = 1
          # HealthAndDV	4.11.B	CurrentlyFleeing	I	1.8	Y	Null unless 4.11.2 = 1
          # HealthAndDV	R10.A	DueDate	D		Y	Null unless R10.1 = 1
          # EmploymentEducation	R6.A	EmploymentType	I	R6.A	Y	Null unless R6.2 = 1
          # EmploymentEducation	R6.B	NotEmployedReason	I	R6.B	Y	Null unless R6.2 = 0
          # Disabilities	W4.2 	TCellCountAvailable	I	1.8	Y	Null unless DisabilityType = 8 AND DisabilityResponse=1
          # Disabilities	W4.A	TCellCount	I		Y	Null unless W4.2 = 1
          # Disabilities	W4.B	TCellSource	I	W4.B	Y	Null unless W4.A is not null
          # Disabilities	W4.3 	ViralLoadAvailable	I	W4.3	Y	Null unless DisabilityType = 8 AND DisabilityResponse=1
          # Disabilities	W4.C	ViralLoad	I		Y	Null unless W4.3 = 1
          # Disabilities	W4.D	ViralLoadSource	I	W4.B	Y	Null unless W4.C is not null
          # Disabilities	W6.2 	AntiRetroviral	I	1.8	Y	Null unless DisabilityType = 8 AND DisabilityResponse=1
          # Services	V2.D	OtherTypeProvided	S50		Y	Null unless RecordType = 144 and TypeProvided = 6 Or RecordType = 210 and TypeProvided = 12
          # Services	V8.A	OtherTypeProvided	S50		Y	Null unless RecordType = 144 and TypeProvided = 6 Or RecordType = 210 and TypeProvided = 12
          # Services	C2.A 	MovingOnOtherType	S50		Y	Null unless RecordType = 300 and TypeProvided = 5
          # Services	V2.A	SubTypeProvided	I	(see note)	Y	Null unless RecordType = 144 and TypeProvided = 3, 4, or 5. For TypeProvided: · 3 – list V2.A · 4 – list V2.B · 5 – list V2.C
          # Services	V2.B	SubTypeProvided	I	(see note)	Y	Null unless RecordType = 144 and TypeProvided = 3, 4, or 5. For TypeProvided: · 3 – list V2.A · 4 – list V2.B · 5 – list V2.C
          # Services	V2.C	SubTypeProvided	I	(see note)	Y	Null unless RecordType = 144 and TypeProvided = 3, 4, or 5. For TypeProvided: · 3 – list V2.A · 4 – list V2.B · 5 – list V2.C
          # Services	W2	FAAmount	M		Y	Null unless RecordType = 152 
          # Services	V3.2	FAAmount	M		Y	Null unless RecordType = 152 
          # Services	P2	ReferralOutcome	I	P2.A	Y	Null unless RecordType = 161
          # CurrentLivingSituation	4.12.A	CLSSubsidyType	I	3.12.A	Y	Null unless CurrentLivingSItuation = 435
          # CurrentLivingSituation	4.12.3	VerifiedBy	S200		Y	Null unless ProjectType = 14
          # CurrentLivingSituation	4.12.B 	LeaveSituation14Days	I	1.8	Y	Null unless CurrentLivingSituation in 215, 206, 207, 225, 204, 205, 302, 329, 314, 332, 336, 335, 410, 435, 421, 411
          # CurrentLivingSituation	4.12.C 	SubsequentResidence	I	1.8	Y	Null unless LeaveSituation14Days = 1
          # CurrentLivingSituation	4.12.D 	ResourcesToObtain	I	1.8	Y	Null unless LeaveSituation14Days = 1
          # CurrentLivingSituation	4.12.E 	LeaseOwn60Day	I	1.8	Y	Null unless LeaveSituation14Days = 1
          # CurrentLivingSituation	4.12.F 	MovedTwoOrMore	I	1.8	Y	Null unless LeaveSituation14Days = 1
          # Event	4.20.E	ResultDate	D		Y	Null unless ReferralResult not null
          # YouthEducationStatus	C3.A	MostRecentEdStatus	I	C3.A	Y	Null unless C3.2 = 0
          # YouthEducationStatus	C3.B	CurrentEdStatus	I	C3.B	Y	Null unless C3.2 = 1 or C3.2 = 2
          # Funder	2.06.A	OtherFunder	S100		Y	Required if 2.06.1 = 46
          
          browser()
        } else {
          !(as.character(dt[[col]]) %in% allowed_values[[col]])
        },
        na.rm = TRUE
      )
    }
  })
  
  csv_summary <- data.table(
    CSV = csv_name,
    name = names(invalid_counts),
    n = as.numeric(invalid_counts)
  )
  
  validation_summary[[csv_name]] <- csv_summary
  # 
  # # 4. Check invalids
  # invalid_flags <- lapply(cols, function(col) {
  #   if (col %in% names(dt)) 
  #     !(as.character(dt[[col]]) %in% allowed_values[[col]])
  # })
  # setNames(invalid_flags, cols)
  # 
  # 
  # # 5. Combine flags across columns (any invalid)
  # invalid_any <- do.call(pmax.int, c(invalid_flags, list(na.rm = TRUE))) > 0
  # 
  # # 6. Keep flagged rows
  # if (any(invalid_any)) {
  #   flagged_rows <- dt[invalid_any, ..cols[which(sapply(invalid_flags, any, na.rm=TRUE))]]
  #   flagged_list[[csv_name]] <- flagged_rows
  # }
}
final_summary <- rbindlist(validation_summary) %>%
  fsubset(n > 0)

browser()  
