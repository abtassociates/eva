output$glossary <- renderDataTable({
  gloss <- tribble(
    ~ Focus,
    ~ Term,
    ~ Definition,
    
    "System Performance Filters",
    "Project Type Groups",
    "A single-select universal filter. Eva allows users to filter system performance 
    data by three project type groups: All, Residential Projects, and Non-Residential Projects. ",
    
    "System Flow Chart",
    "Homeless (Active at Start)",
    "This system status indicates a client was actively experiencing homelessness 
    in the system as of the start of the report period. This includes clients 
    who were enrolled in a permanent housing project, but who did not have a Housing 
    Move-In Date before or on the report start date.",
    
    "System Flow Chart",
    "Housed (Active at Start)",
    "This system status indicates a client was actively housed in the system as 
    of the start of the report period. All clients with this status were enrolled 
    in a permanent housing project with a Housing Move-In Date either before or 
    on the report start date.",
    
    "System Performance Filters",
    "Universal Filters",
    "Filters that affect all system performance charts. These filters are Household 
    Type, Level of Detail, Project Type Group, and Gender and Race/Ethnicity Methodology Type.",
    
    "System Performance Filters",
    "Demographic Filters",
    "Filters that only affect the System Flow and Client System Status charts. 
    These filters are Age, Special Populations, Gender, and Race/Ethnicity.",
    
    "System Performance Filters",
    "Household Type",
    "A single-select universal filter. Eva allows users to filter system performance 
    data by three main household types: Adult Only, Adult Child, and Child Only. 
    Household categorization is based on the age of household members on the first 
    day of the report period (or at entry, if later). Eva also allows users to look 
    at subcategories of household types for Youth and Young Adults.
    Households may be counted in more than one household type. For example, a 16-year-old parent 
    with a 1-year-old child would fall into the Child Only and Youth and Young 
    Adult household types.",
    
    "System Performance Filters",
    "All Households",
    "Households of all household types  as well as households that are not categorized  due to missing data.",
    
    "System Performance Filters",
    "Adult Only (AO)",
    "Households in which all members are 18 years old or older.",
    
    "System Performance Filters",
    "Adult Child (AC)",
    "Households in which one or more members are 18 years old or older and one or 
    more members are under 18 years old.",
    
    "System Performance Filters",
    "Child Only (CO)",
    "Households in which all members are under 18 years old.",
    
    "System Performance Filters",
    "Youth and Young Adult",
    "Households in which all members are under 25 years old. This category includes 
    Child Only (CO) households, Adult Only (AO) 18-24 households, and Adult Child 
    (AC) Parenting Youth households.",
    
    "System Performance Filters",
    "Unaccompanied Youth",
    "Adult Only households in which all household members are between 18 and 24.",
    
    "System Performance Filters",
    "Parenting Youth",
    "Adult Child households in which all adults are between 18 and 24 and one or 
    more of the adults is the parent of a child in the household.",
    
    "System Performance Filters",
    "Level of Detail",
    "A single-select universal filter. Eva allows users to filter system performance 
    data by three levels of detail: All People, Heads of Household Only, and Heads 
    of Household and Adults.",
    
    "System Performance Filters",
    "All People",
    "A Level of Detail selection that includes all clients in all households.",
    
    "System Performance Filters",
    "Heads of Household and Adults",
    "A Level of Detail selection that includes all heads of household regardless 
    of age and people 18 years old or older in all households.",
    
    "System Performance Filters",
    "Heads of Household Only",
    "A Level of Detail selection that includes only the heads of households of all 
    households regardless of age.",
    
    "System Performance Filters",
    "Project Type Group",
    "A single-select universal filter. Eva allows users to filter system performance 
    data by three project type groups: All Project Types, Residential Projects, 
    and Non-Residential Projects.",
    
    "System Performance Filters",
    "All Project Types",
    "A Project Type Group selection that includes all clients in all HUD-defined 
    project types. Currently the System Flow, Client System Status, and Composition 
    of All Served charts exclude Homelessness Prevention projects.",
    
    "System Performance Filters",
    "Residential",
    "A Project Type Group selection that only includes clients in residential projects. 
    A residential project provides overnight accommodations and includes projects 
    that are meant to be long-term. Project types that are considered as residential 
    projects include: Emergency Shelter (ES) – Entry/Exit, ES - Night-by-Night, 
    Safe Haven (SH), Transitional Housing (TH), Permanent Housing (PH) - Housing Only, 
    PH - Housing with Services, PH - Permanent Supportive Housing, PH - Rapid Re-housing (RRH): 
    Housing with or without services, and PH - Rapid Re-housing (RRH): Services 
    Only.Clients are only included in ES - Night-by-Night projects if they have a 
    recorded bed night.",
    
    "System Performance Filters",
    "Non-residential",
    "A Project Type Group selection that only includes clients in non-residential 
    projects who have a Current Living Situation record that indicates the client 
    is experiencing literal homelessness. A non-residential project does not provide 
    overnight accommodations. Project types that are considered as non-residential 
    include: Street Outreach, Day Shelter, Supportive Services Only, Coordinated Entry, and Other.",
    
    "System Performance Filters",
    "Gender and Race/Ethnicity Methodology Type",
    "A single-select universal filter. Eva allows users to choose the way Eva organizes 
    system performance data when filtering by gender and race/ethnicity demographics 
    with possible overlapping groupings, or methodology types: Exclusive Groupings 
    or Inclusive Groupings.",
    
    "System Performance Filters",
    "Exclusive Groupings",
    "A Methodology Type selection where each client is counted in only one gender 
    category and only one race/ethnicity category as well as being included in the 
    “All Genders” and “All Races/Ethnicities” selections. For example, a client 
    that reported as (1) Middle Eastern or North African, (2) Black, African American, 
    or African, and (3) Hispanic/Latina/e/o would be counted under “Multi-Racial & 
    Hispanic/Latina/e/o,” but no other race/ethnicity categories.",
    
    "System Performance Filters",
    "Inclusive Groupings",
    "A Methodology Type selection where each client may be counted in multiple gender 
    categories and multiple race/ethnicity categories as well as being included in 
    the “All Genders” and “All Races/Ethnicities” selections. For example, a client 
    that reported as (1) Middle Eastern or North African, (2) Black, African American, 
    or African, and (3) Hispanic/Latina/e/o would be counted under “Middle Eastern 
    or North African inclusive,” “Black, African American, or African inclusive,” 
    and “Hispanic/Latina/e/o inclusive” race/ethnicity categories.",
    
    "System Performance Filters",
    "Age",
    "A multi-select demographic filter. Eva allows users to filter system performance 
    data by age group. Eva calculates age based on a client’s date of birth and 
    their enrollment entry date. This is the client’s “age at enrollment entry,” 
    and the number is rounded down to the whole number (i.e., 17.9 would become 17). 
    For clients with multiple enrollments within the report period, Eva sets 
    the client’s age to be the oldest “age at enrollment entry.” Age groups included 
    are: 0 to 12, 13 to 17, 18 to 21, 22 to 24, 25 to 34, 35 to 44, 45 to 54, 55 
    to 64, 65 to 74, 75 and older, and unknown. Users can select multiple age groups 
    to filter by.",
    
    "System Performance Filters",
    "All Statuses",
    "A Veteran Status selection that includes all clients, including children, 
    regardless of their Veteran Status.",
    
    "System Performance Filters",
    "Veteran Status",
    "Veteran Status of adult clients (not applicable for children) based on the 
    response to HMIS data element 3.07 Veteran Status. There are two Veteran Status 
    options a user can filter by: Veteran and Non-Veteran/Unknown.",
    
    "System Performance Filters",
    "Veteran",
    "This Special Populations status includes only adult clients (18 years or older) 
    who indicated they have been on active duty in the armed forces of the United States.",
    
    "System Performance Filters",
    "Non-Veteran/Unknown",
    "This Special Populations status only includes adult clients (18 years or older) 
    who either (1) indicated that they have not been on active duty in the armed 
    forces of the United States, or (2) whose Veteran Status was recorded as “Client doesn’t know,” 
    “Client prefers not to answer,” or “Data not collected” in HMIS.",
    
    "System Performance Filters",
    "Gender (Exclusive Methodology Type)",
    "A single-select demographic filter. Eva allows users to filter system performance 
    by gender categories. These categories are dependent on the Gender and Race/Ethnicity 
    Methodology Type selection. When the methodology type is exclusive, each person 
    is only counted in one gender category as well as being included in the “All Genders” 
    selection.",
    
    "System Performance Filters",
    "All Genders",
    "Includes all clients, regardless of the gender they identified as in HMIS, 
    as well as people whose gender is categorized as Unknown. This Gender filter 
    selection is available regardless of the Methodology Type selected.",
    
    "System Performance Filters",
    "Gender expansive, not transgender",
    "Includes all clients who identified as Culturally Specific Identity (e.g., Two-Spirit), 
    Different Identity, Non-Binary, or Questioning in HMIS. This includes people 
    that identified as one of those genders alone or in combination with any other 
    gender category, except Transgender. Also included here are clients who identified 
    as both Woman (Girl, if child) and Man (Boy, if child). This Gender filter 
    selection is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Man (Boy, if child) alone",
    "Includes all clients who identified as Man (Boy, if child) and no other gender. 
    This Gender filter selection is only available when the Methodology Type is 
    set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Transgender, alone or in combination",
    "Includes clients who identified as Transgender. This includes clients who 
    identified as Transgender alone or in combination with any other gender categories. 
    This Gender filter selection is only available when the Methodology Type is 
    set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Woman (Girl, if child) alone",
    "Includes all clients who identified as Woman (Girl, if child) and no other 
    gender. This Gender filter selection is only available when the Methodology 
    Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Unknown Gender",
    "Includes all clients who did not identify as a specific gender response and 
    were recorded as client doesn't know, client prefers not to answer, or data 
    was not collected. This option is not available for users to select on its own. 
    Clients with unknown gender are included within the All Genders filter selection.",
    
    "System Performance Filters",
    "Gender (Inclusive Methodology Type)",
    "A single-select demographic filter. Eva allows users to filter system performance 
    by gender categories. The categories are dependent on the Gender and Race/Ethnicity 
    Methodology Type selection. When the methodology type is inclusive, each person 
    may be counted in multiple gender categories.",
    
    "System Performance Filters",
    "Gender expansive, including transgender",
    "Includes all clients who identified as Culturally Specific Identity (e.g., Two-Spirit), 
    Different Identity, Non-Binary, Questioning, or Transgender in HMIS. This includes 
    clients who identified as one of those genders alone or in combination with 
    any other gender. Also included here are clients who identified as both Woman 
    (Girl, if child) and Man (Boy, if child). This Gender filter selection is only 
    available when the Methodology Type is set to Inclusive Groupings.",
    
    "System Performance Filters",
    "Man (Boy, if child) alone or in combination",
    "Includes all clients who identified as Man (Boy, if child) in HMIS. This includes 
    clients who identified as Man (Boy, if child) alone or in combination with any 
    other gender. This Gender filter selection is only available when the Methodology 
    Type is set to Inclusive Groupings.",
    
    "System Performance Filters",
    "Non-Binary alone or in combination",
    "Includes all clients who identified as Non-Binary in HMIS. This includes clients 
    who identified as Non-Binary alone or in combination with any other gender. 
    This Gender filter selection is only available when the Methodology Type is 
    set to Inclusive Groupings.",
    
    "System Performance Filters",
    "Only Woman (Girl, if child) OR Only Man (Boy, if child)",
    "Includes all clients who identified as Woman (Girl, if child) or Man (Boy, if child) 
    in HMIS and no other genders. This Gender filter selection is only available 
    when the Methodology Type is set to Inclusive Groupings.",
    
    "System Performance Filters",
    "Woman (Girl, if child) alone or in combination",
    "Includes all clients who identified as Woman (Girl, if child) in HMIS. 
    This includes clients who identified as Woman (Girl, if child) alone or in 
    combination with any other gender. This Gender filter selection is only available 
    when the Methodology Type is set to Inclusive Groupings.",
    
    "System Performance Filters",
    "Race/Ethnicity (Exclusive Methodology Type)",
    "A single-select demographic filter. Eva allows users to filter system performance 
    by race/ethnicity categories. These categories are dependent on the Gender and 
    Race/Ethnicity Methodology Type selection. This filter has two sections: Detailed 
    and Summarized. Each section has different race/ethnicity categories for users 
    to choose from. The Detailed section lists all race/ethnicity categories on 
    their own and in combination with Hispanic/Latina/e/o. This section also includes 
    two multi-racial categories. The Summarized section groups individual race/ethnicity 
    categories together to make two options: All People of Color and White Only. 
    When the methodology type is exclusive, each client is only counted in one 
    race/ethnicity category in the Detailed section and one race/ethnicity category 
    in the Summarized section. All clients are also included in the All Races/Ethnicities category.",
    
    "System Performance Filters",
    "All Races/Ethnicities",
    "Includes all clients, regardless of the race/ethnicity they identified as in HIMS, 
    as well as clients whose race/ethnicity is categorized as Unknown. This Race/Ethnicity 
    filter selection is available regardless of the Methodology Type selected.",
    
    "System Performance Filters",
    "American Indian, Alaska Native, or Indigenous alone",
    "Includes clients who identified as only American Indian, Alaska Native, or Indigenous. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "American Indian, Alaska Native, or Indigenous & Hispanic/Latina/e/o",
    "Includes clients who identified as both American Indian, Alaska Native, or Indigenous and as Hispanic/Latina/e/o.This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Asian or Asian American alone",
    "Includes clients who identified as only Asian or Asian American. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Asian or Asian American & Hispanic/Latina/e/o",
    "Includes clients who identified as both Asian or Asian American and Hispanic/Latina/e/o. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Black, African American, or African alone",
    "Includes clients who identified as only Black, African American, or African. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Black, African American, or African & Hispanic/Latina/e/o",
    "Includes clients who identified as both Black, African American, or African and Hispanic/Latina/e/o. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Middle Eastern or North African alone",
    "Includes clients who only selected Middle Eastern or North African and no other race/ethnicity category. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Middle Eastern or North African & Hispanic/Latina/e/o",
    "Includes clients who identified as both Middle Eastern or North African and Hispanic/Latina/e/o. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Native Hawaiian or Pacific Islander alone",
    "Includes clients who identified as only Native Hawaiian or Pacific Islander. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Native Hawaiian or Pacific Islander & Hispanic/Latina/e/o",
    "Includes clients who identified as both Native Hawaiian or Pacific Islander and Hispanic/Latina/e/o. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "White alone",
    "Includes clients who identified as only White. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "White & Hispanic/Latina/e/o",
    "Includes clients who identified as both White and Hispanic/Latina/e/o. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Multi-Racial (not Hispanic/Latina/e/o)",
    "Includes clients who identified as multiple races (2+) but not as Hispanic/Latina/e/o. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Multi-Racial & Hispanic/Latina/e/o",
    "Includes clients who identified as Hispanic/Latina/e/o together with two or more other races/ethnicities. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "All People of Color",
    "Includes clients who identified as a race or ethnicity other than White. This Race/Ethnicity filter selection is listed under the Summarized section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "White Only",
    "Includes clients who identified as White and no other race/ethnicity. This Race/Ethnicity filter selection is listed under the Summarized section of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.",
    
    "System Performance Filters",
    "Race/Ethnicity (Inclusive Methodology Type)",
    "A single-select demographic filter. Eva allows users to filter system performance by race/ethnicity categories. These categories are dependent on the Gender and Race/Ethnicity Methodology Type selection. This filter has two sections: Detailed and Summarized. Each section has different race/ethnicity categories for users to choose from. The Detailed section lists all race/ethnicity categories on their own. The Summarized section groups race/ethnicity categories together into three options: Black, African American or African and Hispanic/Latina/e/o Inclusive, Hispanic Latina/e/o Alone. When the methodology type is inclusive, each client may be counted in multiple race/ethnicity categories in the Detailed section and in multiple race/ethnicity categories in the Summarized section. All clients are also included in the All Races/Ethnicities category.",
    
    "System Performance Filters",
    "American Indian, Alaska Native, or Indigenous inclusive",
    "Includes clients who identified as American Indian, Alaska Native, or Indigenous alone or in combination with any other race/ethnicity. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Inclusive Groupings.",
    
    "System Performance Filters",
    "Asian or Asian American inclusive",
    "Includes clients who identified as Asian or Asian American alone or in combination with any other race/ethnicity. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Inclusive Groupings.", 
    
    "System Performance Filters",
    "Black, African American, or African inclusive",
    "Includes clients who identified as Black, African American, or African alone or in combination with any other race/ethnicity. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Inclusive Groupings.", 
    
    "System Performance Filters",
    "Black, African American or African and Hispanic/Latina/e/o inclusive",
    "Includes clients who identified as both Black, African American or African and Hispanic/Latina/e/o together or in combination with any other race/ethnicity. Listed under the Summarized section. This Race/Ethnicity filter selection is listed under the Summarized section of the filter dropdown list and is only available when the Methodology Type is set to Inclusive Groupings.", 
    
    "System Performance Filters",
    "Hispanic/Latina/e/o alone",
    "Includes clients who identified as only Hispanic/Latina/e/o and no other race/ethnicity. This Race/Ethnicity filter selection is listed under both the Detailed and Summarized sections of the filter dropdown list and is only available when the Methodology Type is set to Exclusive Groupings.", 
    
    "System Performance Filters",
    "Hispanic Latina/e/o inclusive",
    "Includes clients who identified as Hispanic/Latina/e/o alone or in combination with any other race/ethnicity. This Race/Ethnicity filter selection is listed under the Summarized section of the filter dropdown list and is only available when the Methodology Type is set to Inclusive Groupings.", 
    
    "System Performance Filters",
    "Middle Eastern or North African inclusive",
    "Includes clients who identified as Middle Eastern or North African alone or in combination with any other race/ethnicity. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Inclusive Groupings.", 
    
    "System Performance Filters",
    "Native Hawaiian or Pacific Islander inclusive",
    "Includes clients who identified as Native Hawaiian or Pacific Islander alone or in combination with any other race/ethnicity. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Inclusive Groupings.", 
    
    "System Performance Filters",
    "White inclusive",
    "Includes clients who identified as White alone or in combination with any other race/ethnicity. This Race/Ethnicity filter selection is listed under the Detailed section of the filter dropdown list and is only available when the Methodology Type is set to Inclusive Groupings.",
    
    "System Flow Chart",
    "Total Change",
    "Total Change is calculated by subtracting the number of clients who flowed into the system from the number of clients who flowed out of the system (Inflow – Outflow). This value can be positive or negative. A negative change value means more clients left the system than flowed into the system. A positive change value means more clients flowed into the system than left the system.",
    
    "System Flow Chart",
    "Homeless (Active at Start)",
    "This system status indicates a client was actively experiencing homelessness in the system as of the start of the report period. This includes clients who were enrolled in: (1) Emergency Shelter – Entry/Exit, Safe Haven, or Transitional Housing projects, (2) Emergency Shelter – Night-by-Night projects who have a recorded bed night within the 15-day period before the report start date, (3)	Street Outreach, Supportive Services Only, Day Shelter or Other type projects who have a Current Living Situation recorded within the 60-day period before the report start date, (4) Permanent Housing projects, either without a Housing Move-In Date or with a Housing Move-In Date after the report start date, or (5) Coordinated Entry projects who have a Current Living Situation recorded within the 90-day period before the report start date.",
    
    "System Flow Chart",
    "Housed (Active at Start)",
    "This system status indicates a client was actively housed in the system as of the start of the report period. All clients with this status were enrolled in a permanent housing project with a Housing Move-In Date before the report start date.",
    
    "System Flow Chart",
    "Inflow",
    "The number of clients that entered or flowed into the system. This status indicates a client entered a system project after the report period’s start date. This status excludes all clients who were counted as homeless or housed at the start of the report period.",
    
    "System Flow Chart",
    "First-Time Homeless",
    "This inflow system status indicates a client who entered the system after the report period’s start date and who also had not been served in the system within the 24 months prior to their entry into the report period. This inflow system status is only available when a dataset with 36 months of data is uploaded to Eva.",
    
    "System Flow Chart",
    "Returned from Permanent",
    "This inflow system status indicates a client who entered the system after the report period’s start date and who had a previous exit to a permanent destination within the 24 months prior to their entry.",
    
    "System Flow Chart",
    "Re-engaged form Non-Permanent",
    "This inflow system status indicates a client who entered the system after the report period’s start date and who had a previous exit to a homeless, temporary, institutional destination or unknown destination within the 24 months prior to their entry.",
    
    "System Flow Chart",
    "Inflow Unspecified",
    "This system status indicates a client who entered the system after the report start date, but it cannot be determined if they are newly homeless because there is not enough lookback data. This inflow system status takes the place of First-Time Homeless in cases where less than 36 months of data are uploaded to Eva.",
    
    "System Flow Chart",
    "Outflow",
    "The number of clients that left or flowed out of the system. This status indicates a client exited a system project after the report period’s start date and before the report period’s end date. A client cannot be counted in both outflow and active at end.",
    
    "System Flow Chart",
    "Exited, Non-Permanent",
    "This outflow system status indicates a client exited the system to a homeless, temporary, institutional, or unknown destination as defined in the HMIS Data Standards. Only the client’s last exit is counted.",
    
    "System Flow Chart",
    "Exited, Permanent",
    "This outflow system status indicates a client’s last system exit was to a permanent destination.", 
    
    "System Flow Chart",
    "Inactive (Outflow)",
    "A client is counted in inactive outflow if they ended the report period with (1) an open enrollment in an Emergency Shelter – Night-by-Night project that has not had a bed night recorded within the last 15 days of the report period, (2) an open enrollment in Street Outreach, Day Shelter, Supportive Services, and Other project type enrollments without a Current Living Situation (CLS) record within the last 60 days of the report period, or (3) an open enrollment in Coordinated Entry without a CLS record within the last 90 days of the report period.", 
    
    "System Flow Chart",
    "Homeless (Active at End)",
    "This system status indicates a client was actively experiencing homelessness in the system at the end of the report period. This includes clients who were enrolled in: (1) Emergency Shelter – Entry/Exit, Safe Haven, or Transitional Housing projects, (2) Emergency Shelter – Night-by-Night projects who have a recorded bed night within the 15-day period before the report end date, (3) Street Outreach, Supportive Services Only, Day Shelter or Other type projects who have a Current Living Situation recorded within the 60-day period before the report end date, or (4) Permanent Housing projects, either without a Housing Move-In Date or with a Housing Move-In Date after the report end date. Coordinated Entry projects who have a Current Living Situation recorded within the 90-day period before the report end date.",
    
    "System Flow Chart",
    "Housed (Active at End)",
    "This system status indicates a client was actively housed in the system at the end of the report period. All clients with this status were enrolled in a Permanent Housing project with a Housing Move-In Date before the report end date.",
    
    "Client System Status Chart",
    "Homeless (Period Start)",
    "This system status indicates a client was actively experiencing homelessness in the system as of the start of the report period. This includes clients who were enrolled in: (1) Emergency Shelter – Entry/Exit, Safe Haven, or Transitional Housing projects, (2) Emergency Shelter – Night-by-Night projects who have a recorded bed night within the 15-day period before the report start date, (3)	Street Outreach, Supportive Services Only, Day Shelter or Other type projects who have a Current Living Situation recorded within the 60-day period before the report start date, (4) Permanent Housing projects, either without a Housing Move-In Date or with a Housing Move-In Date after the report start date, or (5) Coordinated Entry projects who have a Current Living Situation recorded within the 90-day period before the report start date.",
    
    "Client System Status",
    "Housed (Period Start)",
    "This system status indicates a client was actively housed in the system as of the start of the report period. All clients with this status were enrolled in a permanent housing project with a Housing Move-In Date before the report start date.",
    
    "Client System Status",
    "Exited, Non-Permanent",
    "This status indicates that the client’s last exit in the report period was to a homeless, temporary, institutional, or unknown destination.",
    
    "Client System Status",
    "Exited, Permanent",
    "This status indicates that the client’s last exit in the report period was to a permanent destination.",
    
    "Client System Status",
    "Enrolled, Homeless",
    "This system status indicates a client was actively experiencing homelessness in the system at the end of the report period. This includes clients who were enrolled in: (1) Emergency Shelter – Entry/Exit, Safe Haven, or Transitional Housing projects, (2) Emergency Shelter – Night-by-Night projects who have a recorded bed night within the 15-day period before the report end date, (3) Street Outreach, Supportive Services Only, Day Shelter or Other type projects who have a Current Living Situation recorded within the 60-day period before the report end date, or (4) Permanent Housing projects, either without a Housing Move-In Date or with a Housing Move-In Date after the report end date. Coordinated Entry projects who have a Current Living Situation recorded within the 90-day period before the report end date.",
    
    "Client System Status",
    "Enrolled, Housed",
    "This system status indicates a client was actively housed in the system at the end of the report period. All clients with this status were enrolled in a Permanent Housing project with a Housing Move-In Date before the report end date.",
    
    "Client System Status",
    "Inactive (Period End)",
    "A client is counted as inactive at Period End if they ended the report period with (1) an open enrollment in an Emergency Shelter – Night-by-Night project that has not had a bed night recorded within the last 15 days of the report period, (2) an open enrollment in Street Outreach, Day Shelter, Supportive Services, and Other project type enrollments without a Current Living Situation (CLS) record within the last 60 days of the report period, or (3) an open enrollment in Coordinated Entry without a CLS record within the last 90 days of the report period.  "
    
)
  
  datatable(
    gloss,
    rownames = FALSE,
    options = list(
      searchHighlight = TRUE,
      order = list(list(0, 'asc'), list(1, 'asc'))
    )
  )
  
})