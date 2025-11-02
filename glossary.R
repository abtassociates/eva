output$glossary <- renderDT({
  gloss <- tribble(
    ~ Focus,
    ~ Term,
    ~ Definition,
    
    "System Performance Filters",
    "Project Type Groups",
    "A single-select universal filter. Eva allows users to filter system performance 
    data by three project type groups: All, Residential Projects, and
    Non-Residential Projects.",
    
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
    Type, Level of Detail, Project Type Group, and Race/Ethnicity
    Methodology Type.",
    
    "System Performance Filters",
    "Demographic Filters",
    "Filters that only affect the System Flow and Client System Status charts. 
    These filters are Age, Veteran Status, and Race/Ethnicity.",
    
    "System Performance Filters",
    "Household Type",
    "A single-select universal filter that allows users to filter system performance 
    data by three main household types: Adult Only, Adult Child, and Child Only. 
    Household categorization is based on the age of household members on the first day 
    of the report period (or at entry, if later). Eva also allows users to look at 
    subcategories of household types for Youth and Young Adults. Households may be counted 
    in more than one household type. For example, a 16-year-old parent with a 1-year-old 
    child would fall into the Child Only and Youth and Young Adult household types. Eva 
    system performance metrics and statuses reported by household type are calculated 
    based on project enrollments associated with the respective household type.",
    
    "System Performance Filters",
    "All Households",
    "Households of all household types  as well as households that are not
    categorized  due to missing data.",
    
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
    (AC) Parenting Young Adult households.",
    
    "System Performance Filters",
    "Adult Only (AO) 18-24",
    "Adult Only households in which all household members are between 18 and 24.",
    
    "System Performance Filters",
    "Parenting Young Adult",
    "Adult Child households in which all adults are between 18 and 24 and one or 
    more of the adults is the parent of a child in the household.",
    
    "System Performance Filters",
    "Level of Detail",
    "A single-select universal filter that allows users to filter system performance 
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
    "A single-select universal filter that allows users to filter system performance 
    data by six project type groups: All Project Types, All Residential Projects, Residential: 
    Homeless Projects, Residential: Permanent Housing Projects, All Non-Residential Projects, 
    and Non-Residential: Street Outreach Projects. Eva system performance metrics and statuses 
    reported by Project Type Group are calculated based on project enrollments associated with 
    the respective project types within the selected Project Type Group. This means it is 
    possible for different Project Type Group selections to show different System Performance 
    results for the same client depending on the types of projects the client was enrolled 
    in during the report period.",
    
    "System Performance Filters",
    "All Project Types",
    "A Project Type Group selection that includes all clients in all HUD-defined 
    project types, with the exclusion of Homelessness Prevention projects. This is 
    the default selection provides for the Project Type Group filter and provides
    the most complete picture of system engagement for households who were enrolled in both 
    Residential and Non-Residential projects during the report period.",
    
    "System Performance Filters",
    "All Residential Projects",
    "A Project Type Group selection that only includes clients in projects that provide overnight 
    accommodation. Project types that are considered as residential projects include: Emergency 
    Shelter (ES) – Entry/Exit, ES - Night-by-Night, Safe Haven (SH), Transitional Housing (TH), 
    Permanent Housing (PH) - Housing Only, PH - Housing with Services, PH - Permanent Supportive Housing, 
    PH - Rapid Re-housing (RRH): Housing with or without services, and PH - Rapid Re-housing (RRH): Services Only. 
    Clients enrolled in residential project types are considered to be experiencing homelessness 
    as of the enrollment’s Project Start Date. A client enrolled in a permanent housing project 
    who has moved into housing (i.e., has a Move-In Date) would no longer be considered as experiencing 
    homelessness. A Move-In Date that is after the Project Exit Date is considered a data quality issue, 
    and that Move-In Date is ignored.",

    "System Performance Filters",
    "Residential: Homeless Projects",
    "A Project Type Group selection that only includes clients in a specific subset of residential 
    projects: Emergency Shelter (ES) – Entry/Exit, ES - Night-by-Night, Safe Haven (SH), and 
    Transitional Housing (TH). Clients enrolled in residential homeless project types are 
    considered to be experiencing homelessness as of the enrollment’s Project Start Date. 
    For the purposes of System Performance, for ES – Night-by-Night enrollments without Exit 
    Dates, Eva sets the Exit Date to be 15 days after the last recorded Bed Night or 15 days 
    after the Entry Date, whichever is later.",

    "System Performance Filters",
    "Residential: Permanent Housing Projects",
    "A Project Type Group selection that only includes clients in a specific subset of residential 
    projects: Permanent Housing (PH) - Housing Only, PH - Housing with Services, PH - Permanent 
    Supportive Housing, PH - Rapid Re-housing (RRH): Housing with or without services, and 
    PH - Rapid Re-housing (RRH): Services Only. Clients enrolled in residential project types are 
    considered to be experiencing homelessness as of the enrollment’s Project Start Date. A client 
    enrolled in a permanent housing project who has moved into housing (i.e., has a move-in date) 
    would no longer be considered as experiencing homelessness. A Move-In Date that is after the 
    Project Exit Date is considered a data quality issue, and that Move-In Date is ignored.",
    
    "System Performance Filters",
    "All Non-Residential Projects",
    "A Project Type Group selection that only includes clients in projects that do not provide overnight 
    accommodation. Project types that are considered as non-residential include: Street Outreach, 
    Day Shelter, Supportive Services Only, Coordinated Entry, and Other. Since non-residential projects 
    can serve both people who are experiencing homelessness and those who are housed, non-residential 
    enrollments must have a documented homeless living situation in the Prior Living Situation (PLS) 
    and/or a Current Living Situation (CLS) record to be included in Eva’s system performance calculations. 
    For the purposes of System Performance, clients are considered active in long-term, non-residential 
    project enrollments if there is a homeless living situation documented in a CLS record every 60 days. 
    Coordinated Entry enrollments require a CLS record every 90 days to be considered active. Additionally, 
    non-residential enrollments are considered active 15 days prior to the enrollment exit date if the exit 
    destination is to a homeless, institutional, temporary housing, or permanent housing destination. 
    For all non-residential project enrollments except for Street Outreach, Eva “resets” the enrollment 
    Entry Date to the date of the enrollment’s first literally homeless CLS record if the enrollment does 
    not have a literally homeless PLS. Additionally, for any non-residential enrollment without an Exit Date, 
    Eva sets the Exit Date to be 60 days after the last literally homeless CLS recorded for the enrollment 
    or after the literally homeless PLS, whichever is later. The exception is Coordinated Entry, which uses 90 days.",

    "System Performance Filters",
    "Non-Residential: Street Outreach Projects",
    "A Project Type Group selection that only includes clients in Street Outreach projects. 
    Clients enrolled in Street Outreach are assumed to be experiencing homelessness as of 
    the enrollment’s Project Start Date. For Eva to consider clients active in long-term, 
    Street Outreach enrollments, there must be a homeless living situation documented in a 
    Current Living Situation (CLS) record every 60 days. Additionally, inactive Street Outreach 
    enrollments are considered active 15 days prior to the enrollment exit date if the exit 
    destination is to a homeless, institutional, temporary housing, or permanent housing 
    destination. Clients enrolled in Street Outreach for less than 60 days are considered 
    to be experiencing homelessness for the entire duration of that enrollment, even if 
    there was no CLS record entered within that time period. For any Street Outreach enrollment 
    without an Exit Date, Eva sets the Exit Date to be 60 days after the last literally homeless 
    CLS recorded for the enrollment or after the literally homeless PLS, whichever is later.",
    
    "System Performance Filters",
    "Race/Ethnicity Methodology Type",
    "A single-select universal filter. Eva allows users to choose the way Eva
    organizes system performance data when filtering by race/ethnicity
    demographics with possible overlapping groupings, or methodology types:
    Method 1 (clients are only counted in one category) or Method 2 (clients can
    be counted in more than one category).",
    
    "System Performance Filters",
    "Method 1",
    "A Methodology Type selection where each client is counted in only one 
    race/ethnicity category as well as being included in the 
    \"All Races/Ethnicities\" selections. For example, a client 
    that reported as (1) Middle Eastern or North African, (2) Black, African
    American, or African, and (3) Hispanic/Latina/o would be counted under
    \"Multi-Racial & Hispanic/Latina/o,\" but no other race/ethnicity categories.",
    
    "System Performance Filters",
    "Method 2",
    "A Methodology Type selection where each client may be counted in multiple 
    race/ethnicity categories as well as being included in 
    the \"All Races/Ethnicities\" selections. For example, a client 
    that reported as (1) Middle Eastern or North African, (2) Black, African American, 
    or African, and (3) Hispanic/Latina/o would be counted under \"Middle Eastern 
    or North African,\" \"Black, African American, or African,\" 
    and \"Hispanic/Latina/o\" race/ethnicity categories.",
    
    "System Performance Filters",
    "Age",
    "A multi-select demographic filter. Eva allows users to filter system performance 
    data by age group. Age group is determined based on the client’s age as of the entry 
    date of their last enrollment included in the report period. Age groups included 
    are: 0 to 12, 13 to 17, 18 to 21, 22 to 24, 25 to 34, 35 to 44, 45 to 54, 55 
    to 64, 65 to 74, 75 and older, and unknown.",
    
    "System Performance Filters",
    "All Statuses",
    "A Veteran Status selection that includes all clients, including children, 
    regardless of their Veteran Status.",
    
    "System Performance Filters",
    "Veteran Status (Adult Only)",
    "Veteran Status of adult clients (not applicable for children) based on the 
    response to HMIS data element 3.07 Veteran Status. There are two Veteran Status 
    options a user can filter by: Veteran and Non-Veteran/Unknown.",
    
    "System Performance Filters",
    "Veteran",
    "This Veteran Status includes only adult clients (18 years or older) 
    who indicated they have been on active duty in the armed forces of the United States.",
    
    "System Performance Filters",
    "Non-Veteran/Unknown",
    "This Veteran Status only includes adult clients (18 years or older) 
    who either (1) indicated that they have not been on active duty in the armed 
    forces of the United States, or (2) whose Veteran Status was recorded as
    \"Client doesn’t know\", \"Client prefers not to answer\", or \"Data not
    collected\" in HMIS.",
    
    "System Performance Filters",
    "Race/Ethnicity (Method 1)",
    "A single-select demographic filter. Eva allows users to filter system performance 
    by race/ethnicity categories. These categories are dependent on the 
    Race/Ethnicity Methodology Type selection. This filter has two sections: Detailed 
    and Summarized. Each section has different race/ethnicity categories for users 
    to choose from. The Detailed section lists all race/ethnicity categories on 
    their own and in combination with Hispanic/Latina/o. This section also includes 
    two multi-racial categories. The Summarized section groups individual race/ethnicity 
    categories together to make two options: All People of Color and White Only. 
    When Method 1 is selected, each client is only counted in one race/ethnicity
    category in the Detailed section and one race/ethnicity category in the
    Summarized section. All clients are also included in the All Races/Ethnicities
    category.",
    
    "System Performance Filters",
    "All Races/Ethnicities",
    "Includes all clients, regardless of the race/ethnicity they identified as in HMIS, 
    as well as clients whose race/ethnicity is categorized as Unknown. This
    Race/Ethnicity filter selection is available regardless of the Method
    selected.",
    
    "System Performance Filters",
    "American Indian, Alaska Native, or Indigenous alone",
    "Includes clients who identified as only American Indian, Alaska Native, or
    Indigenous. This Race/Ethnicity filter selection is listed under the Detailed
    section of the filter dropdown list and is only available when Method 1 is
    selected.",
    
    "System Performance Filters",
    "American Indian, Alaska Native, or Indigenous & Hispanic/Latina/o",
    "Includes clients who identified as both American Indian, Alaska Native, or
    Indigenous and as Hispanic/Latina/o.This Race/Ethnicity filter selection
    is listed under the Detailed section of the filter dropdown list and is only
    available when Method 1 is selected.",
    
    "System Performance Filters",
    "Asian or Asian American alone",
    "Includes clients who identified as only Asian or Asian American. This
    Race/Ethnicity filter selection is listed under the Detailed section of the
    filter dropdown list and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "Asian or Asian American & Hispanic/Latina/o",
    "Includes clients who identified as both Asian or Asian American and
    Hispanic/Latina/o. This Race/Ethnicity filter selection is listed under the
    Detailed section of the filter dropdown list and is only available when
    Method 1 is selected.",
    
    "System Performance Filters",
    "Black, African American, or African alone",
    "Includes clients who identified as only Black, African American, or African.
    This Race/Ethnicity filter selection is listed under the Detailed section of
    the filter dropdown list and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "Black, African American, or African & Hispanic/Latina/o",
    "Includes clients who identified as both Black, African American, or African
    and Hispanic/Latina/o. This Race/Ethnicity filter selection is listed under
    the Detailed section of the filter dropdown list and is only available when
    Method 1 is selected.",
    
    "System Performance Filters",
    "Middle Eastern or North African alone",
    "Includes clients who only selected Middle Eastern or North African and no
    other race/ethnicity category. This Race/Ethnicity filter selection is listed
    under the Detailed section of the filter dropdown list and is only available
    when Method 1 is selected.",
    
    "System Performance Filters",
    "Middle Eastern or North African & Hispanic/Latina/o",
    "Includes clients who identified as both Middle Eastern or North African and 
    Hispanic/Latina/o. This Race/Ethnicity filter selection is listed under the
    Detailed section of the filter dropdown list and is only available when
    Method 1 is selected.",
    
    "System Performance Filters",
    "Native Hawaiian or Pacific Islander alone",
    "Includes clients who identified as only Native Hawaiian or Pacific Islander.
    This Race/Ethnicity filter selection is listed under the Detailed section of
    the filter dropdown list and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "Native Hawaiian or Pacific Islander & Hispanic/Latina/o",
    "Includes clients who identified as both Native Hawaiian or Pacific Islander
    and Hispanic/Latina/o. This Race/Ethnicity filter selection is listed under
    the Detailed section of the filter dropdown list and is only available when
    Method 1 is selected.",
    
    "System Performance Filters",
    "White alone",
    "Includes clients who identified as only White. This Race/Ethnicity filter
    selection is listed under the Detailed section of the filter dropdown list
    and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "White & Hispanic/Latina/o",
    "Includes clients who identified as both White and Hispanic/Latina/o. This
    Race/Ethnicity filter selection is listed under the Detailed section of the
    filter dropdown list and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "Multi-Racial (not Hispanic/Latina/o)",
    "Includes clients who identified as multiple races (2+) but not as
    Hispanic/Latina/o. This Race/Ethnicity filter selection is listed under the
    Detailed section of the filter dropdown list and is only available when
    Method 1 is selected.",
    
    "System Performance Filters",
    "Multi-Racial & Hispanic/Latina/o",
    "Includes clients who identified as Hispanic/Latina/o together with two or
    more other races/ethnicities. This Race/Ethnicity filter selection is listed
    under the Detailed section of the filter dropdown list and is only available
    when Method 1 is selected.",
    
    "System Performance Filters",
    "All People of Color",
    "Includes clients who identified as a race or ethnicity other than White.
    This Race/Ethnicity filter selection is listed under the Summarized section
    of the filter dropdown list and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "White Only",
    "Includes clients who identified as White and no other race/ethnicity. This
    Race/Ethnicity filter selection is listed under the Summarized section of
    the filter dropdown list and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "Race/Ethnicity (Method 2)",
    "A single-select demographic filter. Eva allows users to filter system
    performance by race/ethnicity categories. These categories are dependent on
    the Race/Ethnicity Methodology Type selection. This filter has
    two sections: Detailed and Summarized. Each section has different
    race/ethnicity categories for users to choose from. The Detailed section
    lists all race/ethnicity categories on their own. The Summarized section
    groups race/ethnicity categories together into three options: Black, African
    American or African and Hispanic/Latina/o Method 2, Hispanic Latina/o
    Alone. When the Method 2 is selected, each client may be counted in
    multiple race/ethnicity categories in the Detailed section and in multiple
    race/ethnicity categories in the Summarized section. All clients are also
    included in the All Races/Ethnicities category.",
    
    "System Performance Filters",
    "American Indian, Alaska Native, or Indigenous Method 2",
    "Includes clients who identified as American Indian, Alaska Native, or
    Indigenous alone or in combination with any other race/ethnicity. This
    Race/Ethnicity filter selection is listed under the Detailed section of the
    filter dropdown list and is only available when Method 2 is selected.",
    
    "System Performance Filters",
    "Asian or Asian American Method 2",
    "Includes clients who identified as Asian or Asian American alone or in
    combination with any other race/ethnicity. This Race/Ethnicity filter
    selection is listed under the Detailed section of the filter dropdown list
    and is only available when Method 2 is selected.", 
    
    "System Performance Filters",
    "Black, African American, or African Method 2",
    "Includes clients who identified as Black, African American, or African
    alone or in combination with any other race/ethnicity. This Race/Ethnicity
    filter selection is listed under the Detailed section of the filter dropdown
    list and is only available when Method 2 is selected.", 
    
    "System Performance Filters",
    "Black, African American or African and Hispanic/Latina/o Method 2",
    "Includes clients who identified as both Black, African American or African
    and Hispanic/Latina/o together or in combination with any other race/ethnicity.
    Listed under the Summarized section. This Race/Ethnicity filter selection
    is listed under the Summarized section of the filter dropdown list and is
    only available when Method 2 is selected.", 
    
    "System Performance Filters",
    "Hispanic/Latina/o alone",
    "Includes clients who identified as only Hispanic/Latina/o and no other
    race/ethnicity. This Race/Ethnicity filter selection is listed under both 
    the Detailed and Summarized sections of the filter dropdown list and is only
    available when Method 1 is selected.", 
    
    "System Performance Filters",
    "Hispanic Latina/o Method 2",
    "Includes clients who identified as Hispanic/Latina/o alone or in
    combination with any other race/ethnicity. This Race/Ethnicity filter
    selection is listed under the Summarized section of the filter dropdown
    list and is only available when Method 2 is selected.", 
    
    "System Performance Filters",
    "Middle Eastern or North African Method 2",
    "Includes clients who identified as Middle Eastern or North African alone or
    in combination with any other race/ethnicity. This Race/Ethnicity filter
    selection is listed under the Detailed section of the filter dropdown list
    and is only available when Method 2 is selected.", 
    
    "System Performance Filters",
    "Native Hawaiian or Pacific Islander Method 2",
    "Includes clients who identified as Native Hawaiian or Pacific Islander alone
    or in combination with any other race/ethnicity. This Race/Ethnicity filter
    selection is listed under the Detailed section of the filter dropdown list
    and is only available when Method 2 is selected.", 
    
    "System Performance Filters",
    "White Method 2",
    "Includes clients who identified as White alone or in combination with any
    other race/ethnicity. This Race/Ethnicity filter selection is listed under
    the Detailed section of the filter dropdown list and is only available when
    Method 2 is selected.",
    
    "System Flow Chart",
    "Total Change",
    "Total Change is calculated by subtracting the number of clients who flowed out 
    of the system from the number of clients who flowed into the system (Inflow – Outflow). 
    This value can be positive or negative. A negative change value means more clients left 
    the system than flowed into the system. A positive change value means more clients 
    flowed into the system than left the system. Used in the System Flow Summary and Detail charts.",

    "System Flow Chart",
    "Monthly Change",
    "Monthly Change is calculated by subtracting the number of clients who flowed out of the 
    system from the number of clients who flowed into the system for a given month (Inflow – Outflow). 
    This value can be positive or negative. A negative change value means more clients left the 
    system than flowed into the system for that month. A positive change value means more clients 
    flowed into the system than left the system. Used in the System Flow Month-by-Month chart.",

    "System Flow Chart",
    "Average Monthly Inflow",
    "Average Monthly Inflow is calculated by summing all monthly Inflow values and dividing by 12.
    Used in the System Flow Month-by-Month chart.",

    "System Flow Chart",
    "Average Monthly Outflow",
    "Average Monthly Outflow is calculated by summing all monthly Outflow values and dividing by 12.
    Used in the System Flow Month-by-Month chart.",

    "System Flow Chart",
    "Average Monthly Change",
    "Average Monthly Change is calculated by summing all Monthly Change values and dividing by 12. 
    This value can be positive or negative. A negative change value means more clients left the 
    system than flowed into the system for that month. A positive change value means more clients 
    flowed into the system than left the system. Used in the System Flow Month-by-Month chart.",
    
    "System Flow Chart",
    "Homeless (Active at Start)",
    "This system status indicates a client was actively experiencing homelessness as 
    of the start of the report period. This includes clients who were enrolled in: 
    (1) Emergency Shelter – Entry/Exit, Safe Haven, or Transitional Housing projects, 
    (2) Emergency Shelter – Night-by-Night projects with a Project Start Date or 
    recorded bed night within the 15-day period prior to the report start date, 
    (3) Street Outreach projects with a Project Start Date or documented homeless 
    living situation in a Current Living Situation record within the 60-day period 
    prior to the report start date, (4) Supportive Services Only, Day Shelter or Other 
    type projects with a documented homeless living situation in the Prior Living Situation 
    or a Current Living Situation recorded within the 60-day period prior to the report 
    start date, (5) Permanent Housing projects, either without a Housing Move-In Date 
    or with a Housing Move-In Date on or after the report start date, (6) Coordinated 
    Entry projects with a documented homeless living situation in the Prior Living 
    Situation or Current Living Situation recorded prior to the 90-day period before 
    the report start date. Additionally, in the System Flow Summary and Detail charts, 
    clients may be considered experiencing homelessness on the first day of the report 
    if they have an exit right before the report start date that does not qualify 
    as a “system exit” due to a subsequent enrollment entry in the 14 days following the exit.",
    
    "System Flow Chart",
    "Housed (Active at Start)",
    "This system status indicates a client was actively housed in the system as of 
    the start of the report period. All clients with this status were enrolled in a 
    permanent housing (PH) project with a Housing Move-In Date before the report start 
    date. Additionally, clients may be considered housed as of the start of the report 
    period if they have an exit to a permanent destination from a PH project right 
    before the report start date that does not qualify as a “system exit” due to a 
    subsequent enrollment in a PH project in the 14 days following the exit. The 
    previously exited PH enrollment must also have a Move-In Date.",

    "System Flow Chart",
    "Continuous at Start",
    "This system status indicates a client who is not categorized as Active at Start or 
    Inflow for a given month but who is experiencing homelessness during that month. 
    Used in the System Flow Month-by-Month chart. While this category is not displayed 
    on the chart, monthly counts are provided in the chart’s data download.",

    "System Flow Chart",
    "Continuous at End",
    "This system status indicates a client who is not categorized as Active at End or 
    Outflow for a given month but who is experiencing homelessness during that month. 
    Used in the System Flow Month-by-Month chart. While this category is not displayed 
    on the chart, monthly counts are provided in the chart’s data download.",

    "System Flow Chart",
    "Unknown at Start",
    "This system status indicates a client who is inactive in a non-residential 
    enrollment at the start of the period, exits that enrollment later in the same period, 
    and has no other enrollment entry or homeless CLS record during that period which 
    could provide information for a different inflow status. Used in the System Flow 
    Month-by-Month chart. While this category is not displayed on the chart, monthly 
    counts are provided in the chart’s data download.",
    
    "System Flow Chart",
    "Inflow",
    "The number of clients that entered or flowed into the system. This status
    indicates a client entered a system project after the report period’s start
    date. This status excludes all clients who were counted as homeless or housed
    at the start of the report period.",
    
    "System Flow Chart",
    "First-Time Homeless",
    "This inflow system status indicates a client who entered the system after 
    the report period’s start date and who also had not been served in the system
    within the 24 months prior to their entry into the report period. This
    inflow system status is only available when a dataset with 36 months of data
    is uploaded to Eva.",
    
    "System Flow Chart",
    "Returned from Permanent",
    "This inflow system status indicates a client who entered the system after
    the report period’s start date and who had a previous exit to a permanent
    destination within the 24 months prior to their entry. Permanent destinations
    include renting or owning permanent housing with or without subsidy, and
    staying or living with friends or family with a permanent tenure.",

    "System Flow Chart",
    "Re-engaged from Non-Permanent",
    "This inflow system status indicates a client who entered the system after 
    the report period’s start date and who had a previous exit to a homeless, 
    temporary, institutional destination or unknown destination within the 24 
    months prior to their entry. A temporary destination could be a hotel or 
    model paid for without an ES voucher. An institutional destination refers to 
    group/assisted living, a medical facility, or incarceration. An unknown 
    destination often refers to when a client does not report their exit destination. 
    Additionally, clients may be considered Re-engaged from Non-Permanent if they 
    are enrolled in a non-residential project in which they are considered “Inactive” 
    who then: (1) have a documented homeless living situation in a Current Living 
    Situation record later during the report period, (2) are considered active 15 
    days prior to an exit, or (3) are enrolled in a different project with an entry 
    date that overlaps with the Inactive enrollment. These clients are considered 
    as re-engaging with the system from an unknown situation (i.e., a period of inactivity).",

    "System Flow Chart",
    "Inflow Unspecified",
    "This system status indicates a client who entered the system after the
    report start date, but it cannot be determined if they are newly homeless
    because there is not enough lookback data. This inflow system status takes
    the place of First-Time Homeless in cases where less than 36 months of data
    are uploaded to Eva.",
    
    "System Flow Chart",
    "Outflow",
    "The number of clients that left or flowed out of the system. This status
    indicates a client exited a system project after the report period’s start
    date and before the report period’s end date. A client cannot be counted in
    both outflow and active at end.",
    
    "System Flow Chart",
    "Exited, Non-Permanent",
    "This outflow system status indicates a client exited the system to a homeless, 
    temporary, institutional, or unknown destination as defined in the HMIS Data 
    Standards. A temporary destination could be a hotel or motel paid for without 
    an ES voucher. An institutional destination refers to group/assisted living, a 
    medical facility, or incarceration. An unknown destination often refers to when 
    a client does not report their exit destination or when destination data is not 
    collected in HMIS. In the Summary and Detail System Flow charts, this category 
    represents the latest system exit of clients who were not active in the system 
    on the last day of the report.",

    "System Flow Chart",
    "Exited, Permanent",
    "This outflow system status indicates a  client exited the system to a permanent 
    destination as defined in the HMIS Data Standards. Permanent destinations include 
    renting or owning permanent housing with or without subsidy, and staying or living 
    with friends or family with a permanent tenure. In the Summary and Detail System 
    Flow charts, this category represents the latest system exit of clients who were 
    not active in the system on the last day of the report.",

    "System Flow Chart",
    "Inactive (Outflow)",
    "A client is counted in inactive outflow if they ended the report period with (1) an 
    open enrollment in an Emergency Shelter – Night-by-Night project that has not had a 
    bed night recorded within the last 15 days of the report period, (2) an open enrollment 
    in any Street Outreach, Day Shelter, Supportive Services, or Other project without a 
    homeless living situation documented in a Current Living Situation (CLS) record within 
    the last 60 days of the report period, or (3) an open enrollment in Coordinated Entry 
    without a homeless living situation documented in a CLS record within the last 90 days 
    of the report period. For the Month-by-Month chart, clients are counted as Inactive 
    in the first month they become inactive and are then dropped from the chart for 
    subsequent months unless they re-engage with the system again prior to the end of the report.", 
    
    "System Flow Chart",
    "Homeless (Active at End)",
    "This system status indicates a client was actively experiencing homelessness 
    in the system at the end of the report period. This includes clients who were 
    enrolled in: (1) Emergency Shelter – Entry/Exit, Safe Haven, or Transitional Housing projects, 
    (2) Emergency Shelter – Night-by-Night projects with a Project Start Date or recorded bed night 
    within the 15-day period before the report end date, (3) Street Outreach projects with a 
    Project Start Date or documented homeless living situation in a Current Living Situation 
    record within the 60-day period before the report end date, (4) Supportive Services Only, 
    Day Shelter or Other type projects with a documented homeless living situation in the Prior 
    Living Situation or a Current Living Situation recorded within the 60-day period before the 
    report end date, (5) Permanent Housing projects, either without a Housing Move-In Date or 
    with a Housing Move-In Date after the report end date, or (6) Coordinated Entry projects 
    with a documented homeless living situation in the Prior Living Situation or Current Living 
    Situation recorded within the 90-day period before the report end date. Additionally, clients 
    may be considered experiencing homelessness on the last day of the report if they have an 
    exit right before the report end date that does not qualify as a “system exit” due to a subsequent 
    enrollment entry in the 14 days following the exit.",
    
    "System Flow Chart",
    "Housed (Active at End)",
    "This system status indicates a client was actively housed in the system at the end of 
    the report period. All clients with this status were enrolled in a Permanent Housing (PH) 
    project with a Housing Move-In Date before the report end date. Additionally, clients may 
    be considered housed as of the end of the report period if they have an exit to a permanent 
    destination from a PH project right before the report end date that does not qualify as a 
    “system exit” due to a subsequent enrollment in a PH project in the 14 days following 
    the exit. The previously exited PH enrollment must also have a Move-In Date.",

    "System Flow Chart",
    "System Exits",
    "An exit from any project where there is no subsequent enrollment in any project type 
    for the household in the 14 days following the exit. Clients may have more than one 
    system exit during the report period. In the Summary and Detail System Flow charts, 
    the outflow categories “Exited, Non-Permanent” and “Exited, Permanent” represent the 
    latest system exit of clients who were not active in the system on the last day of the 
    report. When looking at system exits during the report period for the purposes of 
    determining Destination, the destination for the last exit during the report period is reported.",
    
    "Client System Status Chart",
    "Homeless (Period Start)",
    "This system status indicates a client was actively experiencing homelessness in the 
    system as of the start of the report period. This includes clients who were enrolled in: 
    (1) Emergency Shelter – Entry/Exit, Safe Haven, or Transitional Housing projects, 
    (2) Emergency Shelter – Night-by-Night projects with a Project Start Date or recorded 
    bed night within the 15-day period before the report start date, (3) Street Outreach 
    projects with a Project Start Date or documented homeless living situation in a Current 
    Living Situation record within the 60-day period before the report start date, 
    (4) Supportive Services Only, Day Shelter or Other type projects with a documented 
    homeless living situation in the Prior Living Situation or a Current Living Situation 
    recorded within the 60-day period before the report start date, (5) Permanent Housing 
    projects, either without a Housing Move-In Date or with a Housing Move-In Date after 
    the report start date, or (6) Coordinated Entry projects with a documented homeless 
    living situation in the Prior Living Situation or Current Living Situation recorded 
    within the 90-day period before the report start date. Additionally, clients may be 
    considered experiencing homelessness as of the first day of the report if they have 
    an exit right before the report start date that does not qualify as a “system exit” 
    due to a subsequent enrollment in the 14 days following the exit.",
    
    "Client System Status",
    "Housed (Period Start)",
    "This system status indicates a client was actively housed in the system as of the 
    start of the report period. All clients with this status were enrolled in a permanent 
    housing (PH) project with a Housing Move-In Date before the report start date. 
    Additionally, clients may be considered housed as of the start of the report period 
    if they have an exit to a permanent destination from a PH project right before the 
    report start date that does not qualify as a “system exit” due to a subsequent 
    enrollment in a PH project in the 14 days following the exit. The previously 
    exited PH enrollment must also have a Move-In Date.",
    
    "Client System Status",
    "Exited, Non-Permanent",
    "This status indicates that the client’s last exit in the report period was to a 
    homeless, temporary, institutional, or unknown destination. A temporary destination 
    could be a hotel or motel paid for without an ES voucher. An institutional 
    destination refers to group/assisted living, a medical facility, or incarceration. 
    An unknown destination often refers to when a client does not report their exit 
    destination or when destination data is not collected in HMIS.",

    "Client System Status",
    "Exited, Permanent",
    "This status indicates that the client’s last exit in the report period was to a 
    permanent destination. Permanent destinations include renting or owning permanent 
    housing with or without subsidy, and staying or living with friends or family with 
    a permanent tenure.",

    "Client System Status",
    "Enrolled, Homeless",
    "This system status indicates a client was actively experiencing homelessness in the 
    system at the end of the report period. This includes clients who were enrolled in: 
    (1) Emergency Shelter – Entry/Exit, Safe Haven, or Transitional Housing projects, 
    (2) Emergency Shelter – Night-by-Night projects with a Project Start Date or recorded 
    bed night within the 15-day period before the report end date, (3) Street Outreach 
    projects with a Project Start Date or documented homeless living situation in a Current 
    Living Situation record within the 60-day period before the report end date, 
    (4) Supportive Services Only, Day Shelter or Other type projects with a documented 
    homeless living situation in the Prior Living Situation or a Current Living Situation 
    recorded within the 60-day period before the report end date, (5) Permanent Housing 
    projects, either without a Housing Move-In Date or with a Housing Move-In Date after 
    the report end date, or (6) Coordinated Entry projects with a documented homeless 
    living situation in the Prior Living Situation or Current Living Situation recorded 
    within the 90-day period before the report end date. Clients may be considered 
    experiencing homelessness on the first day of the report if they have an exit right 
    before the report start date that does not qualify as a “system exit” due to a subsequent 
    enrollment in the 14 days following the exit.",
    
    "Client System Status",
    "Enrolled, Housed",
    "This system status indicates a client was actively housed in the system at the end 
    of the report period. All clients with this status were enrolled in a permanent housing 
    (PH) project with a Housing Move-In Date before the report start date. Clients may be 
    considered housed as of the end of the report period if they have an exit to a permanent 
    destination from a PH project right before the report end date that does not qualify as a 
    “system exit” due to a subsequent enrollment in a PH project in the 14 days following the 
    exit. The previously exited PH enrollment must also have a Move-In Date.",
    
    "Client System Status",
    "Inactive (Period End)",
    "A client is counted as Inactive at Period End if had (1) an open enrollment in an 
    Emergency Shelter – Night-by-Night project that had no bed night recorded within the last 
    15 days of the report period end date, (2) an open enrollment in any Street Outreach, 
    Day Shelter, Supportive Services, or Other project without a homeless living situation 
    documented in a Current Living Situation (CLS) record within the last 60 days of the report 
    period end date, or (3) an open enrollment in Coordinated Entry without a homeless living 
    situation documented in a CLS record within the last 90 days of the report period end date."
    
)
  
  datatable(
    gloss,
    rownames = FALSE,
    options = list(
      searchHighlight = TRUE,
      order = list(list(0, 'asc'), list(1, 'asc'))
    ),
    style = "default"
  )
  
})
