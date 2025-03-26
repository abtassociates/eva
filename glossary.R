output$glossary <- renderDataTable({
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
    "A single-select universal filter. Eva allows users to filter system performance 
    data by three main household types: Adult Only, Adult Child, and Child Only. Eva also allows 
    users to look at subcategories of these household types: Youth and Young Adults, 
    Adult Only 18-24, and Parenting Young Adult. Thus, households may be counted in more than 
    one household type. For example, a 16-year-old parent with a 1-year-old child would fall 
    into both the Child Only and Youth and Young Adult household types. Household type is 
    determined based on the ages of all household members as of the entry date of their 
    earliest enrollment included in the report period.",
    
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
    PH - Housing with Services, PH - Permanent Supportive Housing,
    PH - Rapid Re-housing (RRH): Housing with or without services, and
    PH - Rapid Re-housing (RRH): Services Only. Clients are only included in
    ES - Night-by-Night projects if they have a recorded bed night.",
    
    "System Performance Filters",
    "Non-residential",
    "A Project Type Group selection that only includes clients in non-residential 
    projects who have a Current Living Situation record that indicates the client 
    is experiencing literal homelessness. A non-residential project does not provide 
    overnight accommodations. Project types that are considered as non-residential 
    include: Street Outreach, Day Shelter, Supportive Services Only, Coordinated
    Entry, and Other.",
    
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
    American, or African, and (3) Hispanic/Latina/e/o would be counted under
    \"Multi-Racial & Hispanic/Latina/e/o,\" but no other race/ethnicity categories.",
    
    "System Performance Filters",
    "Method 2",
    "A Methodology Type selection where each client may be counted in multiple 
    race/ethnicity categories as well as being included in 
    the \"All Races/Ethnicities\" selections. For example, a client 
    that reported as (1) Middle Eastern or North African, (2) Black, African American, 
    or African, and (3) Hispanic/Latina/e/o would be counted under \"Middle Eastern 
    or North African,\" \"Black, African American, or African,\" 
    and \"Hispanic/Latina/e/o\" race/ethnicity categories.",
    
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
    their own and in combination with Hispanic/Latina/e/o. This section also includes 
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
    "American Indian, Alaska Native, or Indigenous & Hispanic/Latina/e/o",
    "Includes clients who identified as both American Indian, Alaska Native, or
    Indigenous and as Hispanic/Latina/e/o.This Race/Ethnicity filter selection
    is listed under the Detailed section of the filter dropdown list and is only
    available when Method 1 is selected.",
    
    "System Performance Filters",
    "Asian or Asian American alone",
    "Includes clients who identified as only Asian or Asian American. This
    Race/Ethnicity filter selection is listed under the Detailed section of the
    filter dropdown list and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "Asian or Asian American & Hispanic/Latina/e/o",
    "Includes clients who identified as both Asian or Asian American and
    Hispanic/Latina/e/o. This Race/Ethnicity filter selection is listed under the
    Detailed section of the filter dropdown list and is only available when
    Method 1 is selected.",
    
    "System Performance Filters",
    "Black, African American, or African alone",
    "Includes clients who identified as only Black, African American, or African.
    This Race/Ethnicity filter selection is listed under the Detailed section of
    the filter dropdown list and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "Black, African American, or African & Hispanic/Latina/e/o",
    "Includes clients who identified as both Black, African American, or African
    and Hispanic/Latina/e/o. This Race/Ethnicity filter selection is listed under
    the Detailed section of the filter dropdown list and is only available when
    Method 1 is selected.",
    
    "System Performance Filters",
    "Middle Eastern or North African alone",
    "Includes clients who only selected Middle Eastern or North African and no
    other race/ethnicity category. This Race/Ethnicity filter selection is listed
    under the Detailed section of the filter dropdown list and is only available
    when Method 1 is selected.",
    
    "System Performance Filters",
    "Middle Eastern or North African & Hispanic/Latina/e/o",
    "Includes clients who identified as both Middle Eastern or North African and 
    Hispanic/Latina/e/o. This Race/Ethnicity filter selection is listed under the
    Detailed section of the filter dropdown list and is only available when
    Method 1 is selected.",
    
    "System Performance Filters",
    "Native Hawaiian or Pacific Islander alone",
    "Includes clients who identified as only Native Hawaiian or Pacific Islander.
    This Race/Ethnicity filter selection is listed under the Detailed section of
    the filter dropdown list and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "Native Hawaiian or Pacific Islander & Hispanic/Latina/e/o",
    "Includes clients who identified as both Native Hawaiian or Pacific Islander
    and Hispanic/Latina/e/o. This Race/Ethnicity filter selection is listed under
    the Detailed section of the filter dropdown list and is only available when
    Method 1 is selected.",
    
    "System Performance Filters",
    "White alone",
    "Includes clients who identified as only White. This Race/Ethnicity filter
    selection is listed under the Detailed section of the filter dropdown list
    and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "White & Hispanic/Latina/e/o",
    "Includes clients who identified as both White and Hispanic/Latina/e/o. This
    Race/Ethnicity filter selection is listed under the Detailed section of the
    filter dropdown list and is only available when Method 1 is selected.",
    
    "System Performance Filters",
    "Multi-Racial (not Hispanic/Latina/e/o)",
    "Includes clients who identified as multiple races (2+) but not as
    Hispanic/Latina/e/o. This Race/Ethnicity filter selection is listed under the
    Detailed section of the filter dropdown list and is only available when
    Method 1 is selected.",
    
    "System Performance Filters",
    "Multi-Racial & Hispanic/Latina/e/o",
    "Includes clients who identified as Hispanic/Latina/e/o together with two or
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
    American or African and Hispanic/Latina/e/o Method 2, Hispanic Latina/e/o
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
    "Black, African American or African and Hispanic/Latina/e/o Method 2",
    "Includes clients who identified as both Black, African American or African
    and Hispanic/Latina/e/o together or in combination with any other race/ethnicity.
    Listed under the Summarized section. This Race/Ethnicity filter selection
    is listed under the Summarized section of the filter dropdown list and is
    only available when Method 2 is selected.", 
    
    "System Performance Filters",
    "Hispanic/Latina/e/o alone",
    "Includes clients who identified as only Hispanic/Latina/e/o and no other
    race/ethnicity. This Race/Ethnicity filter selection is listed under both 
    the Detailed and Summarized sections of the filter dropdown list and is only
    available when Method 1 is selected.", 
    
    "System Performance Filters",
    "Hispanic Latina/e/o Method 2",
    "Includes clients who identified as Hispanic/Latina/e/o alone or in
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
    "Total Change is calculated by subtracting the number of clients who flowed
    into the system from the number of clients who flowed out of the system
    (Inflow – Outflow). This value can be positive or negative. A negative change
    value means more clients left the system than flowed into the system. A
    positive change value means more clients flowed into the system than left
    the system.",
    
    "System Flow Chart",
    "Homeless (Active at Start)",
    "This system status indicates a client was actively experiencing
    homelessness in the system as of the start of the report period. This
    includes clients who were enrolled in: (1) Emergency Shelter – Entry/Exit,
    Safe Haven, or Transitional Housing projects, (2) Emergency Shelter –
    Night-by-Night projects who have a recorded bed night within the 15-day
    period before the report start date, (3)	Street Outreach, Supportive 
    Services Only, Day Shelter or Other type projects who have a Current Living
    Situation recorded within the 60-day period before the report start date,
    (4) Permanent Housing projects, either without a Housing Move-In Date or
    with a Housing Move-In Date after the report start date, or (5) Coordinated
    Entry projects who have a Current Living Situation recorded within the
    90-day period before the report start date.",
    
    "System Flow Chart",
    "Housed (Active at Start)",
    "This system status indicates a client was actively housed in the system as
    of the start of the report period. All clients with this status were enrolled
    in a permanent housing project with a Housing Move-In Date before the report
    start date.",
    
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
    "Re-engaged form Non-Permanent",
    "This inflow system status indicates a client who entered the system after
    the report period’s start date and who had a previous exit to a homeless,
    temporary, institutional destination or unknown destination within the 24
    months prior to their entry. A temporary destination could be a hotel or model
    paid for without an ES voucher. An institutional destination refers to
    group/assisted living, a medical facility, or incarceration. An unknown
    destination often refers to when a client does not report their exit
    destination.",

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
    Standards. A temporary destination could be a hotel or model paid for without
    an ES voucher. An institutional destination refers to group/assisted living,
    a medical facility, or incarceration. An unknown destination often refers to
    when a client does not report their exit destination. Only the client’s last
    exit is counted.",

    "System Flow Chart",
    "Exited, Permanent",
    "This outflow system status indicates a client’s last system exit was to a
    permanent destination.  Permanent destinations include renting or owning
    permanent housing with or without subsidy, and staying or living with friends
    or family with a permanent tenure.",

    "System Flow Chart",
    "Inactive (Outflow)",
    "A client is counted in inactive outflow if they ended the report period
    with (1) an open enrollment in an Emergency Shelter – Night-by-Night project
    that has not had a bed night recorded within the last 15 days of the report
    period, (2) an open enrollment in Street Outreach, Day Shelter, Supportive
    Services, and Other project type enrollments without a Current Living
    Situation (CLS) record within the last 60 days of the report period, or (3)
    an open enrollment in Coordinated Entry without a CLS record within the last
    90 days of the report period.", 
    
    "System Flow Chart",
    "Homeless (Active at End)",
    "This system status indicates a client was actively experiencing homelessness
    in the system at the end of the report period. This includes clients who were
    enrolled in: (1) Emergency Shelter – Entry/Exit, Safe Haven, or Transitional
    Housing projects, (2) Emergency Shelter – Night-by-Night projects who have a
    recorded bed night within the 15-day period before the report end date, (3)
    Street Outreach, Supportive Services Only, Day Shelter or Other type projects
    who have a Current Living Situation recorded within the 60-day period before 
    the report end date, or (4) Permanent Housing projects, either without a
    Housing Move-In Date or with a Housing Move-In Date after the report end date.
    Coordinated Entry projects who have a Current Living Situation recorded 
    within the 90-day period before the report end date.",
    
    "System Flow Chart",
    "Housed (Active at End)",
    "This system status indicates a client was actively housed in the system at
    the end of the report period. All clients with this status were enrolled in
    a Permanent Housing project with a Housing Move-In Date before the report
    end date.",
    
    "Client System Status Chart",
    "Homeless (Period Start)",
    "This system status indicates a client was actively experiencing homelessness
    in the system as of the start of the report period. This includes clients who
    were enrolled in: (1) Emergency Shelter – Entry/Exit, Safe Haven, or
    Transitional Housing projects, (2) Emergency Shelter – Night-by-Night projects
    who have a recorded bed night within the 15-day period before the report
    start date, (3)	Street Outreach, Supportive Services Only, Day Shelter or 
    Other type projects who have a Current Living Situation recorded within the
    60-day period before the report start date, (4) Permanent Housing projects,
    either without a Housing Move-In Date or with a Housing Move-In Date after
    the report start date, or (5) Coordinated Entry projects who have a Current
    Living Situation recorded within the 90-day period before the report start
    date.",
    
    "Client System Status",
    "Housed (Period Start)",
    "This system status indicates a client was actively housed in the system as
    of the start of the report period. All clients with this status were enrolled
    in a permanent housing project with a Housing Move-In Date before the report
    start date.",
    
    "Client System Status",
    "Exited, Non-Permanent",
    "This status indicates that the client’s last exit in the report period was
    to a homeless, temporary, institutional, or unknown destination. A temporary
    destination could be a hotel or model paid for without an ES voucher. An
    institutional destination refers to group/assisted living, a medical facility,
    or incarceration. An unknown destination often refers to when a client does
    not report their exit destination.",

    "Client System Status",
    "Exited, Permanent",
    "This status indicates that the client’s last exit in the report period was
    to a permanent destination. Permanent destinations include renting or owning
    permanent housing with or without subsidy, and staying or living with friends
    or family with a permanent tenure.",

    "Client System Status",
    "Enrolled, Homeless",
    "This system status indicates a client was actively experiencing homelessness
    in the system at the end of the report period. This includes clients who were
    enrolled in: (1) Emergency Shelter – Entry/Exit, Safe Haven, or Transitional
    Housing projects, (2) Emergency Shelter – Night-by-Night projects who have a
    recorded bed night within the 15-day period before the report end date, (3)
    Street Outreach, Supportive Services Only, Day Shelter or Other type projects
    who have a Current Living Situation recorded within the 60-day period before
    the report end date, or (4) Permanent Housing projects, either without a
    Housing Move-In Date or with a Housing Move-In Date after the report end date.
    Coordinated Entry projects who have a Current Living Situation recorded within
    the 90-day period before the report end date.",
    
    "Client System Status",
    "Enrolled, Housed",
    "This system status indicates a client was actively housed in the system at
    the end of the report period. All clients with this status were enrolled in
    a Permanent Housing project with a Housing Move-In Date before the report end
    date.",
    
    "Client System Status",
    "Inactive (Period End)",
    "A client is counted as inactive at Period End if they ended the report
    period with (1) an open enrollment in an Emergency Shelter – Night-by-Night
    project that has not had a bed night recorded within the last 15 days of the
    report period, (2) an open enrollment in Street Outreach, Day Shelter,
    Supportive Services, and Other project type enrollments without a Current
    Living Situation (CLS) record within the last 60 days of the report period,
    or (3) an open enrollment in Coordinated Entry without a CLS record within
    the last 90 days of the report period.  "
    
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