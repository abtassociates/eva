
# hc = hard-coded
hc_prior_living_situation_required <- ymd("20161001")
hc_psh_started_collecting_move_in_date <- ymd("20171001")
no_end_date <- ymd("20990909")

# Living Situations Groups (includes PLS, CLS, and destinations) 
#(Updated to match FY2024 DS) ---------------------------------------------
# For reference, these come from the HMIS CSV Export specs, pgs 41-43

allowed_prior_living_sit <- 
  c(116, 101, 118, 215, 206, 207, 225, 204, 205, 302, 329, 314, 332, 335, 336,
    410, 435, 421, 411, 8, 9, 99)

allowed_current_living_sit <- 
  c(116, 101, 118, 215, 206, 207, 225, 204, 205, 302, 329, 314, 332, 335, 336,
    410, 435, 421, 411, 17, 37, 8, 9, 99)

allowed_destinations <- 
  c(116, 101, 118, 215, 206, 207, 225, 204, 205, 302, 329, 314, 332, 312, 313,
    327, 422, 423, 426, 410, 435, 421, 411, 30, 17, 24, 8, 9, 99)

allowed_living_situations <- 
  c(allowed_prior_living_sit,
    allowed_current_living_sit,
    allowed_destinations) %>%
  unique() %>%
  sort()

perm_livingsituation <- c(400:499)

homeless_livingsituation_incl_TH <- c(100:199, 302)

temp_livingsituation <- c(300:399)

institutional_livingsituation <- c(200:299)

other_livingsituation <- c(0:99)

# Project Type Groupings --------------------------------------------------

es_nbn_project_type <- 1

es_ee_project_type <- 0

th_project_type <- 2

psh_project_type <- 3

out_project_type <- 4

sso_project_type <- 6

other_project_project_type <- 7

sh_project_type <- 8

day_project_type <- 11

hp_project_type <- 12

rrh_project_type <- 13

ce_project_type <- 14

lh_project_types_nc <- c(0, 2, 8)

lh_residential_project_types <- c(0, 1, 2, 8)

lh_project_types <- c(0, 1, 2, 4, 8)

psh_project_types <- c(3, 9, 10)

ph_project_types <- c(3, 9, 10, 13)

ph_other_project_types <- c(9, 10)

lh_ph_hp_project_types <- c(0, 1, 2, 3, 4, 8, 9, 12, 13)

coc_funded_project_types <- c(2, 3, 13)

project_types_w_beds <- c(0, 1, 2, 3, 8, 9, 10, 13)

non_res_project_types <- c(4, 6, 7, 11, 12, 14)

project_types_w_cls <- c(1, 4, 6, 14)

long_stayer_98_percentile_project_types <- c(0, 2, 8, 12, 13)

project_types_enrolled_homeless <- c(lh_project_types, 14)
   
long_stayer_percentile_project_types <- c(0, 2, 3, 8, 9, 10, 12, 13)

all_project_types <- c(0, 1, 2, 3, 4, 6, 8, 9, 10, 11, 12, 13, 14) 
# All means All HUD-defined project types, so it excludes "Other"


# Housing Types -------------------------------------------------

client_single_site <- 1
client_multiple_sites <- 2
tenant_scattered_site <- 3


# Funding Source Groupings -------------------------------------------------

ssvf_fund_sources <- 33

# HUD Values ---------------------------------------------------------------

yes_no_enhanced <- c(0, 1, 8, 9, 99)
yes_no <- c(0, 1, 99)
dkr_dnc <- c(8, 9, 99)
dkr <- c(8, 9)

# Expected upload schema (files, columns, and data types) ------------------
cols_and_data_types <- read_csv(here("public-resources/columns.csv"), 
                                col_types = cols()) %>%
  filter(!(File %in% c("Affiliation",
                       "AssessmentResults",
                       "AssessmentQuestions",
                       "Disabilities")))

data_type_mapping <- list(
  character = "character",
  numeric = "numeric",
  date = "Date",
  datetime = "POSIXct"
)

# Allowed Subsidy Types ---------------------------------------------------

subsidy_types <- c(419, 420, 428, 431, 433, 434, 436, 437, 438, 439, 440)

# Issue types and levels --------------------------------------------------
issue_levels <- c("High Priority", "Error", "Warning")

issue_display_cols <- c("Issue", "Type", "Guidance", "Detail")

# System Overview - Filters -----------------------------------------------

syso_hh_types <- list(
  "All Household Types" = "All",
  "- Youth and Young Adult" = "YYA",
  "Adult Only" = "AO",
  "- Adult Only 18-24" = "UY",
  "Adult Child" = "AC",
  "- Parenting Young Adult" = "PY",
  "Child Only" = "CO"
)

syso_level_of_detail <- list(
  "All People" = "All", 
  "Heads of Household and Adults" = "HoHsAndAdults", 
  "Heads of Household Only" = "HoHsOnly"
)

syso_project_types <- list(
  "All Project Types" = "All",
  "Residential" = "Residential",
  "Non-residential" = "NonResidential"
)

syso_age_cats <- c(
  # "All ages",
  "0 to 12",
  "13 to 17",
  "18 to 21",
  "22 to 24",
  "25 to 34",
  "35 to 44",
  "45 to 54",
  "55 to 64",
  "65 to 74",
  "75 and older",
  "Unknown")

race_cols <- c("RaceNone", "AmIndAKNative", "Asian", "BlackAfAmerican", 
               "NativeHIPacific", "White", "MidEastNAfrican", "HispanicLatinaeo")


syso_race_ethnicity_method2 <- list(
  "All Races/Ethnicities" = "All",
  "Detailed" = c(
    "American Indian, Alaska Native, or Indigenous" =
      "AmIndAKNativeMethod2Detailed",
    "Asian or Asian American" =
      "AsianMethod2Detailed",
    "Black, African American, or African" =
      "BlackAfAmericanMethod2Detailed",
    "Hispanic/Latina/e/o" =
      "LatineMethod2Detailed",
    "Middle Eastern or North African" =
      "MidEastNAfricanMethod2Detailed",
    "Native Hawaiian or Pacific Islander" =
      "NativeHIPacificMethod2Detailed",
    "White" =
      "WhiteMethod2Detailed"
  ),
  "Summarized" = c(
    "Black, African American or African and Hispanic/Latina/e/o" =
      "BlackAfAmericanLatineMethod2Summarized",
    "Hispanic/Latina/e/o alone" =
      "LatineAloneMethod2Summarized",
    "Hispanic/Latina/e/o" =
      "LatineMethod2Summarized"
  )
)
syso_race_ethnicity_method1 <- list(
  "All Races/Ethnicities" = "All",
  "Detailed" = c(
    "American Indian, Alaska Native, or Indigenous alone" =
      "AmIndAKNativeAloneMethod1Detailed",
    "American Indian, Alaska Native, or Indigenous & Hispanic/Latina/e/o" =
      "AmIndAKNativeLatineMethod1Detailed",
    "Asian or Asian American alone" =
      "AsianAloneMethod1Detailed",
    "Asian or Asian American & Hispanic/Latina/e/o" =
      "AsianLatineMethod1Detailed",
    "Black, African American, or African alone" =
      "BlackAfAmericanAloneMethod1Detailed",
    "Black, African American, or African & Hispanic/Latina/e/o" =
      "BlackAfAmericanLatineMethod1Detailed",
    "Hispanic/Latina/e/o alone" =
      "LatineAloneMethod1Detailed",
    "Middle Eastern or North African alone" =
      "MidEastNAfricanAloneMethod1Detailed",
    "Middle Eastern or North African & Hispanic/Latina/e/o" =
      "MidEastNAfricanLatineMethod1Detailed",
    "Multi-Racial (not Hispanic/Latina/e/o)" =
      "MultipleNotLatineMethod1Detailed",
    "Multi-Racial & Hispanic/Latina/e/o" =
      "MultipleLatineMethod1Detailed",
    "Native Hawaiian or Pacific Islander alone" =
      "NativeHIPacificAloneMethod1Detailed",
    "Native Hawaiian or Pacific Islander & Hispanic/Latina/e/o" =
      "NativeHIPacificLatineMethod1Detailed" ,
    "White alone" =
      "WhiteAloneMethod1Detailed",
    "White & Hispanic/Latina/e/o" =
      "WhiteLatineMethod1Detailed"
  ),
  "Summarized" = c("All People of Color" = "BILPOCMethod1Summarized",
                   "White alone" = "WhiteMethod1Summarized")
)

# Special Populations -----------------------------------------------------

syso_veteran_pops <- list(
  "Veteran" = "Veteran",
  "Non-Veteran/Unknown" = "NonVeteran"
)

syso_dv_pops <- list(
  "Domestic Violence Survivor: Currently Fleeing" = "DVFleeing",
  "Domestic Violence Survivor: Not Currently Fleeing" = "DVNotFleeing",
  "Domestic Violence Survivor: Total" = "DVTotal",
  "No Domestic Violence Indicated" = "NotDV"
)

syso_chronic_pops <- list(
  "Chronically Homeless" = "Chronic",
  "Long-term Homeless" = "LongTerm"
)

syso_spec_pops_people <- c(
  "All Statuses" = "None",
  # "Inflow",
  syso_veteran_pops#,
  #syso_dv_pops,
  # syso_chronic_pops
)

syso_methodology_types <- c(
  "Method 1: A person is only counted in one race/ethnicity category" = 1,
  "Method 2: A person may be counted in multiple race/ethnicity categories" = 2
)

syso_grouping_detail <- c(
  ""
)

# EvaChecks data (contains issue, type, guidance for each check) ----------
evachecks <- read_csv(here("public-resources/EvaChecks.csv"), show_col_types = FALSE)

evachecks_no_dupes <- evachecks %>%
  janitor::get_dupes(ID) %>% nrow() == 0

# Funding and Project Type Considerations DQ ------------------------------

# if a funder isn't listed, it is not required to collect any of these elements.

inc_ncb_hi_required_prep <- tribble(
  ~Funder, ~ProjectType, ~inc, ~ncb, ~hi, ~dv,
  1, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  2, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  3, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  4, setdiff(all_project_types, 14), TRUE, TRUE, TRUE, TRUE,
  5, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  6, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  7, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  8, setdiff(all_project_types, 1), TRUE, TRUE, TRUE, FALSE,
  9, setdiff(all_project_types, 1), TRUE, TRUE, TRUE, FALSE,
  10, setdiff(all_project_types, 1), TRUE, TRUE, TRUE, FALSE,
  11, setdiff(all_project_types, 1), TRUE, TRUE, TRUE, FALSE,
  8, 1, FALSE, FALSE, FALSE, TRUE,
  9, 1, FALSE, FALSE, FALSE, TRUE,
  10, 1, FALSE, FALSE, FALSE, TRUE,
  11, 1, FALSE, FALSE, FALSE, TRUE,
  13, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  14, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  15, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  16, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  17, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  18, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  19, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  20, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  21, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  22, c(all_project_types), FALSE, TRUE, TRUE, FALSE,
  23, c(all_project_types), TRUE, TRUE, TRUE, FALSE,
  24, c(all_project_types), TRUE, TRUE, TRUE, FALSE,
  25, c(all_project_types), FALSE, FALSE, TRUE, FALSE,
  26, c(all_project_types), TRUE, TRUE, TRUE, FALSE,
  27, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  30, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  33, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  35, c(ph_project_types), TRUE, TRUE, TRUE, TRUE,
  37, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  38, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  39, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  40, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  41, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  42, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  45, c(all_project_types), TRUE, TRUE, TRUE, TRUE,
  53, setdiff(all_project_types, c(0, 1, 4)), TRUE, TRUE, TRUE, TRUE,
  54, setdiff(all_project_types, 14), TRUE, TRUE, TRUE, TRUE,
  55, setdiff(all_project_types, 14), TRUE, TRUE, TRUE, TRUE
)

# this will break out all the project types so they each get a row

inc_ncb_hi_required <- unnest_longer(inc_ncb_hi_required_prep, ProjectType) %>%
  unique()

sys_comp_selection_choices = c(
  "Age", 
  #"Domestic Violence", #VL 9/20/24: Not including this for launch
  # "Homelessness Type", # Victoria, 8/15/24: Not including this for Launch
  "All Races/Ethnicities",
  "Grouped Races/Ethnicities",
  "Veteran Status (Adult Only)"
)

rm(inc_ncb_hi_required_prep)


# Font sizing -------------------------------------------------------------
# regular font can be set in css as pixels
# geom_text or annotate are in mm, not points
# element_text (for title, legend, axis labels, etc.) is in points, not mm
  # http://www.cookbook-r.com/Graphs/Fonts/
  # Also: https://www.christophenicault.com/post/understand_size_dimension_ggplot2/
base_font_px <- 14 # 14px = 14*0.75 pts/px = 10.5pts = 10.5 pts = 3.7mm
base_font_pts <- base_font_px*0.75
base_font_mm <- base_font_pts / .pt # .pt is the constant to convert pt to mm (1pt = 3.5mm)

dq_axis_font <- 12 # 12 pts = 16px
sys_chart_text_font <- 14 / .pt # 14 pts = 4.92mm = 18.67px
sys_chart_text_font_pts <- 14 # 14 pts = 4.92mm = 18.67px
sys_axis_text_font <- 15 #15 pts = 22px
sys_comp_axis_text_font <- 14 #14 pts = 22px
sys_legend_text_font <- sys_axis_text_font #16 pts = 22px
sys_chart_title_font <- sys_axis_text_font # 16 pts
sys_chart_export_font_reduction <- 0.7
ppt_summary_slide_font <- 19 # 19 pts = 25px
ppt_chart_title_font_size <- 36