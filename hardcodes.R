
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

perm_livingsituation <- c(336, 410, 411, 421, 422, 423, 426, 435)

lh_livingsituation <- c(101, 116, 118)

homeless_livingsituation <- c(101, 302, 116, 118)

temp_livingsituation <- c(101, 302, 312, 313, 314, 116, 118, 327, 332, 335)

institutional_livingsituation <- c(204, 205, 206, 207, 215, 225, 327, 329)

other_livingsituation <- c(8, 9, 17, 24, 30, 37, 99)

not_homeless_livingsituation <- 
  c(204, 205, 206, 207, 215, 225, 302, 314, 329, 332, 335, 336, 410, 411, 421, 435)


# Project Type Groupings --------------------------------------------------

es_nbn_project_type <- 1

es_ee_project_type <- 0

th_project_type <- 2

psh_project_type <- 3

out_project_type <- 4

sso_project_type <- 6

other_project_project_type <- 7

sh_project_type <- 8

hp_project_type <- 12

rrh_project_type <- 13

ce_project_type <- 14

lh_residential_project_types <- c(0, 1, 2, 8)

lh_project_types <- c(0, 1, 2, 4, 8)

psh_project_types <- c(3, 9, 10)

ph_project_types <- c(3, 9, 10, 13)

ph_other_project_types <- c(9, 10)

lh_ph_hp_project_types <- c(0, 1, 2, 3, 4, 8, 9, 12, 13)

coc_funded_project_types <- c(2, 3, 13)

project_types_w_beds <- c(0, 1, 2, 3, 8, 9, 10, 13)

project_types_w_cls <- c(0, 4, 14)

long_stayer_98_percentile_project_types <- c(1, 2, 8, 12, 13)

long_stayer_percentile_project_types <- c(1, 2, 3, 8, 9, 10, 12, 13)

all_project_types <- c(0, 1, 2, 3, 4, 6, 8, 9, 10, 11, 12, 13, 14) # minus Other

# Funding Source Groupings -------------------------------------------------

ssvf_fund_sources <- 33


# Race Values --------------------------------------------------------------

yes_no_enhanced <- c(0, 1, 8, 9, 99)
yes_no <- c(0, 1, 99)
dkr_dnc <- c(8, 9, 99)
dkr <- c(8, 9)

# Expected upload schema (files, columns, and data types) ------------------
cols_and_data_types <- read_csv("public-resources/columns.csv", 
                                col_types = cols()) %>%
  filter(!(File %in% c("Affiliation",
                       "AssessmentResults",
                       "AssessmentQuestions",
                       "Disabilities")))

data_type_mapping <- c(
  character = "c", 
  numeric = "n", 
  date = "D",
  datetime = "T"
)

# Allowed Subsidy Types ---------------------------------------------------

subsidy_types <- c(419, 420, 428, 431, 433, 434, 436, 437, 438, 439, 440)

# Issue types and levels --------------------------------------------------
issue_levels <- c("High Priority", "Error", "Warning")

issue_display_cols <- c("Issue", "Type", "Guidance", "Detail")

# EvaChecks data (contains issue, type, guidance for each check) ----------
evachecks <- read_csv("public-resources/EvaChecks.csv", show_col_types = FALSE)

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

projects_funders_types <- Funder %>%
  left_join(Project %>%
              select(ProjectID, ProjectType),
            join_by(ProjectID)) %>%
  filter(is.na(EndDate) | EndDate > meta_HUDCSV_Export_Start) %>%
  select(ProjectID, ProjectType, Funder) %>%
  unique() %>%
  left_join(inc_ncb_hi_required, join_by(ProjectType, Funder)) %>%
  mutate(inc = replace_na(inc, FALSE),
         ncb = replace_na(ncb, FALSE),
         hi = replace_na(hi, FALSE),
         dv = replace_na(dv, FALSE)) %>%
  group_by(ProjectID) %>%
  summarise(inc = max(inc, na.rm = TRUE),
            ncb = max(ncb, na.rm = TRUE),
            hi = max(hi, na.rm = TRUE),
            dv = max(dv, na.rm = TRUE)) %>%
  ungroup()
