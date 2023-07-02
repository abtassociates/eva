
# hc = hard-coded
hc_prior_living_situation_required <- ymd("20161001")
hc_psh_started_collecting_move_in_date <- ymd("20171001")

# Living Situations Groups (includes PLS, CLS, and destinations) 
#(Updated to match FY2022 DS) ---------------------------
# For reference, these come from the HMIS CSV Export specs, pgs 41-43

allowed_living_situations <- 
  c(8, 9, 99, 101, 116, 118, 204, 205, 206, 207, 215, 225, 302, 314, 329, 332,
    335, 336, 410, 411, 421, 435)

allowed_prior_living_sit <- 
  c(8, 9, 17, 37, 99, 101, 116, 118, 204, 205, 206, 207, 215, 225, 302, 314, 329,
    332, 335, 336, 410, 411, 421, 435)

allowed_current_living_sit <- 
  c(8, 9, 30, 17, 24, 99, 101, 116, 118, 204, 205, 206, 207, 215, 225, 302, 312,
    313, 327, 314, 329, 332, 410, 411, 421, 422, 423, 426, 435)

allowed_destinations <- 
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
    22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 99)

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

project_types_w_beds <- c(1, 2, 3, 8, 9, 10, 13)

# Funding Source Groupings ------------------------------------------------

ssvf_fund_sources <- 33


# Race Values -------------------------------------------------------------

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

# Issue types and levels ------------------------------------------------
issue_levels <- c("High Priority", "Error", "Warning")

issue_display_cols <- c("Issue", "Type", "Guidance", "Detail")
