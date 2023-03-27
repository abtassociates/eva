
# hc = hard-coded
hc_prior_living_situation_required <- ymd("20161001")
hc_psh_started_collecting_move_in_date <- ymd("20171001")

# Living Situations Groups (includes PLS, CLS, and destinations) 
#(Updated to match FY2022 DS) ---------------------------
allowed_living_situations <- 
  c(16, 1, 18, 15, 6, 7, 25, 4, 5, 29, 14, 2, 32, 13, 36, 12, 22, 35, 23, 26,
    27, 28, 19, 3, 31, 33, 34, 10, 20, 21, 11, 30, 17, 24, 37, 8, 9, 99)

perm_livingsituation <- c(3, 10, 11, 19:23, 26, 28, 31, 33, 34, 36)

lh_livingsituation <- c(1,16,18)

temp_livingsituation <- c(1, 2, 12, 13, 14, 16, 18, 27, 32, 35) 

institutional_livingsituation <- c(4:7, 15, 25, 27, 29)

other_livingsituation <- c(8, 9, 17, 24, 30, 37, 99)

# Project Type Groupings --------------------------------------------------

es_nbn_project_type <- 0

es_ee_project_type <- 1

th_project_type <- 2

psh_project_type <- 3

out_project_type <- 4

sso_project_type <- 6

other_project_project_type <- 7

sh_project_type <- 8

hp_project_type <- 12

rrh_project_type <- 13

ce_project_type <- 14

lh_residential_project_types <- c(1, 2, 8)

lh_project_types <- c(1, 2, 4, 8)

psh_project_types <- c(3, 9, 10)

ph_project_types <- c(3, 9, 10, 13)

ph_other_project_types <- c(9, 10)

lh_at_entry_project_types <- c(1, 2, 3, 4, 8, 9, 13)

lh_ph_hp_project_types <- c(1, 2, 3, 4, 8, 9, 12, 13)

coc_funded_project_types <- c(2, 3, 13)

project_types_w_beds <- c(1, 2, 3, 8, 9, 10, 13)

# Funding Source Groupings ------------------------------------------------

ssvf_fund_sources <- 33

# Expected upload schema (files, columns, and data types) ------------------
cols_and_data_types <- read_csv("public-resources/columns.csv", 
                                col_types = cols()) %>%
  filter(!(File %in% c("AssessmentResults","AssessmentQuestions", "Disabilities")))

data_type_mapping <- c(
  character = "c", 
  numeric = "n", 
  date = "D",
  datetime = "T"
)

# Issue types and levels ------------------------------------------------
issue_levels <- c("High Priority", "Error", "Warning")

issue_display_cols <- c("Issue", "Type", "Guidance", "Detail")