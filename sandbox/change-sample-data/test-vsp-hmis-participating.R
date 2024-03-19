# run 00-Start_here.R 

PDDEcols = c("OrganizationName",
             "ProjectID",
             "ProjectName",
             "Issue",
             "Type",
             "Guidance",
             "Detail")

vsp_projects <- Project %>%
  inner_join(Organization %>%
              filter(VictimServiceProvider == 1),
            by = "OrganizationID") %>%
  pull(ProjectID) %>%
  unique()
  
participating_projects <- Project %>%
  inner_join(HMISParticipation %>%
               filter(HMISParticipationType == 1),
             by = "ProjectID") %>%
  pull(ProjectID) %>%
  unique()

vsps_that_are_hmis_participating <- 
  base::intersect(vsp_projects, participating_projects)

# once you've run this, you should be able to test two things:

# 1. if 1385 and 1564 are not flagged, that means the logic is either not
# recognizing that the projects belong to Org ID 80 or that Org 80 is set as
# a VSP. We want 1385 and 1564 to flag!

# 2. if 1800 flags that means Eva is not recognizing that its HMISParticipation
# is set correctly to Comparable Database (2). We do NOT want 1800 to flag.
