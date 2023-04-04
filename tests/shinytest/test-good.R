app <- ShinyDriver$new("../../", loadTimeout = 1e+05)
app$snapshotInit("test-good")

app$setInputs(Go_to_upload = "click")
app$uploadFile(imported = "../test_uploads/HMIS CSV Export - Current Good.zip") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
# Input 'fileStructureAnalysis_rows_current' was set, but doesn't have an input binding.
# Input 'fileStructureAnalysis_rows_all' was set, but doesn't have an input binding.
# Input 'fileStructureAnalysis_state' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(sidebarmenuid = "tabClientCount")
# Input 'clientCountSummary_rows_current' was set, but doesn't have an input binding.
# Input 'clientCountSummary_rows_all' was set, but doesn't have an input binding.
# Input 'clientCountSummary_state' was set, but doesn't have an input binding.
# Input 'clientCountData_rows_current' was set, but doesn't have an input binding.
# Input 'clientCountData_rows_all' was set, but doesn't have an input binding.
# Input 'clientCountData_state' was set, but doesn't have an input binding.
app$snapshot()
app$snapshotDownload("downloadClientCountsReport")
app$setInputs(sidebarItemExpanded = "AssessDataQuality")
app$setInputs(sidebarmenuid = "tabPDDE")
# Input 'pdde_summary_table_rows_current' was set, but doesn't have an input binding.
# Input 'pdde_summary_table_rows_all' was set, but doesn't have an input binding.
# Input 'pdde_summary_table_state' was set, but doesn't have an input binding.
# Input 'pdde_guidance_summary_rows_current' was set, but doesn't have an input binding.
# Input 'pdde_guidance_summary_rows_all' was set, but doesn't have an input binding.
# Input 'pdde_guidance_summary_state' was set, but doesn't have an input binding.
app$snapshot()
app$snapshotDownload("downloadPDDEReport")
app$setInputs(sidebarmenuid = "tabDQSystem")
app$snapshot()
app$snapshotDownload("downloadSystemDQReport")
app$setInputs(sidebarmenuid = "tabDQOrg")
# Input 'dq_org_guidance_summary_rows_current' was set, but doesn't have an input binding.
# Input 'dq_org_guidance_summary_rows_all' was set, but doesn't have an input binding.
# Input 'dq_org_guidance_summary_state' was set, but doesn't have an input binding.
# Input 'dq_organization_summary_table_rows_current' was set, but doesn't have an input binding.
# Input 'dq_organization_summary_table_rows_all' was set, but doesn't have an input binding.
# Input 'dq_organization_summary_table_state' was set, but doesn't have an input binding.
app$snapshot()
app$snapshotDownload("downloadOrgDQReport")
