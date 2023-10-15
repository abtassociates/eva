library(shinytest2)
source("../testing_functions.R", local = TRUE)

test_that("{shinytest2} recording: test-good", {
    print("Running test-good")
    app <- AppDriver$new(
        variant = platform_variant(), 
        name = "test-good", 
        seed = 1234,
        load_timeout=1e+05)
app$set_inputs(Go_to_upload = "click")
app$upload_file(imported = "../FY24-ICF-hashed-current-good.zip") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
# app$find_element("button[data-dismiss='modal']")$click()

print("about to cusotm download")
customDownload(app, "downloadFileStructureAnalysis","File-Structure-Analysis-Download")
app$expect_values()

print("going to client counts")
app$set_inputs(sidebarmenuid = "tabClientCount")
customDownload("downloadClientCountsReport", "Client-Counts-Download")
app$expect_values()

app$set_inputs(sidebarItemExpanded = "AssessDataQuality")
app$set_inputs(sidebarmenuid = "tabPDDE")
customDownload("downloadPDDEReport", "PDDE-Download")
app$expect_values()

app$set_inputs(sidebarmenuid = "tabDQSystem")
app$waitForShiny()
customDownload("downloadSystemDQReport", "System-DQ-Download")

app$set_inputs(sidebarmenuid = "tabDQOrg")
customDownload("downloadOrgDQReport", "Org-DQ-Download")

app$snapshot(items = list(export=TRUE), filename="AllExports.json")
})