print("Running test-good")
app <- ShinyDriver$new("../../", seed = 1234, loadTimeout = 1e+04, shinyOptions = list(test.mode = TRUE))
source("../testing_functions.R", local = TRUE)

app$snapshotInit("test-good", screenshot = FALSE)
app$setInputs(Go_to_upload = "click")
app$uploadFile(imported = "../FY24-ICF-hashed-current-good.zip") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$findElement("button[data-dismiss='modal']")$click()
Sys.sleep(2)
customDownload("downloadFileStructureAnalysis","File-Structure-Analysis-Download")
app$snapshot()


app$setInputs(sidebarmenuid = "tabClientCount")
app$waitForShiny()
customDownload("downloadClientCountsReport", "Client-Counts-Download")
app$snapshot()

app$setInputs(sidebarItemExpanded = "AssessDataQuality")
app$setInputs(sidebarmenuid = "tabPDDE")
customDownload("downloadPDDEReport", "PDDE-Download")
app$snapshot()

app$setInputs(sidebarmenuid = "tabDQSystem")
app$waitForShiny()
customDownload("downloadSystemDQReport", "System-DQ-Download")

app$setInputs(sidebarmenuid = "tabDQOrg", timeout_=10000)
customDownload("downloadOrgDQReport", "Org-DQ-Download")

app$snapshot(items = list(export=TRUE), filename="AllExports.json")