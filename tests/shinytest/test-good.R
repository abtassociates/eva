customDownload <- function(downloadHandler, fname) {
  app$waitForShiny()
  print(paste("downloading",downloadHandler))
  app$snapshotDownload(downloadHandler, paste0(fname,".xlsx"))
  file.remove(paste0("test-good-current/",fname,".xlsx"))
}

app <- ShinyDriver$new("../../", seed = 1234, loadTimeout = 1e+04)
app$snapshotInit("test-good", screenshot = FALSE)

app$setInputs(Go_to_upload = "click")
app$uploadFile(imported = "../test_uploads/FY24-ICF-hashed.zip") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
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