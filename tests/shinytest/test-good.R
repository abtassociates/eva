customDownload <- function(downloadHandler, fname) {
  app$snapshotDownload(downloadHandler, paste0(fname,".xlsx"))
  downloadedFile <- import_xlsx(paste0("test-good-current/",fname,".xlsx"))
  saveRDS(downloadedFile, paste0("test-good-current/",fname,".rds"))
  file.remove(paste0("test-good-current/",fname,".xlsx"))
}

import_xlsx <- function(fpath) {
  sheets <- excel_sheets(fpath)
  data <- lapply(sheets, read_xlsx, path = fpath) 
  return(data)
}

app <- ShinyDriver$new("../../", seed=1234, loadTimeout = 1e+05, checkNames = FALSE)
app$snapshotInit("test-good")

app$setInputs(Go_to_upload = "click")
app$uploadFile(imported = "../test_uploads/HMIS CSV Export - Current Good.zip") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$findElement("button[data-dismiss='modal']")$click()
Sys.sleep(2)
customDownload("downloadFileStructureAnalysis","File-Structure-Analysis-Download")
app$takeScreenshot("test-good-current/File-Structure-Analysis.png")

app$setInputs(sidebarmenuid = "tabClientCount")
app$waitForValue("clientCountData", iotype = "output", ignore = list(NULL))
customDownload("downloadClientCountsReport", "Client-Counts-Download")
app$takeScreenshot("test-good-current/tabClientCount.png")

app$setInputs(sidebarItemExpanded = "AssessDataQuality")
app$setInputs(sidebarmenuid = "tabPDDE")
customDownload("downloadPDDEReport", "PDDE-Download")
app$takeScreenshot("test-good-current/tabPDDE.png")

app$setInputs(sidebarmenuid = "tabDQSystem")
customDownload("downloadSystemDQReport", "System-DQ-Download")
app$takeScreenshot("test-good-current/tabDQSystem.png")

app$setInputs(sidebarmenuid = "tabDQOrg")
app$waitForShiny()
customDownload("downloadOrgDQReport", "Org-DQ-Download")
app$takeScreenshot("test-good-current/tabDQOrg.png")

app$snapshot(items = list(export=TRUE))
