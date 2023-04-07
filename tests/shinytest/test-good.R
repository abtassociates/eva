# customDownload <- function(downloadHandler, fname) {
#   app$snapshotDownload(downloadHandler, paste0(fname,".xlsx"))
#   downloadedFile <- import_xlsx(paste0("test-good-current/",fname,".xlsx"))
#   saveRDS(downloadedFile, paste0("test-good-current/",fname,".rds"))
#   file.remove(paste0("test-good-current/",fname,".xlsx"))
# }

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
app$takeScreenshot("test-good-expected/File-Structure-Analysis.png")

app$setInputs(sidebarmenuid = "tabClientCount")
app$waitForValue("clientCountData", iotype = "output", ignore = list(NULL))
app$takeScreenshot("test-good-expected/tabClientCount.png")

app$setInputs(sidebarItemExpanded = "AssessDataQuality")
app$setInputs(sidebarmenuid = "tabPDDE")
app$takeScreenshot("test-good-expected/tabPDDE.png")

app$setInputs(sidebarmenuid = "tabDQSystem")

app$setInputs(sidebarmenuid = "tabDQOrg", timeout_=10000)

app$snapshot(items = list(export=TRUE))
