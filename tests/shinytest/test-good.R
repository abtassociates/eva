customDownload <- function(downloadHandler, fname) {
  # this allows us to trigger the download code, which thus exports the test values
  # as specified in server.R (exportTestValues())
  # but then we delete the xlsx file so they don't get compared
  # xlsx files seem to retain metadata that changes with each run and thus
  # the two files are not identical in GitHub Actions, even though identical() says they are
  # Also, RStudio can't view binary file comparisons interactively the way it can
  # with other sorts of files
  # I had tried saving as RDS, but that was having the same comparibility issue 
  # with GitHubActions.
  
  app$snapshotDownload(downloadHandler, paste0(fname,".xlsx"))
  #downloadedFile <- import_xlsx(paste0("test-good-current/",fname,".xlsx"))
  #saveRDS(downloadedFile, paste0("test-good-current/",fname,".rds"))
  file.remove(paste0("test-good-current/",fname,".xlsx"))
}

# import_xlsx <- function(fpath) {
#   sheets <- excel_sheets(fpath)
#   data <- lapply(sheets, read_xlsx, path = fpath) 
#   return(data)
# }

app <- ShinyDriver$new("../../", seed=1234, loadTimeout = 1e+05, checkNames = FALSE)
app$snapshotInit("test-good")

app$setInputs(Go_to_upload = "click")
app$uploadFile(imported = "../test_uploads/HMIS CSV Export - Current Good.zip") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$findElement("button[data-dismiss='modal']")$click()
Sys.sleep(2)
customDownload("downloadFileStructureAnalysis","File-Structure-Analysis-Download")
# without specifying anything here, it will download numerically 
# incrementing .json and .png files. We will only compare JSON, not png, since
# png the plots seem to load slightly differently each time which can throw off
# exact comparisons
app$snapshot() 


app$setInputs(sidebarmenuid = "tabClientCount")
app$waitForValue("clientCountData", iotype = "output", ignore = list(NULL))
customDownload("downloadClientCountsReport", "Client-Counts-Download")
app$snapshot()

app$setInputs(sidebarItemExpanded = "AssessDataQuality")
app$setInputs(sidebarmenuid = "tabPDDE")
customDownload("downloadPDDEReport", "PDDE-Download")
app$snapshot()

app$setInputs(sidebarmenuid = "tabDQSystem")
customDownload("downloadSystemDQReport", "System-DQ-Download")

# waitForValue doesn't seem to work here
app$setInputs(sidebarmenuid = "tabDQOrg", timeout_=10000)
customDownload("downloadOrgDQReport", "Org-DQ-Download")

app$snapshot(items = list(export=TRUE), filename="AllExports.json")