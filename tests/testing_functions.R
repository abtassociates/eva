customDownload <- function(downloadHandler, fname) {
  app$waitForShiny()
  print(paste("downloading",downloadHandler))
  app$snapshotDownload(downloadHandler, paste0(fname,".xlsx"))
  file.remove(paste0("test-good-current/",fname,".xlsx"))
}