call_docket_data <- function(docket_id) {
  base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
  rdata_url <- paste0(base_url, "dockets_archive.rdata")
  load(url(rdata_url))

  subset_dockets <- dockets_archive[dockets_archive$docket_number %in% docket_id, ]

  if (nrow(subset_dockets) == 0) {
    archive_collect <- data.frame()  # Empty dataframe
  } else {
    archive_collect <- subset_dockets
  }

  return(archive_collect)
}
