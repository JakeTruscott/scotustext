write_csv_func <- function(docket_entries, write_csv) {
  if (write_csv) {
    cat("Code to Save Dataframe as (.csv):\n")
    cat("library(utils)\n")
    cat("write.csv(<SEARCH QUERY OBJECT NAME>, file = '<SAVE FILE>.csv', row.names = FALSE)\n")
  }
}
