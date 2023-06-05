write_xlsx_func <- function(docket_entries, write_xlsx) {
  if (write_xlsx) {
    cat("Code to Save Dataframe as (.xlsx):\n")
    cat("library(writexl)\n")
    cat("write_xlsx(<SEARCH QUERY OBJECT NAME>, path = '<SAVE FILE>.xlsx')\n")
  }
}
