#' SCOTUS Docket Search
#'
#' @param docket_id Character or comma-separated vector of docket ID numbers (e.g., "19-1392")
#' @param rate Rate of docket processing (default is **50**)
#' @param sleep Seconds to wait between docket requests (default is **30**)
#' @param include Character vector to limit the output variables to only those requested (default is **NULL**).
#' @param exclude Character vector that will exclude any output variables requested (default is **NULL**)
#' @param write_csv Logical value indicating whether to print code to convert the output dataframe to a .csv file (default is **FALSE**)
#' @param write_xlsx Logical value indicating whether to print code to convert the output dataframe to a excel file (default is **FALSE**)
#' @param write_reactable Logical value indicating whether to print code to convert the output dataframe to a .html file using `reactable()` (default is **FALSE**).
#'
#' @return Parsed Supreme Court Docket Information
#' @export
#'
#' @examples
#' docket_search(docket_id = "19-1392")
docket_search <- function(docket_id, rate = 50, sleep = 30, include = NULL, exclude = NULL, write_csv = FALSE, write_xlsx = FALSE, write_reactable = FALSE) {
  start_time <- Sys.time()
  urls <- suppressWarnings(process_docket_id(docket_id))
  urls <- gsub(" ", "", urls)
  urls <- gsub("- ", "-", urls)
  urls <- gsub(" -", "-", urls)
  cat("\nDocket Sheets to Collect: ", length(unique(urls)), "\n")

  docket_entries <- NULL
  success_count <- 0

  for (url in urls) {
    tryCatch({
      docket_frame <- docket_call(url, rate, sleep)

      docket_frame$text <- gsub("\\xe2\\x80\\xa2", " ", docket_frame$text)
      docket_frame$text <- trimws(docket_frame$text)

      docket_entities <- suppressWarnings(clean_docket_frame(docket_frame = docket_frame))

      if (!is.null(include)) {
        docket_entities <- include_func(docket_entities, include)
      }

      if (!is.null(exclude)) {
        docket_entities <- exclude_func(docket_entities, exclude)
      }

      if (is.null(docket_entries))
        docket_entries <- docket_entities
      else
        docket_entries <- rbind(docket_entries, docket_entities)

      write_csv_func(docket_entities, write_csv)
      write_xlsx_func(docket_entities, write_xlsx)
      write_reactable_func(docket_entries, write_reactable)

      success_count <- success_count + 1
      if (success_count %% 50 == 0) {
        message("Successfully Collected ", success_count, " Docket Sheets")
      }
    }, error = function(e) {
      message("Error encountered during cleaning process for URL:", url, ". Saving Data Collection and Exporting.")
      if (!is.null(docket_entries)) {
        write_csv_func(docket_entries, write_csv)
        write_xlsx_func(docket_entries, write_xlsx)
        write_reactable_func(docket_entries, write_reactable)
      }
      return()
    })
  }

  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  message("\nSuccess!")
  cat("\n - - - - - - - Summary - - - - - - - ")
  cat("\nCompletion Time:", round(elapsed_time, 2), " Seconds")
  cat("\nDocket Sheets Collected:", length(unique(docket_entries$docket_number)))
  cat("\nBreakdown:")
  cat("\n Docketed Petitions:", length(docket_entries$petition_type[docket_entries$petition_type == "Docketed Case"]))
  cat("\n Applications:", length(docket_entries$petition_type[docket_entries$petition_type == "Application"]))
  cat("\n Motions:", length(docket_entries$petition_type[docket_entries$petition_type == "Motion"]))


  return(docket_entries)
}
