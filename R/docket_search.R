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
  urls <- suppressWarnings(process_docket_id(docket_id))
  urls <- gsub(" ", "", urls)
  urls <- gsub("- ", "-", urls)
  urls <- gsub(" -", "-", urls)
  cat("\nDocket Sheets to Collect: ", length(unique(urls)), "\n")

  docket_frame <- docket_call(urls, rate, sleep)
  cat("\nCompleted Docket Collection\nMoving to Parsing and Cleaning\n")

  docket_frame$text <- gsub("\\xe2\\x80\\xa2", " ", docket_frame$text)
  docket_frame$text <- trimws(docket_frame$text)

  tryCatch({
    docket_entities <- suppressWarnings(clean_docket_frame(docket_frame = docket_frame))

    if (!is.null(include)) {
      docket_entities <- include_func(docket_entities, include)
    }

    if (!is.null(exclude)) {
      docket_entities <- exclude_func(docket_entities, exclude)
    }

    write_csv_func(docket_entities, write_csv)
    write_xlsx_func(docket_entities, write_xlsx)
    write_reactable_func(docket_entries, write_reactable)

    message("\nSuccess!")
  }, error = function(e) {
    message("Error Encountered During Cleaning Process: Saving Data Collection and Exporting.")
    return(docket_frame)
  })

  return(docket_entities)

} # Primary Docket Search
