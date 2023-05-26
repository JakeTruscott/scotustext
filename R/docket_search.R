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
