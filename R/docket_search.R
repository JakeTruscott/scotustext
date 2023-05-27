docket_search <- function(docket_id, rate = 50, sleep = 30, include = NULL, exclude = NULL, write_csv = FALSE, write_xlsx = FALSE, write_reactable = FALSE) {
  start_time <- Sys.time()  # Measure the start time

  urls <- suppressWarnings(process_docket_id(docket_id))
  urls <- gsub(" ", "", urls)
  urls <- gsub("- ", "-", urls)
  urls <- gsub(" -", "-", urls)
  cat("\nDocket Sheets to Collect: ", length(unique(urls)), "\n")

  docket_entries <- NULL
  completed_count <- 0

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

      completed_count <- completed_count + 1

      if (completed_count %% 50 == 0) {
        message("Successfully Retrieved ", completed_count, " Docket Sheets")
      }
    }, error = function(e) {
      message("Error encountered during cleaning process for URL:", url, ". Saving Data Collection and Exporting.")
      if (!is.null(docket_entries)) {
        write_csv_func(docket_entries, write_csv)
        write_xlsx_func(docket_entries, write_xlsx)
        write_reactable_func(docket_entries, write_reactable)
      }
      return(docket_entries)
    })
  }

  end_time <- Sys.time()  # Measure the end time
  elapsed_time <- end_time - start_time  # Calculate the elapsed time
  elapsed_time <- round((elapsed_time/60), 2)

  cat("\n- - - - - - - - COMPLETION SUMMARY - - - - - - - -")
  cat("\nElapsed Time: ", elapsed_time, " Minutes")  # Display the elapsed time
  cat("\nTotal Docket Sheets Retrieved:", length(docket_entries$docket_number))
  cat("\nDocket Types Retrieved...")
  cat("\n   Docketed Petitions: ", length(docket_entries$petition_type[docket_entries$petition_type == "Docketed Case"]))
  cat("\n   Docketed Petitions (Original Jurisdiction): ", length(docket_entries$petition_type[docket_entries$petition_type == "Docketed Case - Original Jurisdiction"]))
  cat("\n   Applications: ", length(docket_entries$petition_type[docket_entries$petition_type == "Application"]))
  cat("\n   Motions: ", length(docket_entries$petition_type[docket_entries$petition_type == "Motion"]))

  return(docket_entries)
}
