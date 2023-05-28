docket_search <- function(docket_id, rate = 1000, sleep = 30, include = NULL, exclude = NULL, write_csv = FALSE, write_xlsx = FALSE, write_reactable = FALSE) {

  urls <- suppressWarnings(process_docket_id(docket_id))
  urls <- gsub(" ", "", urls)
  urls <- gsub("- ", "-", urls)
  urls <- gsub(" -", "-", urls)
  cat("\nDocket Sheets to Collect: ", length(unique(urls)), "\n")

  docket_entries <- NULL
  completed_count <- 0
  error_urls <- list()

  for (url in urls) {
    error_count <- 0

    repeat {
      tryCatch({
        docket_frame <- suppressWarnings(docket_call(url, rate, sleep))

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

        break  # Exit the repeat loop if successful
      }, error = function(e) {
        error_count <- error_count + 1
        message(paste("Error Cleaning Docket Information from URL", url))
        if (error_count > 5) {
          error_urls <- c(error_urls, url)
          message("Maximum Number of Errors Reached: Moving on to Next")
          break  # Exit the repeat loop if maximum errors reached
        } else {
          message("Waiting 30 seconds then trying again")
          Sys.sleep(30)
        }
      })
    }
  }

  cat("\n- - - - - - - - COMPLETION SUMMARY - - - - - - - -")
  cat("\nTotal Docket Sheets Retrieved:", length(docket_entries$docket_number))
  cat("\nDocket Types Retrieved...")
  cat("\n   Docketed Petitions: ", length(docket_entries$petition_type[docket_entries$petition_type == "Docketed Case"]))
  cat("\n   Docketed Petitions (Original Jurisdiction): ", length(docket_entries$petition_type[docket_entries$petition_type == "Docketed Case - Original Jurisdiction"]))
  cat("\n   Applications: ", length(docket_entries$petition_type[docket_entries$petition_type == "Application"]))
  cat("\n   Motions: ", length(docket_entries$petition_type[docket_entries$petition_type == "Motion"]))
  if (length(error_urls) > 0) {
    errors_url <- gsub(".*(/Public/)", "", errors_url, ignore.case = TRUE)
    errors_url <- gsub(".*docketfiles\\/", "", errors_url )
    errors_url <- gsub(".html", "", errors_url)
    errors_url <- gsub(".htm", "", errors_url)
    cat("\nSummary: Failed to Retrieve Docket Information for:\n")
    cat(error_urls, sep = ", ")
  }
  return(docket_entries)
}
