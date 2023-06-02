docket_search <- function(docket_id, rate = 5000, sleep = 30, include = NULL, exclude = NULL, write_csv = FALSE, write_xlsx = FALSE, write_reactable = FALSE) {
  urls <- suppressWarnings(process_docket_id(docket_id))
  urls <- gsub(" ", "", urls)
  urls <- gsub("- ", "-", urls)
  urls <- gsub(" -", "-", urls)
  cat("\nDocket Sheets to Collect: ", length(unique(urls)), "\n")

  docket_entries <- NULL
  completed_count <- 0
  final_urls <- c()
  retry_urls <- c()
  failed_urls <- c()
  non_collect <- c()

  process_url <- function(url, docket_entries) {
    docket_frame <- suppressWarnings(docket_call(url, sleep))
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

    completed_count <<- completed_count + 1

    if (completed_count %% 50 == 0) {
      message("Successfully Retrieved ", completed_count, " Docket Sheets")
    }

    return(docket_entries)
  }

  for (url in urls) {
    tryCatch({
      docket_entries <- process_url(url, docket_entries)
    }, error = function(e) {
      docket_number <- gsub(".*docketfiles\\/", "", url)
      docket_number <- gsub(".*(/Public/)", "", docket_number, ignore.case = TRUE)
      docket_number <- gsub("\\.html", "", docket_number)
      docket_number <- gsub("\\.htm", "", docket_number)
      message(paste("Error Retrieving and Cleaning Docket Information from Docket ID:", docket_number))
      retry_urls <<- c(retry_urls, url)
      message("Saving URL Information and Trying Again Later")
      #message("Waiting 10 Seconds Before Moving On...")
      #Sys.sleep(10)
      #cat("Resuming Collection Effort\n")
    })
  }

  if (length(retry_urls) > 0) {
    docket_url_newer <- "https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/"
    docket_url_older <- "https://www.supremecourt.gov/search.aspx?filename=/docketfiles/"
    for (retry_url in retry_urls) {
      message("\nBeginning Second Attempt at Processing...\n")
      if (grepl("\\.html", retry_url)) {
        docket_number <- gsub(".*(/Public/)", "", retry_url, ignore.case = TRUE)
        docket_number <- gsub("\\.html", "", docket_number)
        docket_year <- gsub("\\-.*", "", docket_number)
        docket_year <- as.numeric(docket_year)
        if (as.numeric(docket_year) == 16) {
          retry_url <- paste0(docket_url_older, docket_number, ".htm")
        } else {
          retry_url <- paste0(docket_url_newer, docket_number, ".html")
        }
      } else {
        docket_number <- gsub(".*docketfiles\\/", "", retry_url)
        docket_number <- gsub("\\.htm", "", docket_number)
        docket_year <- gsub("\\-.*", "", docket_number)
        docket_year <- as.numeric(docket_year)
        if (as.numeric(docket_year) == 16) {
          retry_url <- paste0(docket_url_newer, docket_number, ".html")
        } else {
          retry_url <- paste0(docket_url_older, docket_number, ".htm")
        }
      }

      tryCatch({
        docket_entries <- process_url(retry_url, docket_entries)
      }, error = function(e) {
        message(paste("Error In Second Attempt -- Adjusting HTTP Conditions & Trying Again Later ( Docket ID:", docket_number, ")"))
        final_urls <<- c(final_urls, retry_url)
      })
    }
  }

  if (length(final_urls) > 0) {
    docket_url_newer <- "https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/"
    docket_url_older <- "https://www.supremecourt.gov/search.aspx?filename=/docketfiles/"
    for (final_url in final_urls) {
      if (grepl("\\.html", final_url)) {
        docket_number <- gsub(".*(/Public/)", "", final_url, ignore.case = TRUE)
        docket_number <- gsub("\\.html", "", docket_number)
        final_url <- paste0(docket_url_older, docket_number, ".htm")
      } else {
        docket_number <- gsub(".*docketfiles\\/", "", final_url)
        docket_number <- gsub("\\.htm", "", docket_number)
        final_url <- paste0(docket_url_newer, docket_number, ".html")
      }

      tryCatch({
        docket_entries <- process_url(final_url, docket_entries)
      }, error = function(e) {
        message(paste("Error In Final Attempt -- Saving For Completion Report ( Docket ID:", docket_number, ")"))
        non_collect <<- c(non_collect, final_url)
      })
    }
  }


  cat("\n- - - - - - - - COMPLETION SUMMARY - - - - - - - -")
  cat("\nTotal Docket Sheets Successfully Retrieved:", length(docket_entries$docket_number))
  cat("\nTotal Docket Sheets Failed to Retrieve:", length(failed_urls))
  cat("\nDocket Types Retrieved...")
  cat("\n   Docketed Petitions: ", length(docket_entries$petition_type[docket_entries$petition_type == "Docketed Case"]))
  cat("\n   Docketed Petitions (Original Jurisdiction): ", length(docket_entries$petition_type[docket_entries$petition_type == "Docketed Case - Original Jurisdiction"]))
  cat("\n   Applications: ", length(docket_entries$petition_type[docket_entries$petition_type == "Application"]))
  cat("\n   Motions: ", length(docket_entries$petition_type[docket_entries$petition_type == "Motion"]))

  if (length(non_collect) > 0) {
    non_collect <- gsub(".*(/Public/)", "", non_collect, ignore.case = TRUE)
    non_collect <- gsub(".*docketfiles\\/", "", non_collect)
    non_collect <- gsub(".html", "", non_collect)
    non_collect <- gsub(".htm", "", non_collect)
    cat("\nFailed to Retrieve Docket Information for: ")
    cat(non_collect, sep = ifelse(length(non_collect) > 1, ", ", ""))
  }

  return(docket_entries)
}
