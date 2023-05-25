docket_call <- function(urls, rate, sleep) {
  docket_frame <- data.frame(url = character(), text = character(), stringsAsFactors = FALSE)
  error_count <- 0
  url_counter <- 0

  for (i in seq(1, length(urls), by = rate)) {
    batch_urls <- urls[i:min(i+rate-1, length(urls))]
    for (j in seq_along(batch_urls)) {
      url <- batch_urls[j]
      try_count <- 0
      while (try_count < 6) {
        tryCatch({
          text <- gettxt(url, encoding = "UTF-8")
          docket_frame <- rbind(docket_frame, data.frame(url = url, text = text, stringsAsFactors = FALSE))
          error_count <- 0  # Reset error count if there was a successful retrieval
          url_counter <- url_counter + 1  # Increment the URL counter
          if (url_counter %% 500 == 0) {
            cat(paste("Completed Docket:", url_counter, "\n"))
          }
        }, error = function(e) {
          error_count <- error_count + 1
          if (error_count == 1) {
            cat(paste("Error occurred with URL:", url, "\n"))
            cat(paste("Waiting 1 minute, then trying again...\n"))
            Sys.sleep(60)  # Pause for 1 minute
            try_count <- try_count + 1
          } else if (error_count == 21) {
            cat(paste("Could Not Collect From URL:", url, "\n"))
            cat(paste("Stopping Function. Please Ensure Docket Number Written Correctly and Try Again\n"))
            break  # Exit the inner loop
          } else {
            cat(paste("Error occurred with URL:", url, "\n"))
            try_count <- try_count + 1
          }
        })
        if (error_count == 0) {
          break  # Exit the retry loop if there was a successful retrieval
        }
      }
    }
    if (error_count == 21) {
      break  # Exit the outer loop if there were too many errors
    }
    if (i+4999 < length(urls)) {
      cat("Pausing for 30 seconds...\n")
      Sys.sleep(30)
    }
  }

  return(docket_frame)
} #Call to Docket via Internet
