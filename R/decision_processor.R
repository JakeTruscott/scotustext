decision_processor <- function(dir_path) {
  files <- list.files(dir_path, full.names = TRUE)
  num_files <- length(files)
  cat("\nDecisions to Process: ", num_files)
  all_decisions <- NULL

  start_time <- Sys.time()
  for (i in files) {
    cleaned_decisions <- suppressWarnings(decisions_cleaner(file_path = i, dir_path = i))
    all_decisions <- rbind(all_decisions, cleaned_decisions)
  }

  end_time <- Sys.time()
  elapsed_time <- end_time - start_time

  cat("\nSuccess!")
  cat("\n - - - - - - - Summary - - - - - - - ")
  cat("\nCompletion Time: ", round(as.numeric(elapsed_time), 2), "Seconds")
  cat("\nNumber of Unique Opinions: ", length(unique(all_decisions$argument)))
  cat("\nNumber of Majority Opinions: ", sum(all_decisions$opinion_type == "Majority Opinion"))
  cat("\nNumber of Dissents: ", sum(all_decisions$opinion_type == "Dissent"))
  cat("\nNumber of Concurrences: ", sum(all_decisions$opinion_type == "Concurrence"))
  cat("\nNumber of Per Curiam: ", sum(all_decisions$opinion_type == "Per Curiam"))

  return(all_decisions)
}
