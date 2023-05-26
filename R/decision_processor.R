decision_processor <- function(dir_path){
  cat("\nDecisions to Process: ", length(list.files(dir_path)))
  start_time <- Sys.time()
  cleaned_decisions <- suppressWarnings(decision_cleaner(dir_path = dir_path))
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  cat("\nSuccess!")
  cat("\n - - - - - - - Summary - - - - - - - ")
  cat("\nCompletion Time: ", round(as.numeric(elapsed_time), 2), "Seconds")
  cat("\nNumber of Unique Opinions: ", length(cleaned_decisions$argument))
  cat("\nNumber of Majority Opinions", length(cleaned_decisions$opinion_type[cleaned_decisions$opinion_type == "Majority Opinion"]))
  cat("\nNumber of Dissents:", length(cleaned_decisions$opinion_type[cleaned_decisions$opinion_type == "Dissent"]))
  cat("\nNumber of Concurrences:", length(cleaned_decisions$opinion_type[cleaned_decisions$opinion_type == "Concurrence"]))
  cat("\nNumber of Per Curiam:", length(cleaned_decisions$opinion_type[cleaned_decisions$opinion_type == "Per Curiam"]))

  return(cleaned_decisions)
}
