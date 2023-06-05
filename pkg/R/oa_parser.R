
oa_parser <- function(dir_path = NULL) {
  cleaned_corpus <- suppressMessages({suppressWarnings(oa_corpus_cleaner(dir_path))})
  cleaned_frame <- suppressWarnings(oa_frame_cleaner(cleaned_corpus))
  message("\nCompletion Summary:\n")
  message(paste0("Total Arguments:", "  ", format(length(unique(cleaned_frame$argument)), big.mark = ","), "\n"))
  message(paste0("Total Statements:", "  ", format(length(cleaned_frame$text), big.mark = ","), "\n"))
  message(paste0("Number of Justices:", "  ", format(length(unique(cleaned_frame$speaker[cleaned_frame$speaker_type == "Justice" & cleaned_frame$speaker != "JUSTICE UNKNOWN"])), big.mark = ","), "\n"))
  message(paste0("Number of Unique Attorneys:", "  ", format(length(unique(cleaned_frame$speaker[cleaned_frame$speaker_type == "Attorney"])), big.mark = ","), "\n"))
  return(cleaned_frame)
}
