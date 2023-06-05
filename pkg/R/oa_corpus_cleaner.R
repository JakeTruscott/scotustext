oa_corpus_cleaner <- function(dir_path) {
  directory <- trimws(dir_path)
  file_directory <- paste0(directory, "/")

  file_names <- list.files(file_directory)
  total_files <- length(file_names)
  cat("Total Number of Transcripts to be Processed: ", format(total_files, big.mark = ","), "\n")

  dir_path <- trimws(dir_path)
  dir_path <- paste0(dir_path, "/*")
  rtext <- readtext::readtext(dir_path)
  corpus <- Corpus(VectorSource(rtext$text))

  patterns <- c("\\s\\b\\d+\\b\\s", "\n", "Heritage Reporting Corporation",
                "C O N T E N T S", "P R O C E E D I N G S", "Official ORAL ARGUMENT",
                "Official", "PAGE")
  replacements <- c(" ", "", "", "", "", "", "", "")

  for (i in seq_along(patterns)) {
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = patterns[i], replacement = replacements[i])
  }

  corpus <- tm_map(corpus, content_transformer(sub), pattern = ".*?\n", replacement = "")
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(iconv), to = "ASCII//TRANSLIT")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "-", replacement = "-")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "case is submitted.*", replacement = "case is submitted", ignore.case = TRUE)
  corpus <- tm_map(corpus, content_transformer(gsub),
                   pattern = "^(.*?)(CHIEF JUSTICE ROBERTS:|CHIEF JUSTICE REHNQUIST:)",
                   replacement = "\\2", ignore.case = TRUE)

  scotus <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE) # Convert Corpus to Dataframe
  scotus$text <- iconv(scotus$text, to = "UTF-8")

  file_names_processed <- c()  # Initialize character vector for processed file names

  for (file_name in file_names) {
    file_names_processed <- c(file_names_processed, file_name)
  }

  # Process the file names at the end
  file_names_processed <- gsub("\\_.*", "", file_names_processed)
  file_names_processed <- gsub(".pdf", "", file_names_processed)
  file_names_processed <- data.frame(file_names_processed)
  names(file_names_processed)[1] <- "file_names"
  scotus$argument <- file_names_processed$file_names

  return(scotus)
}
