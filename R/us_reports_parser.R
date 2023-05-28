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

decision_cleaner <- function(dir_path){

  {

    dir_path = "C:/Users/Jake Truscott/OneDrive - University of Georgia/Active/scotustext/us_reports_test"
    directory <- trimws(dir_path)
    file_directory <- paste0(directory, "/")
    file_names <- list.files(file_directory)
    total_files <- length(file_names)
    dir_path <- trimws(dir_path)
    dir_path <- paste0(dir_path, "/*")
    rtext <- readtext::readtext(dir_path)
  } #File Path
  {
    corpus <- Corpus(VectorSource(rtext$text))
    corpus <- tm_map(corpus, content_transformer(iconv), to = "ASCII//TRANSLIT")
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = ".*Cases Adjudged", replacement = "", ignore.case = T)
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on writ of certiorari", replacement = "<DECISION BREAK> <KEEP>", ignore.case = T)
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on petition for writ of certiorari", replacement = "<DECISION BREAK> <KEEP>", ignore.case = T)
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "certiorari to the (United States|Court|Supreme)", replacement = "<DECISION BREAK> <KEEP>", ignore.case = T)
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "appeal from the united states district court", replacement = "<DECISION BREAK> <KEEP>", ignore.case = T)
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on bill of complaint", replacement = "<DECISION BREAK> <KEEP>", ignore.case = T)

    decisions <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE) # Convert Corpus to Dataframe

  } #Corpus Transformation
  {
    file_names <- list.files(directory)
    file_names_processed <- gsub("\\_.*", "", file_names)
    file_names_processed <- gsub(".pdf", "", file_names_processed)
    file_names_processed <- data.frame(file_names_processed)
    names(file_names_processed)[1] <- "file_names"
    decisions$argument <- file_names_processed$file_names

  } #File Metadata

  {
    decisions_test <- decisions %>%
      group_by(argument) %>%
      mutate(text = str_split(text, "\\<DECISION BREAK\\>")) %>%
      unnest(text) %>%
      filter(grepl("<KEEP>", text, ignore.case = TRUE)) %>%
      mutate(text = gsub("<KEEP>", "", text)) %>%
      mutate(text = trimws(text)) %>%
      mutate(text = gsub("\\n\\n", " \n\n ", text, perl = TRUE)) %>%
      mutate(text = gsub("\\n\\n ------\\n[ ]*", "<BEGIN FOOTNOTE> ", text)) %>%
      mutate(text = gsub("\\n------\\n", " \n------\n ", text, perl = TRUE)) %>%
      mutate(text = gsub("\\n------\\n", " <BEGIN FOOTNOTE> ", text, perl = TRUE)) %>%
      mutate(text = str_replace_all(text, "J., dissenting\\s+\\n\\n", "J., dissenting \n\n <END HEADER> ")) %>%
      mutate(text = str_replace_all(text, "J., concurring\\s+\\n\\n", "J., dissenting \n\n <END HEADER> ")) %>%
      mutate(text = str_replace_all(text, "Opinion of the Court\\s+\\n\\n", "J., dissenting \n\n <END HEADER> ")) %>%
      mutate(text = str_replace_all(text, "\\n\\n\\s+\\d+", "<BEGIN HEADER>")) %>%
      mutate(text = str_replace_all(text, "Cite as:", " <BEGIN HEADER> Cite as:"))  %>%
      mutate(footnotes = text) %>%
      mutate(footnotes = str_extract_all(text, "(?<=<BEGIN FOOTNOTE>)([\\s\\S]*?)(?=<BEGIN HEADER>)")) %>%
      mutate(footnotes = sapply(footnotes, function(x) if (length(x) > 0) paste(x, collapse = "\n\n") else NA_character_)) %>%
      mutate(text = gsub("<BEGIN FOOTNOTE>.*?<END HEADER>", "", text)) %>%
      mutate(text = gsub("<BEGIN HEADER>.*?<END HEADER>", "", text)) %>%
      mutate(footnotes = gsub("\\n\\n {3,}(\\d+)", "<FOOTNOTE BREAK> \\1", footnotes)) %>%
      mutate(footnotes = gsub("\\n\\n", "", footnotes)) %>%
      mutate(footnotes = gsub("<FOOTNOTE BREAK>", "\n\n", footnotes)) %>%
      mutate(footnotes = gsub("\\-  ", "", footnotes)) %>%
      mutate(footnotes = gsub("\\- ", "", footnotes)) %>%
      mutate(footnotes = gsub("\\n", " ", footnotes)) %>%
      mutate(footnotes = gsub("  (\\d+)", "\n\n\\1", footnotes)) %>%
      mutate(footnotes = gsub("(?<![A-Za-z0-9\\n])\\s{5,}(?![A-Za-z0-9\\n])", " ", footnotes, perl = TRUE)) %>%
      mutate(footnotes = gsub("^\\s+", "", footnotes)) %>%
      mutate(footnotes = gsub("(?<=\\n)(\\d+)", "[\\1]", footnotes, perl = TRUE)) %>%
      mutate(footnotes = gsub("\\n\\n(?=\\[\\d{4,}\\])", "", footnotes, perl = TRUE))%>%
      mutate(footnotes = str_replace(footnotes, "^(\\d+)", "[\\1]")) %>%
      mutate(text = gsub("\\n(?=\\d)", " [", text, perl = TRUE)) %>%
      mutate(text = gsub("\\s(?=\\d)", "] ", text, perl = TRUE)) %>%
      mutate(text = gsub("\\s(?=\\n)", "] ", text, perl = TRUE)) %>%
      mutate(text = gsub("\\[(?=\\d)", "", text, perl = TRUE)) %>%
      mutate(text = gsub("\\](?=\\n)", "", text, perl = TRUE)) %>%
      mutate(text = gsub("\\](?=\\s)", "", text, perl = TRUE)) %>%
      mutate(opinion = sub("\\.(.*)", "", text)) %>%
      mutate(opinion = sub("\\.(.*)", "", text)) %>%
      mutate(opinion = trimws(opinion) %>%
               paste0(".")) %>%
      mutate(opinion_writer = str_extract_all(opinion, "(CHIEF JUSTICE|JUSTICE)\\s+(\\w+)") %>%
               sapply(paste, collapse = "; ") %>%
               sapply(function(x) gsub(" joins| join", "", x))) %>%
      mutate(opinion_type = case_when(
        grepl("delivered", opinion, ignore.case = T) & grepl("court", opinion, ignore.case = T) ~ "Majority Opinion",
        grepl("dissenting", opinion) ~ "Dissent",
        grepl("concurring", opinion) ~ "Concurrence",
        grepl("concurrence", opinion) & grepl("dissenting", opinion) & grepl("in part", opinion) ~ "Concur & Dissent (In Part)")) %>%
      mutate(opinion_type = ifelse(is.na(opinion_type), "Per Curiam", opinion_type))  %>%
      mutate(text = ifelse(opinion_type == "Per Curiam", gsub(".*PER CURIAM", "", text), text)) %>%
      mutate(opinion_writer = ifelse(opinion_type == "Per Curiam", "Per Curiam", opinion_writer)) %>%
      mutate(text = sub(".*?\\.", "", text)) %>%
      mutate(text = trimws(text)) %>%
      mutate(text = gsub("U\\.\\sS\\.\\sC\\.\\s\\?", "U. S. C. \u00A7", text)) %>%
      mutate(text = gsub("\\?(?=\\d)", "\u00A7", text, perl = T)) %>%
      mutate(text = gsub("\\s{2,}", " ", text)) %>%
      mutate(text = gsub("SUPREME COURT OF THE UNITED STATES.*", "", text)) %>%
      mutate(text = gsub("-\\n", "", text)) %>%
      mutate(text = gsub("\\n", " ", text)) %>%
      select(-c(opinion)) %>%
      mutate(case_number = gsub("\\..*", "", text))



  } #Process Opinions

}
