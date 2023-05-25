{
  dir_path = "C:/Users/Jake Truscott/OneDrive - University of Georgia/Active/Justices Oral Argument Rhetoric/SCOTUS_Transcripts/decision_sample"
  directory <- trimws(dir_path)
  file_directory <- paste0(directory, "/")

  file_names <- list.files(file_directory)
  total_files <- length(file_names)


  dir_path <- trimws(dir_path)
  dir_path <- paste0(dir_path, "/*")
  rtext <- readtext::readtext(dir_path)

  corpus <- Corpus(VectorSource(rtext$text))
  corpus <- tm_map(corpus, content_transformer(iconv), to = "ASCII//TRANSLIT")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on writ of certiorari", replacement = "<DECISION BREAK> on writ of certiorari", ignore.case = T)
  remove_spaces_after_pattern <- function(x) {
    gsub("\\n------\\n[ ]{2}(\\d)", "\n------\n\\1", x)
  }
  corpus <- tm_map(corpus, content_transformer(remove_spaces_after_pattern))
  remove_text_between_patterns <- function(x) {
    gsub("\\n------\\n.*?Opinion of the Court\\n", "", x)
  }
  corpus <- tm_map(corpus, content_transformer(remove_text_between_patterns))
  remove_text_between_patterns <- function(x) {
    gsub("\\n------\\n.*?dissenting\\n", "", x, ignore.case = T)
  }
  corpus <- tm_map(corpus, content_transformer(remove_text_between_patterns))
  remove_text_between_patterns <- function(x) {
    gsub("\\n------\\n.*?concurring\\n", "", x, ignore.case = T)
  }
  corpus <- tm_map(corpus, content_transformer(remove_text_between_patterns))
  remove_text_between_phrases <- function(x) {
    gsub("\\s*Cite as:[^\n]*\n\n.*?\n\n", " ", x, perl = TRUE)
  } #Removing between "\n\n" and "\n\n" so long as "Cite as:" is present
  corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
  remove_text_between_phrases <- function(x) {
    gsub("\n\n(.*?Opinion of the Court.*?)\n\n\n\n\n", " ", x, perl = TRUE)
  } #Remove any text between "\n\n" and the first instance of "\n\n\n\n\n", so long as "Opinion of the Court" appears within it.
  corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
  remove_text_between_phrases <- function(x) {
    gsub("\n\n------\n(.*?Opinion of the Court.*?)\n\n", " ", x, perl = TRUE)
  } #Remove anything between "\n\n------\n" and "\n\n\n\n\n", so long as "v." is between them.
  corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
  remove_text_between_phrases <- function(x) {
    gsub("\n------\n(.*?v\\.*?)\n\n\n\n\n", " ", x, perl = TRUE)
  } #Remove anything between "\n------\n" and "\n\n\n\n\n", so long as "v." is between them.
  corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
  corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\-\n\n", replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\-\n", replacement = "")
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("\n\n", " <CUT> ", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("- ", "", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("-\n", "", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("U\\.\\sS\\.\\sC\\.\\s\\?", "U. S. C. \u00A7", x)))
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\?(?=\\d)", "\u00A7", x, perl = TRUE)))
  remove_text_between_phrases <- function(x) {
    gsub("------\n.*?\n", " ", x, perl = TRUE)
  } #Remove anything between "\n------\n" and "\n"
  corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
  remove_cut_text <- function(x) {
    gsub("<CUT> \\d+.*?<CUT>", "", x)
  }
  corpus <- tm_map(corpus, content_transformer(remove_cut_text))
  #corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  replace_between_cuts <- function(text){
    pattern = "\n------\n  "
    replacement = "\n------\n"
    gsub(pattern, replacement, text, perl = TRUE)
  }
  corpus <- tm_map(corpus, content_transformer(remove_cut_text))

  decisions <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE) # Convert Corpus to Dataframe

  # Process the file names at the end
  file_names <- list.files(directory)
  file_names_processed <- gsub("\\_.*", "", file_names)
  file_names_processed <- gsub(".pdf", "", file_names_processed)
  file_names_processed <- data.frame(file_names_processed)
  names(file_names_processed)[1] <- "file_names"
  decisions$argument <- file_names_processed$file_names


  decisions_test <- decisions %>%
    group_by(argument) %>%
    mutate(text = str_split(text, "\\<DECISION BREAK\\>")) %>%
    unnest(text) %>%
    filter(grepl("on writ of certiorari", text, ignore.case = T)) %>%
    mutate(text = sub(".*?(<CUT>)", "", text)) %>%
    mutate(opinion = sub("\\.(.*)", "", text)) %>%
    mutate(opinion = trimws(opinion) %>%
             paste0(".")) %>%
    mutate(opinion_writer = str_extract_all(opinion, "(CHIEF JUSTICE|JUSTICE)\\s+(\\w+)") %>%
             sapply(paste, collapse = "; ") %>%
             sapply(function(x) gsub(" joins| join", "", x))) %>%
    mutate(text = sapply(seq_along(opinion), function(i) gsub(opinion[i], "", text[i])) %>%
             trimws()) %>%
    mutate(opinion_type = case_when(
      grepl("opinion of the court", opinion, ignore.case = T) ~ "Majority Opinion",
      grepl("dissent", opinion, ignore.case = T) ~ "Dissent",
      grepl("concur", opinion, ignore.case = T) ~ "Concurrence")) %>%
    mutate(text = gsub("It is so ordered\\.(.*<CUT>).*", "It is so ordered.", text, ignore.case = T, perl = T)) %>%
    mutate(text = gsub("Affirmed\\.(.*<CUT>).*", "Affirmed.", text, ignore.case = T)) %>%
    mutate(text = gsub("reversed\\.(.*<CUT>).*", "Reversed.", text, ignore.case = T, perl = T)) %>%
    mutate(text = gsub("<CUT>  <CUT> ", "<CUT>", text)) %>%
    mutate(text = gsub("   ", "<LONG BREAK>", text)) %>%
    mutate(text = gsub("\n", " ", text)) %>%
    mutate(text = gsub("<LONG BREAK><LONG BREAK>", "<LONG BREAK>", text)) %>%
    mutate(text = gsub("<LONG BREAK>", "\n", text)) %>%
    mutate(text = gsub("\n", "\n\n", text)) %>%
    mutate(text = gsub(" {3,}", "\n\n", text)) %>%
    mutate(text = gsub("\n{3,}", "\n\n", text)) %>%
    mutate(text = gsub("<CUT>", " ", text)) %>%
    mutate(text = gsub("\\.([0-9])", ".", text)) %>%
    mutate(text = gsub(" {3,}", "\n\n", text)) %>%
    mutate(text = gsub("\n{2,}", "\n", text)) %>%
    mutate(text = gsub(" SUPREME COURT OF THE UNITED STATES.*", "", text, ignore.case = F))

} #Clean Decision Parser Code


dir_path = "C:/Users/Jake Truscott/OneDrive - University of Georgia/Active/Justices Oral Argument Rhetoric/SCOTUS_Transcripts/decision_sample"
directory <- trimws(dir_path)
file_directory <- paste0(directory, "/")

file_names <- list.files(file_directory)
total_files <- length(file_names)


dir_path <- trimws(dir_path)
dir_path <- paste0(dir_path, "/*")
rtext <- readtext::readtext(dir_path)

corpus <- Corpus(VectorSource(rtext$text))
corpus <- tm_map(corpus, content_transformer(iconv), to = "ASCII//TRANSLIT")
corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on writ of certiorari", replacement = "<DECISION BREAK> on writ of certiorari", ignore.case = T)
remove_spaces_after_pattern <- function(x) {
  gsub("\\n------\\n[ ]{2}(\\d)", "\n------\n\\1", x)
}
corpus <- tm_map(corpus, content_transformer(remove_spaces_after_pattern))
remove_text_between_patterns <- function(x) {
  gsub("\\n------\\n.*?Opinion of the Court\\n", "", x)
}
corpus <- tm_map(corpus, content_transformer(remove_text_between_patterns))
remove_text_between_patterns <- function(x) {
  gsub("\\n------\\n.*?dissenting\\n", "", x, ignore.case = T)
}
corpus <- tm_map(corpus, content_transformer(remove_text_between_patterns))
remove_text_between_patterns <- function(x) {
  gsub("\\n------\\n.*?concurring\\n", "", x, ignore.case = T)
}
corpus <- tm_map(corpus, content_transformer(remove_text_between_patterns))
remove_text_between_phrases <- function(x) {
  gsub("\\s*Cite as:[^\n]*\n\n.*?\n\n", " ", x, perl = TRUE)
} #Removing between "\n\n" and "\n\n" so long as "Cite as:" is present
corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
remove_text_between_phrases <- function(x) {
  gsub("\n\n(.*?Opinion of the Court.*?)\n\n\n\n\n", " ", x, perl = TRUE)
} #Remove any text between "\n\n" and the first instance of "\n\n\n\n\n", so long as "Opinion of the Court" appears within it.
corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
remove_text_between_phrases <- function(x) {
  gsub("\n\n------\n(.*?Opinion of the Court.*?)\n\n", " ", x, perl = TRUE)
} #Remove anything between "\n\n------\n" and "\n\n\n\n\n", so long as "v." is between them.
corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
remove_text_between_phrases <- function(x) {
  gsub("\n------\n(.*?v\\.*?)\n\n\n\n\n", " ", x, perl = TRUE)
} #Remove anything between "\n------\n" and "\n\n\n\n\n", so long as "v." is between them.
corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\-\n\n", replacement = "")
corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\-\n", replacement = "")
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\n\n", " <CUT> ", x)))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("- ", "", x)))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("-\n", "", x)))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("U\\.\\sS\\.\\sC\\.\\s\\?", "U. S. C. \u00A7", x)))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\?(?=\\d)", "\u00A7", x, perl = TRUE)))
remove_text_between_phrases <- function(x) {
  gsub("------\n.*?\n", " ", x, perl = TRUE)
} #Remove anything between "\n------\n" and "\n"
corpus <- tm_map(corpus, content_transformer(remove_text_between_phrases))
remove_cut_text <- function(x) {
  gsub("<CUT> \\d+.*?<CUT>", "", x)
}
corpus <- tm_map(corpus, content_transformer(remove_cut_text))
#corpus <- tm_map(corpus, content_transformer(stripWhitespace))
replace_between_cuts <- function(text){
  pattern = "\n------\n  "
  replacement = "\n------\n"
  gsub(pattern, replacement, text, perl = TRUE)
}
corpus <- tm_map(corpus, content_transformer(remove_cut_text))

decisions <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE) # Convert Corpus to Dataframe

# Process the file names at the end
file_names <- list.files(directory)
file_names_processed <- gsub("\\_.*", "", file_names)
file_names_processed <- gsub(".pdf", "", file_names_processed)
file_names_processed <- data.frame(file_names_processed)
names(file_names_processed)[1] <- "file_names"
decisions$argument <- file_names_processed$file_names


decisions_test <- decisions %>%
  group_by(argument) %>%
  mutate(text = str_split(text, "\\<DECISION BREAK\\>")) %>%
  unnest(text) %>%
  filter(grepl("on writ of certiorari", text, ignore.case = T)) %>%
  mutate(text = sub(".*?(<CUT>)", "", text)) %>%
  mutate(opinion = sub("\\.(.*)", "", text)) %>%
  mutate(opinion = trimws(opinion) %>%
           paste0(".")) %>%
  mutate(opinion_writer = str_extract_all(opinion, "(CHIEF JUSTICE|JUSTICE)\\s+(\\w+)") %>%
           sapply(paste, collapse = "; ") %>%
           sapply(function(x) gsub(" joins| join", "", x))) %>%
  mutate(text = sapply(seq_along(opinion), function(i) gsub(opinion[i], "", text[i])) %>%
           trimws()) %>%
  mutate(opinion_type = case_when(
    grepl("opinion of the court", opinion, ignore.case = T) ~ "Majority Opinion",
    grepl("dissent", opinion, ignore.case = T) ~ "Dissent",
    grepl("concur", opinion, ignore.case = T) ~ "Concurrence")) %>%
  mutate(text = gsub("It is so ordered\\.(.*<CUT>).*", "It is so ordered.", text, ignore.case = T, perl = T)) %>%
  mutate(text = gsub("Affirmed\\.(.*<CUT>).*", "Affirmed.", text, ignore.case = T)) %>%
  mutate(text = gsub("reversed\\.(.*<CUT>).*", "Reversed.", text, ignore.case = T, perl = T)) %>%
  mutate(text = gsub("<CUT>  <CUT> ", "<CUT>", text)) %>%
  mutate(text = gsub("   ", "<LONG BREAK>", text)) %>%
  mutate(text = gsub("\n", " ", text)) %>%
  mutate(text = gsub("<LONG BREAK><LONG BREAK>", "<LONG BREAK>", text)) %>%
  mutate(text = gsub("<LONG BREAK>", "\n", text)) %>%
  mutate(text = gsub("\n", "\n\n", text)) %>%
  mutate(text = gsub(" {3,}", "\n\n", text)) %>%
  mutate(text = gsub("\n{3,}", "\n\n", text)) %>%
  mutate(text = gsub("<CUT>", " ", text)) %>%
  mutate(text = gsub("\\.([0-9])", ".", text)) %>%
  mutate(text = gsub(" {3,}", "\n\n", text)) %>%
  mutate(text = gsub("\n{2,}", "\n", text)) %>%
  mutate(text = gsub(" SUPREME COURT OF THE UNITED STATES.*", "", text, ignore.case = F))


cat(decisions_test$text[3])
