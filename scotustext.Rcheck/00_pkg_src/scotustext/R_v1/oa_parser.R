#' SCOTUS Oral Argument Parser
#'
#' @param dir_path Folder directory path to a folder on your local machine containing oral argument transcripts from the United States Supreme Court. *Note*: Files must be in PDF format.
#'
#' @return Parsed and cleaned text of oral argument transcripts, separated by speaker, text/statement, term, speaker type (i.e., Justice or Attorney), argument (case title), argument docket number, and word count.
#' @export
#'
#' @examples
#' \dontrun{
#'   #Replace "<Folder Directory>" with appropriate location.
#'   sample_oral_argument <- oa_parser(dir_path = "<Folder Directory>")
#' }
oa_parser <- function(dir_path = NULL) {

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
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "the case was submitted.*", replacement = "the case was submitted", ignore.case = TRUE)

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

  oa_frame_cleaner <- function(cleaned_corpus){
    scotus <- cleaned_corpus %>%
      group_by(argument) %>%
      mutate(text = str_split(text, "(?=\\bCHIEF JUSTICE\\b|(?<!CHIEF )JUSTICE|MR\\.|MS\\.|GENERAL|QUESTION:)")) %>%
      unnest(text) %>%
      filter(!is.na(text)) %>%
      separate(text, c("justice", "text"), sep = ": ", extra = "merge") %>%
      mutate(text = gsub("::", ":", text)) %>%
      mutate(justice = gsub(":", "", justice)) %>%
      mutate(justice = trimws(justice)) %>%
      filter(!grepl("[^A-Za-z.' ]", justice)) %>%
      filter(!is.na(text)) %>%
      mutate(justice = trimws(justice)) %>%
      group_by(argument) %>%
      filter(grepl("JUSTICE|MR.|MS.|QUESTION:|GENERAL", justice, ignore.case = F)) %>%
      mutate(id = row_number()) %>%
      mutate(text = gsub(" ORAL ARGUMENT OF.*","", text)) %>%
      mutate(text = gsub(" REBUTTAL ARGUMENT OF.*","", text)) %>%
      mutate(speaker_type = ifelse(grepl("JUSTICE", justice), "Justice", "Attorney")) %>%
      mutate(text = gsub("- Subject to Final Review", "", text, ignore.case = T)) %>%
      mutate(text = gsub(" Subject to Final Review", "", text, ignore.case = T)) %>%
      mutate(text = gsub("Alderson Reporting Company", "", text, ignore.case = T)) %>%
      mutate(case_name = ifelse(id == 1, text, NA)) %>%
      mutate(case_name = gsub(".*case|.*argument in|.*argument", "", case_name, ignore.case = T)) %>%
      mutate(case_name = gsub("^[^,]+,\\s*", "", case_name)) %>%
      mutate(case_name = ifelse(grepl("^ the\\s", case_name), sub("^ the\\s", "", case_name), case_name)) %>%
      mutate(case_name = ifelse(grepl("^the\\s", case_name), sub("^the\\s", "", case_name), case_name)) %>%
      mutate(case_name = gsub("\\. Mr.*|\\. Ms.*|\\. General.*", "", case_name, ignore.case = T )) %>%
      mutate(case_name = na.locf(case_name)) %>%
      mutate(case_name = gsub("versus|vs\\.", "v.", case_name)) %>%
      mutate(case_name = gsub("against", "v.", case_name)) %>%
      mutate(word_count = str_count(text, "\\w+")) %>%
      mutate(justice_fix = case_when(
        grepl("JUSTICE ROB", justice) & speaker_type == "Justice" ~ "CHIEF JUSTICE ROBERTS",
        grepl("JUSTICE GI", justice) & speaker_type == "Justice" ~ "JUSTICE GINSBURG",
        grepl("JUSTICE A", justice) & speaker_type == "Justice" ~ "JUSTICE ALITO",
        grepl("JUSTICE BR", justice) & speaker_type == "Justice" ~ "JUSTICE BREYER",
        grepl("JUSTICE KE", justice) & speaker_type == "Justice" ~ "JUSTICE KENNEDY",
        grepl("JUSTICE SC", justice) & speaker_type == "Justice" ~ "JUSTICE SCALIA",
        grepl("JUSTICE SOU", justice) & speaker_type == "Justice" ~ "JUSTICE SOUTER",
        grepl("JUSTICE ST", justice) & speaker_type == "Justice" ~ "JUSTICE STEVENS",
        grepl("JUSTICE SOT", justice) & speaker_type == "Justice" ~ "JUSTICE SOTOMAYOR",
        grepl("JUSTICE KAG", justice) & speaker_type == "Justice" ~ "JUSTICE KAGAN",
        grepl("JUSTICE T", justice) & speaker_type == "Justice" ~ "JUSTICE THOMAS",
        grepl("JUSTICE BA", justice) & speaker_type == "Justice" ~ "JUSTICE BARRETT",
        grepl("JUSTICE J", justice) & speaker_type == "Justice" ~ "JUSTICE JACKSON",
        grepl("JUSTICE KAV", justice) & speaker_type == "Justice" ~ "JUSTICE KAVANAUGH",
        grepl("JUSTICE GO", justice) & speaker_type == "Justice" ~ "JUSTICE GORSUCH"
      )) %>%
      mutate(justice = ifelse(is.na(justice_fix), justice, justice_fix)) %>%
      mutate(justice = ifelse(justice == "JUSTICE" & speaker_type == "Justice", "JUSTICE UNKNOWN", justice)) %>%
      mutate(justice = ifelse(justice == "CHIEF JUSTICE" & speaker_type == "Justice", "CHIEF JUSTICE ROBERTS", justice)) %>%
      mutate(justice = ifelse(justice == "JUSTICE PHILLIPS" & speaker_type == "Justice", "JUSTICE KENNEDY", justice)) %>%
      select(-c(justice_fix)) %>%
      mutate(justice = gsub(".*ON BEHALF", "", justice, ignore.case = T)) %>%
      mutate(text = gsub("Alderson Court Reporting 1", "", text)) %>%
      mutate(text = gsub("Alderson Court Reporting", "", text)) %>%
      mutate(speaker = justice) %>%
      select(speaker, text, argument, id, speaker_type, case_name, word_count)


    return(scotus)
  }

  argument <- NULL
  justice <- NULL
  id <- NULL
  case_name <- NULL
  justice_fix <- NULL
  speaker_type <- NULL
  speaker <- NULL
  word_count <- NULL
  argument <- NULL

  cleaned_corpus <- suppressMessages({suppressWarnings(oa_corpus_cleaner(dir_path))})
  cleaned_frame <- suppressWarnings(oa_frame_cleaner(cleaned_corpus))

  cat("\n- - - - - - - - COMPLETION SUMMARY - - - - - - - -")
  cat("\nTotal Arguments:", format(length(unique(cleaned_frame$argument)), big.mark = ","))
  cat("\nTotal Statements:", format(length(cleaned_frame$text), big.mark = ","))
  cat("\nNumber of Justices:", format(length(unique(cleaned_frame$speaker[cleaned_frame$speaker_type == "Justice" & cleaned_frame$speaker != "JUSTICE UNKNOWN"]))))
  cat("\nNumber of Unique Attorneys:", format(length(unique(cleaned_frame$speaker[cleaned_frame$speaker_type == "Attorney"]))))

  return(cleaned_frame)
}


