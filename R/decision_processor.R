#' SCOTUS Decision Text Processor
#'
#' @param dir_path A directory path to a folder on your local machine containing decisions by the United States Supreme Court. *Note*: Files must be in PDF format.
#'
#' @return Parsed and cleaned text of decisions, separated by main text, footnotes, and opinion type, as well as opinion author and joinining justices (if any).
#' @export
#'
#' @examples
#' \dontrun{
#' #Replace "<Folder Directory>" with appropriate location.
#' sample_decision <- decision_processor(dir_path = "<Folder Directory>")
#' }
decision_processor <- function(dir_path) {
  files <- list.files(dir_path, full.names = TRUE)
  num_files <- length(files)
  cat("\nDecisions to Process: ", num_files)
  all_decisions <- NULL

  decisions_cleaner <- function(file_path, dir_path) {

    {
      rtext <- readtext::readtext(file_path)
      corpus <- Corpus(VectorSource(rtext$text))
      corpus <- tm_map(corpus, content_transformer(iconv), to = "ASCII//TRANSLIT")
      corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on writ of certiorari", replacement = "<DECISION BREAK> on writ of certiorari", ignore.case = TRUE)
      corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on petition for writ of certiorari", replacement = "<DECISION BREAK> on writ of certiorari", ignore.case = TRUE)
      corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on writ of certiorari", replacement = "<DECISION BREAK> <KEEP>", ignore.case = TRUE)
      corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on petition for writ of certiorari", replacement = "<DECISION BREAK> <KEEP>", ignore.case = TRUE)
      corpus <- tm_map(corpus, content_transformer(gsub), pattern = "certiorari to the (United States|Court|Supreme)", replacement = "<DECISION BREAK> <KEEP>", ignore.case = TRUE)
      corpus <- tm_map(corpus, content_transformer(gsub), pattern = "appeal from the united states district court", replacement = "<DECISION BREAK> <KEEP>", ignore.case = TRUE)
      corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on bill of complaint", replacement = "<DECISION BREAK> <KEEP>", ignore.case = TRUE)
      decisions <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
    } #Collect and Pre-Process Text

    # Check if 'decisions' data frame is empty
    if (nrow(decisions) == 0) {
      message("\nError Processing PDF:", file_path)
      return(NULL)
    }


    {
      file_names_processed <- gsub(paste0(dir_path, "/"), "", file_path)
      file_names_processed <- gsub("\\_.*", "", file_names_processed)
      file_names_processed <- gsub(".pdf", "", file_names_processed)
      decisions$argument <- file_names_processed

      case_names <- c()

      for (pdf_file in file_path) {
        pdf_metadata <- pdf_info(pdf_file)
        title <- pdf_metadata$keys$Title
        case_names <- c(case_names, title)
      }

      case_names <- data.frame(case_names)
      case_names$docket_number <- sapply(case_names$case_names, function(x) {
        if (grepl(" ", x) & grepl("-", x)) {
          return(sub(" .*", "", x))
        } else {
          return(NA)
        }
      })
      case_names$published <- sapply(case_names$case_names, function(x) {
        if (grepl("\\(", x)) {
          x <- sub(".*\\(|\\).*", "", x)
          x <- paste0("(", x)
          return(x)
        } else {
          return(NA)
        }
      })

      case_names$case_names <- mapply(function(case_name, published, docket_number) {
        if (!is.na(published)) {
          case_name <- gsub(published, "", case_name, fixed = TRUE)
        }
        if (!is.na(docket_number)) {
          case_name <- gsub(docket_number, "", case_name, fixed = TRUE)
        }
        return(case_name)
      }, case_names$case_names, case_names$published, case_names$docket_number)

      decisions$argument <- case_names$case_names
      decisions$docket_id <- case_names$docket_number
      decisions$docket_id <- gsub("\\.", "", decisions$docket_id)
      decisions$published <- case_names$published


    } #File Metadata
    {
      decisions_test <- decisions %>%
        mutate(text = str_split(text, "\\<DECISION BREAK\\>")) %>%
        unnest(text) %>%
        filter(grepl("\\<Keep\\>", text, ignore.case = TRUE)) %>%
        filter(!grepl("syllabus\\n\\n", text, ignore.case = TRUE)) %>%
        mutate(text = gsub("J.\\n\\n", "<END HEADER>", text, perl = TRUE)) %>%
        mutate(text = gsub("(?<!\\S) {2,}(?!\\S)", " ", text, perl = TRUE)) %>%
        mutate(text = gsub("(?<!\\S)\n(?!\\S)", " \n", text, perl = TRUE)) %>%
        mutate(text = gsub("\\n\\n", " \n\n ", text, perl = TRUE)) %>%
        mutate(text = gsub("\\n\\n ------\\n[ ]*", "<BEGIN FOOTNOTE> ", text)) %>%
        mutate(text = gsub("\\n------\\n", " \n------\n ", text, perl = TRUE)) %>%
        mutate(text = gsub("\\n------\\n", " <BEGIN FOOTNOTE> ", text, perl = TRUE)) %>%
        mutate(text = gsub("in part \\n\\n", "<END HEADER> ", text, ignore.case = T)) %>%
        mutate(text = gsub("in judgement \\n\\n", "<END HEADER> ", text, ignore.case = T)) %>%
        mutate(text = gsub("C\\.J\\.\\, concurring", "<END HEADER> ", text, ignore.case = T)) %>%
        mutate(text = gsub("J\\.\\, concurring", "<END HEADER> ", text, ignore.case = T)) %>%
        mutate(text = gsub("C\\.J\\.\\, dissenting", "<END HEADER> ", text, ignore.case = T)) %>%
        mutate(text = gsub("J\\.\\, dissenting", "<END HEADER> ", text, ignore.case = T)) %>%
        mutate(text = str_replace_all(text, "Opinion of the Court\\s+\\n\\n", "<END HEADER> ")) %>%
        mutate(text = str_replace_all(text, "Opinion of the Court", "<END HEADER> ")) %>%
        mutate(text = str_replace_all(text, "Opinion of .*? , J", "<END HEADER>")) %>%
        mutate(text = str_replace_all(text, "Order of (.*?), C\\.J\\.", "<END HEADER>")) %>%
        mutate(text = str_replace_all(text, "\\n\\n\\s+\\d+", "<BEGIN HEADER>")) %>%
        mutate(text = gsub("\\s{2,}", " ", text)) %>%
        mutate(text = str_replace_all(text, "Cite as:", " <BEGIN HEADER> Cite as:"))  %>%
        mutate(footnotes = text) %>%
        mutate(footnotes = str_extract_all(text, "(?<=<BEGIN FOOTNOTE>)([\\s\\S]*?)(?=<BEGIN HEADER>)")) %>%
        mutate(footnotes = sapply(footnotes, function(x) if (length(x) > 0) paste(x, collapse = "\n\n") else NA_character_))  %>%
        mutate(text = gsub("<BEGIN FOOTNOTE>.*?<END HEADER>", "", text)) %>%
        mutate(text = str_replace_all(text, "<BEGIN HEADER>.*?(<END HEADER>|<END SPECIAL HEADER>)", "")) %>%
        mutate(text = gsub("<BEGIN HEADER>.*?<END SPECIAL HEADER>", "", text)) %>%
        mutate(text = gsub(" C\\. J\\.\\,", "", text)) %>%
        mutate(text = gsub(" C\\.J\\.\\,", "", text)) %>%
        mutate(text = gsub(" J\\.\\,", "", text)) %>%
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
        mutate(footnotes = gsub("\\n\\n(?=\\[\\d{4,}\\])", "", footnotes, perl = TRUE)) %>%
        mutate(footnotes = str_replace(footnotes, "^(\\d+)", "[\\1]")) %>%
        mutate(footnotes = gsub("\\- ", "", footnotes)) %>%
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
          grepl("deliv", opinion, ignore.case = T) & grepl("court", opinion, ignore.case = T) ~ "Majority Opinion",
          grepl("dis", opinion) ~ "Dissent",
          grepl("conc", opinion) ~ "Concurrence",
          grepl("conc", opinion) & grepl("dis", opinion) & grepl("in part", opinion) ~ "Concur & Dissent (In Part)")) %>%
        mutate(opinion_type = ifelse(is.na(opinion_type), "Per Curiam", opinion_type))  %>%
        filter(!(opinion_type == "Per Curiam" & !grepl("PER CURIAM", text, ignore.case = TRUE))) %>%
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
        mutate(word_count = str_count(text, "\\w+")) %>%
        mutate(text = gsub("\\.(?!.*\\..*$)\\s?.*", ".", text, perl = TRUE)) %>%
        mutate(text = gsub("\\<END HEADER\\>", "", text, perl = T)) %>%
        mutate(text = gsub("\\<BEGIN HEADER\\>", "", text, perl = T)) %>%
        mutate(text = gsub("\\,\\)", ", J.)", text, perl = T)) %>%
        mutate(text = gsub("\\, \\)", ", J.)", text, perl = T)) %>%
        select(argument, docket_id, published, text, footnotes, opinion_writer, opinion_type, word_count)
    } #Process & Clean Docket Frame

  } #Process Opinions

  footnotes <- NULL
  opinion <- NULL
  opinion_type <- NULL
  opinion_writer <- NULL
  argument <- NULL
  docket_id <- NULL
  published <- NULL


  start_time <- Sys.time()
  count <- 1
  for (i in files) {
    tryCatch({
      cleaned_decisions <- suppressWarnings(decisions_cleaner(file_path = i, dir_path = i))
      all_decisions <- rbind(all_decisions, cleaned_decisions)

      if (count %% 25 == 0) {
        message("\nCompleted ", count, "Decisions of", length(files))
      }

      count <- count + 1
    }, error = function(e) {
      cat("Failure with Decision ", i, "...Moving On\n")
    })
  }

  end_time <- Sys.time()
  elapsed_time <- end_time - start_time

  cat("\n- - - - - - - - COMPLETION SUMMARY - - - - - - - -")
  cat("\nCompletion Time: ", round(as.numeric(elapsed_time), 2), "Seconds")
  cat("\nNumber of Unique Decisions: ", length(unique(all_decisions$argument)))
  cat("\nNumber of Majority Opinions: ", sum(all_decisions$opinion_type == "Majority Opinion"))
  cat("\nNumber of Dissents: ", sum(all_decisions$opinion_type == "Dissent"))
  cat("\nNumber of Concurrences: ", sum(all_decisions$opinion_type == "Concurrence"))
  cat("\nNumber of Per Curiam: ", sum(all_decisions$opinion_type == "Per Curiam"))

  return(all_decisions)
}




