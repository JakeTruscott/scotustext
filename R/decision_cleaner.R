importFrom("tm", "Corpus", "VectorSource", "tm_map", "content_transformer")
importFrom("dplyr", "%>%", "group_by", "mutate")
importFrom("stringr", "str_split", "str_replace_all", "str_extract_all")


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
      if (grepl(" ", x)) {
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
    decisions$published <- case_names$published


  } #File Metadata
  {
    decisions_test <- decisions %>%
      mutate(text = str_split(text, "\\<DECISION BREAK\\>")) %>%
      unnest(text) %>%
      filter(grepl("\\<Keep\\>", text, ignore.case = TRUE)) %>%
      filter(!grepl("syllabus\\n\\n", text, ignore.case = TRUE)) %>%
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
      select(argument, docket_id, published, text, footnotes, opinion_writer, opinion_type)
  } #Process & Clean Docket Frame

} #Process Opinions
