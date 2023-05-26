clean_docket_frame <- function(docket_frame, include, exclude){

  {
    corpus <- tm::Corpus(VectorSource(docket_frame[["text"]]))
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\xe2\\x80\\xa2", replacement = " ")
    corpus <- tm_map(corpus, stripWhitespace)
    docket_entries <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE) #Convert Corpus to Dataframe
    removeWordsWithBackslash <- function(inputString) {
      outputString <- gsub("\\S*\\\\\\\\\\S*\\s?", "", inputString)
      outputString <- iconv(outputString, "UTF-8", "ASCII", sub="")
      return(outputString)}
    docket_entries$text <- sapply(docket_entries$text, removeWordsWithBackslash)
  } #Base Clean (Corpus -> Toss "\\")
  {
    docket_entries$docket_id <- docket_frame$url
    docket_entries$docket_url <- docket_frame$url
    docket_entries$docket_id <- gsub(".*(/Public/)", "", docket_entries$docket_id, ignore.case = TRUE)
    docket_entries$docket_id <- gsub(".*docketfiles\\/", "", docket_entries$docket_id )
    docket_entries$docket_id <- gsub(".html", "", docket_entries$docket_id)
    docket_entries$docket_id <- gsub(".htm", "", docket_entries$docket_id)
    docket_entries$docket_number <- docket_entries$docket_id
    docket_entries$text_original <- docket_entries$text
    docket_entries <- separate(docket_entries, col = text, into = c("toss", "text"), sep = "Title: ", extra = "merge")
    docket_entries <- docket_entries %>% select(-c(toss))
    docket_entries <- docket_entries %>%
      mutate(text_original = text) %>%
      select(text_original, docket_id, docket_url, docket_number, text)
    docket_entries$text <- gsub("In Re Grand Jury", "In Re Grand Jury, Petitioner ", docket_entries$text)
    docket_entries <- separate(docket_entries, col = text, into = c("petitioner", "text"), sep = ", (Petitioner|Plaintiff|Petioners|Plaintiffs|Appellant|Appellants|In Re|Applicant|Applicants)", extra = "merge")
    docket_entries <- separate(docket_entries, col = text, into = c("respondent", "text"), sep = "Docketed: ", extra = "merge")
    docket_entries$respondent <- gsub("\n", " ", docket_entries$respondent)
    docket_entries$respondent <- gsub(".*v. ", "", docket_entries$respondent)
    docket_entries$respondent <- paste0(" ", docket_entries$respondent)
    docket_entries$respondent <- gsub(" s ", "", docket_entries$respondent)
    docket_entries$respondent <- trimws(docket_entries$respondent)
    docket_entries$petitioner <- gsub("\\*\\*\\* CAPITAL CASE \\*\\*\\*", "", docket_entries$petitioner)
    docket_entries$petitioner <- trimws(docket_entries$petitioner)

    docket_entries <- separate(docket_entries, col = text, into = c("docketed", "text"), sep = "(Lower Ct: |~Date)", extra = "merge")
    docket_entries <- separate(docket_entries, col = text, into = c("lower_court", "text"), sep = "(Case Numbers: |Case Nos.: )", extra = "merge")
    docket_entries <- separate(docket_entries, col = text, into = c("lower_court_case_number", "text"), sep = "Decision Date: ", extra = "merge")
    docket_entries <- separate(docket_entries, col = text, into = c("lower_court_decision_date", "text"), sep = " ", extra = "merge")
    docket_entries <- separate(docket_entries, col = text, into = c("lower_court_decision_date2", "text"), sep = " ", extra = "merge")
    docket_entries <- separate(docket_entries, col = text, into = c("lower_court_decision_date3", "text"), sep = " ", extra = "merge")
    docket_entries$lower_court_decision_date <- paste0(docket_entries$lower_court_decision_date, " ", docket_entries$lower_court_decision_date2, " ", docket_entries$lower_court_decision_date3)
    docket_entries <- docket_entries %>% select(-c(lower_court_decision_date2, lower_court_decision_date3, docket_id))
    docket_entries <- separate(docket_entries, col = text, into = c("lower_court_rehearing_denied", "text"), sep = "Proceedings and Orders ", extra = "merge")
    docket_entries$lower_court_rehearing_denied <- gsub("Rehearing Denied: ", "", docket_entries$lower_court_rehearing_denied)
    docket_entries$lower_court_rehearing_denied <- gsub("Date", "", docket_entries$lower_court_rehearing_denied)
    docket_entries$lower_court_rehearing_denied<- gsub("^([^ ]+ [^ ]+ [^ ]+).*", "\\1", docket_entries$lower_court_rehearing_denied)
    docket_entries$lower_court_rehearing_denied <- ifelse(docket_entries$lower_court_rehearing_denied == "Discretionary Court Decision", "", docket_entries$lower_court_rehearing_denied)
    docket_entries$lower_court_rehearing_denied <- ifelse(docket_entries$lower_court_rehearing_denied == "Court Decision :", "", docket_entries$lower_court_rehearing_denied)
    docket_entries$lower_court_decision_date <- ifelse(docket_entries$lower_court_decision_date == "Rehearing Denied: Discretionary", "", docket_entries$lower_court_decision_date)
    docket_entries$lower_court_decision_date <- ifelse(docket_entries$lower_court_decision_date == "Rehearing Denied:", "", docket_entries$lower_court_decision_date)
    docket_entries$file_type <- ifelse(grepl("in forma pauperis", docket_entries$text), "IFP", "Paid")
    #docket_entries$amicus_indicator <- ifelse(grepl("brief amic", docket_entries$text), "Amici Filed", "No Amici")
    docket_entries$text2<- iconv(docket_entries$text_original, to = "UTF-8")
    #docket_entries$amicus_count <- str_count(docket_entries$text_original, "brief amic[i|us]")
    docket_entries <- separate(docket_entries, col = text2, into = c("pre_conference", "post_conference"), sep = "DISTRIBUTED for Conference of ", extra = "drop", fill = "right", convert = FALSE)
    # docket_entries$amicus_pre_merits_count <- str_count(docket_entries$pre_conference, "brief amic[i|us]")
    docket_entries <- separate(docket_entries, col = post_conference, into = c("conference_date", "toss"), sep = "\\.(?=\\s)", extra = "drop", fill = "right", convert = FALSE )
    docket_entries <- docket_entries %>% select(-c(toss, pre_conference))

    docket_entries$conference_text <- iconv(docket_entries$text_original, to = "UTF-8")
    docket_entries$conference_count <- str_count(docket_entries$conference_text, "DISTRIBUTED for Conference")

    docket_entries <- docket_entries %>%
      select(-c(conference_text))

    docket_entries$certiorari <- ifelse(grepl("Petition DENIED", docket_entries$text_original), "Denied", "Granted")

    docket_entries <- docket_entries %>%
      mutate(special_filing = ifelse(
        grepl("mandamus", text_original, ignore.case = T), "Mandamus", ifelse(grepl("habeas", text_original, ignore.case = T), "Habeas", ifelse(grepl("capital case", text_original, ignore.case = T), "Capital Case", ifelse(grepl("o", docket_number, ignore.case = T), "Original Jurisdiction", NA)))))

    docket_entries$rehearing_filed <- ifelse(grepl("Rehearing filed", docket_entries$text_original), "Rehearing Filed", "No Rehearing Petition")
    docket_entries$rehearing_order <- ifelse(docket_entries$rehearing_filed == "No Rehearing Petition", " ", ifelse(grepl("Rehearing DENIED", docket_entries$text_original), "Rehearing Denied", "Rehearing Granted"))
    docket_entries$text <- docket_entries$text_original
    docket_entries <- separate(docket_entries, col = text, into = c("text", "pc"), sep = "Attorneys for (Petitioner|Plaintiff|Petioners|Plaintiff)", extra = "merge")
    docket_entries <- separate(docket_entries, col = pc, into = c("petitioner_counsel", "text"), sep = "Party ", extra = "merge")
    docket_entries$petitioner_counsel <- paste0(" ", docket_entries$petitioner_counsel)
    docket_entries$petitioner_counsel <- gsub(" s ", "", docket_entries$petitioner_counsel)
    docket_entries$petitioner_counsel <- trimws(docket_entries$petitioner_counsel)
    docket_entries$petitioner_counsel <- gsub("Counsel of Record ", "", docket_entries$petitioner_counsel)
    docket_entries$petitioner_counsel <- trimws(docket_entries$petitioner_counsel)

    docket_entries$respondent_counsel <- ifelse(grepl("for Respondent", docket_entries$text), gsub(".*(for Respondent)", "", docket_entries$text, ignore.case = TRUE), "" )
    docket_entries <- separate(docket_entries, col = respondent_counsel, into = c("respondent_counsel", "text"), sep = " Party ", extra = "merge")
    docket_entries$respondent_counsel <- paste0(" ", docket_entries$respondent_counsel)
    docket_entries$respondent_counsel <- gsub(" s ", "", docket_entries$respondent_counsel)
    docket_entries$respondent_counsel <- trimws(docket_entries$respondent_counsel)
    docket_entries$respondent_counsel <- gsub("Counsel of Record ", "", docket_entries$respondent_counsel)
    docket_entries$respondent_counsel <- trimws(docket_entries$respondent_counsel)

    docket_entries <- docket_entries %>%
      mutate(type = case_when(
        grepl("a", docket_number, ignore.case = T) ~ "Application",
        grepl("m", docket_number, ignore.case = T) ~ "Motion",
        grepl("-", docket_number, ignore.case = T) ~ "Docketed Case",
        grepl("o", docket_number, ignore.case = T) ~ "Docketed Case"
      )) %>%
      filter(!is.na(text_original))

    docket_entries$lower_court_rehearing_denied[grep("~", docket_entries$lower_court_rehearing_denied)] <- ""
    docket_entries$lower_court_rehearing_denied[grep("=", docket_entries$lower_court_rehearing_denied)] <- ""
    docket_entries$lower_court_rehearing_denied[grep("<", docket_entries$lower_court_rehearing_denied)] <- ""
    docket_entries$lower_court_rehearing_denied[grep(">", docket_entries$lower_court_rehearing_denied)] <- ""

    docket_entries <- docket_entries %>%
      mutate(linked_with = ifelse(grepl("linked with", docketed, ignore.case = T), gsub(".*Linked with ", "", docketed, ignore.case = T), "")) %>%
      mutate(docketed = ifelse(grepl("linked with", docketed, ignore.case = T), gsub(" Linked with.*", "", docketed, ignore.case = T), docketed))

    docket_entries <- docket_entries %>%
      select(-c(text))

  } #Main Cleaning and Parsing
  {
    months <- c("Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ", "Aug ", "Sep ", "Oct ", "Nov ", "Dec ")

    for (i in 1:nrow(docket_entries)) {
      original_text <- docket_entries$text_original[i]

      for (month in months) {
        original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
      }

      docket_entries$amicus_check[i] <- iconv(original_text, to = "UTF-8", sub = "")
      encoded_string <- docket_entries$amicus_check[i]

      split_rows <- strsplit(encoded_string, "\n###\n")
      filtered_rows <- lapply(split_rows, function(row) {
        has_amicis_curiae_brief <- grepl("brief amici curiae|brief amicus curiae", row, ignore.case = TRUE)
        row[has_amicis_curiae_brief]
      })
      cleaned_rows <- lapply(filtered_rows, function(row) {
        row <- iconv(row, to = "UTF-8", sub = "")
        row <- str_remove(row, ".*amici curiae of |.*amicus curiae of ")
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        str_remove(row, " filed\\..*")
      })
      nested_list <- lapply(cleaned_rows, function(row) list(row))

      replace_empty <- function(lst) {
        modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", "", x))
        return(modified_lst)
      }

      nested_list <- replace_empty(nested_list)

      docket_entries$amicus_filers[i] <- nested_list
      docket_entries <- docket_entries %>%
        mutate(amicus_filers = sapply(amicus_filers, function(x) paste(unlist(x), collapse = "; "))) %>%
        mutate(amicus_indicator = ifelse(is.na(amicus_filers), 0, 1)) %>%
        mutate(amicus_count = sapply(docket_entries$amicus_filers, function(x) length(unlist(x))))
    }
  } #Amicus Filings
  {
    months <- c("Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ", "Aug ", "Sep ", "Oct ", "Nov ", "Dec ")

    for (i in 1:nrow(docket_entries)) {
      original_text <- docket_entries$text_original[i]

      for (month in months) {
        original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
      }

      docket_entries$argument_check[i] <- original_text
    }

    encoded_string <- docket_entries$argument_check
    split_rows <- strsplit(encoded_string, "\n###\n")
    filtered_rows <- lapply(split_rows, function(row) {
      has_argument <- grepl("Argued\\.", row, ignore.case = TRUE)
      row[has_argument]
    })
    cleaned_rows <- lapply(filtered_rows, function(row) {
      row <- iconv(row, to = "UTF-8", sub = "")
      row <- str_remove(row, " Argued\\..*")
    })
    nested_list <- lapply(cleaned_rows, function(row) paste(row, collapse = ", "))

    docket_entries$argument_date <- nested_list
    docket_entries <- docket_entries %>%
      mutate(argument_date = sapply(argument_date, function(x) paste(x, collapse = "; ")))


  } #Oral Argument Date
  {
    months <- c("Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ", "Aug ", "Sep ", "Oct ", "Nov ", "Dec ")

    for (i in 1:nrow(docket_entries)) {
      original_text <- docket_entries$text_original[i]

      for (month in months) {
        original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
      }

      docket_entries$multiple_conference_check[i] <- original_text
    }

    split_rows <- strsplit(docket_entries$multiple_conference_check, "\n###\n")
    filtered_rows <- lapply(split_rows, function(row) {
      has_multiple_conference <- grepl("DISTRIBUTED for Conference", row, ignore.case = TRUE)
      row[has_multiple_conference]
    })
    cleaned_rows <- lapply(filtered_rows, function(row) {
      str_remove(row, ".*DISTRIBUTED for Conference of ")
    })
    cleaned_rows <- lapply(cleaned_rows, function(row) {
      str_remove(row, "\\..*")
    })
    nested_list <- lapply(cleaned_rows, function(row) list(row))

    replace_empty <- function(lst) {
      modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", "", x))
      return(modified_lst)
    }

    nested_list <- replace_empty(nested_list)

    docket_entries$multiple_conference_dates <- nested_list
    docket_entries <- docket_entries %>%
      mutate(multiple_conference_dates = sapply(multiple_conference_dates, function(x) paste(unlist(x), collapse = "; ")))


  } #Multiple Conference Dates
  {
    months <- c("Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ", "Aug ", "Sep ", "Oct ", "Nov ", "Dec ")

    for (i in 1:nrow(docket_entries)) {
      original_text <- docket_entries$text_original[i]

      for (month in months) {
        original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
      }

      docket_entries$opinion_writer_check[i] <- original_text
    }

    split_rows <- strsplit(docket_entries$opinion_writer_check, "\n###\n")
    filtered_rows <- lapply(split_rows, function(row) {
      has_opinion_writer <- grepl("delivered the opinion", row, ignore.case = TRUE)
      row[has_opinion_writer]
    })
    cleaned_rows <- lapply(filtered_rows, function(row) {
      str_remove(row, " delivered the opinion.*")
    })
    cleaned_rows <- lapply(cleaned_rows, function(row) {
      gsub(", J\\.,", "", row)
    })
    cleaned_rows <- lapply(cleaned_rows, function(row) {
      gsub(", C\\. J\\.,", "", row)
    })
    cleaned_rows <- lapply(cleaned_rows, function(row) {
      str_remove(row, ".*\\. ")
    })
    nested_list <- lapply(cleaned_rows, function(row) paste(row, collapse = ", "))

    docket_entries$majority_opinion_writer <- nested_list

    docket_entries <- docket_entries %>%
      mutate(majority_opinion_writer = sapply(majority_opinion_writer, function(x) paste(x, collapse = "; "))) %>%
      mutate(majority_opinion_writer = ifelse(grepl("opinion per curiam", text_original, ignore.case = TRUE), "Per Curiam", majority_opinion_writer))

  } #Majority Opinion Writer
  {
    for (i in 1:nrow(docket_entries)) {
      original_text <- docket_entries$text_original[i]

      for (month in months) {
        original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
      }

      docket_entries$opinion_writer_check[i] <- original_text
    }

    split_rows <- strsplit(docket_entries$opinion_writer_check, "\n###\n")
    filtered_rows <- lapply(split_rows, function(row) {
      has_opinion_writer <- grepl("delivered the opinion", row, ignore.case = TRUE)
      row[has_opinion_writer]
    })
    cleaned_rows <- lapply(filtered_rows, function(row) {
      gsub(", J\\.,", "", row)
    })
    count_filed <- str_count(cleaned_rows, pattern = "filed")
    cleaned_rows <- str_count(cleaned_rows, pattern = "delivered") + count_filed

    docket_entries$number_of_opinions <- as.integer(cleaned_rows)
    docket_entries$number_of_opinions <- ifelse(grepl("opinion per curiam", docket_entries$text_original, ignore.case = TRUE), "Per Curiam", docket_entries$number_of_opinions)

  } #Number of Opinions Filed
  {

    for (i in 1:nrow(docket_entries)) {
      original_text <- docket_entries$text_original[i]

      for (month in months) {
        original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
      }

      docket_entries$opinion_writer_check[i] <- original_text
    }

    split_rows <- strsplit(docket_entries$opinion_writer_check, "\n###\n")
    filtered_rows <- lapply(split_rows, function(row) {
      has_opinion_writer <- grepl("delivered the opinion", row, ignore.case = TRUE)
      row[has_opinion_writer]
    })
    cleaned_rows <- lapply(filtered_rows, function(row) {
      gsub(", J\\.,", "", row)
    })
    dissent_count <- sapply(cleaned_rows, function(row) {
      sum(grepl("dissent", row, ignore.case = TRUE))
    })
    concur_count <- sapply(cleaned_rows, function(row) {
      sum(grepl("concur", row, ignore.case = TRUE))
    })

    nested_filings <- lapply(seq_along(docket_entries$text_original), function(i) {
      list(
        dissent_count = paste0("Dissents: ", as.character(dissent_count[i])),
        concur_count = paste0("Concurrences: ", as.character(concur_count[i]))
      )
    })

    docket_entries$opinions_by_type <- nested_filings
    docket_entries <- docket_entries %>%
      mutate(opinions_by_type = sapply(opinions_by_type, function(x) paste(x, collapse = "; ")))



  } #Number of Opinions Filed (Breakdown)
  {
    months <- c("Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ", "Aug ", "Sep ", "Oct ", "Nov ", "Dec ")

    for (i in 1:nrow(docket_entries)) {
      original_text <- docket_entries$text_original[i]

      for (month in months) {
        original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
      }

      docket_entries$opinion_writer_check[i] <- original_text

      split_rows <- strsplit(docket_entries$opinion_writer_check[i], "\n###\n")
      filtered_rows <- lapply(split_rows, function(row) {
        has_opinion_writer <- grepl("delivered the opinion", row, ignore.case = TRUE)
        row[has_opinion_writer]
      })
      cleaned_rows <- lapply(filtered_rows, function(row) {
        gsub(", J\\.,", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        trimws(row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        gsub(", JJ\\.\\,", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        gsub(", J\\.\\,", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        gsub(", C\\. J\\.\\,", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        gsub("\\. ", "\n###\n", row)
      })
      cleaned_rows <- unlist(cleaned_rows)
      split_rows <- strsplit(cleaned_rows, "\n###\n")

      contains_file_or_delivered <- function(row) {
        any(grepl("filed|delivered", row, ignore.case = TRUE))
      }

      split_rows <- lapply(split_rows, function(lst) {
        lst <- Filter(contains_file_or_delivered, lst)
        if (length(lst) > 0) {
          lst <- lst[!sapply(lst, is.null)]
        } else {
          lst <- ""
        }
        lst
      })

      docket_entries$opinion_filings[i] <- paste(unlist(split_rows), collapse = "; ")
    }
  } #Who Filed (And Joined)
  {
    docket_entries <- docket_entries %>%
      mutate(opinion_type = case_when(
        grepl("improvidently granted", text_original, ignore.case = TRUE) ~ "DIG",
        grepl("AFFIRMED", text_original, ignore.case = TRUE) ~ "AFFIRMED",
        grepl("Judgment VACATED and case REMANDED", text_original, ignore.case = TRUE) ~ "Vacated and Remanded",
        grepl("Judgments VACATED and cases REMANDED", text_original, ignore.case = TRUE) ~ "Vacated and Remanded",
        grepl("Judgment REVERSED and case REMANDED", text_original, ignore.case = TRUE) ~ "Reversed and Remanded",
        grepl("Judgments REVERSED and cases REMANDED", text_original, ignore.case = TRUE) ~ "Reversed and Remanded",
        grepl("REVERSED", text_original, ignore.case = TRUE) ~ "Reversed"
      )) %>%
      mutate(opinion_type = ifelse(is.na(opinion_type), "No Opinion Issued", opinion_type))

    months <- c("Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ", "Aug ", "Sep ", "Oct ", "Nov ", "Dec ")

    for (i in 1:nrow(docket_entries)) {
      original_text <- docket_entries$text_original[i]

      for (month in months) {
        original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
      }

      docket_entries$gvr_check[i] <- iconv(original_text, to = "UTF-8", sub = "")
    }

    encoded_string <- docket_entries$gvr_check
    split_rows <- strsplit(encoded_string, "\n###\n")
    filtered_rows <- lapply(split_rows, function(row) {
      has_gvr_brief <- grepl("GRANTED", row, ignore.case = TRUE) &
        grepl("VACATED", row, ignore.case = TRUE) &
        grepl("REMANDED", row, ignore.case = TRUE)
      if (any(has_gvr_brief))
        "GVR"
      else
        row
    })

    docket_entries$gvr <- filtered_rows

    docket_entries <- docket_entries %>%
      mutate(opinion_type = ifelse(gvr == "GVR", "GVR", opinion_type))


  } #Type of Decision
  {

    petitioner_counsel <- data.frame(docket_entries$petitioner_counsel)
    names(petitioner_counsel)[1] <- "petitioner_counsel"
    petitioner_counsel$petitioner_counsel <- gsub(".*\\: ", "", petitioner_counsel$petitioner_counsel)
    remove_text_after_third_space <- function(x) {
      # Split the text string into separate words
      words <- strsplit(x, " ")[[1]]

      # Remove any words beyond the third word
      truncated_words <- words[1:3]

      # Combine the truncated words back into a single string
      truncated_string <- paste(truncated_words, collapse = " ")

      # Return the truncated string
      return(truncated_string)
    } #Function to Trim Petitioner Counesl Name
    petitioner_counsel$petitioner_counsel_truncated <- sapply(petitioner_counsel$petitioner_counsel, remove_text_after_third_space) #Remove All Text After Third Space (Captures Most Names Re: First - Middle - Last)
    petitioner_counsel$petitioner_counsel_truncated <- gsub("POB", "", petitioner_counsel$petitioner_counsel_truncated )    #Remove any "PO" or "P.O"
    petitioner_counsel$petitioner_counsel_truncated <- gsub("P.O", "", petitioner_counsel$petitioner_counsel_truncated )    #Remove any "PO" or "P.O"
    petitioner_counsel$petitioner_counsel_truncated <- gsub("PO", "", petitioner_counsel$petitioner_counsel_truncated )    #Remove any "PO" or "P.O"
    petitioner_counsel$petitioner_counsel_truncated <- gsub("[[:digit:]].*", "", petitioner_counsel$petitioner_counsel_truncated )    #Remove Any Numbers
    petitioner_counsel$petitioner_counsel_truncated <- gsub("\\,", "", petitioner_counsel$petitioner_counsel_truncated )    #Remove Any Numbers
    petitioner_counsel$petitioner_counsel_truncated <- trimws(petitioner_counsel$petitioner_counsel_truncated)
    #Trim WS
    petitioner_counsel$petitioner_counsel_truncated <- gsub(" The", "", petitioner_counsel$petitioner_counsel_truncated )    #Remove any "The *Lawfirm*"
    petitioner_counsel$petitioner_counsel_truncated <- gsub("Univ.*", "", petitioner_counsel$petitioner_counsel_truncated )    #Remove any University
    petitioner_counsel$petitioner_counsel_truncated <- gsub(" \\..*", "", petitioner_counsel$petitioner_counsel_truncated )    #Remove trailing periods
    petitioner_counsel$petitioner_counsel_truncated <- ifelse(grepl("NA NA", petitioner_counsel$petitioner_counsel_truncated), "", petitioner_counsel$petitioner_counsel_truncated)

    petitioner_counsel$petitioner_counsel_address <- petitioner_counsel$petitioner_counsel


    petitioner_counsel$petitioner_counsel_address <- trimws(petitioner_counsel$petitioner_counsel_address)
    petitioner_counsel$petitioner_counsel_address <- sub("^,\\s*", "", petitioner_counsel$petitioner_counsel_address)

    petitioner_counsel <- petitioner_counsel %>%
      mutate(petitioner_counsel_truncated = ifelse(grepl("Jr.", petitioner_counsel_address), paste0(petitioner_counsel_truncated, " Jr."), petitioner_counsel_truncated)) %>%
      mutate(petitioner_counsel_truncated = ifelse(grepl("Sr.", petitioner_counsel_address), paste0(petitioner_counsel_truncated, " Sr."), petitioner_counsel_truncated)) %>%
      mutate(petitioner_counsel_truncated = ifelse(grepl("II", petitioner_counsel_address), paste0(petitioner_counsel_truncated, " II"), petitioner_counsel_truncated)) %>%
      mutate(petitioner_counsel_truncated = ifelse(grepl("III", petitioner_counsel_address) & !grepl("II", petitioner_counsel_truncated), paste0(petitioner_counsel_truncated, " III"), petitioner_counsel_truncated)) %>%
      mutate(petitioner_counsel_address = gsub("Jr.", "", petitioner_counsel_address)) %>%
      mutate(petitioner_counsel_address = gsub("Sr.", "", petitioner_counsel_address)) %>%
      mutate(petitioner_counsel_address = gsub("II.", "", petitioner_counsel_address)) %>%
      mutate(petitioner_counsel_address = gsub("III.", "", petitioner_counsel_address)) %>%
      mutate(petitioner_counsel_address = gsub("II", "", petitioner_counsel_address)) %>%
      mutate(petitioner_counsel_address = gsub("III", "", petitioner_counsel_address))
    extract_address <- function(address) {
      match <- regexpr("[a-zA-Z0-9]", address)
      if (match == -1) {
        return(address)
      } else {
        return(substr(address, match, nchar(address)))
      }
    } #Grab from first alpha-numeric character
    petitioner_counsel$petitioner_counsel_address <- sapply(petitioner_counsel$petitioner_counsel_address, function(x) {
      x <- iconv(x, to = "UTF-8")
      extract_address(x)
    })
    # Apply the function to the petitioner_counsel_address column
    petitioner_counsel$petitioner_counsel_address <- sapply(petitioner_counsel$petitioner_counsel_address, extract_address)

    petitioner_counsel <- petitioner_counsel %>%
      mutate(petitioner_counsel_fix = case_when(
        grepl("Law Offices of", petitioner_counsel_address, ignore.case = T) ~ "Law Offices of",
        grepl("Law Office of", petitioner_counsel_address, ignore.case = T) ~ "Law Office of",
        grepl("Law Group", petitioner_counsel_address, ignore.case = T) ~ "Law Group",
        grepl("Associates", petitioner_counsel_address, ignore.case = T) ~ "Associates"))

    petitioner_counsel$petitioner_counsel_address = ifelse(!is.na(petitioner_counsel$petitioner_counsel_fix), apply(petitioner_counsel, 1, function(x){
      gsub(x["petitioner_counsel_fix"], "", x["petitioner_counsel_address"])}), petitioner_counsel$petitioner_counsel_address)


    petitioner_counsel <- petitioner_counsel %>%
      mutate(petitioner_counsel_address = case_when(
        is.na(petitioner_counsel_fix) ~ petitioner_counsel_address,
        petitioner_counsel_fix == "Law Offices of" ~ paste0(petitioner_counsel_fix, " ", petitioner_counsel_truncated, petitioner_counsel_address),
        petitioner_counsel_fix == "Law Office of" ~ paste0(petitioner_counsel_fix, " ", petitioner_counsel_truncated, petitioner_counsel_address),
        petitioner_counsel_fix == "Law Group" ~ paste0(petitioner_counsel_truncated, " ", petitioner_counsel_fix, petitioner_counsel_address),
        petitioner_counsel_fix == "Associates" ~ paste0(petitioner_counsel_truncated, " &", petitioner_counsel_fix, petitioner_counsel_address),
      )) %>%
      select(-c(petitioner_counsel_fix))


  } #Petitioner Counsel Parse
  {
    respondent_counsel <- data.frame(docket_entries$respondent_counsel)
    names(respondent_counsel)[1] <- "respondent_counsel"
    respondent_counsel$respondent_counsel <- gsub(".*\\: ", "", respondent_counsel$respondent_counsel)
    remove_text_after_third_space <- function(x) {
      # Split the text string into separate words
      words <- strsplit(x, " ")[[1]]

      # Remove any words beyond the third word
      truncated_words <- words[1:3]

      # Combine the truncated words back into a single string
      truncated_string <- paste(truncated_words, collapse = " ")

      # Return the truncated string
      return(truncated_string)
    } #Function to Trim respondent Counesl Name
    respondent_counsel$respondent_counsel_truncated <- sapply(respondent_counsel$respondent_counsel, remove_text_after_third_space) #Remove All Text After Third Space (Captures Most Names Re: First - Middle - Last)
    respondent_counsel$respondent_counsel_truncated <- gsub("POB", "", respondent_counsel$respondent_counsel_truncated )    #Remove any "PO" or "P.O"
    respondent_counsel$respondent_counsel_truncated <- gsub("P.O", "", respondent_counsel$respondent_counsel_truncated )    #Remove any "PO" or "P.O"
    respondent_counsel$respondent_counsel_truncated <- gsub("PO", "", respondent_counsel$respondent_counsel_truncated )    #Remove any "PO" or "P.O"
    respondent_counsel$respondent_counsel_truncated <- gsub("[[:digit:]].*", "", respondent_counsel$respondent_counsel_truncated )    #Remove Any Numbers
    respondent_counsel$respondent_counsel_truncated <- gsub("\\,", "", respondent_counsel$respondent_counsel_truncated )    #Remove Any Numbers
    respondent_counsel$respondent_counsel_truncated <- trimws(respondent_counsel$respondent_counsel_truncated)
    #Trim WS
    respondent_counsel$respondent_counsel_truncated <- gsub(" The", "", respondent_counsel$respondent_counsel_truncated )    #Remove any "The *Lawfirm*"
    respondent_counsel$respondent_counsel_truncated <- gsub("Univ.*", "", respondent_counsel$respondent_counsel_truncated )    #Remove any University
    respondent_counsel$respondent_counsel_truncated <- gsub(" \\..*", "", respondent_counsel$respondent_counsel_truncated )    #Remove trailing periods
    respondent_counsel$respondent_counsel_truncated <- ifelse(grepl("NA NA", respondent_counsel$respondent_counsel_truncated), "", respondent_counsel$respondent_counsel_truncated)

    respondent_counsel$respondent_counsel_address <- respondent_counsel$respondent_counsel

    respondent_counsel$respondent_counsel_address <- trimws(respondent_counsel$respondent_counsel_address)
    respondent_counsel$respondent_counsel_address <- sub("^,\\s*", "", respondent_counsel$respondent_counsel_address)

    respondent_counsel <- respondent_counsel %>%
      mutate(respondent_counsel_truncated = ifelse(grepl("Jr.", respondent_counsel_address), paste0(respondent_counsel_truncated, " Jr."), respondent_counsel_truncated)) %>%
      mutate(respondent_counsel_truncated = ifelse(grepl("Sr.", respondent_counsel_address), paste0(respondent_counsel_truncated, " Sr."), respondent_counsel_truncated)) %>%
      mutate(respondent_counsel_truncated = ifelse(grepl("II", respondent_counsel_address), paste0(respondent_counsel_truncated, " II"), respondent_counsel_truncated)) %>%
      mutate(respondent_counsel_truncated = ifelse(grepl("III", respondent_counsel_address) & !grepl("II", respondent_counsel_truncated), paste0(respondent_counsel_truncated, " III"), respondent_counsel_truncated)) %>%
      mutate(respondent_counsel_address = gsub("Jr.", "", respondent_counsel_address)) %>%
      mutate(respondent_counsel_address = gsub("Sr.", "", respondent_counsel_address)) %>%
      mutate(respondent_counsel_address = gsub("II.", "", respondent_counsel_address)) %>%
      mutate(respondent_counsel_address = gsub("III.", "", respondent_counsel_address)) %>%
      mutate(respondent_counsel_address = gsub("II", "", respondent_counsel_address)) %>%
      mutate(respondent_counsel_address = gsub("III", "", respondent_counsel_address))
    extract_address <- function(address) {
      match <- regexpr("[a-zA-Z0-9]", address)
      if (match == -1) {
        return(address)
      } else {
        return(substr(address, match, nchar(address)))
      }
    }
    respondent_counsel$respondent_counsel_address <- sapply(respondent_counsel$respondent_counsel_address, function(x) {
      x <- iconv(x, to = "UTF-8")
      extract_address(x)
    })
    # Apply the function to the respondent_counsel_address column
    respondent_counsel$respondent_counsel_address <- sapply(respondent_counsel$respondent_counsel_address, extract_address)

    # respondent_counsel$name_count<- sapply(1:nrow(respondent_counsel), function(x) str_count(respondent_counsel$respondent_counsel[x], respondent_counsel$respondent_counsel_truncated[x]))

    respondent_counsel <- respondent_counsel %>%
      mutate(respondent_counsel_fix = case_when(
        grepl("Law Offices of", respondent_counsel_address, ignore.case = T) ~ "Law Offices of",
        grepl("Law Office of", respondent_counsel_address, ignore.case = T) ~ "Law Office of",
        grepl("Law Group", respondent_counsel_address, ignore.case = T) ~ "Law Group",
        grepl("Associates", respondent_counsel_address, ignore.case = T) ~ "Associates"))

    respondent_counsel$respondent_counsel_address = ifelse(!is.na(respondent_counsel$respondent_counsel_fix), apply(respondent_counsel, 1, function(x){
      gsub(x["respondent_counsel_fix"], "", x["respondent_counsel_address"])}), respondent_counsel$respondent_counsel_address)


    respondent_counsel <- respondent_counsel %>%
      mutate(respondent_counsel_address = case_when(
        is.na(respondent_counsel_fix) ~ respondent_counsel_address,
        respondent_counsel_fix == "Law Offices of" ~ paste0(respondent_counsel_fix, " ", respondent_counsel_truncated, respondent_counsel_address),
        respondent_counsel_fix == "Law Office of" ~ paste0(respondent_counsel_fix, " ", respondent_counsel_truncated, respondent_counsel_address),
        respondent_counsel_fix == "Law Group" ~ paste0(respondent_counsel_truncated, " ", respondent_counsel_fix, respondent_counsel_address),
        respondent_counsel_fix == "Associates" ~ paste0(respondent_counsel_truncated, " &", respondent_counsel_fix, respondent_counsel_address),
      )) %>%
      select(-c(respondent_counsel_fix))

  } #Respondent Counsel Parse
  {

    docket_entries$petitioner_counsel_address <- petitioner_counsel$petitioner_counsel_address
    docket_entries$petitioner_counsel_address <- ifelse(is.na(docket_entries$petitioner_counsel_address), "", docket_entries$petitioner_counsel_address)
    docket_entries$petitioner_counsel <- petitioner_counsel$petitioner_counsel_truncated
    docket_entries$petitioner_counsel_address <- str_replace_all(docket_entries$petitioner_counsel_address, docket_entries$petitioner_counsel, "")

    docket_entries$respondent_counsel_address <- respondent_counsel$respondent_counsel_address
    docket_entries$respondent_counsel_address <- ifelse(is.na(docket_entries$respondent_counsel_address), "", docket_entries$respondent_counsel_address)
    docket_entries$respondent_counsel <- respondent_counsel$respondent_counsel_truncated
    docket_entries$respondent_counsel_address <- str_replace_all(docket_entries$respondent_counsel_address, docket_entries$respondent_counsel, "")

  } #Add Parsed & Cleaned Counsel
  {
    {
      for (i in 1:nrow(docket_entries)) {
        original_text <- docket_entries$text_original[i]

        for (month in months) {
          original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
        }

        docket_entries$submitted_to_check[i] <- original_text
      }

      split_rows <- strsplit(docket_entries$submitted_to_check, "\n###\n")
      filtered_rows <- lapply(split_rows, function(row) {
        has_submitted_to <- grepl("submitted to", row, ignore.case = TRUE)
        row[has_submitted_to]
      })
      cleaned_rows <- lapply(filtered_rows, function(row) {
        gsub("Justice ", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        gsub(".*submitted to ", "", row, ignore.case = T)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        gsub("\\..*", "", row, ignore.case = T)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        gsub("\\. ", "\n###\n", row)
      })

      docket_entries$submitted_to <- cleaned_rows

      docket_entries <- docket_entries %>%
        mutate(submitted_to = sapply(submitted_to, paste, collapse = "; "))

      docket_entries$submitted_to <- ifelse(docket_entries$type == "Docketed Case", "", docket_entries$submitted_to)
    } #Submitted To
    {
      docket_entries$referred_to_court <- ifelse(grepl("referred to the Court", docket_entries$text_original, ignore.case = T), "Yes", "No")
      docket_entries$referred_to_court <- ifelse(docket_entries$type == "Docketed Case", "", docket_entries$referred_to_court)
    } #Referred to Court
    {
      for (i in 1:nrow(docket_entries)) {
        original_text <- docket_entries$text_original[i]

        for (month in months) {
          original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
        }

        docket_entries$application_type_check[i] <- original_text
      }

      split_rows <- strsplit(docket_entries$application_type_check, "\n###\n")
      filtered_rows <- lapply(split_rows, function(row) {
        has_application_type <- grepl(", submitted to", row, ignore.case = TRUE)
        row[has_application_type]
      })

      cleaned_rows <- lapply(filtered_rows, function(row) {
        gsub("\\([^()]*\\)", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        trimws(row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        gsub(".*for ", "", row, ignore.case = )
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        gsub("\\,.*", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        gsub("a ", "", row)
      })

      docket_entries$application_type = cleaned_rows
      docket_entries$application_type = ifelse(docket_entries$type == "Docketed Case", "", docket_entries$application_type)
      docket_entries$application_type = str_to_title(docket_entries$application_type)


    } #Application Type



  } #Special Application Terms
  {

    docket_entries <- docket_entries %>%
      mutate(docket_number = gsub("No. ", "", docket_number)) %>%
      mutate(filing_type = file_type) %>%
      mutate(petitioner_counsel = gsub("[^[:alpha:].\\s ]+", "", petitioner_counsel)) %>%
      mutate(respondent_counsel = gsub("[^[:alpha:].\\s ]+", "", respondent_counsel)) %>%
      mutate(petition_type = type) %>%
      mutate(most_recent_order = ifelse(grepl("SET FOR ARGUMENT", text_original, ignore.case = TRUE), "SET FOR ARGUMENT", NA)) %>%
      mutate(most_recent_order = ifelse(grepl("Argued. ", text_original, ignore.case = TRUE), "ARGUED", most_recent_order)) %>%
      mutate(most_recent_order = ifelse(grepl("ISSUED", text_original, ignore.case = TRUE), "JUDGEMENT ISSUED", most_recent_order)) %>%
      mutate(most_recent_order = ifelse(is.na(most_recent_order), paste0(petition_type, " ", certiorari), most_recent_order)) %>%
      mutate(most_recent_order = ifelse(is.na(most_recent_order), paste0(petition_type, " ", certiorari), most_recent_order)) %>%
      mutate(petitioner_address_fix = gsub("Jr\\.|Sr\\.|II|III", "", petitioner_counsel),
             respondent_address_fix = gsub("Jr\\.|Sr\\.|II|III", "", respondent_counsel)) %>%
      mutate(petitioner_address_fix = trimws(petitioner_address_fix),
             respondent_address_fix = trimws(respondent_address_fix)) %>%
      mutate(petitioner_counsel_address = gsub(petitioner_address_fix, "", petitioner_counsel_address),
             respondent_counsel_address = gsub(respondent_address_fix, "", respondent_counsel_address)) %>%
      mutate(petitioner_counsel_address = trimws(petitioner_counsel_address),
             respondent_counsel_address = trimws(respondent_counsel_address)) %>%
      mutate(amicus_indicator = ifelse(amicus_indicator == 1, "Yes", "No")) %>%
      mutate(first_conference = conference_date) %>%
      select(docket_number, petition_type, petitioner, petitioner_counsel, petitioner_counsel_address, respondent, respondent_counsel, respondent_counsel_address, docketed, submitted_to, referred_to_court, application_type, linked_with, lower_court, lower_court_case_number, lower_court_decision_date, lower_court_rehearing_denied, amicus_indicator, amicus_filers, amicus_count, first_conference, conference_count, multiple_conference_dates, most_recent_order, majority_opinion_writer, opinion_type, number_of_opinions, opinions_by_type, opinion_filings, argument_date, special_filing, rehearing_filed, rehearing_order, docket_url)

    chr_cols <- names(docket_entries)[sapply(docket_entries, is.character)]
    for (col in chr_cols) {
      if (any(sapply(docket_entries[[col]], inherits, "raw"))) {
        docket_entries[[col]] <- iconv(docket_entries[[col]], from = "ISO-8859-1", to = "UTF-8")
      } else {
        docket_entries[[col]] <- iconv(docket_entries[[col]], to = "UTF-8")
      }
    }

  } #Final Cleaning (Arrange columns & convert bytes to UTF-8)




  return(docket_entries)

} #Parse Individual Docket Entries
