clean_docket_frame <- function(docket_frame, include, exclude){

  {
    corpus <- tm::Corpus(VectorSource(docket_frame[["text"]]))
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\xe2\\x80\\xa2", replacement = " ")
    #corpus <- tm_map(corpus, stripWhitespace)
    docket_entries <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE) #Convert Corpus to Dataframe
    removeWordsWithBackslash <- function(inputString) {
      outputString <- gsub("\\S*\\\\\\\\\\S*\\s?", "", inputString)
      outputString <- iconv(outputString, "UTF-8", "ASCII", sub="")
      return(outputString)}
    docket_entries$text <- sapply(docket_entries$text, removeWordsWithBackslash)
  } #Base Clean (Corpus -> Toss "\\")
  {
    {
      docket_entries$docket_id <- docket_frame$url
      docket_entries$docket_url <- docket_frame$url
      docket_entries$docket_id <- gsub(".*(/Public/)", "", docket_entries$docket_id, ignore.case = TRUE)
      docket_entries$docket_id <- gsub(".*docketfiles\\/", "", docket_entries$docket_id )
      docket_entries$docket_id <- gsub(".html", "", docket_entries$docket_id)
      docket_entries$docket_id <- gsub(".htm", "", docket_entries$docket_id)
      docket_entries$docket_number <- docket_entries$docket_id
      docket_entries$text_original <- docket_entries$text
      docket_entries$metadata <- sub("PROCEEDINGS AND ORDERS.*", "", docket_entries$text_original, ignore.case = T)
      split_pr <- strsplit(docket_entries$metadata, "Docketed:")
      split_metadata <- strsplit(docket_entries$metadata, "\n")
      split_full <- strsplit(docket_entries$text_original, "\n")
    } #Initial Frame Process & Text Split
    {
      petitioner <- lapply(split_pr, function(row) {
        has_petitioner <- grepl("(Petitioner|Plaintiff|Petioners|Plaintiffs|Appellant|Appellants|In Re|Applicant|Applicants)", row, ignore.case = TRUE)
        row[has_petitioner]
      })
      cleaned_petitioner <- lapply(petitioner, function(row) {
        row <- str_replace_all(row, "\n", "")
        row <- str_replace_all(row, ".*Title:", "")
        row <- str_replace_all(row, "v\\..*", "")
        row <- str_replace_all(row, ", (Petitioner|Plaintiff|Petitioners|Plaintiffs|Appellant|Appellants|In Re|Applicant|Applicants).*", "")
      })
      nested_petitioner <- lapply(cleaned_petitioner, function(row) list(row))
      replace_empty <- function(lst) {
        modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", NA, x))
        return(modified_lst)
      }
      nested_petitioner <- replace_empty(nested_petitioner)
      docket_entries$petitioner <- sapply(nested_petitioner, function(x) paste(unlist(x), collapse = "; "))

    } #Petitioner
    {
      respondent <- lapply(split_pr, function(row) {
        has_respondent <- grepl("(Petitioner|Plaintiff|Petioners|Plaintiffs|Appellant|Appellants|In Re|Applicant|Applicants)", row, ignore.case = TRUE)
        row[has_respondent]
      })
      cleaned_respondent <- lapply(respondent, function(row) {
        row <- str_replace_all(row, "\n", "")
        row <- str_replace(row, ".*v\\.", "")
      })
      nested_respondent <- lapply(cleaned_respondent, function(row) list(row))
      replace_empty <- function(lst) {
        modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", NA, x))
        return(modified_lst)
      }
      nested_respondent <- replace_empty(nested_respondent)
      docket_entries$respondent <- sapply(nested_respondent, function(x) paste(unlist(x), collapse = "; "))

    } #Respondent
    {
      docketed <- lapply(split_metadata, function(row) {
        has_docketed <- grepl("Docketed:", row, ignore.case = TRUE)
        row[has_docketed]
      })
      cleaned_docketed <- lapply(docketed, function(row) {
        row <- str_replace_all(row, ".*Docketed: ", "")
        row <- trimws(row)
      })
      nested_docketed <- lapply(cleaned_docketed, function(row) list(row))
      replace_empty <- function(lst) {
        modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", NA, x))
        return(modified_lst)
      }
      nested_docketed <- replace_empty(nested_docketed)
      docket_entries$docketed <- sapply(nested_docketed, function(x) paste(unlist(x), collapse = "; "))
    } #Docketed
    {
      linked_with <- lapply(split_metadata, function(row) {
        has_linked_with <- grepl("Linked", row, ignore.case = TRUE)
        row[has_linked_with]
      })
      cleaned_linked_with <- lapply(linked_with, function(row) {
        row <- str_replace_all(row, ".*Linked with", "")
        row <- str_replace_all(row, "\\:", "")
        row <- trimws(row)
      })
      nested_linked_with <- lapply(cleaned_linked_with, function(row) list(row))
      replace_empty <- function(lst) {
        modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", NA, x))
        return(modified_lst)
      }
      nested_linked_with <- replace_empty(nested_linked_with)
      docket_entries$linked_with <- sapply(nested_linked_with, function(x) paste(unlist(x), collapse = "; "))
    } #Linked With
    {
      lower_court <- lapply(split_metadata, function(row) {
        has_lower_court <- grepl("Lower", row, ignore.case = TRUE)
        row[has_lower_court]
      })
      cleaned_lower_court <- lapply(lower_court, function(row) {
        row <- str_replace_all(row, ".*(Lower Ct: |Lower Court:)", "")
        row <- trimws(row)
      })
      nested_lower_court <- lapply(cleaned_lower_court, function(row) list(row))
      replace_empty <- function(lst) {
        modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", NA, x))
        return(modified_lst)
      }
      nested_lower_court <- replace_empty(nested_lower_court)
      docket_entries$lower_court <- sapply(nested_lower_court, function(x) paste(unlist(x), collapse = "; "))
    } #Lower Court
    {
      lower_court_case_number <- lapply(split_metadata, function(row) {
        has_lower_court_case_number <- grepl("(Case Number|Case Numbers|Case No)", row, ignore.case = TRUE)
        row[has_lower_court_case_number]
      })
      cleaned_lower_court_case_number <- lapply(lower_court_case_number, function(row) {
        row <- str_replace_all(row, ".*(Case Number: |Case Numbers: |Case No: )", "")
        row <- trimws(row)
      })
      nested_lower_court_case_number <- lapply(cleaned_lower_court_case_number, function(row) list(row))
      replace_empty <- function(lst) {
        modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", NA, x))
        return(modified_lst)
      }
      nested_lower_court_case_number <- replace_empty(nested_lower_court_case_number)
      docket_entries$lower_court_case_number <- sapply(nested_lower_court_case_number, function(x) paste(unlist(x), collapse = "; "))
    } #Lower Court Case Number
    {
      lower_court_decision_date <- lapply(split_metadata, function(row) {
        has_lower_court_decision_date <- grepl("Decision Date: ", row, ignore.case = TRUE)
        row[has_lower_court_decision_date]
      })
      cleaned_lower_court_decision_date <- lapply(lower_court_decision_date, function(row) {
        row <- str_replace_all(row, ".*(Decision Date:)", "")
        row <- trimws(row)
      })
      nested_lower_court_decision_date <- lapply(cleaned_lower_court_decision_date, function(row) list(row))
      replace_empty <- function(lst) {
        modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", NA, x))
        return(modified_lst)
      }
      nested_lower_court_decision_date <- replace_empty(nested_lower_court_decision_date)
      docket_entries$lower_court_decision_date <- sapply(nested_lower_court_decision_date, function(x) paste(unlist(x), collapse = "; "))
    } #Lower Court Case Number
    {
      ifp <- lapply(split_metadata, function(row) {
        has_ifp <- grepl("leave to proceed in forma pauperis", row, ignore.case = TRUE)
        row[has_ifp]
      })
      cleaned_ifp <- lapply(split_metadata, function(row) {
        if (length(row) == 0) {
          "In Forma Pauperis"
        } else {
          NA
        }
      })

      nested_ifp <- lapply(cleaned_ifp, function(row) list(row))
      docket_entries$ifp <- sapply(nested_ifp, function(x) paste(unlist(x), collapse = "; "))
    } #File Type
    {
      docket_entries <- docket_entries %>%
        mutate(special_filing = ifelse(
          grepl("mandamus", text_original, ignore.case = T), "Mandamus", ifelse(grepl("habeas", text_original, ignore.case = T), "Habeas", ifelse(grepl("capital case", text_original, ignore.case = T), "Capital Case", ifelse(grepl("o", docket_number, ignore.case = T), "Original Jurisdiction", NA))))) %>%
        mutate(type = case_when(
          grepl("a", docket_number, ignore.case = T) ~ "Application",
          grepl("m", docket_number, ignore.case = T) ~ "Motion",
          grepl("-", docket_number, ignore.case = T) ~ "Docketed Case",
          grepl("o", docket_number, ignore.case = T) ~ "Docketed Case - Original Jurisdiction"
        )) %>%
        filter(!is.na(text_original)) %>%
        select(-c(text))

    } #Additional Terms

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
        sub(" filed\\..*", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        str_remove(row, "\\nMain.*")
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        str_remove(row, "\\n")
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
        mutate(amicus_indicator = ifelse(amicus_filers == "", 0, 1)) %>%
        mutate(amicus_indicator = ifelse(is.na(amicus_filers), 0, amicus_indicator)) %>%
        mutate(amicus_count = sapply(docket_entries$amicus_filers, function(x) length(unlist(x)))) %>%
        mutate(amicus_count = ifelse(amicus_indicator == 0, 0, amicus_count))
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
    cleaned_rows <- lapply(cleaned_rows, function(row) {
      str_remove(row, "\n\n")
    })
    nested_list <- lapply(cleaned_rows, function(row) list(row))

    replace_empty <- function(lst) {
      modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", "", x))
      return(modified_lst)
    }

    nested_list <- replace_empty(nested_list)

    docket_entries$multiple_conference_dates <- nested_list
    docket_entries <- docket_entries %>%
      mutate(multiple_conference_dates = sapply(multiple_conference_dates, function(x) paste(unlist(x), collapse = "; "))) %>%
      mutate(conference_count = sapply(docket_entries$multiple_conference_dates, function(x) length(unlist(x))))

    docket_entries$first_conference_date <- sapply(docket_entries$multiple_conference_dates, function(x) {
      dates <- strsplit(x, "; ")[[1]]
      if (length(dates) > 0) {
        return(dates[1])
      } else {
        return("")
      }
    })


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
    cleaned_rows <- lapply(cleaned_rows, function(row) {
      str_remove(row, "\n\n")
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
    cleaned_rows <- lapply(cleaned_rows, function(row) {
      str_remove(row, "\n\n")
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
    cleaned_rows <- lapply(cleaned_rows, function(row) {
      str_remove(row, "\n\n")
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
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        str_remove(row, "\n\n")
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
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        str_remove(row, "\n\n")
      })

      docket_entries$submitted_to <- cleaned_rows

      docket_entries <- docket_entries %>%
        mutate(submitted_to = sapply(submitted_to, paste, collapse = "; "))

      docket_entries$submitted_to <- ifelse(docket_entries$type == "Docketed Case", NA, docket_entries$submitted_to)
    } #Submitted To
    {
      docket_entries$referred_to_court <- ifelse(grepl("referred to the Court", docket_entries$text_original, ignore.case = T), "Yes", "No")
      docket_entries$referred_to_court <- ifelse(docket_entries$type == "Docketed Case", NA, docket_entries$referred_to_court)
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
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        str_remove(row, "\n\n")
      })

      docket_entries$application_type = cleaned_rows
      docket_entries$application_type = ifelse(docket_entries$type == "Docketed Case", NA, docket_entries$application_type)
      docket_entries$application_type = str_to_title(docket_entries$application_type)


    } #Application Type



  } #Special Application Terms
  {
    {

      petitioner_pattern <- "(Attorneys for (Petitioner|Plaintiff|Petitioners|Plaintiff|Appellant|Appellants|Applicant|Applicants))\\n\\n([\\s\\S]*?)(?=\\n\\nAttorneys for (Respondent|Respondents|Appellee|Appellees)|\\n\\nOther|\\n\\n\\{1\\})"

      counsel <- docket_entries %>%
        select(text_original) %>%
        mutate(all_petitioner_counsel = str_extract_all(text_original, petitioner_pattern)) %>%
        mutate(all_respondent_counsel = str_extract(text_original, "(Attorneys for (Respondent|Respondents|Appellee|Appellees))\\n\\n([\\s\\S]*?)(?=\\n\\n|Other)")) %>%
        mutate(all_respondent_counsel = gsub("Attorneys for (Respondent|Respondents|Appellee|Appellees)", "", all_respondent_counsel)) %>%
        mutate(all_other_counsel = gsub("^.*Attorneys for (Respondent|Respondents|Appellee|Appellees)", "", text_original)) %>%
        mutate(all_other_counsel = gsub("\n\nOther", "<OTHER BREAK>", all_other_counsel)) %>%
        mutate(all_other_counsel = ifelse(grepl("<OTHER BREAK>", all_other_counsel),
                                          gsub(".*<OTHER BREAK>", "", all_other_counsel),
                                          ""))  %>%
        mutate(all_other_counsel = gsub("\n\n\\{1\\}", " <OTHER END> ", all_other_counsel)) %>%
        mutate(all_other_counsel = sub("<OTHER END>.*", "", all_other_counsel)) %>%
        mutate(all_respondent_counsel = gsub("\n", " <BREAK> ", all_respondent_counsel),
               all_petitioner_counsel = gsub("\n", " <BREAK> ", all_petitioner_counsel),
               all_other_counsel = gsub("\n", " <BREAK> ", all_other_counsel),) %>%
        mutate(all_respondent_counsel = gsub("(<BREAK>)(.*?Party.*?)(<BREAK>)", "\\1\\2 Attorney:\n\nAttorney:\\3", all_respondent_counsel, perl = TRUE),
               all_petitioner_counsel = gsub("(<BREAK>)(.*?Party.*?)(<BREAK>)", "\\1\\2 Attorney:\n\nAttorney:\\3", all_petitioner_counsel, perl = TRUE),
               all_other_counsel = gsub("(<BREAK>)(.*?Party.*?)(<BREAK>)", "\\1\\2 Attorney:\n\nAttorney:\\3", all_other_counsel, perl = TRUE)) %>%
        mutate(all_respondent_counsel = trimws(all_respondent_counsel),
               all_petitioner_counsel = trimws(all_petitioner_counsel),
               all_other_counsel = trimws(all_other_counsel))  %>%
        mutate(all_respondent_counsel = gsub("Attorneys for (Respondent|Respondents|Appellee|Appellees)", "\nAttorney:", all_respondent_counsel),
               all_petitioner_counsel = gsub("Attorneys for (Petitioner|Plaintiff|Petitioners|Plaintiff|Appellant|Appellants)", "\nAttorney:", all_petitioner_counsel)) %>%
        mutate(all_petitioner_counsel = gsub("Attorney: \\K<BREAK>\\s*<BREAK>", "", all_petitioner_counsel, perl = TRUE),
               all_respondent_counsel = gsub("Attorney: \\K<BREAK>\\s*<BREAK>", "", all_respondent_counsel, perl = TRUE),
               all_other_counsel = gsub("Attorney: \\K<BREAK>\\s*<BREAK>", "", all_other_counsel, perl = TRUE)) %>%
        mutate(all_respondent_counsel = gsub("<BREAK>", "\n", all_respondent_counsel),
               all_petitioner_counsel = gsub("<BREAK>", "\n", all_petitioner_counsel),
               all_other_counsel = gsub("<BREAK>", "\n", all_other_counsel)) %>%
        mutate(all_respondent_counsel = gsub("\nAttorney: \n", "\nAttorney:", all_respondent_counsel),
               all_petitioner_counsel = gsub("\nAttorney: \n", "\nAttorney:", all_petitioner_counsel),
               all_other_counsel = gsub("\nAttorney: \n", "\nAttorney:", all_other_counsel)) %>%
        mutate(all_respondent_counsel = gsub("Attorney:\n\n", "\n\n", all_respondent_counsel),
               all_petitioner_counsel = gsub("Attorney:\n\n", "\n\n", all_petitioner_counsel),
               all_other_counsel = gsub("Attorney:\n\n", "\n\n", all_other_counsel)) %>%
        mutate(all_respondent_counsel = gsub("\n\nAttorney:\n", "\n\n Attorney:", all_respondent_counsel),
               all_petitioner_counsel = gsub("Attorney:\n\n", "\n\n", all_petitioner_counsel),
               all_other_counsel = gsub("Attorney:\n\n", "\n\n", all_other_counsel)) %>%
        mutate(all_respondent_counsel = gsub("\nAttorney:", "Attorney:", all_respondent_counsel),
               all_petitioner_counsel = gsub("\nAttorney:", "Attorney:", all_petitioner_counsel),
               all_other_counsel = gsub("\nAttorney:", "Attorney:", all_other_counsel)) %>%
        mutate(all_respondent_counsel = gsub("Counsel of Record", "Organization:", all_respondent_counsel),
               all_petitioner_counsel = gsub("Counsel of Record", "Organization:", all_petitioner_counsel),
               all_other_counsel = gsub("Counsel of Record", "Organization:", all_other_counsel)) %>%
        mutate(all_petitioner_counsel = gsub("\\nAttorney:(.*)(\\nOrganization)", "\\nAttorney:\\1Organization", all_petitioner_counsel)) %>%
        mutate(all_respondent_counsel = trimws(all_respondent_counsel),
               all_petitioner_counsel = trimws(all_petitioner_counsel),
               all_other_counsel = trimws(all_other_counsel)) %>%
        mutate(all_respondent_counsel = gsub(" \n ", "\n", all_respondent_counsel),
               all_petitioner_counsel = gsub(" \n ", "\n", all_petitioner_counsel),
               all_other_counsel = gsub(" \n ", "\n", all_other_counsel)) %>%
        mutate(all_respondent_counsel = gsub(" \n\n ", "\n\n", all_respondent_counsel),
               all_petitioner_counsel = gsub(" \n\n ", "\n\n", all_petitioner_counsel),
               all_other_counsel = gsub(" \n\n ", "\n\n", all_other_counsel)) %>%
        mutate(all_petitioner_counsel = gsub(" {2}", " ", all_petitioner_counsel),
               all_respondent_counsel = gsub(" {2}", " ", all_respondent_counsel),
               all_other_counsel = gsub(" {2}", " ", all_other_counsel)) %>%
        mutate(all_petitioner_counsel = gsub("Attorney:\n", "\n\nAttorney:", all_petitioner_counsel),
               all_respondent_counsel = gsub("Attorney:\n", "\n\nAttorney:", all_respondent_counsel ),
               all_other_counsel = gsub("Attorney:\n", "\nAttorney:", all_other_counsel)) %>%
        mutate(all_respondent_counsel = ifelse(all_respondent_counsel != "" & !grepl("^Attorney:", all_respondent_counsel), paste0("Attorney: ", all_respondent_counsel), all_respondent_counsel),
               all_petitioner_counsel = ifelse(all_petitioner_counsel != "" & !grepl("^Attorney:", all_petitioner_counsel), paste0("Attorney: ", all_petitioner_counsel), all_petitioner_counsel),
               all_other_counsel = ifelse(all_other_counsel != "" & !grepl("^Attorney:", all_other_counsel), paste0("Attorney: ", all_other_counsel), all_other_counsel)) %>%
        mutate(contains_org_petitioner = !grepl("Organization", all_petitioner_counsel),
               contains_org_respondent = !grepl("Organization", all_respondent_counsel),
               contains_org_other = !grepl("Organization", all_other_counsel)) %>%
        mutate(all_petitioner_counsel = if_else(contains_org_petitioner, sub("\n", "\nOrganization: ", all_petitioner_counsel, fixed = TRUE), all_petitioner_counsel),
               all_respondent_counsel = if_else(contains_org_respondent, sub("\n", "\nOrganization: ", all_respondent_counsel, fixed = TRUE), all_respondent_counsel),
               all_other_counsel = if_else(contains_org_other, sub("\n", "\nOrganization: ", all_other_counsel, fixed = TRUE), all_other_counsel)) %>%
        mutate(all_petitioner_counsel = gsub("\n\nParty", "\nParty", all_petitioner_counsel),
               all_other_counsel = gsub("\n\nParty", "\nParty", all_other_counsel),
               all_respondent_counsel = gsub("\n\nParty", "\nParty", all_respondent_counsel))




    } #Parsing Counsel
    {
      extract_info <- function(record) {
        lines <- strsplit(record, "\n")[[1]]
        info <- list()

        for (line in lines) {
          if (grepl("^Attorney:", line)) {
            if (!exists("Attorney", info))
              info$Attorney <- list()
            info$Attorney <- c(info$Attorney, gsub("^Attorney: ", "", line))
          } else if (grepl("^Organization:", line)) {
            info$Organization <- gsub("^Organization: ", "", line)
          } else if (grepl("^Party name:", line)) {
            info$PartyName <- gsub("^Party name: ", "", line)
          }
        }

        return(info)
      }

      if (is.na(counsel$all_respondent_counsel) || counsel$all_respondent_counsel == "") {
        records_respondent <- NA
      } else {
        records_respondent <- lapply(counsel$all_respondent_counsel, function(x) unlist(strsplit(x, "\n\n")))
      }
      if (is.na(counsel$all_other_counsel) || counsel$all_other_counsel == "") {
        records_other <- NA
      } else {
        records_other <- lapply(counsel$all_other_counsel, function(x) unlist(strsplit(x, "\n\n")))
      }
      if (is.na(counsel$all_petitioner_counsel) || counsel$all_petitioner_counsel == "") {
        records_petitioner <- NA
      } else {
        records_petitioner <- lapply(counsel$all_petitioner_counsel, function(x) unlist(strsplit(x, "\n\n")))
      }


      if (is.na(records_petitioner)) {
        nested_petitioner_counsel <- NA
      } else {
        nested_petitioner_counsel <- lapply(records_petitioner, extract_info)
      }

      if (is.na(nested_petitioner_counsel)) {
        petitioner_counsel <- NA
      } else {
        petitioner_counsel <- sapply(nested_petitioner_counsel, function(x) {
          # Concatenate key-value pairs with a semi-colon separator
          info <- paste(names(x), unlist(x), sep = ": ", collapse = "; ")
          # Remove trailing space if it exists
          info <- sub(" $", "", info)
          return(info)
        })
      }

      combined_petitioner_counsel <- paste(petitioner_counsel, collapse = "\n")
      docket_entries$all_petitioner_counsel <- combined_petitioner_counsel

      if (is.na(records_other)) {
        nested_other_counsel <- NA
      } else {
        nested_other_counsel <- lapply(records_other, extract_info)
      }

      if (is.na(nested_other_counsel)) {
        other_counsel <- NA
      } else {
        other_counsel <- sapply(nested_other_counsel, function(x) {
          # Concatenate key-value pairs with a semi-colon separator
          info <- paste(names(x), unlist(x), sep = ": ", collapse = "; ")
          # Remove trailing space if it exists
          info <- sub(" $", "", info)
          return(info)
        })
      }


      combined_other_counsel <- paste(other_counsel, collapse = "\n")
      docket_entries$all_other_counsel <- combined_other_counsel

      if (is.na(records_respondent)) {
        nested_respondent_counsel <- NA
      } else {
        nested_respondent_counsel <- lapply(records_respondent, extract_info)
      }

      if (is.na(nested_respondent_counsel)) {
        respondent_counsel <- NA
      } else {
        respondent_counsel <- sapply(nested_respondent_counsel, function(x) {
          # Concatenate key-value pairs with a semi-colon separator
          info <- paste(names(x), unlist(x), sep = ": ", collapse = "; ")
          # Remove trailing space if it exists
          info <- sub(" $", "", info)
          return(info)
        })
      }
      combined_respondent_counsel <- paste(respondent_counsel, collapse = "\n")
      docket_entries$all_respondent_counsel <- combined_respondent_counsel

      docket_entries$petitioner_counsel <- sapply(docket_entries$all_petitioner_counsel, function(x) {
        dates <- strsplit(x, "\n")[[1]]
        if (length(dates) > 0) {
          return(dates[1])
        } else {
          return("")
        }
      })
      docket_entries$respondent_counsel <- sapply(docket_entries$all_respondent_counsel, function(x) {
        dates <- strsplit(x, "\n")[[1]]
        if (length(dates) > 0) {
          return(dates[1])
        } else {
          return("")
        }
      })

      docket_entries <- docket_entries %>%
        mutate(petitioner_counsel = gsub("\\;.*", "", petitioner_counsel),
               respondent_counsel = gsub("\\;.*", "", respondent_counsel)) %>%
        mutate(petitioner_counsel = gsub(".*Attorney: ", "", petitioner_counsel),
               respondent_counsel = gsub(".*Attorney: ", "", respondent_counsel)) %>%
        mutate(petitioner_counsel = trimws(petitioner_counsel),
               respondent_counsel = trimws(respondent_counsel)) %>%
        mutate(petitioner_counsel = gsub("(?:PO\\s|P\\.O\\.|#|\\d+|[^a-zA-Z]|(?:Attorney:))([^A-Z]*)(?:\\s{3}.*)?$", "\\1", petitioner_counsel, perl = TRUE),
               respondent_counsel = gsub("(?:PO\\s|P\\.O\\.|#|\\d+|[^a-zA-Z]|(?:Attorney:))([^A-Z]*)(?:\\s{3}.*)?$", "\\1", respondent_counsel, perl = TRUE)) %>%
        mutate(petitioner_counsel = gsub("\\bP\\.O\\.", "", petitioner_counsel),
               respondent_counsel = gsub("\\bP\\.O\\.", "", respondent_counsel)) %>%
        mutate(petitioner_counsel = gsub("\\b PO ", "", petitioner_counsel),
               respondent_counsel = gsub("\\b PO ", "", respondent_counsel)) %>%
        mutate(petitioner_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.| II| III)\\b).*", "\\1", petitioner_counsel),
               respondent_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.| II| III)\\b).*", "\\1", respondent_counsel)) %>%
        mutate(petitioner_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.|II|III)?).*", "\\1", petitioner_counsel),
               respondent_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.|II|III)?).*", "\\1", respondent_counsel)) %>%
        mutate(petitioner_counsel = gsub("[0-9]+\\s*$", "", petitioner_counsel),
               respondent_counsel = gsub("[0-9]+\\s*$", "", respondent_counsel))


    } #Extracting Counsel Info
  } #Counsel
  {
    months <- c("Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ", "Aug ", "Sep ", "Oct ", "Nov ", "Dec ")

    for (i in 1:nrow(docket_entries)) {
      original_text <- docket_entries$text_original[i]

      for (month in months) {
        original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
      }

      docket_entries$most_recent_order_check[i] <- original_text
    }

    split_rows <- strsplit(docket_entries$most_recent_order_check, "\n###\n")

    filtered_rows <- lapply(split_rows, function(row) {
      most_recent_orders <- grepl(paste(months, collapse = "|"), row, ignore.case = TRUE)
      row[most_recent_orders]
    })

    filtered_rows <- lapply(filtered_rows, function(x) tail(x, 1))


    cleaned_rows <- lapply(filtered_rows, function(row) {
      modified_row <- gsub("\n\n", " \n\n ", row)
      split_text <- strsplit(modified_row, " \n\n ")[[1]]
      split_text[1]
    })
    cleaned_rows <- lapply(cleaned_rows, function(row) {
      str_remove(row, "\\nMain.*")
    })


    nested_list <- lapply(cleaned_rows, function(row) list(row))

    replace_empty <- function(lst) {
      modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", "", x))
      return(modified_lst)
    }

    nested_list <- replace_empty(nested_list)

    docket_entries$most_recent_order <- sapply(nested_list, function(x) paste(unlist(x), collapse = "; "))

  } #Most Recent Order
  {


    months <- c("Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ", "Aug ", "Sep ", "Oct ", "Nov ", "Dec ")

    for (i in 1:nrow(docket_entries)) {
      original_text <- docket_entries$text_original[i]

      for (month in months) {
        original_text <- gsub(paste0("\\b", month), paste0("\n###\n", month), original_text)
      }

      docket_entries$most_recent_order_check[i] <- original_text
    }

    split_rows <- strsplit(docket_entries$most_recent_order_check, "\n###\n")


    filtered_rows <- lapply(split_rows, function(row) {
      row <- gsub("\n\n.*", "", row, perl = TRUE)
      row <- row[-1]  # Delete the first row
      row <- gsub("\nMain.*", "", row)
      row <- gsub("\nAttorneys for Petitioners.*", "", row)
      row <- gsub("\n", "", row, perl = TRUE)
      row
    })

    nested_list <- lapply(filtered_rows, function(row) list(row))

    replace_empty <- function(lst) {
      modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", "", x))
      return(modified_lst)
    }

    nested_list <- replace_empty(nested_list)

    docket_entries$all_docket_entries <- sapply(nested_list, function(x) paste(unlist(x), collapse = "; "))
  } #All Dated Entries & Orders
  {

    docket_entries <- docket_entries %>%
      mutate(docket_number = gsub("No. ", "", docket_number)) %>%
      mutate(petition_type = type) %>%
      mutate(amicus_indicator = ifelse(amicus_indicator == 1, "Yes", "No")) %>%
      select(docket_number, petition_type, petitioner, petitioner_counsel, all_petitioner_counsel, respondent, respondent_counsel, all_respondent_counsel, all_other_counsel, docketed, submitted_to, referred_to_court, application_type, linked_with, lower_court, lower_court_case_number, lower_court_decision_date, amicus_indicator, amicus_filers, amicus_count, first_conference_date, conference_count, multiple_conference_dates, most_recent_order, all_docket_entries, majority_opinion_writer, opinion_type, number_of_opinions, opinions_by_type, opinion_filings, argument_date, special_filing, ifp, docket_url)
    docket_entries[docket_entries == ""] <- NA




  } #Final Cleaning (Arrange columns & convert bytes to UTF-8)

  return(docket_entries)

}
