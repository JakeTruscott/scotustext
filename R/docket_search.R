#' SCOTUS Docket Search
#'
#' @param docket_id The docket number of interest - written as a character vector in R. This can take the form of filed petitions (e.g., “19-1392”), applications (e.g., “22A948”), or motions (e.g., “22M11”). docket_id parameter is REQUIRED for all searches. *Note*: The function will also accept list objects containing comma-separated character vectors of docket numbers (e.g., docket_ids <- c(“19-108”, “19-184”, “19-199”, “19-251”), as well as original jurisdiction petitions (e.g., "22O145")
#' @param rate The number of requests to process per function call (default is 5000).
#' @param sleep The number of seconds to wait between docket requests (default is 30 seconds).
#' @param include A character vector that will limit the output variables to only those requested (default is NULL).
#' @param exclude A character vector that will exclude any output variables requested (default is NULL)
#'
#' @return Cleaned and parsed information from docketed petitions, applications, and motions to the United States Supreme Court.
#' @export
#'
#' @examples
#' \dontrun{
#' sample_docket <- docket_search(docket_id = c("19-1392", "22A122", "22M101", "22O145"))
#' }
docket_search <- function(docket_id, rate = 5000, sleep = 30, include = NULL, exclude = NULL) {
  exclude_func <- function(docket_entities, cols){
    if (!is.null(cols)) {
      docket_entities <- docket_entities %>%
        select(-cols)
    }
    return(docket_entities)
  } #Exclude Parameter

  include_func <- function(docket_entities, cols){
    if (!is.null(cols)) {
      docket_entities <- docket_entities %>%
        select(all_of(cols))
    }
    return(docket_entities)
  } #Include Parameter

  process_docket_id <- function(docket_id) {
    docket_url_newer <- "https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/"
    docket_url_older <- "https://www.supremecourt.gov/search.aspx?filename=/docketfiles/"
    term <- as.data.frame(docket_id)
    term <- term %>%
      mutate(type = 3) %>%
      mutate(type = ifelse(grepl("m", docket_id, ignore.case = T), 2, type)) %>%
      mutate(type = ifelse(grepl("a", docket_id, ignore.case = T), 1, type)) %>%
      mutate(type = ifelse(grepl("o", docket_id, ignore.case = T), 4, type)) %>%
      mutate(year = as.numeric(gsub("\\-.*|a.*|A.*|m.*|M.*|Original.*|original.*|o.*|O.*|Orig.*|orig.*", "", docket_id))) %>%
      mutate(year = ifelse(as.numeric(year) %in% 0:9, as.character(paste0(sprintf("%02d", year))), as.character(year))) %>%
      mutate(docket_number = gsub(".*\\-|.*a|.*A|.*m|.*M|.*Original|.*original|.*o|.*O|.*Orig|.*orig", "", docket_id)) %>%
      mutate(year = gsub("[^0-9]", "", year),
             docket_number = gsub("[^0-9]", "", docket_number)) %>% #Delete Non-Numbers
      mutate(html = ifelse(as.numeric(year) > 50, "HTM",
                           ifelse(as.numeric(year) < 17, "HTM",
                                  ifelse(as.numeric(year) >= 17 & as.numeric(year) < 30, "HTML", "HTM")))) %>%
      mutate(url = case_when(
        type == 1 & html == "HTM" ~ paste0(docket_url_older, year, "a", docket_number, ".htm"),
        type == 1 & html == "HTML" ~ paste0(docket_url_newer, year, "a", docket_number, ".html"),

        type == 2 & html == "HTM" ~ paste0(docket_url_older, year, "m", docket_number, ".htm"),
        type == 2 & html == "HTML" ~ paste0(docket_url_newer, year, "m", docket_number, ".html"),

        type == 3 & html == "HTM" ~ paste0(docket_url_older, year, "-", docket_number, ".htm"),
        type == 3 & html == "HTML" ~ paste0(docket_url_newer, year, "-", docket_number, ".html"),

        type == 4 & html == "HTM" ~ paste0(docket_url_older, year, "o", docket_number, ".htm"),
        type == 4 & html == "HTML" ~ paste0(docket_url_newer, year, "o", docket_number, ".html")
      ))
    urls <- term$url
    return(urls)
  } #Translate Docket IDs to URLs

  docket_call <- function(urls, rate, sleep) {
    docket_frame <- data.frame(url = character(), text = character(), stringsAsFactors = FALSE)
    error_count <- 0
    url_counter <- 0

    for (i in seq(1, length(urls), by = rate)) {
      batch_urls <- urls[i:min(i + rate - 1, length(urls))]

      for (j in seq_along(batch_urls)) {
        url <- batch_urls[j]
        try_count <- 0

        while (try_count < 6) {
          tryCatch({
            text <- gettxt(url, encoding = "UTF-8")
            docket_frame <- rbind(docket_frame, data.frame(url = url, text = text, stringsAsFactors = FALSE))
            error_count <- 0  # Reset error count if there was a successful retrieval
            url_counter <- url_counter + 1  # Increment the URL counter

            if (url_counter %% 500 == 0) {
              cat(paste("Completed Docket:", url_counter, "\n"))
            }
          }, error = function(e) {
            error_count <- error_count + 1

            if (error_count == 1) {
              cat(paste("Error occurred with URL:", url, "\n"))
              cat(paste("Waiting a few seconds then trying again...\n"))
              Sys.sleep(5)  # Pause for 5 seconds
              try_count <- try_count + 1
            } else if (error_count == 21) {
              cat(paste("Could Not Collect From URL:", url, "\n"))
              cat(paste("Stopping Function. Please Ensure Docket Number Written Correctly and Try Again\n"))
              break  # Exit the outer loop
            } else {
              cat(paste("Error occurred with URL:", url, "\n"))
              cat(paste("Waiting a few seconds then trying again...\n"))
              Sys.sleep(5)  # Pause for 5 seconds
              try_count <- try_count + 1
            }
          })

          if (error_count == 0) {
            break  # Exit the inner loop
          }
        }
      }

      if (error_count == 21) {
        break  # Exit the outer loop
      }

      if (i + rate - 1 < length(urls)) {
        cat("Pausing for 30 seconds...\n")
        Sys.sleep(30)
      }
    }


    return(docket_frame)
  } # Call to Docket

  clean_docket_frame <- function(docket_frame, include, exclude){

    {
      corpus <- tm::Corpus(VectorSource(docket_frame[["text"]]))
      corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\xe2\\x80\\xa2", replacement = " ")
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
        case_title <- lapply(split_pr, function(row) {
          has_title <- grepl("(Petitioner|Plaintiff|Petitioners|Plaintiffs|Appellant|Appellants|In Re|Applicant|Applicants)", row, ignore.case = TRUE)
          row[has_title]
        })

        cleaned_case_title <- lapply(case_title, function(row) {
          row <- str_replace_all(row, "\n", " ")
          row <- str_replace_all(row, ".*Title:", "")
          row <- trimws(row)
          if (length(row) > 1) {
            row <- row[1]
          }

          row
        })

        nested_case_title <- lapply(cleaned_case_title, function(row) list(row))
        replace_empty <- function(lst) {
          modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", NA, x))
          return(modified_lst)
        }
        nested_case_title <- replace_empty(nested_case_title)
        docket_entries$case_title <- sapply(nested_case_title, function(x) paste(unlist(x), collapse = "; "))
        docket_entries$case_title <- trimws(docket_entries$case_title)



      } #Case Title
      {
        petitioner <- lapply(split_pr, function(row) {
          has_petitioner <- grepl("(Petitioner|Plaintiff|Petitioners|Plaintiffs|Appellant|Appellants|In Re|Applicant|Applicants)", row, ignore.case = TRUE)
          row[has_petitioner]
        })
        cleaned_petitioner <- lapply(petitioner, function(row) {
          row <- str_replace_all(row, "\n", " ")
          row <- str_replace_all(row, ".*Title:", "")
          row <- str_replace_all(row, "v\\..*", "")
          row <- str_replace_all(row, ", (Petitioner|Plaintiff|Petitioners|Plaintiffs|Appellant|Appellants|In Re|Applicant|Applicants).*", "")
          if (length(row) > 1) {
            row <- row[1]
          }

          row
        })
        nested_petitioner <- lapply(cleaned_petitioner, function(row) list(row))
        replace_empty <- function(lst) {
          modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", NA, x))
          return(modified_lst)
        }
        nested_petitioner <- replace_empty(nested_petitioner)
        docket_entries$petitioner <- sapply(nested_petitioner, function(x) paste(unlist(x), collapse = "; "))
        docket_entries$petitioner <- trimws(docket_entries$petitioner)


      } #Petitioner
      {
        respondent <- lapply(split_pr, function(row) {
          has_respondent <- grepl("(Petitioner|Plaintiff|Petitioners|Plaintiffs|Appellant|Appellants|In Re|Applicant|Applicants)", row, ignore.case = TRUE)
          row[has_respondent]
        })
        cleaned_respondent <- lapply(respondent, function(row) {
          row <- str_replace_all(row, "\n", "")
          row <- str_replace(row, ".*v\\.", "")
          if (length(row) > 1) {
            row <- row[1]
          }

          row
        })
        nested_respondent <- lapply(cleaned_respondent, function(row) list(row))
        replace_empty <- function(lst) {
          modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", NA, x))
          return(modified_lst)
        }
        nested_respondent <- replace_empty(nested_respondent)
        docket_entries$respondent <- sapply(nested_respondent, function(x) paste(unlist(x), collapse = "; "))
        docket_entries$respondent <- trimws(docket_entries$respondent)
        docket_entries$respondent = ifelse(grepl("In Re", docket_entries$petitioner, ignore.case = T), NA, docket_entries$respondent)

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
          original_text <- gsub(paste0("(\n)", month), paste0("\n###\n", month), original_text)
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
          original_text <- gsub(paste0("(\n)", month), paste0("\n###\n", month), original_text)
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
        row <- gsub("Argued\\..*", "", row)
        row <- trimws(row)
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
          original_text <- gsub(paste0("(\n)", month), paste0("\n###\n", month), original_text)
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
        gsub("\n\n.*", "", row)
        gsub("\n.*", "", row)
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
          original_text <- gsub(paste0("(\n)", month), paste0("\n###\n", month), original_text)
        }

        docket_entries$opinion_writer_check[i] <- original_text
      }

      split_rows <- strsplit(docket_entries$opinion_writer_check, "\n###\n")
      filtered_rows <- lapply(split_rows, function(row) {
        has_opinion_writer <- grepl("delivered the opinion", row, ignore.case = TRUE)
        row[has_opinion_writer]
      })
      cleaned_rows <- lapply(filtered_rows, function(row) {
        gsub("\n\n.*", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
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

        for (month in months){
          original_text <- gsub(paste0("(\n)", month), paste0("\n###\n", month), original_text)
        }

        docket_entries$opinion_writer_check[i] <- original_text
      }

      split_rows <- strsplit(docket_entries$opinion_writer_check, "\n###\n")
      filtered_rows <- lapply(split_rows, function(row) {
        has_opinion_writer <- grepl("delivered the opinion", row, ignore.case = TRUE)
        row[has_opinion_writer]
      })
      cleaned_rows <- lapply(filtered_rows, function(row) {
        gsub("\n\n.*", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        gsub(", J\\.,", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
        str_remove(row, "\n\n")
      })
      count_filed <- str_count(cleaned_rows, pattern = "filed")
      cleaned_rows <- str_count(cleaned_rows, pattern = "delivered") + count_filed

      docket_entries$number_of_opinions <- as.integer(cleaned_rows)
      docket_entries$number_of_opinions <- ifelse(grepl("per curiam", docket_entries$text_original, ignore.case = TRUE), "Per Curiam", docket_entries$number_of_opinions)

    } #Number of Opinions Filed
    {

      for (i in 1:nrow(docket_entries)) {
        original_text <- docket_entries$text_original[i]

        for (month in months){
          original_text <- gsub(paste0("(\n)", month), paste0("\n###\n", month), original_text)
        }

        docket_entries$opinion_writer_check[i] <- original_text
      }

      split_rows <- strsplit(docket_entries$opinion_writer_check, "\n###\n")
      filtered_rows <- lapply(split_rows, function(row) {
        has_opinion_writer <- grepl("delivered the opinion", row, ignore.case = TRUE)
        row[has_opinion_writer]
      })
      cleaned_rows <- lapply(filtered_rows, function(row) {
        gsub("\n\n.*", "", row)
      })
      cleaned_rows <- lapply(cleaned_rows, function(row) {
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

        for (month in months){
          original_text <- gsub(paste0("(\n)", month), paste0("\n###\n", month), original_text)
        }

        docket_entries$opinion_writer_check[i] <- original_text

        split_rows <- strsplit(docket_entries$opinion_writer_check[i], "\n###\n")
        filtered_rows <- lapply(split_rows, function(row) {
          has_opinion_writer <- grepl("delivered the opinion", row, ignore.case = TRUE)
          row[has_opinion_writer]
        })
        cleaned_rows <- lapply(filtered_rows, function(row) {
          gsub("\n\n.*", "", row)
        })
        cleaned_rows <- lapply(cleaned_rows, function(row) {
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

      months <- c("Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ", "Aug ", "Sep ", "Oct ", "Nov ", "Dec ")

      for (i in 1:nrow(docket_entries)) {
        original_text <- docket_entries$text_original[i]

        for (month in months) {
          original_text <- gsub(paste0("(\n)", month), paste0("\n###\n", month), original_text)
        }

      }

      docket_entries$amicus_check[i] <- iconv(original_text, to = "UTF-8", sub = "")
      encoded_string <- docket_entries$amicus_check[i]

      split_rows <- strsplit(encoded_string, "\n###\n")


      filtered_rows <- lapply(split_rows, function(row) {
        has_ruling <- grepl("Judgment |Judgments |Adjudged ", row, ignore.case = TRUE) & !grepl("JUDGMENT ISSUED", row, ignore.case = T)
        row[has_ruling]
      })

      cleaned_rows <- lapply(filtered_rows, function(row) {
        row <- iconv(row, to = "UTF-8", sub = "")
        row <- gsub("\\n.*", "", row)
        has_ruling <- grepl("Affirmed | Vacated | Remanded | Reversed", row, ignore.case = TRUE) & !grepl("JUDGMENT ISSUED", row, ignore.case = T)
        row[has_ruling]
      })

      nested_list <- lapply(cleaned_rows, function(row) list(row))

      replace_empty <- function(lst) {
        modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", "", x))
        return(modified_lst)
      }

      nested_list <- replace_empty(nested_list)

      docket_entries$opinion_raw[i] <- nested_list
      docket_entries <- docket_entries %>%
        mutate(opinion_raw = sapply(opinion_raw, function(x) paste(unlist(x), collapse = "; "))) %>%
        mutate(opinion_raw = sapply(opinion_raw, function(x) paste(unlist(x), collapse = "; "))) %>%
        mutate(
          opinion_type = ifelse(
            grepl("Petition", opinion_raw, ignore.case = TRUE) &
              grepl("GRANTED", opinion_raw, ignore.case = T) &
              grepl("VACATED", opinion_raw, ignore.case = T) &
              grepl("REMANDED", opinion_raw, ignore.case = T),
            "GVR",
            ifelse(
              grepl("improvidently granted", opinion_raw, ignore.case = TRUE),
              "DIG",
              ifelse(
                grepl("AFFIRMED", opinion_raw, ignore.case = TRUE) &
                  !grepl("REMANDED", opinion_raw, ignore.case = T) &
                  !grepl("REVERSED", opinion_raw, ignore.case = T),
                "Affirmed",
                ifelse(
                  grepl("REVERSED", opinion_raw, ignore.case = TRUE) &
                    !grepl("REMANDED", opinion_raw, ignore.case = T) &
                    !grepl("AFFIRMED", opinion_raw, ignore.case = T),
                  "Reversed",
                  ifelse(
                    grepl("VACATED", opinion_raw, ignore.case = TRUE) &
                      grepl("REMANDED", opinion_raw, ignore.case = T) &
                      !grepl("AFFIRMED", opinion_raw, ignore.case = T),
                    "Vacated and Remanded",
                    ifelse(
                      grepl("REVERSED", opinion_raw, ignore.case = TRUE) &
                        grepl("REMANDED", opinion_raw, ignore.case = T) &
                        !grepl("AFFIRMED", opinion_raw, ignore.case = T),
                      "Reversed and Remanded",
                      NA ))))))) %>%
        mutate(opinion_type = ifelse(is.na(opinion_type), "No Opinion Issued", opinion_type)) %>%
        mutate(opinion_type = ifelse(grepl("IN PART", opinion_raw, ignore.case = T), paste0(opinion_type, " (IN PART)"), opinion_type))


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
          has_application_type <- grepl(", submitted to", row, ignore.case = F)
          row[has_application_type]
        })

        cleaned_rows <- lapply(filtered_rows, function(row) {
          gsub("\\([^()]*\\)", "", row)
        })
        cleaned_rows <- lapply(cleaned_rows, function(row) {
          row <- gsub("(.*Application |.*Motion )", "", row)
          row <- gsub("(\\, submitted .*|submitted .*|)", "", row)
          str_remove(row, "\n\n")
          trimws(row)
          row <- gsub("^ ", "", row)
          str_to_title(row)
        })

        docket_entries$application_type = cleaned_rows
        docket_entries$application_type = ifelse(docket_entries$type == "Docketed Case", NA, docket_entries$application_type)
        docket_entries$application_type = str_to_title(docket_entries$application_type)


      } #Application Type



    } #Special Application Terms
    {
      if (grepl(".html", docket_entries$docket_url, ignore.case = F)){
        {
          {

            petitioner_pattern <- "(Attorneys for (Petitioner|Plaintiff|Petitioners|Plaintiff|Appellant|Appellants|Applicant|Applicants))\\n\\n([\\s\\S]*?)(?=\\n\\nAttorneys for (Respondent|Respondents|Appellee|Appellees)|(\\n|\\n\\n)Other|\\n\\n\\{1\\})"

            counsel <- docket_entries %>%
              select(text_original) %>%
              mutate(all_petitioner_counsel = str_extract_all(text_original, petitioner_pattern)) %>%
              mutate(all_respondent_counsel = str_extract(text_original, "(Attorneys for (Respondent|Respondents|Appellee|Appellees))\\n\\n([\\s\\S]*?)(?=(\\n\\n)|Other)")) %>%
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
                     all_respondent_counsel = gsub("\n\nParty", "\nParty", all_respondent_counsel)) %>%
              mutate(all_petitioner_counsel = gsub("( Jr\\. | II | III )", "", all_petitioner_counsel),
                     all_other_counsel = gsub("( Jr\\. | II | III ) ", "", all_other_counsel),
                     all_respondent_counsel = gsub("( Jr\\. | II | III )", "", all_respondent_counsel))




          } #Parsing Counsel (HTML)
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
                  if (!exists("Organization", info))
                    info$Organization <- list()
                  info$Organization <- c(info$Organization, gsub("^Organization: ", "", line))
                } else if (grepl("^Party name:", line)) {
                  if (!exists("PartyName", info))
                    info$PartyName <- list()
                  info$PartyName <- c(info$PartyName, gsub("^Party name: ", "", line))
                }
              }

              return(info)
            }


            {
              if (is.na(counsel$all_petitioner_counsel) || counsel$all_petitioner_counsel == "") {
                records_petitioner <- NA
              } else {
                records_petitioner <- lapply(counsel$all_petitioner_counsel, function(x) {
                  flattened <- unlist(strsplit(x, "\n\n"))
                  flattened <- lapply(flattened, function(x) {
                    if (grepl("Organization:(.*)\\nParty name:", x, perl = TRUE)) {
                      x <- gsub("Organization:(.*)\\nParty name:", "Organization:\\1\nParty name:", x, perl = TRUE)
                    }
                    x <- gsub("\n(?!\\n)", " ", x, perl = TRUE)
                    x <- trimws(x)
                    x <- gsub(" Organization:", " \nOrganization:", x)
                    x <- gsub(" Party name:", " \nParty name:", x)
                    x <- gsub("\\; ", " \\(\\&\\) ", x)
                    x
                  })
                  flattened <- unlist(flattened)
                  flattened
                })
                records_petitioner <- unlist(records_petitioner)
                records_petitioner <- paste(records_petitioner, collapse = "; ")
              }
              docket_entries$all_petitioner_counsel <- records_petitioner

            } #Petitioner Counsel

            {
              if (is.na(counsel$all_respondent_counsel) || counsel$all_respondent_counsel == "") {
                records_respondent <- NA
              } else {
                records_respondent <- lapply(counsel$all_respondent_counsel, function(x) {
                  flattened <- unlist(strsplit(x, "\n\n"))
                  flattened <- lapply(flattened, function(x) {
                    if (grepl("Organization:(.*)\\nParty name:", x, perl = TRUE)) {
                      x <- gsub("Organization:(.*)\\nParty name:", "Organization:\\1\nParty name:", x, perl = TRUE)
                    }
                    x <- gsub("\n(?!\\n)", " ", x, perl = TRUE)
                    x <- trimws(x)
                    x <- gsub(" Organization:", " \nOrganization:", x)
                    x <- gsub(" Party name:", " \nParty name:", x)
                    x <- gsub("\\; ", " \\(\\&\\) ", x)
                    x
                  })
                  flattened <- unlist(flattened)
                  flattened
                })
                records_respondent <- unlist(records_respondent)
                records_respondent <- paste(records_respondent, collapse = "; ")
              }
              docket_entries$all_respondent_counsel <- records_respondent
            } #Respondent Counsel

            {
              if (is.na(counsel$all_other_counsel) || counsel$all_other_counsel == "") {
                records_other <- NA
              } else {
                records_other <- lapply(counsel$all_other_counsel, function(x) {
                  flattened <- unlist(strsplit(x, "\n\n"))
                  flattened <- lapply(flattened, function(x) {
                    if (grepl("Organization:(.*)\\nParty name:", x, perl = TRUE)) {
                      x <- gsub("Organization:(.*)\\nParty name:", "Organization:\\1\nParty name:", x, perl = TRUE)
                    }
                    x <- gsub("\n(?!\\n)", " ", x, perl = TRUE)
                    x <- trimws(x)
                    x <- gsub(" Organization:", " \nOrganization:", x)
                    x <- gsub(" Party name:", " \nParty name:", x)
                    x <- gsub("\\; ", " \\(\\&\\) ", x)
                    x
                  })
                  flattened <- unlist(flattened)
                  flattened
                })
                records_other <- unlist(records_other)
                records_other <- paste(records_other, collapse = "; ")
              }
              docket_entries$all_other_counsel <- records_other
            } #Other Counsel

            docket_entries <- docket_entries %>%
              mutate(petitioner_counsel = gsub("\\;.*", "", all_petitioner_counsel),
                     respondent_counsel = gsub("\\;.*", "", all_respondent_counsel)) %>%
              mutate(petitioner_counsel = gsub(".*Attorney: ", "", petitioner_counsel),
                     respondent_counsel = gsub(".*Attorney: ", "", respondent_counsel)) %>%
              mutate(petitioner_counsel = trimws(petitioner_counsel),
                     respondent_counsel = trimws(respondent_counsel)) %>%
              mutate(petitioner_counsel = gsub("(?:\\s{3}.*)?$", "", petitioner_counsel, perl = TRUE),
                     respondent_counsel = gsub("(?:\\s{3}.*)?$", "", respondent_counsel , perl = TRUE)) %>%
              mutate(petitioner_counsel = gsub("\\bP\\.O\\..*", "", petitioner_counsel),
                     respondent_counsel = gsub("\\bP\\.O\\..*", "", respondent_counsel)) %>%
              mutate(petitioner_counsel = gsub("\\b PO .*", "", petitioner_counsel),
                     respondent_counsel = gsub("\\b PO .*", "", respondent_counsel)) %>%
              mutate(petitioner_counsel = gsub("\\#.*", "", petitioner_counsel),
                     respondent_counsel = gsub("\\#.*", "", respondent_counsel)) %>%
              mutate(petitioner_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.| II| III)\\b).*", "\\1", petitioner_counsel),
                     respondent_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.| II| III)\\b).*", "\\1", respondent_counsel)) %>%
              mutate(petitioner_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.|II|III)?).*", "\\1", petitioner_counsel),
                     respondent_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.|II|III)?).*", "\\1", respondent_counsel)) %>%
              mutate(petitioner_counsel = gsub("[0-9]+\\s*$", "", petitioner_counsel),
                     respondent_counsel = gsub("[0-9]+\\s*$", "", respondent_counsel)) %>%
              mutate(petitioner_counsel = gsub("\\d", "", petitioner_counsel),
                     respondent_counsel = gsub("\\d", "", respondent_counsel)) %>%
              mutate(all_other_counsel = gsub("Attorney: Attorneys", "Attorney: ", all_other_counsel),
                     all_petitioner_counsel = gsub("Attorney: Attorneys", "Attorney: ", all_petitioner_counsel),
                     all_respondent_counsel = gsub("Attorney: Attorneys", "Attorney: ", all_respondent_counsel)) %>%
              mutate(all_other_counsel = gsub("\\nOrganization:  \\nOrganization:", "\nOrganization: ", all_other_counsel),
                     all_petitioner_counsel = gsub("\\nOrganization:  \\nOrganization:", "\nOrganization: ", all_petitioner_counsel),
                     all_respondent_counsel = gsub("\\nOrganization:  \\nOrganization:", "\nOrganization: ", all_respondent_counsel)) %>%
              mutate(all_other_counsel = gsub("Attorney\\: \\:", "Attorney:", all_other_counsel),
                     all_petitioner_counsel = gsub("Attorney\\: \\:", "Attorney:", all_petitioner_counsel),
                     all_respondent_counsel = gsub("Attorney\\: \\:", "Attorney:", all_respondent_counsel))



            if (is.na(docket_entries$petitioner)) {
              docket_entries$petitioner_counsel <- NA
            } else {
              if (grepl(docket_entries$petitioner, docket_entries$petitioner_counsel, ignore.case = TRUE)) {
                docket_entries$petitioner_counsel <- docket_entries$petitioner
              }
            }

            if (is.na(docket_entries$respondent)) {
              docket_entries$respondent_counsel <- NA
            } else {
              if (grepl(docket_entries$respondent, docket_entries$respondent_counsel, ignore.case = TRUE)) {
                docket_entries$respondent_counsel <- docket_entries$respondent
              }
            }


          } #Extracting Counsel Info (HTML)
        } #Counsel
      } else {
        {

          petitioner_pattern <- "(?s)(Attorneys for (Petitioner|Plaintiff|Petitioners|Plaintiff|Appellant|Appellants|Applicant|Applicants)).*?(Attorneys for (Respondent|Respondents|Appellee|Appellees)|\\n\\nOther|\\n\\n\\{1\\})"
          respondent_pattern <- "(?s)(Attorneys for (Respondent|Respondents|Appellee|Appellees)).*?(\\n\\nOther|\\n\\n\\{1\\})"


          counsel <- docket_entries %>%
            select(text_original) %>%
            mutate(all_petitioner_counsel = str_extract(text_original, petitioner_pattern)) %>%
            mutate(all_petitioner_counsel = gsub(".*Attorneys for (Petitioner|Plaintiff|Petitioners|Plaintiff|Appellant|Appellants|Applicant|Applicants)\\:", "", all_petitioner_counsel)) %>%
            mutate(all_petitioner_counsel = gsub("Attorneys for.*", "", all_petitioner_counsel)) %>%
            mutate(all_petitioner_counsel = sub("\n\n(?!\\s)", "", all_petitioner_counsel, perl = TRUE))  %>%
            mutate(all_respondent_counsel = str_extract(text_original, respondent_pattern)) %>%
            mutate(all_respondent_counsel = gsub("Attorneys for (Respondent|Respondents|Appellee|Appellees)\\:", "", all_respondent_counsel)) %>%
            mutate(all_respondent_counsel = sub("\n\n(?!\\s)", "", all_respondent_counsel, perl = TRUE))  %>%
            mutate(all_other_counsel = gsub("^.*Attorneys for (Respondent|Respondents|Appellee|Appellees)", "", text_original)) %>%
            mutate(all_other_counsel = gsub("\n\nOther", "<OTHER BREAK>", all_other_counsel)) %>%
            mutate(all_other_counsel = ifelse(grepl("<OTHER BREAK>", all_other_counsel),
                                              gsub(".*<OTHER BREAK>", "", all_other_counsel),
                                              ""))  %>%
            mutate(all_other_counsel = gsub("\n\n\\{1\\}", " <OTHER END> ", all_other_counsel)) %>%
            mutate(all_other_counsel = sub("<OTHER END>.*", "", all_other_counsel)) %>%
            mutate(all_respondent_counsel = sub("(\\n\\nOther|\\n\\n\\{1\\})", "", all_respondent_counsel, perl = TRUE))  %>%
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
            mutate(all_respondent_counsel = gsub("Counsel of Record", "", all_respondent_counsel),
                   all_petitioner_counsel = gsub("Counsel of Record", "", all_petitioner_counsel),
                   all_other_counsel = gsub("Counsel of Record", "", all_other_counsel)) %>%
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
            mutate(all_petitioner_counsel = gsub(":\n\n", "", all_petitioner_counsel),
                   all_respondent_counsel = gsub(":\n\n", "", all_respondent_counsel),
                   all_other_counsel = gsub(":\n\n", "", all_other_counsel)) %>%
            mutate( contains_org_petitioner = !grepl("Organization", all_petitioner_counsel),
                    contains_org_respondent = !grepl("Organization", all_respondent_counsel),
                    contains_org_other = !grepl("Organization", all_other_counsel)) %>%
            mutate(all_petitioner_counsel = if_else(contains_org_petitioner, sub("(^(?:\\S+\\s+){3}\\S+)", "\\1 \nOrganization:", all_petitioner_counsel, perl = TRUE),
                                                    if_else( grepl("\\d", all_petitioner_counsel),sub("(\\d+)", " \nOrganization: \\1", all_petitioner_counsel, perl = TRUE), all_petitioner_counsel )),all_respondent_counsel = if_else(contains_org_respondent, sub("(^(?:\\S+\\s+){3}\\S+)", "\\1 \nOrganization:", all_respondent_counsel, perl = TRUE),
                                                                                                                                                                                                                                         if_else(grepl("\\d", all_respondent_counsel), sub("(\\d+)", " \nOrganization: \\1", all_respondent_counsel, perl = TRUE), all_respondent_counsel)),
                   all_other_counsel = if_else( contains_org_other, sub("(^(?:\\S+\\s+){3}\\S+)", "\\1 \nOrganization:", all_other_counsel, perl = TRUE),
                                                if_else(grepl("\\d", all_other_counsel),sub("(\\d+)", " \nOrganization: \\1", all_other_counsel, perl = TRUE), all_other_counsel))) %>%
            mutate(all_petitioner_counsel = gsub("\n\nParty", "\nParty", all_petitioner_counsel),
                   all_other_counsel = gsub("\n\nParty", "\nParty", all_other_counsel),
                   all_respondent_counsel = gsub("\n\nParty", "\nParty", all_respondent_counsel)) %>%
            mutate(all_petitioner_counsel = if_else(
              stringr::str_detect(all_petitioner_counsel, "\nAttorney:$"),
              stringr::str_replace(all_petitioner_counsel, "\nAttorney:$", ""),
              all_petitioner_counsel )) %>%
            mutate(
              all_petitioner_counsel = str_replace_all(all_petitioner_counsel, "(?<!\\S)\\s{2,}(?!\\S)", " "),
              all_other_counsel = str_replace_all(all_other_counsel, "(?<!\\S)\\s{2,}(?!\\S)", " "),
              all_respondent_counsel = str_replace_all(all_respondent_counsel, "(?<!\\S)\\s{2,}(?!\\S)", " ")) %>%
            mutate(all_petitioner_counsel = gsub("\\n\\n", "\n", all_petitioner_counsel),
                   all_respondent_counsel = gsub("\\n\\n", "\n", all_respondent_counsel),
                   all_other_counsel = gsub("\\n\\n", "\n", all_other_counsel)) %>%
            mutate(all_petitioner_counsel = gsub("( Jr\\. | II | III )", "", all_petitioner_counsel),
                   all_other_counsel = gsub("( Jr\\. | II | III ) ", "", all_other_counsel),
                   all_respondent_counsel = gsub("( Jr\\. | II | III )", "", all_respondent_counsel)) %>%
            mutate(all_petitioner_counsel = gsub(" \\nAttorney:", " \n\nAttorney", all_petitioner_counsel),
                   all_other_counsel = gsub(" \\nAttorney:", " \n\nAttorney", all_other_counsel),
                   all_respondent_counsel = gsub(" \\nAttorney:", " \n\nAttorney", all_respondent_counsel))



        } #Parsing Counsel (HTM)
        {

          counsel <- counsel %>%
            mutate(all_other_counsel = ifelse(all_other_counsel == all_respondent_counsel, NA, all_other_counsel)) %>%
            mutate(all_other_counsel = ifelse(all_other_counsel == all_petitioner_counsel, NA, all_other_counsel))

          {
            if (is.na(counsel$all_petitioner_counsel) || counsel$all_petitioner_counsel == "") {
              records_petitioner <- NA
            } else {
              records_petitioner <- lapply(counsel$all_petitioner_counsel, function(x) {
                flattened <- unlist(strsplit(x, "\n\n"))
                flattened <- lapply(flattened, function(x) {
                  if (grepl("Organization:(.*)\\nParty name:", x, perl = TRUE)) {
                    x <- gsub("Organization:(.*)\\nParty name:", "Organization:\\1\nParty name:", x, perl = TRUE)
                  }
                  x <- gsub("\n(?!\\n)", " ", x, perl = TRUE)
                  if (!grepl("\\\\nOrganization", x, fixed = TRUE)) {
                    if (!grepl("\\d+\\s|\\(|#", x)) {
                      x <- sub("(\\S+\\s+\\S+\\s+\\S+\\s+)", "\\1 \nOrganization: ", x, perl = TRUE)
                    } else {
                      x <- sub("(\\d+\\s|\\(|#)", " \nOrganization: \\1", x, perl = TRUE)
                    }
                  }
                  x <- gsub(" Organization:", " \nOrganization:", x)
                  x <- gsub(" Party name:", " \nParty name:", x)
                  x <- gsub("(?<!General)Attorney\\s(?![A-Za-z])", "Attorney:", x, perl = TRUE)
                  x <- gsub("\\; ", " \\(\\&\\) ", x)
                  x
                })
                flattened <- unlist(flattened)
                flattened
              })
              records_petitioner <- unlist(records_petitioner)
              records_petitioner <- paste(records_petitioner, collapse = "; ")
            }
            docket_entries$all_petitioner_counsel <- records_petitioner
            } #petitioner Counsel

          {
            if (is.na(counsel$all_respondent_counsel) || counsel$all_respondent_counsel == "") {
              records_respondent <- NA
            } else {
              records_respondent <- lapply(counsel$all_respondent_counsel, function(x) {
                flattened <- unlist(strsplit(x, "\n\n"))
                flattened <- lapply(flattened, function(x) {
                  if (grepl("Organization:(.*)\\nParty name:", x, perl = TRUE)) {
                    x <- gsub("Organization:(.*)\\nParty name:", "Organization:\\1\nParty name:", x, perl = TRUE)
                  }
                  x <- gsub("\n(?!\\n)", " ", x, perl = TRUE)
                  if (!grepl("\\\\nOrganization", x, fixed = TRUE)) {
                    if (!grepl("\\d+\\s|\\(|#", x)) {
                      x <- sub("(\\S+\\s+\\S+\\s+\\S+\\s+)", "\\1 \nOrganization: ", x, perl = TRUE)
                    } else {
                      x <- sub("(\\d+\\s|\\(|#)", " \nOrganization: \\1", x, perl = TRUE)
                    }
                  }
                  x <- gsub(" Organization:", " \nOrganization:", x)
                  x <- gsub(" Party name:", " \nParty name:", x)
                  x <- gsub("(?<!General)Attorney\\s(?![A-Za-z])", "Attorney:", x, perl = TRUE)
                  x <- gsub("\\; ", " \\(\\&\\) ", x)
                  x
                })
                flattened <- unlist(flattened)
                flattened
              })
              records_respondent <- unlist(records_respondent)
              records_respondent <- paste(records_respondent, collapse = "; ")
            }
            docket_entries$all_respondent_counsel <- records_respondent
          } #Respondent Counsel

          {
            if (is.na(counsel$all_other_counsel) || counsel$all_other_counsel == "") {
              records_other <- NA
            } else {
              records_other <- lapply(counsel$all_other_counsel, function(x) {
                flattened <- unlist(strsplit(x, "\n\n"))
                flattened <- lapply(flattened, function(x) {
                  if (grepl("Organization:(.*)\\nParty name:", x, perl = TRUE)) {
                    x <- gsub("Organization:(.*)\\nParty name:", "Organization:\\1\nParty name:", x, perl = TRUE)
                  }
                  x <- gsub("\n(?!\\n)", " ", x, perl = TRUE)
                  if (!grepl("\\\\nOrganization", x, fixed = TRUE)) {
                    if (!grepl("\\d+\\s|\\(|#", x)) {
                      x <- sub("(\\S+\\s+\\S+\\s+\\S+\\s+)", "\\1 \nOrganization: ", x, perl = TRUE)
                    } else {
                      x <- sub("(\\d+\\s|\\(|#)", " \nOrganization: \\1", x, perl = TRUE)
                    }
                  }
                  x <- gsub(" Organization:", " \nOrganization:", x)
                  x <- gsub(" Party name:", " \nParty name:", x)
                  x <- gsub("(?<!General)Attorney\\s(?![A-Za-z])", "Attorney:", x, perl = TRUE)
                  x <- gsub("\\; ", " \\(\\&\\) ", x)
                  x
                })
                flattened <- unlist(flattened)
                flattened
              })
              records_other <- unlist(records_other)
              records_other <- paste(records_other, collapse = "; ")
            }
            docket_entries$all_other_counsel <- records_other
          } #Other Counsel


          docket_entries <- docket_entries %>%
            mutate(petitioner_counsel = gsub("\\;.*", "", all_petitioner_counsel),
                   respondent_counsel = gsub("\\;.*", "", all_respondent_counsel)) %>%
            mutate(petitioner_counsel = gsub(".*Attorney: ", "", petitioner_counsel),
                   respondent_counsel = gsub(".*Attorney: ", "", respondent_counsel)) %>%
            mutate(petitioner_counsel = trimws(petitioner_counsel),
                   respondent_counsel = trimws(respondent_counsel)) %>%
            mutate(petitioner_counsel = gsub("(?:\\s{3}.*)?$", "", petitioner_counsel, perl = TRUE),
                   respondent_counsel = gsub("(?:\\s{3}.*)?$", "", respondent_counsel , perl = TRUE)) %>%
            mutate(petitioner_counsel = gsub("\\bP\\.O\\..*", "", petitioner_counsel),
                   respondent_counsel = gsub("\\bP\\.O\\..*", "", respondent_counsel)) %>%
            mutate(petitioner_counsel = gsub("\\b PO .*", "", petitioner_counsel),
                   respondent_counsel = gsub("\\b PO .*", "", respondent_counsel)) %>%
            mutate(petitioner_counsel = gsub("\\#.*", "", petitioner_counsel),
                   respondent_counsel = gsub("\\#.*", "", respondent_counsel)) %>%
            mutate(petitioner_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.| II| III)\\b).*", "\\1", petitioner_counsel),
                   respondent_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.| II| III)\\b).*", "\\1", respondent_counsel)) %>%
            mutate(petitioner_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.|II|III)?).*", "\\1", petitioner_counsel),
                   respondent_counsel = gsub("^((?:[^ ]* ){2}[^ ]*(?: Jr\\.| Sr\\.|II|III)?).*", "\\1", respondent_counsel)) %>%
            mutate(petitioner_counsel = gsub("[0-9]+\\s*$", "", petitioner_counsel),
                   respondent_counsel = gsub("[0-9]+\\s*$", "", respondent_counsel)) %>%
            mutate(petitioner_counsel = gsub("\\d", "", petitioner_counsel),
                   respondent_counsel = gsub("\\d", "", respondent_counsel)) %>%
            mutate(all_other_counsel = gsub("Attorney: Attorneys", "Attorney: ", all_other_counsel),
                   all_petitioner_counsel = gsub("Attorney: Attorneys", "Attorney: ", all_petitioner_counsel),
                   all_respondent_counsel = gsub("Attorney: Attorneys", "Attorney: ", all_respondent_counsel)) %>%
            mutate(all_other_counsel = gsub("\\nOrganization:  \\nOrganization:", "\nOrganization: ", all_other_counsel),
                   all_petitioner_counsel = gsub("\\nOrganization:  \\nOrganization:", "\nOrganization: ", all_petitioner_counsel),
                   all_respondent_counsel = gsub("\\nOrganization:  \\nOrganization:", "\nOrganization: ", all_respondent_counsel))




          if (is.na(docket_entries$petitioner)) {
            docket_entries$petitioner_counsel <- NA
          } else {
            if (grepl(docket_entries$petitioner, docket_entries$petitioner_counsel, ignore.case = TRUE)) {
              docket_entries$petitioner_counsel <- docket_entries$petitioner
            }
          }

          if (is.na(docket_entries$respondent)) {
            docket_entries$respondent_counsel <- NA
          } else {
            if (grepl(docket_entries$respondent, docket_entries$respondent_counsel, ignore.case = TRUE)) {
              docket_entries$respondent_counsel <- docket_entries$respondent
            }
          }


        } #Extracting Counsel Info (HTM)
      }



    } #Counsel
    {
      months <- c("Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ", "Aug ", "Sep ", "Oct ", "Nov ", "Dec ")

      for (i in 1:nrow(docket_entries)) {
        original_text <- docket_entries$text_original[i]

        for (month in months) {
          original_text <- gsub(paste0("(\n)", month), paste0("\n###\n", month), original_text)
        }

        docket_entries$most_recent_order_check[i] <- original_text
      }

      split_rows <- strsplit(docket_entries$most_recent_order_check, "\n###\n")


      filtered_rows <- lapply(split_rows, function(row) {
        most_recent_orders <- grepl(paste(months, collapse = "|"), row, ignore.case = TRUE)
        row[most_recent_orders]
      })


      filtered_rows <- lapply(filtered_rows, function(row) {
        row <- gsub("\n\n.*", "", row)
        row <- row[!grepl("Party name", row)]
        row <- row[!grepl("SEARCH TIPS", row, ignore.case = T)]
        row
      })

      filtered_rows <- lapply(filtered_rows, function(x) tail(x, 1))

      cleaned_rows <- lapply(filtered_rows, function(row) {
        modified_row <- gsub("\n\n", " \n\n ", row)
        modified_row <- gsub("\\n", " ", modified_row)
        modified_row <- gsub(" Main.*", "", modified_row)
        split_text <- strsplit(modified_row, " \n\n ")[[1]]
        split_text[1]
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
          original_text <- gsub(paste0("(\n)", month), paste0("\n###\n", month), original_text)
        }

        docket_entries$most_recent_order_check[i] <- original_text
      }

      split_rows <- strsplit(docket_entries$most_recent_order_check, "\n###\n")

      # Remove lines containing "\nParty name"
      split_rows <- lapply(split_rows, function(row) {
        row <- gsub("\n\n.*", "", row)
        row <- gsub("\n.*", "", row)
        row <- row[!grepl("Party name", row)]
        row <- row[!grepl("SEARCH TIPS", row, ignore.case = T)]
        row
      })


      filtered_rows <- lapply(split_rows, function(row) {
        row <- gsub("\n\n.*", "", row, perl = TRUE)
        row <- gsub("\nMain.*", "", row)
        row <- gsub("\nAttorneys for (Petitioner|Plaintiff|Petitioners|Plaintiff|Appellant|Appellants|Applicant|Applicants).*", "", row)
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

      months <- c("Jan ", "Feb ", "Mar ", "Apr ", "May ", "Jun ", "Jul ", "Aug ", "Sep ", "Oct ", "Nov ", "Dec ")

      for (i in 1:nrow(docket_entries)) {
        original_text <- docket_entries$text_original[i]

        for (month in months) {
          original_text <- gsub(paste0("(\n)", month), paste0("\n###\n", month), original_text)
        }

        docket_entries$most_recent_order_check[i] <- original_text
      }

      split_rows <- strsplit(docket_entries$most_recent_order_check, "\n###\n")
      split_rows <- lapply(split_rows, function(row){
        row <- gsub("\\n.*", "", row)
        row
      })

      filtered_rows <- lapply(split_rows, function(row) {
        has_ruling <- grepl("Petition", row, ignore.case = TRUE)  & grepl("(GRANTED|DENIED)", row, ignore.case = TRUE)
        has_ruling <- grepl("Petition (GRANTED|DENIED)| certiorari (GRANTED|DENIED)", row, ignore.case = TRUE)
        row[has_ruling]
      })

      nested_list <- lapply(filtered_rows, function(row) list(row))

      replace_empty <- function(lst) {
        modified_lst <- lapply(lst, function(x) ifelse(x == "character(0)", "", x))
        return(modified_lst)
      }

      nested_list <- replace_empty(nested_list)

      docket_entries$certiorari_full <- sapply(nested_list, function(x) paste(unlist(x), collapse = "; "))

      docket_entries <- docket_entries %>%
        mutate(certiorari_order = ifelse(grepl("GRANTED", certiorari_full, ignore.case = T), "Petition GRANTED",
                                         ifelse(grepl("DENIED", certiorari_full, ignore.case = T), "Petition DENIED", NA))) %>%
        mutate(certiorari_order = ifelse(grepl("; ", certiorari_full, ignore.case = T), "Multiple Petition Orders", certiorari_order))



    } #Certiorari Order
    {

      docket_entries <- docket_entries %>%
        mutate(docket_number = gsub("No. ", "", docket_number)) %>%
        mutate(docketed = ifelse(grepl('Docketed:', docketed, ignore.case = T), gsub(";.*?", "", docketed), docketed)) %>%
        mutate(petition_type = type) %>%
        mutate(amicus_indicator = ifelse(amicus_indicator == 1, "Yes", "No")) %>%
        select(docket_number, petition_type, case_title, petitioner, petitioner_counsel, all_petitioner_counsel, respondent, respondent_counsel, all_respondent_counsel, all_other_counsel, docketed, submitted_to, referred_to_court, application_type, linked_with, lower_court, lower_court_case_number, lower_court_decision_date, amicus_indicator, amicus_filers, amicus_count, first_conference_date, conference_count, multiple_conference_dates, certiorari_order, most_recent_order, all_docket_entries, majority_opinion_writer, opinion_type, number_of_opinions, opinions_by_type, opinion_filings, argument_date, special_filing, docket_url)

      if (grepl('Docketed:', docket_entries$docketed)) {
        modified_text <- gsub("^((?:\\S+\\s+){2}\\S+).*", "\\1", docket_entries$all_docket_entries)
        modified_text <- gsub("^(\\S+\\s+\\S+)", "\\1,", modified_text)
        modified_text <- gsub("Jan ", "January ", modified_text)
        modified_text <- gsub("Feb ", "February ", modified_text)
        modified_text <- gsub("Mar ", "March ", modified_text)
        modified_text <- gsub("Apr ", "April ", modified_text)
        modified_text <- gsub("May ", "May ", modified_text)
        modified_text <- gsub("Jun ", "June ", modified_text)
        modified_text <- gsub("Jul ", "July ", modified_text)
        modified_text <- gsub("Aug ", "August ", modified_text)
        modified_text <- gsub("Sep ", "September ", modified_text)
        modified_text <- gsub("Oct ", "October ", modified_text)
        modified_text <- gsub("Nov ", "November ", modified_text)
        modified_text <- gsub("Dec ", "December ", modified_text)

        docket_entries$docketed <- modified_text
      }

      docket_entries[docket_entries == ""] <- NA




    } #Final Cleaning (Arrange columns & convert bytes to UTF-8)

    return(docket_entries)

  } #Clean and Parse Retrieved Docket Text

  all_of <- NULL
  type <- NULL
  year <- NULL
  text_original <- NULL
  amicus_filers <- NULL
  amicus_indicator <- NULL
  amicus_count <- NULL
  argument_date <- NULL
  multiple_conference_dates <- NULL
  majority_opinion_writer <- NULL
  opinions_by_type <- NULL
  opinion_raw <- NULL
  opinion_type <- NULL
  submitted_to <- NULL
  all_respondent_counsel <- NULL
  all_other_counsel <- NULL
  all_petitioner_counsel <- NULL
  contains_org_petitioner <- NULL
  contains_org_respondent <- NULL
  contains_org_other <- NULL
  petitioner_counsel <- NULL
  respondent_counsel <- NULL
  certiorari_full <- NULL
  certiorari_order <- NULL
  type <- NULL
  petition_type <- NULL
  referred_to_court <- NULL
  application_type <- NULL
  first_conference_date <- NULL
  conference_count <- NULL
  most_recent_order <- NULL
  all_docket_entries <- NULL
  number_of_opinions <- NULL
  opinion_filings <- NULL
  special_filing <- NULL
  docket_url <- NULL


  urls <- suppressWarnings(process_docket_id(docket_id))
  urls <- gsub(" ", "", urls)
  urls <- gsub("- ", "-", urls)
  urls <- gsub(" -", "-", urls)
  cat("\nDocket Sheets to Collect: ", length(unique(urls)), "\n")

  docket_entries <- NULL
  completed_count <- 0
  final_urls <- c()
  retry_urls <- c()
  failed_urls <- c()
  non_collect <- c()

  process_url <- function(url, docket_entries) {
    docket_frame <- suppressWarnings(docket_call(url, sleep))
    docket_frame$text <- gsub("\\xe2\\x80\\xa2", " ", docket_frame$text)
    docket_frame$text <- trimws(docket_frame$text)
    docket_entities <- suppressWarnings(clean_docket_frame(docket_frame = docket_frame))

    if (!is.null(include)) {
      docket_entities <- include_func(docket_entities, include)
    }

    if (!is.null(exclude)) {
      docket_entities <- exclude_func(docket_entities, exclude)
    }

    if (is.null(docket_entries))
      docket_entries <- docket_entities
    else
      docket_entries <- rbind(docket_entries, docket_entities)


    completed_count <<- completed_count + 1

    if (completed_count %% 50 == 0) {
      message("Successfully Retrieved ", completed_count, " Docket Sheets")
    }

    return(docket_entries)
  }

  for (url in urls) {
    tryCatch({
      docket_entries <- process_url(url, docket_entries)
    }, error = function(e) {
      docket_number <- gsub(".*docketfiles\\/", "", url)
      docket_number <- gsub(".*(/Public/)", "", docket_number, ignore.case = TRUE)
      docket_number <- gsub("\\.html", "", docket_number)
      docket_number <- gsub("\\.htm", "", docket_number)
      message(paste("Error Retrieving and Cleaning Docket Information from Docket ID:", docket_number))
      retry_urls <<- c(retry_urls, url)
      message("Saving URL Information and Trying Again Later")
      message("Waiting 5 Seconds Before Moving On...")
      Sys.sleep(5)
      cat("Resuming Collection Effort\n")
    })
  }

  if (length(retry_urls) > 0) {
    docket_url_newer <- "https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/"
    docket_url_older <- "https://www.supremecourt.gov/search.aspx?filename=/docketfiles/"
    for (retry_url in retry_urls) {
      message("\nBeginning Second Attempt at Processing...\n")
      if (grepl("\\.html", retry_url)) {
        docket_number <- gsub(".*(/Public/)", "", retry_url, ignore.case = TRUE)
        docket_number <- gsub("\\.html", "", docket_number)
        docket_year <- gsub("([moa].*)|\\-.*", "", docket_number)
        docket_year <- as.numeric(docket_year)
        if (as.numeric(docket_year) == 16) {
          retry_url <- paste0(docket_url_older, docket_number, ".htm")
        } else {
          retry_url <- paste0(docket_url_newer, docket_number, ".html")
        }
      } else {
        docket_number <- gsub(".*docketfiles\\/", "", retry_url)
        docket_number <- gsub("\\.htm", "", docket_number)
        docket_year <- gsub("([moa].*)|\\-.*", "", docket_number)
        docket_year <- as.numeric(docket_year)
        if (as.numeric(docket_year) == 16) {
          retry_url <- paste0(docket_url_newer, docket_number, ".html")
        } else {
          retry_url <- paste0(docket_url_older, docket_number, ".htm")
        }
      }

      tryCatch({
        docket_entries <- process_url(retry_url, docket_entries)
      }, error = function(e) {
        message(paste("Error In Second Attempt -- Adjusting HTTP Conditions & Trying Again Later ( Docket ID:", docket_number, ")"))
        final_urls <<- c(final_urls, retry_url)
      })
    }
  }

  if (length(final_urls) > 0) {
    docket_url_newer <- "https://www.supremecourt.gov/search.aspx?filename=/docket/docketfiles/html/public/"
    docket_url_older <- "https://www.supremecourt.gov/search.aspx?filename=/docketfiles/"
    for (final_url in final_urls) {
      if (grepl("\\.html", final_url)) {
        docket_number <- gsub(".*(/Public/)", "", final_url, ignore.case = TRUE)
        docket_number <- gsub("\\.html", "", docket_number)
        final_url <- paste0(docket_url_older, docket_number, ".htm")
      } else {
        docket_number <- gsub(".*docketfiles\\/", "", final_url)
        docket_number <- gsub("\\.htm", "", docket_number)
        final_url <- paste0(docket_url_newer, docket_number, ".html")
      }

      tryCatch({
        docket_entries <- process_url(final_url, docket_entries)
      }, error = function(e) {
        message(paste("Error In Final Attempt -- Saving For Completion Report ( Docket ID:", docket_number, ")"))
        non_collect <<- c(non_collect, final_url)
      })
    }
  }


  cat("\n- - - - - - - - COMPLETION SUMMARY - - - - - - - -")
  cat("\nTotal Docket Sheets Successfully Retrieved:", length(docket_entries$docket_number))
  cat("\nTotal Docket Sheets Failed to Retrieve:", length(failed_urls))
  cat("\nDocket Types Retrieved...")
  cat("\n   Docketed Petitions: ", length(docket_entries$petition_type[docket_entries$petition_type == "Docketed Case"]))
  cat("\n   Docketed Petitions (Original Jurisdiction): ", length(docket_entries$petition_type[docket_entries$petition_type == "Docketed Case - Original Jurisdiction"]))
  cat("\n   Applications: ", length(docket_entries$petition_type[docket_entries$petition_type == "Application"]))
  cat("\n   Motions: ", length(docket_entries$petition_type[docket_entries$petition_type == "Motion"]))

  if (length(non_collect) > 0) {
    non_collect <- gsub(".*(/Public/)", "", non_collect, ignore.case = TRUE)
    non_collect <- gsub(".*docketfiles\\/", "", non_collect)
    non_collect <- gsub(".html", "", non_collect)
    non_collect <- gsub(".htm", "", non_collect)
    cat("\nFailed to Retrieve Docket Information for: ")
    cat(non_collect, sep = ifelse(length(non_collect) > 1, ", ", ""))
  }

  return(docket_entries)
}



