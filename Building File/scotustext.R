################################################################################
#scotustext.R
#Developed by Jake S. Truscott (May 2023)
#University of Georgia, Department of Political Science (Athens, GA)
#Center for C-SPAN Scholarship & Engagement, Purdue University (West Lafayette, Indiana)
################################################################################


################################################################################
#Stuff to Do
# 9) Independent tool for processing OA transcripts
# 10) Independent tool for processing decisions?
################################################################################

#######################################
#SCOTUS DOCKET
#######################################
# Make ASCII Art Print when library loaded
# Add all counsel beyond counsel of record? Maybe as a nested list - idk

#######################################
#OA Transcripts
#######################################

#######################################
#Decisions
#######################################
#Key break is "On writ of certiorari" --> "]" --- Indicates beginning of all new opinions
#Ex: ON WRIT OF CERTIORARI TO THE UNITED STATES COURT OF APPEALS FOR THE SECOND CIRCUIT [May 18, 2023]
# justice *BLANK* delivered = majority
# concurring = concurrence *Need to be cognizant of "with whom..."
# dissenting = dissent


################################################################################
#Necessary Packages
################################################################################

library(tidyverse); library(tidytext); library(tm); library(plyr); library(textdata); library(dplyr); library(repurrrsive); library(tidyr); library(ggplot2); library(data.table); library(ggrepel); library(lubridate); library(strex); library(utils); library(anytime); library(zoo); library(rvest); library(webshot); library(htmltools); library(pdftools); library(stringr); library(htm2txt); library(sjPlot); library(reactable); library(reactablefmtr); library(httr); library(devtools); library(writexl); library(readtext); library(usethis)



################################################################################
#Docket Search
################################################################################

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
    mutate(docket_number = gsub(".*\\-|.*a|.*A|.*m|.*M|.*Original|.*original|.*o|.*O|.*Orig|.*orig", "", docket_id)) %>%
    mutate(url = ifelse(as.numeric(year) < 10,
                        ifelse(type == 1, paste0(docket_url_older, "0", year, "a", docket_number, ".htm"),
                               ifelse(type == 2, paste0(docket_url_older, "0", year, "m", docket_number, ".htm"),
                                      ifelse(type == 3, paste0(docket_url_older, "0", year, "-", docket_number, ".htm"),
                                             ifelse(type == 4, paste0(docket_url_older, "0", year, "o", docket_number, ".htm"), NA)
                                      )
                               )
                        ),
                        ifelse(as.numeric(year) >= 10,
                               ifelse(as.numeric(year) >= 17,
                                      ifelse(type == 1, paste0(docket_url_newer, year, "a", docket_number, ".html"),
                                             ifelse(type == 2, paste0(docket_url_newer, year, "m", docket_number, ".html"),
                                                    ifelse(type == 3, paste0(docket_url_newer, year, "-", docket_number, ".html"),
                                                           ifelse(type == 4, paste0(docket_url_newer, year, "o", docket_number, ".html"), NA)
                                                    )
                                             )
                                      ),
                                      ifelse(as.numeric(year) < 17,
                                             ifelse(type == 1, paste0(docket_url_newer, year, "a", docket_number, ".htm"),
                                                    ifelse(type == 2, paste0(docket_url_newer, year, "m", docket_number, ".htm"),
                                                           ifelse(type == 3, paste0(docket_url_newer, year, "-", docket_number, ".htm"),
                                                                  ifelse(type == 4, paste0(docket_url_newer, year, "o", docket_number, ".htm"), NA)
                                                           )
                                                    )
                                             ),
                                             NA
                                      )
                               ),
                               NA
                        )
    ))
  urls <- term$url
  return(urls)
} #Process Docket ID Submission

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


      petitioner_counsel <- petitioner_counsel %>%
        mutate(petitioner_counsel_address = mapply(function(truncated, address) {
          gsub(truncated, "", address)
        }, petitioner_counsel_truncated, petitioner_counsel_address))













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

      respondent_counsel <- respondent_counsel %>%
        mutate(respondent_counsel_address = mapply(function(truncated, address) {
          gsub(truncated, "", address)
        }, respondent_counsel_truncated, respondent_counsel_address))



    } #Respondent Counsel Parse
    {

      docket_entries$petitioner_counsel_address <- petitioner_counsel$petitioner_counsel_address
      docket_entries$petitioner_counsel_address <- ifelse(is.na(docket_entries$petitioner_counsel_address), "", docket_entries$petitioner_counsel_address)
      docket_entries$petitioner_counsel <- petitioner_counsel$petitioner_counsel_truncated
      docket_entries$petitioner_counsel_address <- gsub(docket_entries$petitioner_counsel, "", docket_entries$petitioner_counsel_address)

      docket_entries$respondent_counsel_address <- respondent_counsel$respondent_counsel_address
      docket_entries$respondent_counsel_address <- ifelse(is.na(docket_entries$respondent_counsel_address), "", docket_entries$respondent_counsel_address)
      docket_entries$respondent_counsel <- respondent_counsel$respondent_counsel_truncated
      docket_entries$respondent_counsel_address <- gsub(docket_entries$respondent_counsel, "", docket_entries$respondent_counsel_address)

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

docket_call <- function(urls, rate, sleep) {
  docket_frame <- data.frame(url = character(), text = character(), stringsAsFactors = FALSE)
  error_count <- 0
  url_counter <- 0

  for (i in seq(1, length(urls), by = 5000)) {
    batch_urls <- urls[i:min(i+4999, length(urls))]
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
            cat(paste("Waiting 1 minute, then trying again...\n"))
            Sys.sleep(60)  # Pause for 1 minute
            try_count <- try_count + 1
          } else if (error_count == 21) {
            cat(paste("Could Not Collect From URL:", url, "\n"))
            cat(paste("Stopping Function. Please Ensure Docket Number Written Correctly and Try Again\n"))
            break  # Exit the inner loop
          } else {
            cat(paste("Error occurred with URL:", url, "\n"))
            try_count <- try_count + 1
          }
        })
        if (error_count == 0) {
          break  # Exit the retry loop if there was a successful retrieval
        }
      }
    }
    if (error_count == 21) {
      break  # Exit the outer loop if there were too many errors
    }
    if (i+4999 < length(urls)) {
      cat("Pausing for 30 seconds...\n")
      Sys.sleep(30)
    }
  }

  return(docket_frame)
} #Call to Docket via Internet

include_func <- function(docket_entities, cols){
  if (!is.null(cols)) {
    docket_entities <- docket_entities %>%
      select(all_of(cols))
  }
  return(docket_entities)
} #Include Parameter

exclude_func <- function(docket_entities, cols){
  if (!is.null(cols)) {
    docket_entities <- docket_entities %>%
      select(-cols)
  }
  return(docket_entities)
} #Exclude Parameter

write_csv_func <- function(docket_entries, write_csv) {
  if (write_csv) {
    cat("Code to Save Dataframe as (.csv):\n")
    cat("library(utils)\n")
    cat("write.csv(<SEARCH QUERY OBJECT NAME>, file = '<SAVE FILE>.csv', row.names = FALSE)\n")
  }
}

write_xlsx_func <- function(docket_entries, write_xlsx) {
  if (write_xlsx) {
    cat("Code to Save Dataframe as (.xlsx):\n")
    cat("library(writexl)\n")
    cat("write_xlsx(<SEARCH QUERY OBJECT NAME>, path = '<SAVE FILE>.xlsx')\n")
  }
}

write_reactable_func <- function(docket_entries, write_reactable){
  if (write_reactable){
    cat("Code to Create (.HTML) Table Using Reactable:\n")
    cat("library(reactable); library(reactablefmtr)\n")
    cat("reactable(docket_entries, searchable = T,\n defaultPageSize = 20,\n theme = fivethirtyeight(font_size = 12, font_color = 'grey',\n cell_padding = 6)) %>%\n")
    cat("add_title('<YOUR TITLE HERE>') %>%\n")
    cat("add_subtitle('<YOUR SUBTITLE HERE>')\n")
    }
}

docket_search <- function(docket_id, rate = 50, sleep = 30, include = NULL, exclude = NULL, write_csv = FALSE, write_xlsx = FALSE, write_reactable = FALSE) {
  urls <- suppressWarnings(process_docket_id(docket_id))
  urls <- gsub(" ", "", urls)
  urls <- gsub("- ", "-", urls)
  urls <- gsub(" -", "-", urls)
  cat("\nDocket Sheets to Collect: ", length(unique(urls)), "\n")

  docket_frame <- docket_call(urls, rate, sleep)
  cat("\nCompleted Docket Collection\nMoving to Parsing and Cleaning\n")


  docket_frame$text <- gsub("\\xe2\\x80\\xa2", " ", docket_frame$text)
  docket_frame$text <- trimws(docket_frame$text)
  docket_entities <- suppressWarnings(clean_docket_frame(docket_frame = docket_frame))

  if (!is.null(include)) {
    docket_entities <- include_func(docket_entities, include)
  }

  if (!is.null(exclude)) {
    docket_entities <- exclude_func(docket_entities, exclude)
  }

  write_csv_func(docket_entities, write_csv)
  write_xlsx_func(docket_entities, write_xlsx)
  write_reactable_func(docket_entries, write_reactable)

  message("\nSuccess!")

  return(docket_entities)

} # Primary Docket Search

print_flattened_list <- function(column) {
  if (length(column) > 1) {
    for (row in column) {
      if (length(row) > 0) {
        elements <- unlist(strsplit(row, "; "))
        print(list(elements))
      } else {
        print(list())
      }
    }
  } else {
    nested_list <- lapply(column[[1]], function(element) list(unlist(strsplit(element, "; "))))
    print(nested_list)
  }
}

print_flattened_list(test$opinion_filings)

plf_docket_ids <- docket_id[1:10000]

################################################################################
#Oral Argument Transcript Processor
################################################################################

oa_search <- function(term = NULL, justice = NULL, attorney = NULL, speaker_type = NULL, docket_id = NULL, party = NULL) {
  base_url <- "https://github.com/JakeTruscott/scotustext/blob/fbd53c4bbd755f724cc9a02afb05a2ffe391f75a/Data/"
  rdata_url <- paste0(base_url, "scotus_transcripts_04_22.rdata")
  load(url(rdata_url))

  if (!is.null(term)) {
    if (is.character(term)) {
      term <- list(term)
    }
    selection <- lapply(term, function(t) {
      scotus %>% filter(term == t)
    })
    scotus <- do.call(rbind, selection)
  }

  if (!is.null(docket_id)) {
    if (is.character(docket_id)) {
      docket_id <- list(docket_id)
    }
    selection <- lapply(docket_id, function(t) {
      scotus %>% filter(argument %in% t)
    })
    scotus <- do.call(rbind, selection)
  }

  if (!is.null(party)) {
    if (is.character(party)) {
      party <- list(party)
    }

    if (length(party) > 0) {
      party_short <- scotus$case_name
      party_short <- toupper(party_short)
      scotus$party_short <- party_short
      scotus <- scotus[grepl(toupper(party), party_short), ]

      if (nrow(scotus) > 0) {
        filtered_parties <- unique(scotus$case_name)
        filtered_parties <- filtered_parties[!is.na(filtered_parties)]
        #message("Oral Argument Data Filtered for:")
        #message(paste("Parties:", paste(filtered_parties, collapse = ", ")))
      }

            scotus$party_short <- NULL
    }
  }

  if (!is.null(speaker_type)) {
    if (is.character(speaker_type)) {
      speaker_type <- list(speaker_type)
    }
    selection <- lapply(speaker_type, function(t) {
      scotus %>% filter(type == t)
    })
    scotus <- do.call(rbind, selection)
  }

  if (!is.null(justice)) {
    justice_values <- toupper(justice)
    valid_speakers <- ifelse(grepl("roberts", justice, ignore.case = T), paste0("CHIEF JUSTICE ", justice), paste0("JUSTICE ", justice))

    justice_data <- scotus %>% filter(type == "Justice" & toupper(speaker) %in% toupper(valid_speakers))
    scotus <- justice_data
  }

  if (!is.null(attorney)) {
    if (is.character(attorney)) {
      attorney <- list(attorney)
    }

    if (length(attorney) > 0) {
      attorney_short <- gsub("(MR\\. |MS\\. |GENERAL\\. |GENERAL )", "", scotus$speaker)
      attorney_short <- toupper(attorney_short)
      scotus$attorney_short <- attorney_short
      scotus <- scotus[attorney_short %in% toupper(attorney), ]

      if (nrow(scotus) > 0) {
        filtered_attorneys <- unique(scotus$attorney_short)
        filtered_attorneys <- filtered_attorneys[!is.na(filtered_attorneys)]
        message("Oral Argument Data Filtered for:")
        message(paste("Attorneys:", paste(filtered_attorneys, collapse = ", ")))
      }

      # Remove the "attorney_short" column
      scotus$attorney_short <- NULL
    }
  }


  # Print the Filters

  collected_terms <- unique(scotus$term)
  if (length(collected_terms) > 0) {
    collected_terms <- sort(collected_terms)
    terms_string <- paste(collected_terms, collapse = ", ")
    message("Oral Argument Data Collected for...")
    message(paste("Terms:", terms_string))
  }

  if (!is.null(docket_id)) {
    collected_arguments <- unique(scotus$argument)
    if (length(collected_arguments) > 0) {
      collected_arguments <- sort(collected_arguments)
      arguments_string <- paste(collected_arguments, collapse = ", ")
      message(paste("Arguments:", arguments_string))
    }
  }

  if (!is.null(party)) {
    collected_cases <- unique(scotus$case_name)
    if (length(collected_cases) > 0) {
      collected_cases <- sort(collected_cases)
      cases_string <- paste(collected_cases, collapse = ", ")
      message(paste("Cases:", cases_string))
    }
  }

  collected_speaker_type <- unique(scotus$type)
  if (length(collected_speaker_type) > 0) {
    if ("Justice" %in% collected_speaker_type && "Attorney" %in% collected_speaker_type) {
      message("Speaker Type: Both Justices and Attorneys")
    } else if ("Justice" %in% collected_speaker_type) {
      message("Speaker Type: Justices ONLY")
    } else if ("Attorney" %in% collected_speaker_type) {
      message("Speaker Type: Attorneys Only")
    }
  }

  collected_justices <- unique(scotus$speaker)
  if (!is.null(justice)) {
    collected_justices <- unique(scotus$speaker)
    if (length(collected_justices) > 0) {
      collected_justices <- sort(collected_justices)
      justices_string <- paste(collected_justices, collapse = ", ")
      message(paste("Justices:", justices_string))
    }
  }

  collected_justices <- unique(scotus$speaker)
  if (!is.null(justice)) {
    collected_justices <- unique(scotus$speaker)
    if (length(collected_justices) > 0) {
      collected_justices <- sort(collected_justices)
      justices_string <- paste(collected_justices, collapse = ", ")
      message(paste("Justices:", justices_string))
    }
  }

  return(scotus)
}

test <- oa_search(docket_id = c("19-1392", "02-1472", "02-1672"))

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
    dplyr::mutate(id = row_number()) %>%
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

scotus <- oa_parser(dir_path = "C:/Users/Jake Truscott/OneDrive - University of Georgia/Active/Justices Oral Argument Rhetoric/SCOTUS_Transcripts/Combined Transcripts")



################################################################################
#Decision Processor
################################################################################
#Key break is "On writ of certiorari" --> "]" --- Indicates beginning of all new opinions
#Ex: ON WRIT OF CERTIORARI TO THE UNITED STATES COURT OF APPEALS FOR THE SECOND CIRCUIT [May 18, 2023]
# justice *BLANK* delivered = majority
# concurring = concurrence *Need to be cognizant of "with whom..."
# dissenting = dissent

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
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "on writ of certiorari", replacement = "<DECISION BREAK>", ignore.case = T)
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
  replace_between_cuts <- function(text) {
    pattern <- "(?<=<CUT>  <CUT> \n)(.*?)(?=\n?<CUT>)"
    replacement <- "[FIGURE AND CAPTION INSERTED HERE] "
    gsub(pattern, replacement, text, perl = TRUE)
  }
  corpus<- tm_map(corpus, content_transformer(replace_between_cuts))
  #corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  replace_between_cuts <- function(text){
    pattern = "\n------\n  "
    replacement = "\n------\n"
    gsub(pattern, replacement, text, perl = TRUE)
  }
  corpus <- tm_map(corpus, content_transformer(remove_cut_text))

  corpus[["1"]][["content"]]
  cat(corpus[["1"]][["content"]])


  decisions <- data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE) # Convert Corpus to Dataframe
  for (file_name in file_names) {
    file_names_processed <- c(file_names_processed, file_name)
  }

  # Process the file names at the end
  file_names <- list.files(directory)
  file_names_processed <- gsub("\\_.*", "", file_names)
  file_names_processed <- gsub(".pdf", "", file_names_processed)
  file_names_processed <- data.frame(file_names_processed)
  names(file_names_processed)[1] <- "file_names"
  decisions$argument <- file_names_processed$file_names


  decisions_test <- decisions %>%
    group_by(argument) %>%
    mutate(text = str_remove(text, ".*?<DECISION BREAK>")) %>%
    mutate(text = str_split(text, "\\<DECISION BREAK\\>")) %>%
    unnest(text) %>%
    filter(!text == "") %>%
    mutate(text = str_replace(text, ".*?\\]", "")) %>%
    mutate(text = gsub("U\\. S\\. C\\. \\?", "U. S. C. \u00A7", text)) %>%
    mutate(text = gsub("\\?(?=[0-9])", "\u00A7", text, perl = TRUE)) %>%
    mutate(text = gsub("Cite as:(.*?)\\)([^\\s]*\\s[^\\s]*)", "", text, perl = TRUE)) %>%
    mutate(text = gsub("Opinion of the Court", "", text, ignore.case = F)) %>%
    mutate(text = gsub("   ", " ", text)) %>%
    mutate(text = gsub("------", "", text)) %>%
    mutate(text = gsub("- ", "", text)) %>%
    mutate(text = gsub("\\\"", "", text)) %>%
    mutate(text = gsub("<CUT> APPENDIX.*", "", text)) %>%
    mutate(text = gsub("SUPREME COURT OF THE UNITED STATES \\_.*", "", text)) %>%
    mutate(text = gsub("<CUT> <CUT>", "[FIGURE INSERTED HERE].", text)) %>%
    mutate(text = gsub("<CUT>", "", text)) %>%
    mutate(text = trimws(text)) %>%
    mutate(text = gsub("\\[FIGURE INSERTED HERE\\]. \\[FIGURE AND CAPTION INSERTED HERE\\]", "[FIGURE AND CAPTION INSERTED HERE].", text)) %>%
    mutate(text = str_replace(text, pattern = "---.*?\\.(?= [a-z])", "")) %>%
    #mutate(text = gsub("  ", " ", text)) %>%
    mutate(text = str_replace(text, "\\.\\d[ \n]", ". "))
  #Replaces Footnote Numbers in Text

} #Baseline Code for Opinion Parser (need to add author, type, etc.)



cat(decisions_test$text[3])





#Need to be able to differentiate between Footnotes and Regular text




art <- readLines("scotus_text.txt")
dput(art)
art <- c("", "=============================================================================================",
         "=============================================================================================",
         " _______  _______  _______ _________          _______   _________ _______          _________",
         "(  ____ \\(  ____ \\(  ___  )\\__   __/|\\     /|(  ____ \\  \\__   __/(  ____ \\|\\     /|\\__   __/",
         "| (    \\/| (    \\/| (   ) |   ) (   | )   ( || (    \\/     ) (   | (    \\/( \\   / )   ) (   ",
         "| (_____ | |      | |   | |   | |   | |   | || (_____      | |   | (__     \\ (_) /    | |   ",
         "(_____  )| |      | |   | |   | |   | |   | |(_____  )     | |   |  __)     ) _ (     | |   ",
         "      ) || |      | |   | |   | |   | |   | |      ) |     | |   | (       / ( ) \\    | |   ",
         "/\\____) || (____/\\| (___) |   | |   | (___) |/\\____) |     | |   | (____/\\( /   \\ )   | |   ",
         "\\_______)(_______/(_______)   )_(   (_______)\\_______)     )_(   (_______/|/     \\|   )_(   ",
         "                                                                                            ",
         "=============================================================================================",
         "=============================================================================================",
         "                                    Jake S. Truscott, Ph.D",
         "                                      Developed May 2023", "                                 Thanks for using SCOTUS TEXT!"
)
cat(art, sep = "\n")
