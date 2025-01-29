#' SCOTUS Oral Argument Search
#'
#' @param term Term of interest as character or vector
#' @param justice Justice(s) of interest as character or vector
#' @param attorney Attorney(s) of interest as character or vector
#' @param speaker_type Factor indicating return Justices or Attorneys only
#' @param docket_id Supreme Court docket identification number coinciding with argument(s) of interest
#' @param party Party or parties represented in argument(s) of interest
#'
#' @return Parsed data frame of oral argument data (2004 to present or reflective of optional parameters)
#' @export
#'
#' @examples
#' sample_oa <- oa_search(term = "2005", justice = "Breyer")
oa_search <- function(term = NULL, justice = NULL, attorney = NULL, speaker_type = NULL, docket_id = NULL, party = NULL) {

  type <- NULL
  speaker <- NULL
  argument <- NULL

  base_url <- "https://github.com/JakeTruscott/scotustext/raw/master/Data/"
  rdata_url <- paste0(base_url, "scotus_transcripts.rdata")
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


  cat("\n- - - - - - - - COMPLETION SUMMARY - - - - - - - -")


  collected_terms <- unique(scotus$term)
  if (length(collected_terms) > 0) {
    collected_terms <- sort(collected_terms)
    terms_string <- paste(collected_terms, collapse = ", ")
    cat("\nTerms: ", terms_string)
  }

  if (!is.null(docket_id)) {
    collected_arguments <- unique(scotus$argument)
    if (length(collected_arguments) > 0) {
      collected_arguments <- sort(collected_arguments)
      arguments_string <- paste(collected_arguments, collapse = ", ")
      cat("\nArguments: ", arguments_string)
    }
  }

  if (!is.null(party)) {
    collected_cases <- unique(scotus$case_name)
    if (length(collected_cases) > 0) {
      collected_cases <- sort(collected_cases)
      cases_string <- paste(collected_cases, collapse = ", ")
      cat("\nCases: ", cases_string)
    }
  }

  collected_justices <- unique(scotus$speaker)
  if (!is.null(justice)) {
    collected_justices <- unique(scotus$speaker)
    if (length(collected_justices) > 0) {
      collected_justices <- sort(collected_justices)
      justices_string <- paste(collected_justices, collapse = ", ")
      cat("\nJustices: ", justices_string)    }
  }

  collected_speaker_type <- unique(scotus$type)
  if (length(collected_speaker_type) > 0) {
    if ("Justice" %in% collected_speaker_type && "Attorney" %in% collected_speaker_type) {
      cat("\nSpeaker Type: Both Justices and Attorneys\n")
    } else if ("Justice" %in% collected_speaker_type) {
      cat("\nSpeaker Type: Justices ONLY\n")
    } else if ("Attorney" %in% collected_speaker_type) {
      cat("\nSpeaker Type: Attorneys Only\n")
    }
  }


  return(scotus)
}

