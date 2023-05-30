oa_search <- function(term = NULL, justice = NULL, attorney = NULL, speaker_type = NULL, docket_id = NULL, party = NULL) {
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

  return(scotus)
}

