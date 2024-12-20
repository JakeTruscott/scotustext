#' Oyez Oral Argument Transcript Retrieval & Processor
#'
#' @param docket Character denoting Supreme Court docket number (e.g., '19-1392')
#' @param term  Character denoting Supreme Court term (e.g., '2021')
#'
#' @return Returns dataframe object with cleaned and parsed oral argument transcript
#' @export
#'
#' @examples
#' \dontrun{
#' dobbs_sample <- oyez_transcript_search(docket = '19-1392', term = '2021')
#' }
oyez_transcript_search <- function(docket = NULL, # Docket Number (Character, Req.)
                                   term = NULL,
                                   progress = T){ # Term (Character, Req)

  {

    oyez_meta_search <- "

import pandas as pd
import requests
import oyez_api_wrapper
import json

def get_case_data(term, docket):
    temp_case_obj = oyez_api_wrapper.court_case(str(term), str(docket))
    temp_audio_links = temp_case_obj.get_audio_links()
    return temp_audio_links

term = {term}
docket = '{docket}'

case_data = get_case_data(term, docket)
"


  } # Oyez Meta Search (Python)

  check_dependencies_oyez <- function(){

    installed <- reticulate::py_list_packages() # Check Installed Py Packages

    install_if_needed <- function(packages) {

      for (pkg in packages) {
        if (!(pkg %in% installed$package)) {
          cat(paste("Installing", pkg, "...\n"))
          py_install(pkg)
        } else {
          next
        }
      }
    }

    packages_to_check <- c("pandas", "requests", "oyez-api-wrapper") # Check if Already Downloaded

    if (!all(packages_to_check %in% installed$package)){
      install_if_needed(packages_to_check) # Install If Needed
    }

  } # Check Dependencies

  run_oyez_script <- function(term, docket) {

    python_script <- gsub("\\{term\\}", term, oyez_meta_search)
    python_script <- gsub("\\{docket\\}", docket, python_script)

    # Write the script to a temporary file
    temp_file <- tempfile(fileext = ".py")
    writeLines(python_script, temp_file)

    output <- reticulate::py_run_file(temp_file) # Run Script

    if (file.exists(temp_file)){
      file.remove(temp_file)
    } # Delete Temp Python File

    # Parse the JSON output
    case_data <- output$case_data # Retrieve Oral Argument Meta

    return(case_data) # Return Meta
  }

  retrieve_argument_transcript <- function(api_location, docket_number, term){


    {

      tryCatch({
        url <- api_location[[1]]
        response <- GET(url)

        if (status_code(response) == 200) {
          json_content <- content(response, as = "text", encoding = "UTF-8")  # Specify the encoding
          parsed_json <- jsonlite::fromJSON(json_content)

        } else {
          stop(paste("Failed to retrieve JSON data from the URL with status code:", status_code(response)))
        }
      }, error = function(e) {
        cat("Error downloading file: ", json_output_file, "\n")
      }) # Get Json (parsed_json)


    } # Retrieve JSON

    {

      temp <- data.frame()

      for (j in 1:length(parsed_json$transcript$sections$turns)) {
        temp_text <- parsed_json$transcript$sections$turns[j]
        temp <- bind_rows(temp, temp_text)
      }

      temp_meta <- data.frame(
        case_name = parsed_json$transcript$title[1]
      )

      temp_complete <- data.frame()

      for (k in 1:nrow(temp)) {
        text_blocks <- temp$text_blocks[k]
        flattened_text <- paste(sapply(text_blocks, function (block) block$text), collapse = " ")

        temp_full <- data.frame(
          case_name = temp_meta$case_name,
          text_start = temp$start[k],
          text_stop = temp$stop[k],
          speaker = temp$speaker$name[k],
          role = ifelse(is.null(temp$speaker$roles[k][[1]]$role_title) && is.null(temp$speaker$name[k]), "NA",
                        ifelse(is.null(temp$speaker$roles[k][[1]]$role_title), "Attorney", temp$speaker$roles[k][[1]]$role_title)),
          text = flattened_text,
          word_count = stringr::str_count(flattened_text, "\\w+"),
          row_id = k,
          object_title = parsed_json$title
        )

        temp_complete <- bind_rows(temp_complete, temp_full)
      }


      temp_complete <- temp_complete %>%
        mutate(
          argument_duration = temp_meta$argument_duration,
          docket_number = docket_number,
          term = term,
        ) %>%
        relocate(case_name, docket_number) %>%
        relocate(term, .after = docket_number) %>%
        mutate(role = ifelse(is.na(speaker), NA, role)) %>%
        mutate(role = ifelse(grepl('Justice', role, ignore.case = T), 'Justice', role))



    } # Convert to Rdata

    return(temp_complete)

  }

  check_venv <- function(){
    env_name <- file.path("~/.virtualenvs/r-reticulate")

    # Force remove existing virtualenv if it exists
    if (virtualenv_exists(env_name)){
      use_virtualenv(env_name, required = T)  # Ensures removal
    }

    # Create and use the virtual environment
    virtualenv_create(env_name, packages = c("pandas", "requests", "oyez-api-wrapper", "numpy"))
    use_virtualenv(env_name, required = TRUE)
    message("Virtual Environment Created & Activated")
  } # Check if venv is active


  check_venv() # Check Venv (Created & Active)
  check_dependencies_oyez() # Check if Modules Installed

  transcripts <- data.frame()

  for (temp_docket in 1:length(docket)){

    tryCatch({
      argument_links <- run_oyez_script(term = term, docket = docket[temp_docket])

      for (i in 1:length(argument_links)){

        temp_transcript <- retrieve_argument_transcript(api_location = argument_links[[i]], docket_number = docket[temp_docket], term = term)
        transcripts <- bind_rows(transcripts, temp_transcript)

      }

      title_date_combinations = paste(unique(transcripts$case_name), ' ', unique(transcripts$object_title))

      if (progress == TRUE){
        message('Completed Transcript Compilation for:')
        for (i in 1:length(title_date_combinations)){
          message(title_date_combinations[i])
        }
      }


    }, error = function(e) {
      message("No Transcript Found for ", term, "-", docket[temp_docket], '   ---- Moving On')
      NULL
    })

  }

  return(transcripts)


} # Main Function - Returns Transcript Object
