#' SCOTUS Docket Search (2.0.0)
#'
#' @param docket_id The docket number of interest - written as a character vector in R. This can take the form of filed petitions (e.g., “19-1392”), applications (e.g., “22A948”), or motions (e.g., “22M11”). docket_id parameter is REQUIRED for all searches.
#' @return Cleaned and parsed information from docketed petitions, applications, and motions to the United States Supreme Court.
#' @export
#'
#' @examples
#' \dontrun{
#' sample_docket <- docket_search(docket_id = '19-1392')
#' }
docket_search <- function(docket_id){

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

  docket_info <- function(response, docket){

    html_content <- httr::content(response, as = 'text')
    parsed_html <- rvest::read_html(html_content)

    info_meta <- parsed_html %>%
      rvest::html_element('#docketinfo') %>%
      rvest::html_table(fill = T) %>%
      dplyr::select(X1, X2) %>%
      dplyr::rename(information = X1,
             entry = X2) %>%
      dplyr::mutate(information = ifelse(grepl('Linked with', information, ignore.case = T), gsub('Linked with', 'Linked with:', information, ignore.case = T), information),
                    entry = ifelse(grepl('Linked with\\:', information, ignore.case = T), gsub('.*with\\:', '', trimws(information)), entry),
                    information = ifelse(grepl('Linked with\\:', information), gsub('with\\:.*', 'with:', information), information),
                    entry = trimws(entry)) %>%
      dplyr::filter(grepl('\\:', information)) %>%
      dplyr::mutate(information = gsub('\\:', '', information),
             entry = ifelse(entry == '', NA, entry)) %>%
      tidyr::pivot_wider(names_from = information,
                         values_from = entry) %>%
      dplyr::rename_with(~ gsub(' ', '_', tolower(.x))) %>%
      dplyr::mutate(petitioner = trimws(gsub(' v\\..*', '', title)),
                    petitioner = trimws(gsub(' Petitioner.*', '', petitioner)),
                    petitioner = trimws(gsub('\\, et al\\.\\,$', ', et al.', petitioner)),
                    respondent = trimws(gsub('.* v\\.', '', title)),
                    docket = docket) %>%
      dplyr::relocate(petitioner, respondent, .after = title) %>%
      dplyr::relocate(docket, .after = title)

    if (!'linked_with' %in% names(info_meta)){
      info_meta <- info_meta %>%
        dplyr::mutate(linked_with = NA) %>%
        dplyr::relocate(linked_with, .after = docketed)
    }

    return(info_meta)

  } # Docket Info Table

  docket_entries <- function(response){

    temp_docket_entries <- httr::content(response, as = 'text') %>%
      rvest::read_html(html_content) %>%
      rvest::html_element('#proceedings')  %>%
      rvest::html_table(fill = T) %>%
      select(X1, X2) %>%
      rename(date = X1,
             entry = X2) %>%
      mutate(date = ifelse(date == '', NA, date),
             date = ifelse(date == 'Date', NA, date)) %>%
      filter(!is.na(date)) %>%
      mutate(date = lubridate::mdy(date))

    return(temp_docket_entries)

  } # Docket Entries

  counsel <- function(response){

    html_content <- httr::content(response, as = 'text')

    counsel_temp <- gsub("(<br>|<br/>|<br />|\\n)", "\n", html_content) %>%
      rvest::read_html(.) %>%
      rvest::html_element("#Contacts") %>%
      rvest::html_table(fill = TRUE) %>%
      dplyr::mutate(across(everything(), ~ gsub("\\n", " ", .))) %>%
      dplyr::select(X1, X2, X3) %>%
      dplyr::rename(name = X1,
             address = X2,
             contact = X3) %>%
      dplyr::filter(!grepl('NAME', name, ignore.case = T)) %>%
      dplyr::filter(!grepl('Party name\\:', name, ignore.case = T)) %>%
      dplyr::mutate(counsel_of_record = ifelse(grepl('Counsel of Record', name, ignore.case = T), 'Yes', 'No'),
             name = gsub('Counsel of Record', '', name, ignore.case = T),
             name = trimws(name),
             party = case_when(
               grepl('(Petitioner|Applicant|Appellant)', name, ignore.case = T) ~ 'Petitioner',
               grepl('(Respondent|Appellee)', name, ignore.case = T) ~ 'Respondent',
               name %in% c('Other', 'other') ~ 'Other')) %>%
      tidyr::fill(party, .direction = 'down') %>%
      dplyr::filter(!grepl('Attorneys for ', name)) %>%
      dplyr::filter(!name %in% c('Other', 'other'))

    return(counsel_temp)

  } # Return Counsel

  principal_counsel <- function(temp_counsel){

    principal_counsel <- temp_counsel %>%
      dplyr::filter(counsel_of_record == 'Yes') %>%
      dplyr::filter(party %in% c('Petitioner', 'Respondent')) %>%
      dplyr::select(name, party) %>%
      tidyr::pivot_wider(names_from = party, values_from = name)

    if ("Petitioner" %in% names(principal_counsel)) {
      names(principal_counsel)[names(principal_counsel) == "Petitioner"] <- "petitioner_counsel"
    }

    if ("Respondent" %in% names(principal_counsel)) {
      names(principal_counsel)[names(principal_counsel) == 'Respondent'] <- "respondent_counsel"
    }

    return(principal_counsel)

  } # Get Primary Petitioner & Respondent Counsels

  check_package_install <- function(){

    check_and_load_packages <- function(packages) {
      for (pkg in packages) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
          cat(sprintf("Package '%s' is not installed. Do you want to install it? (Y/N): ", pkg))
          response <- tolower(readline())
          if (response == "y") {
            install.packages(pkg)
          } else if (response == "n") {
            stop(sprintf("Cannot proceed without '%s' installed. Exiting.", pkg))
          } else {
            stop("Invalid input. Please enter Y or N. Exiting.")
          }
        }
        # Load the library after installation or if already installed
        library(pkg, character.only = TRUE)
      }
    }

    required_packages <- c('dplyr', 'rvest', 'reticulate', 'httr', 'tm', 'stringr', 'tm', 'tidyr', 'graphics', 'utils', 'htm2txt', 'zoo', 'pdftools')
    check_and_load_packages(required_packages)
    library(dplyr); library(rvest); library(reticulate); library(httr); library(tm); library(stringr); library(tm); library(tidyr); library(graphics); library(utils); library(htm2txt); library(zoo); library(pdftools)

  } # Check if dplyr, rvest, reticulate, and httr are installed

  check_package_install() # Check If Necessary Packages are Installed

  temp_docket_url <- process_docket_id(docket_id)
  response <- httr::GET(temp_docket_url) # Retrieve HTML
  temp_docket_info <- docket_info(response, docket_id) # Get Docket Meta Info
  temp_docket_entries <- docket_entries(response) # Get Docket Entries
  temp_counsel <- counsel(response) # Get Counsel (Full)
  temp_principal_counsel <- principal_counsel(temp_counsel) # Retrieve Principal Counsel


  combined_temp <- temp_docket_info %>%
    dplyr::mutate(docket_entries = list(temp_docket_entries),
           counsel = list(temp_counsel),
           petitioner_counsel = ifelse(!is.na(temp_principal_counsel$petitioner_counsel), temp_principal_counsel$petitioner_counsel, NA),
           respondent_counsel = ifelse(!is.na(temp_principal_counsel$respondent_counsel), temp_principal_counsel$respondent_counsel, NA)) %>%
    dplyr::relocate(petitioner_counsel, .after = petitioner) %>%
    dplyr::relocate(respondent_counsel, .after = respondent) # Combine

  return(combined_temp)

  }

