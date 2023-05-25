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
