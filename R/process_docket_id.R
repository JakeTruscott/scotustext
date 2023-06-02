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
}
