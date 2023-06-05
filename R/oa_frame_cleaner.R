importFrom("dplyr", "%>%", "group_by", "mutate", "filter", "select")
importFrom("stringr", "str_split", "str_to_title")
importFrom("tidyr", "separate")


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
