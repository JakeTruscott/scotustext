write_reactable_func <- function(docket_entries, write_reactable){
  if (write_reactable){
    cat("Code to Create (.HTML) Table Using Reactable:\n")
    cat("library(reactable); library(reactablefmtr)\n")
    cat("reactable(docket_entries, searchable = T,\n defaultPageSize = 20,\n theme = fivethirtyeight(font_size = 12, font_color = 'grey',\n cell_padding = 6)) %>%\n")
    cat("add_title('<YOUR TITLE HERE>') %>%\n")
    cat("add_subtitle('<YOUR SUBTITLE HERE>')\n")
  }
}
