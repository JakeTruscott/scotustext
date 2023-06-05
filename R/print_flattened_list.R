#' Print Flattened List (scotustext)
#'
#' @param column Dataframe and index of interest to convert data structured as flattend semi-colon separated list object ("; ") to nested list format.
#'
#' @return Nested list format of flattend data.
#' @export
#'
#' @examples
#' \dontrun{
#' dobbs_sample <- docket_search(docket_id = "19-1392")
#' print_flattened_list(dobbs_sample$all_other_counsel)
#' }
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
