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
