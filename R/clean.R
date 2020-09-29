#' Title
#'
#' Description
#'
#' @param description
#' @return description
#' @export
clean <- function(string){

  illegal_strings <- c(
    '\xc2', '\xa0', '\xe1', '\x9a', '\x80', '\xe2', '\x81',
    '\x82', '\x83', '\x84', '\x85', '\x86', '\x87', '\x88',
    '\x89', '\x8a', '\x8b', '\xaf', '\x9f', '\xe3'
  ) %>%
    paste0(collapse = '')

  export <- string %>%
    sapply(FUN = function(x) gsub(pattern = illegal_strings, replacement = '', x = x), USE.NAMES = FALSE) %>%
    stringr::str_replace_all(pattern = '[^[:print:]]', replacement = '') %>%
    textclean::replace_non_ascii()

  return(export)
}
