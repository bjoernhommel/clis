#' Title
#'
#' Description
#'
#' @param description
#' @return description
#' @export
is_junk <- function(string){

  whitelist <- '[^a-zA-Z0-9\\w+\\s\\.\'\\,\\-\\(\\)\\%\\;\\&]'
  blacklist <- '[^\\#\\<\\/\\|\\>\\"\\!\\?\\:\\*\\[\\]\\\\\\`\\}\\{\\~\\$\\=]'

  export <- sapply(string, function(s) {
    str_matches <- stringr::str_match(
      string = s,
      pattern = whitelist)[1,1]
    validated_str <- if (is.na(str_matches)) FALSE else TRUE

    return(validated_str)

  })

  return(export)
}
