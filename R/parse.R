#' Title
#'
#' Description
#'
#' @param description
#' @return description
#' @export
parse <- function(string, start_flag = '<|startoftext|>', stop_flag = '<|endoftext|>', construct_delim = '#', stem_delim = '@') {

  start_flag <- paste0('\\Q', start_flag, '\\E')
  stop_flag <- paste0('\\Q', stop_flag, '\\E')

  export <- string %>%
    stringr::str_extract_all(
      pattern = paste0('(', start_flag, ')(.*?)(', stop_flag, ')') %>%
        stringr::regex()
    ) %>%
    lapply(stringr::str_replace_all, pattern = start_flag, replacement = '') %>%
    lapply(stringr::str_replace_all, pattern = stop_flag, replacement = '') %>%
    lapply(clis::clean) %>%
    lapply(FUN = function(x) {

      x <- stringr::str_split(string = x, pattern = stem_delim, simplify = TRUE)

      if (ncol(x) > 1) {

        stems <- matrix(x[, 2:ncol(x)], nrow = nrow(x))

        constructs <- stringr::str_split(
          string = x[, 1],
          pattern = construct_delim,
          simplify = TRUE
        ) %>%
          .[, 2:ncol(.)] %>%
          matrix(nrow = nrow(x))

        clis <- data.frame(
          constructs = constructs,
          stems = stems,
          stringsAsFactors = FALSE
        )

        n_constructs <- if (ncol(constructs) < 2) 1 else ncol(constructs)
        n_stems <- if (ncol(stems) < 2) 1 else ncol(stems)

        clis_colnames <- c(
          paste0('construct', '_', 1:n_constructs),
          paste0('stem', '_', 1:n_stems)
        )

        clis <- magrittr::set_colnames(clis, clis_colnames)

        return(clis)

      } else {
        return(NULL)
      }
    })

  return(export)
}
