#' Title
#'
#' Description
#'
#' @param description
#' @return description
#' @export
similarity_matrix <- function(clis_vec_x, clis_vec_y, method = 'lv', echo = TRUE) {

  if(is.null(dim(clis_vec_x)) | is.null(dim(clis_vec_y))) {
    stop('`clis_vec_x` and `clis_vec_y` must be data frame-like results from `clis::parse()`')
  }

  if (echo) {
    warning('Similarity matrix computation for multiple stems per item is currently not supported (only the first column will be used)!')
  }

  stems_x <- dplyr::select(clis_vec_x, starts_with('stem'))[, 1] %>%
    unlist(use.names = FALSE)
  stems_y <- dplyr::select(clis_vec_y, starts_with('stem'))[, 1] %>%
    unlist(use.names = FALSE)

  if (echo) cat('Calculating distance-matrix...\n')

  distance_mtx <- stringdist::stringdistmatrix(
    a = stems_x,
    b = stems_y,
    method = method
  )

  if (echo) cat('Calculating length-matrix...\n')

  length_mtx <- outer(
    X = nchar(stems_x),
    Y = nchar(stems_y),
    FUN = 'pmax'
  )

  std_distance_mtx <- 1 - (distance_mtx / length_mtx)

  divergent_similarity_mtx <- std_distance_mtx
  convergent_similarity_mtx <- std_distance_mtx

  construct_x <- dplyr::select(clis_vec_x, starts_with('construct'))
  construct_y <- dplyr::select(clis_vec_y, starts_with('construct'))

  if (echo) cat('Calculating convergence-matrix...\n')

  # convergence_mtx <- Map(
  #   f = function(x, y) outer(x, y, FUN = '=='),
  #   construct_x[seq_len(ncol(construct_x))],
  #   construct_y[seq_len(ncol(construct_y))]
  # ) %>%
  #   simplify2array() %>%
  #   apply(1:2, function(x) {
  #     if (all(is.na(x))) NA else max(x, na.rm = TRUE)
  #   })

  # convergence_mtx <- outer(
  #   X = asplit(construct_x, 1),
  #   Y = asplit(construct_y, 1),
  #   FUN = Vectorize(
  #     FUN = function(x, y) ifelse(all(is.na(u <- x==y)), NA, max(u,na.rm = TRUE)))
  #   )

  extend_columns <- function(obj, add_columns) {

    start_ncol <- ncol(obj) + 1
    target_ncol <- ncol(obj) + add_columns

    export <- obj %>% tibble::add_column(
      !!!setNames(
        object = as.list(rep(NA, add_columns)),
        nm = paste0('construct_', start_ncol:target_ncol)
      )
    )
    return(export)
  }
  if (ncol(construct_x) > ncol(construct_y)) {
    construct_y <- extend_columns(construct_y, ncol(construct_x) - ncol(construct_y))
  } else if (ncol(construct_x) < ncol(construct_y)) {
    construct_x <- extend_columns(construct_x, ncol(construct_y) - ncol(construct_x))
  }


  convergence_mtx <- Reduce(
    function(...) pmax(..., na.rm = TRUE),
    Map(function(x, y) outer(x, y, `==`), construct_x, construct_y)
  )

  convergent_similarity_mtx[!convergence_mtx] <- NA

  export <- list(
    'convergent' = convergent_similarity_mtx,
    'divergent' = divergent_similarity_mtx
  )

  return(export)

}
