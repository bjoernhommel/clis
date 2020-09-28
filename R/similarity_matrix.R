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
  length_mtx <- matrix(
    data = apply(
      X = expand.grid(
        sapply(stems_x, nchar, USE.NAMES = FALSE),
        sapply(stems_y, nchar, USE.NAMES = FALSE)
      ),
      MARGIN = 1,
      FUN = max
    ),
    nrow = nrow(distance_mtx)
  )

  std_distance_mtx <- 1 - (distance_mtx / length_mtx)

  divergent_similarity_mtx <- std_distance_mtx
  convergent_similarity_mtx <- std_distance_mtx

  construct_x <- dplyr::select(clis_vec_x, starts_with('construct'))
  construct_y <- dplyr::select(clis_vec_y, starts_with('construct'))

  if (echo) cat('Calculating convergence-matrix...\n')
  convergence_mtx <- Map(
    f = function(x, y) outer(x, y, FUN = '=='),
    construct_x[seq_len(ncol(construct_x))],
    construct_y[seq_len(ncol(construct_y))]
  ) %>%
    simplify2array() %>%
    apply(1:2, function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    })

  convergent_similarity_mtx[!convergence_mtx] <- NA

  export <- list(
    'convergent' = convergent_similarity_mtx,
    'divergent' = divergent_similarity_mtx
  )

  return(export)

}
