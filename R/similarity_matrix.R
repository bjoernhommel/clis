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

  distance_mtx <- stringdist::stringdistmatrix(
    a = stems_x,
    b = stems_y,
    method = method
  )

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

  divergent <- std_distance_mtx
  convergent  <- list()

  construct_x <- dplyr::select(clis_vec_x, starts_with('construct'))
  construct_y <- dplyr::select(clis_vec_y, starts_with('construct'))

  for (x in seq_along(construct_x)) {

    vec_x <- unlist(construct_x[, x], use.names = FALSE)

    for (y in seq_along(construct_y)) {

      vec_y <- unlist(construct_y[, y], use.names = FALSE)

      convergence_mtx <- matrix(
        data = apply(
          X = expand.grid(
            'x' = vec_x,
            'y' = vec_y
          ),
          MARGIN = 1,
          FUN = function(df) {
            identical(df[['x']], df[['y']])
          }
        ),
        nrow = nrow(distance_mtx)
      )
      key <- paste0(x, ':', y)
      convergent[[key]] <- std_distance_mtx
      convergent[[key]][!convergence_mtx] <- NA
    }
  }

  export <- list(
    'divergent' = divergent,
    'convergent' = convergent
  )

  return(export)

}
