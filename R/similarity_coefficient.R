#' Title
#'
#' Description
#'
#' @param description
#' @return description
#' @export
similarity_coefficient <- function(similarity_mtx, x_axis = TRUE, threshold = NULL, echo = TRUE) {

  if (!is.null(threshold) & !is.numeric(threshold)) {
    stop('`threshold` needs to be NULL or numeric!')
  }
  margin <- if (x_axis) 1 else 2

  check_obj <- function(x) if (!is.null(dim(x)) & typeof(x) == 'double') TRUE else FALSE

  similarity <- function(.similarity_mtx, .threshold) {

    # choose maximum similarity from matrix
    coefficients <- apply(
      X = .similarity_mtx,
      MARGIN = margin,
      FUN = function(row) {
        similarities <- na.omit(row)
        max_similarity <- if (length(similarities) < 1) NA else max(row, na.rm = TRUE)
      }
    )

    # coerce into dichotomous similarity score if threshold is defined
    if (!is.null(.threshold)) {
      coefficients <- dplyr::if_else(coefficients >= .threshold, 1, 0)
    }

    return(coefficients)
  }


  if (check_obj(similarity_mtx)) {

    export <- similarity(similarity_mtx, threshold)

  } else if (all(sapply(similarity_mtx, check_obj))) {

    high_order_mtx <- simplify2array(similarity_mtx)

    low_order_mtx <- apply(high_order_mtx, 1:2, function(x) {
      if (all(is.na(x))) NA else max(x, na.rm = TRUE)
    })

    export <- similarity(low_order_mtx, threshold)

  } else {
    stop('`similarity_mtx` must be matrix from `clis::similarity_matrix()`')
  }

  return(export)

}
