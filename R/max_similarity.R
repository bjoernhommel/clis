#' Title
#'
#' Description
#'
#' @param description
#' @return description
#' @export
max_similarity <- function(similarity_mtx, x_axis = TRUE, echo = TRUE) {

  margin <- if (x_axis) 1 else 2

  check_obj <- function(x) if (!is.null(dim(x)) & typeof(x) == 'double') TRUE else FALSE

  if (check_obj(similarity_mtx)) {

    # find maximum similarity from matrix
    if (echo) cat('Finding most similar indicies...\n')
    export <- apply(
      X = similarity_mtx,
      MARGIN = margin,
      FUN = function(similarities) {

        valid_similarities <- na.omit(similarities)

        if (length(valid_similarities) < 1) {
          return(NA)
        } else if (length(which.max(valid_similarities)) < 1){
          return(NA)
        } else {
          return(which.max(similarities))
        }
      }
    )

    return(export)

  } else {
    stop('`similarity_mtx` must be matrix from `clis::similarity_matrix()`')
  }

}
