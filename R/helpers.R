#' Title
#'
#' @return
#' @export
#'
#' @examples
get_pkg_env <- function() return(pkg_env)


get_beepers_df_row <- function() {
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  cell_present <- which(pkg_env$beepers_now$cell == cell)
  return(cell_present)
}
