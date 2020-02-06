beepers_presentes <- function() {
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  return(cell %in% pkg_env$beepers_now$cell)
}
