#' Get Karel's environment
#'
#' This function returns the environment called pkg_env created by the package.
#' It's useful for debugging and checking. It's an internal function.
#'
#' @return An enviroment with objects that represent Karel's world.
#'
#' @details \code{pkg_env} is an environment created inside the package to store and share between functions all the objects related to Karel's world and its state. Since the functions that will be used by the students should be simple and without arguments (for example, \code{move()}), these functions modify internally \code{pkg_env}.
#'
#' The components of this environment are:
#' \enumerate{
#'   \item \code{nx}:
#'   \item \code{ny}:
#'   \item \code{hor_walls}:
#'   \item \code{ver_walls}:
#'   \item \code{open_moves}:
#'   \item \code{karel}:
#'   \item \code{dir_now}:
#'   \item \code{x_now}:
#'   \item \code{y_now}:
#'   \item \code{moment}:
#'   \item \code{beepers_any}:
#'   \item \code{beepers_bag}:
#'   \item \code{beepers_now}:
#'   \item \code{beepers_all}:
#'   \item \code{base_plot}:
#' }
get_pkg_env <- function() return(pkg_env)

#' Plot the world at a given time
#'
#' This function plots Karel'w wort at the requested time. Initially, time is 1 and with each action that Karel performs, time is incremented by one. Current time is stored in \code{pkg_env$moment}. This function is useful for debuggint and to get static images to be used in the examples in the handouts for studentes.
#'
#' @param time The requested time
#'
#' @return Prints the plot.
#'
plot_static_world <- function(time) {

  if (time < 1) stop("First time available is 1.\nEl primer tiempo disponible es 1")
  if (time > pkg_env$moment) stop("Latest time available is ", pkg_env$moment,
                                  "\nEl ultimo tiempo disponible es ", pkg_env$moment)

  # Filter to keep only this moment
  karel_for_drawing <- dplyr::filter(pkg_env$karel, moment == time)
  karel_for_drawing <- draw_karel_df(karel_for_drawing$x, karel_for_drawing$y, karel_for_drawing$direction, time)
  beepers_moment <- dplyr::filter(pkg_env$beepers_all, moment == time)
  if (nrow(beepers_moment) == 0) {
    # This means that in this moment there were no beepers in the world
    # In order the plot to work, I will create a dataset with NA values
    beepers_moment[1, ] <- list(NA, NA, NA, NA, time)
  }

  p <-
    pkg_env$base_plot +
    geom_tile(data = beepers_moment,
              aes(x = x - 0.5, y = y - 0.5, width = 0.4, height = 0.4),
              fill = "purple", color = "black", size = 0.5) +
    geom_text(data = beepers_moment, aes(x = x - 0.5, y = y - 0.5, label = n), color = "white") +
    geom_rect(data = karel_for_drawing,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              alpha = karel_for_drawing$alpha,
              fill = karel_for_drawing$fill, color = "black")

  suppressWarnings(print(p))
}

#' Get row number in beepers dataset
#'
#' Given current position of Karel, it calculates the cell id (\code{cell}) and returns the row number of the respective cell in the beepers_now dataset. It's used to add or remove beepers in this row when Karel picks or puts beepers.
#'
#' @return row number or numeric of length 0
#' @keywords internal
get_beepers_df_row <- function() {
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  cell_present <- which(pkg_env$beepers_now$cell == cell)
  return(cell_present)
}
