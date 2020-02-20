# All the functions in this file are suposed to be internal, but two of them are
# useful for testing and for ploting different states of the world:
# plot_static_world() and get_pkg_env(). I prefer not to export them so they are
# not available for the general user, but I can use them with ::


#' Components of a world
#'
#' TODO: describe each component of a list so that it can be world.
#'
#' @seealso \code{\link{generar_mundo}}
#'
#' @keywords internal
#' @name world_components
NULL
#> NULL


#' Get Karel's environment
#'
#' This function returns the environment called pkg_env created by the package.
#' It's useful for debugging and checking. It's an internal function.
#'
#' @return An enviroment with objects that represent Karel's world.
#'
#' @details \code{pkg_env} is an environment created inside the package to store
#'   and share between functions all the objects related to Karel's world and
#'   its state. Since the functions that will be used by the students should be
#'   simple and without arguments (for example, \code{move()}), these functions
#'   modify internally \code{pkg_env}.
#'
#'   The components of this environment are:
#'   \enumerate{
#'     \item \code{nx}:
#'     \item \code{ny}:
#'     \item \code{hor_walls}:
#'     \item \code{ver_walls}:
#'     \item \code{open_moves}:
#'     \item \code{karel}:
#'     \item \code{dir_now}:
#'     \item \code{x_now}:
#'     \item \code{y_now}:
#'     \item \code{moment}:
#'     \item \code{beepers_any}:
#'     \item \code{beepers_bag}:
#'     \item \code{beepers_now}:
#'     \item \code{beepers_all}:
#'     \item \code{base_plot}:
#'   }
get_pkg_env <- function() {
  return(pkg_env)
}

#' Plot the world at a given time
#'
#' This function plots Karel'w wort at the requested time. Initially, time is 1
#' and with each action that Karel performs, time is incremented by one. Current
#' time is stored in \code{pkg_env$moment}. This function is useful for
#' debuggint and to get static images to be used in the examples in the handouts
#' for studentes.
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

#' Plot base world
#'
#' This functions produces the initial plot of the world, with its size and all
#' the walls if there are any. It doesn't plot Karel or the beepers, since those
#' things can change with time.
#'
#' @return A ggplot with the world
#'
#' @keywords internal
#'
plot_base_world <- function() {

  # Make this data handy
  nx <- pkg_env$nx
  ny <- pkg_env$ny

  pkg_env$base_plot <-
    ggplot(NULL) +
    geom_point(data = tidyr::expand_grid(x = (1:nx) - 0.5, y = (1:ny) - 0.5),
               aes(x = x, y = y), size = 2) +
    scale_x_continuous("", expand = c(0, 0), limits = c(0, nx),
                       breaks = 0.5:(nx - 0.5), labels = 1:nx) +
    scale_y_continuous("", expand = c(0, 0), limits = c(0, ny),
                       breaks = 0.5:(ny - 0.5), labels = 1:ny) +
    coord_fixed() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(face = "bold")
    )

  # Add walls if there are any
  if (!is.null(pkg_env$ver_walls)) {
    pkg_env$base_plot <-
      pkg_env$base_plot +
      geom_segment(data = pkg_env$ver_walls,
                   aes(x = x, y = y, xend = x, yend = y + lgth), size = 2)
  }
  if (!is.null(pkg_env$hor_walls)) {
    pkg_env$base_plot <-
      pkg_env$base_plot +
      geom_segment(data = pkg_env$hor_walls,
                   aes(x = x, y = y, xend = x + lgth, yend = y), size = 2)
  }
}

#' Generate a dataset to plot Karel's figure in each time
#'
#' Karel is drawn with 6 squares (body, feet, eyes and mouth) using geom_rect(),
#' so we need arguments xmin, xmax, ymin and ymax for each of them. So this
#' function takes Karel's current position (x, y) and direction (1 east, 2
#' north, 3 west, 4 south) and generates the corresponding values of xmin, xmax,
#' ymin and ymax.
#'
#' This function is called repeatedly for each position of Karel through time.
#'
#' @return A 6x7 tibble. First row is for the body, 2nd for the left foot, then
#'   right foot, left eye, right eye and finally the mouth. Columns are xmin,
#'   xmax, ymin, ymax, moment (time), a color (for fill) and a transparency
#'   value (alpha)
#'
#' @keywords internal
#'
draw_karel_df <- function(x, y, direction, moment) {
  switch(direction,
         # In the tibbles, this is the order of the coordinates:
         # object = c("body", "left_foot", "right_foot", "left_eye", "right_eye", "mouth")
         # Direction 1, going east
         tibble(
           xmin = x - c(.9, .2, .2, .75, .75, .45),
           xmax = x - c(.2, .1, .1, .65, .65, .35),
           ymin = y - c(.85, .75, .45, .65, .45, .65),
           ymax = y - c(.15, .55, .25, .55, .35, .35),
           moment = moment,
           fill = c("orange", "black", "black", "brown", "brown", "red"),
           alpha = c(0.25, rep(1, 5))
         ),
         # Direction 2, going north
         tibble(
           xmin = x - c(.85, .45, .75, .45, .65, .65),
           xmax = x - c(.15, .25, .55, .35, .55, .35),
           ymin = y - c(.9, .2, .2, .75, .75, .45),
           ymax = y - c(.2, .1, .1, .65, .65, .35),
           moment = moment,
           fill = c("orange", "black", "black", "brown", "brown", "red"),
           alpha = c(0.25, rep(1, 5))
         ),
         # Direction 3, going west
         tibble(
           xmin = x - c(.8, .9, .9, .35, .35, .65),
           xmax = x - c(.1, .8, .8, .25, .25, .55),
           ymin = y - c(.85, .45, .75, .45, .65, .65),
           ymax = y - c(.15, .25, .55, .35, .55, .35),
           moment = moment,
           fill = c("orange", "black", "black", "brown", "brown", "red"),
           alpha = c(0.25, rep(1, 5))
         ),
         # Direction 4, going south
         tibble(
           xmin = x - c(.85, .75, .45, .65, .45, .65),
           xmax = x - c(.15, .55, .25, .55, .35, .35),
           ymin = y - c(.8, .9, .9, .35, .35, .65),
           ymax = y - c(.1, .8, .8, .25, .25, .55),
           moment = moment,
           fill = c("orange", "black", "black", "brown", "brown", "red"),
           alpha = c(0.25, rep(1, 5))
         )
  )
}

#' Get row number in beepers dataset
#'
#' Given current position of Karel, it calculates the cell id (\code{cell}) and
#' returns the row number of the respective cell in the beepers_now dataset.
#' It's used to add or remove beepers in this row when Karel picks or puts
#' beepers.
#'
#' @return row number or numeric of length 0
#' @keywords internal
get_beepers_df_row <- function() {
  cell <- pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx
  cell_present <- which(pkg_env$beepers_now$cell == cell)
  return(cell_present)
}
