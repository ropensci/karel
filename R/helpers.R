# All the functions in this file are suposed to be internal, but two of them are
# useful for testing and for ploting different states of the world:
# plot_static_world() and get_pkg_env(). I prefer not to export them so they are
# not available for the general user, but I can use them with ::

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
#'     \item \code{nx}: TODO
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

  if (time < 1) stop("\nFirst time available is 1.\nEl primer tiempo disponible es 1")
  if (time > pkg_env$moment) stop("\nLatest time available is ", pkg_env$moment,
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

#' Check user's own world
#'
#' This function analyzes if a world provided by the user satisfies all
#' requirements.
#'
#' @param world The world provided by the user. It's a list. See details in
#'   \code{\link{generar_mundo}}.
#'
#' @return If a misespecification is found, this function produces a stop and
#'   provides a descriptive error message.
#'
#' @keywords internal
#'
#' @details This function is called by \code{\link{generar_mundo}}.
#'
check_user_world <- function(world) {
  # I'm not using assertthat package because I want to
  # write bilingual error messages

  # Are all the elements present?
  elements <- c("nx", "ny", "hor_walls", "ver_walls", "karel_x", "karel_y",
                "karel_dir", "beepers_x", "beepers_y", "beepers_n", "beepers_bag")
  for (elem in elements) {
    if (!elem %in% names(world)) stop(paste0("\n", elem, " is missing in the provided world.\nFalta ", elem, " en el mundo provisto."))
  }

  # Check nx
  msg <- "\nnx, the number of avenues, must be numeric of length 1.
         nx, el numero de avenidas, debe ser numerico de largo 1."
  # I do these two separated because the second one could be a vector of T o F
  # if the length is greater than 1 and that gives a warning, so I first check
  # the length
  if (length(world$nx) != 1 | !is.numeric(world$nx)) stop(msg)
  if (world$nx %% 1 != 0) stop(msg)

  # Check ny
  msg <- "\nny, the number of streets, must be numeric of length 1.
         ny, el numero de calles, debe ser numerico de largo 1."
  if (length(world$ny) != 1 | !is.numeric(world$ny)) stop(msg)
  if (world$ny %% 1 != 0) stop(msg)

  # Check karel_x
  msg <- "\nkarel_x, the x-coordinate for Karel's initial position, must be numeric of length 1, and between 1 and nx.\nkarel_x, la coordenada en el eje x para la posicion inicial de Karel, debe ser numerico de largo 1 y estar entre 1 y nx."
  if (length(world$karel_x) != 1 | !is.numeric(world$karel_x)) stop(msg)
  if (world$karel_x %% 1 != 0) stop(msg)
  if (world$karel_x < 1 | world$karel_x > world$nx) stop(msg)

  # Check karel_y
  msg <- "\nkarel_y, the y-coordinate for Karel's initial position, must be numeric of length 1, and between 1 and ny.\nkarel_y, la coordenada en el eje y para la posicion inicial de Karel, debe ser numerico de largo 1 y estar entre 1 y ny."
  if (length(world$karel_y) != 1 | !is.numeric(world$karel_y)) stop(msg)
  if (world$karel_y %% 1 != 0) stop(msg)
  if (world$karel_y < 1 | world$karel_y > world$ny) stop(msg)

  # Check karel_dir
  msg <- "\nkarel_dir, Karel's initial direction, must be numeric of length 1, either 1, 2, 3 or 4.\nkarel_dir, la direccion inicial de Karel, debe ser numerico de largo 1, puede ser 1, 2, 3 o 4."
  if (length(world$karel_dir) != 1 | !is.numeric(world$karel_dir)) stop(msg)
  if (world$karel_dir %% 1 != 0) stop(msg)
  if (!world$karel_dir %in% 1:4) stop(msg)

  # Check beepers_bag
  msg <- "\nbeepers_bag, the number of beepers in Karel's bag, must be numeric of length 1, greater or equal than zero, including Inf.\nbeepers_bag, el numero de cosos en la mochila de Karel, debe ser numerico de largo 1 y mayor o igual a 0, incluyendo Inf."
  if (length(world$beepers_bag) != 1 | !is.numeric(world$beepers_bag)) stop(msg)
  if (!is.infinite(world$beepers_bag) & world$beepers_bag %% 1 != 0) stop(msg)
  if (world$beepers_bag < 0) stop(msg)

  # Check "beepers_x", "beepers_y", "beepers_n"
  msg <- "\nbeepers_x, beepers_y and beepers_n must be all NULL or numeric vectors of the same length.\nbeepers_x and beepers_y must be between 1 and nx or ny, respectively.\nbeepers_n must be greater or equal than 1.\nbeepers_x, beepers_y y beepers_n deben ser todos NULL o vectores numericos del mismo largo.\nbeepers_x y beepers_y deben estar entre 1 y nx o ny, respectivamentey.\nbeepers_n debe ser mayor o igual a 1."
  if (any(is.null(world$beepers_x), is.null(world$beepers_y), is.null(world$beepers_n))) {
    # All NULL?
    if (!all(is.null(world$beepers_x), is.null(world$beepers_y), is.null(world$beepers_n))) stop(msg)
  } else {
    # All numeric?
    if (!all(is.numeric(world$beepers_x), is.numeric(world$beepers_y), is.numeric(world$beepers_n))) stop(msg)
    # All same length?
    if (!(length(world$beepers_x) == length(world$beepers_y) & length(world$beepers_x) == length(world$beepers_n))) stop(msg)
    # All integer?
    if (!all(world$beepers_x %% 1 == 0, world$beepers_y %% 1 == 0, world$beepers_n %% 1 == 0)) stop(msg)
    # All >= 1?
    if (any(world$beepers_x < 1, world$beepers_y < 1, world$beepers_n < 1)) stop(msg)
    # Coordinates ok?
    if (any(world$beepers_x > world$nx | world$beepers_y > world$ny)) stop(msg)
  }

  # Check ver_walls and hor_walls
  check_walls(world$ver_walls, "ver_walls", world$nx, world$ny)
  check_walls(world$hor_walls, "hor_walls", world$nx, world$ny)


  # if (!is.null(world$ver_walls) | !is.data.frame(world$ver_walls)) {
  #   stop("\nver_walls must be either NULL of a data.frame.\nver_walls debe ser NULL o un data.frame")
  # }
  # if (is.data.frame(world$ver_walls)) {
  #
  #   # Has rows?
  #   if (nrow(world$ver_walls) < 1) stop("\nver_walls has 0 rows.\nver_walls tiene 0 filas.")
  #
  #   # Are all the columns present?
  #   elements <- c("x", "y", "lgth")
  #   for (elem in elements) {
  #     if (!elem %in% names(world))
  #       stop(paste0("\nColumn ", elem, " is missing in ver_walls data.frame.\nFalta la columna ", elem, " en el data.frame ver_walls."))
  #   }
  #
  #   # Any NA?
  #   if (any(is.na(world$ver_walls)))
  #     stop("\nver_walls can't have NA values.\nNo puede haber NAs en ver_walls")
  #
  #   # All columns are numeric?
  #   if (!all(apply(world$ver_walls, 2, is.numeric)))
  #     stop("\nAll columns in ver_walls must be numeric.\nTodas las columnas de ver_walls deben ser numericas.")
  #
  #   # All integers?
  #   if (!all(apply(world$ver_walls, 2, function(x) x %% 1 == 0)))
  #     stop("\nAll numbers in ver_walls must be integer, at least in the math sense, not necessarily of class integer.\nTodos los numeros en ver_walls deben ser enteros, al menos en el sentido matematico, no necesariamente de clase integer.")
  #
  #   # Range of values
  #   if (min(world$ver_walls$x) < 1 | max(world$ver_walls$x) >= world$nx)
  #     stop("\nAll x values in ver_walls must lie between 1 and nx-1.\nTodos los valores x en ver_walls deben estar entre 1 y nx-1.")
  #   if (min(world$ver_walls$y) < 0 | max(world$ver_walls$y) >= world$ny)
  #     stop("\nAll y values in ver_walls must lie between 0 and ny-1.\nTodos los valores y en ver_walls deben estar entre 0 y ny-1.")
  # }


}

#' Check the walls user's provided world
#'
#' This is a helper function to check ver_walls and hor_walls. It's called by check_user_world twice.
#'
#' @param dataset Either hor_walls or ver_walls
#' @param name Character string: "hor_walls" or "ver_walls"
#' @param nx, ny Size of the world
#'
#' @return If a misespecification is found, this function produces a stop and
#'   provides a descriptive error message.
#'
#' @keywords internal
#'
check_walls <- function(dataset, name, nx, ny) {
  if (!is.null(dataset) & !is.data.frame(dataset)) {
    stop(paste0("\n", name, " must be either NULL of a data.frame.\n", name, " debe ser NULL o un data.frame"))
  }
  if (is.data.frame(dataset)) {

    # Has rows?
    if (nrow(dataset) < 1) stop(paste0("\n", name, " has 0 rows.\n", name, " tiene 0 filas."))

    # Are all the columns present?
    for (elem in c("x", "y", "lgth")) {
      if (!elem %in% names(dataset))
        stop(paste0("\nColumn ", elem, " is missing in ", name, " data.frame.\nFalta la columna ", elem, " en el data.frame ", name, "."))
    }

    # Any NA?
    if (any(is.na(dataset)))
      stop(paste0("\n", name, " can't have NA values.\nNo puede haber NAs en ", name, "."))

    # All columns are numeric?
    if (!all(apply(dataset, 2, is.numeric)))
      stop(paste0("\nAll columns in ", name, " must be numeric.\nTodas las columnas de ", name, " deben ser numericas."))

    # All integers?
    if (!all(apply(dataset, 2, function(x) x %% 1 == 0)))
      stop(paste0("\nAll numbers in ", name, " must be integer, at least in the math sense, not necessarily of class integer.\nTodos los numeros en ", name, " deben ser enteros, al menos en el sentido matematico, no necesariamente de clase integer."))

    # Range of values
    if (any(dataset$x < 0) | any(dataset$x >= nx))
      stop(paste0("\nAll x values in ", name, " must lie between 0 and nx-1.\nTodos los valores x en ", name, " deben estar entre 0 y nx-1."))
    if (any(dataset$y < 0) | any(dataset$y >= ny))
      stop(paste0("\nAll y values in ", name, " must lie between 0 and ny-1.\nTodos los valores y en ", name, " deben estar entre 0 y ny-1."))
    if (any(dataset$lgth < 0))
      stop(paste0("\nAll lgth values in ", name, " must be 1 or greater.\nTodos los valores lgth en ", name, " deben ser mayores o iguales a 1."))
    if (name == "hor_walls" & any(dataset$lgth > (nx - dataset$x)))
      stop("\nSome lengths in hor_walls are longer than allowed by nx.\nAlgunos largos en hor_walls exceden lo permitido por nx.")
    if (name == "ver_walls" & any(dataset$lgth > (ny - dataset$y)))
      stop("\nSome lengths in ver_walls are longer than allowed by ny.\nAlgunos largos en ver_walls exceden lo permitido por ny.")
  }
}
