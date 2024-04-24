# Solution for the "no visible binding for global variable" with variables in
# ggplot
utils::globalVariables(c("x", "y", "lgth", "n", "xmin", "xmax", "ymin", "ymax",
                         "moment", "."))

# -----------------------------------------------------------------------------

# Create environment
pkg_env <- new.env(parent = emptyenv())

# -----------------------------------------------------------------------------

#' Create Karel's world
#'
#' This function takes a "world" (i.e. a list with data about its size, walls,
#' beepers and Karel's position and direction), plots it and prepares everything
#' so that Karel can start performing actions in it. It must be run always
#' before Karel starts working on her goals, especially if we have made a
#' mistake, we must start all over again by first running this function.
#'
#' After running \code{.generate_world()}, we can run Karel's actions and
#' finally visualize it all with the function \code{.run_actions()}. Thiese are
#' all internal functions, users just call the corresponding external wrapper
#' according to their language.
#'
#' @param world Character vector of length 1 with the name of one of the
#'   provided worlds in the package or a list provided by the user with all the
#'   components that a world needs (see more below in details).
#'
#' @return Plots the initial state of Karel's world and prepares everything to
#'   start recording her actions.
#'
#' @details Argument \code{world} can be create by the user. In this case, it
#'   must be a list with the following components:
#'
#'   \enumerate{
#'     \item \code{nx}: size of Karel's world, number of cells in x-axis.
#'     \item \code{ny}: size of Karel's world, number of cells in y-axis.
#'     \item \code{hor_walls}: a data.frame with a row for each horizontal wall
#'     in Karel's world and 3 columns: x (coordinate of the start of the wall in
#'     the x axis), y (coordinate of the start of the wall in the y axis), lgth
#'     (length of the wall, in number of cells it covers). If it is NULL, there
#'     are no horizontal walls in the world.
#'     \item \code{ver_walls}: a data.frame with a row for each vertical wall in
#'     Karel's world and 3 columns: x (coordinate of the start of the wall in
#'     the x axis), y (coordinate of the start of the wall in the y axis), lgth
#'     (length of the wall, in number of cells it covers). If it takes the value
#'     NULL, there are no vertical walls in the world.
#'     \item \code{karel_x}: x-coordinate for Karel's initial position.
#'     \item \code{karel_y}: y-coordinate for Karel's initial position.
#'     \item \code{karel_dir}: Karel's starting direction: 1 (facing west), 2
#'     (facing north), 3 (facing west), or 4 (facing south).
#'     \item \code{beepers_x}: Numeric vector with the x-axis coordinates of the
#'     cells where there are beepers initially. The length of the vectors
#'     beepers_x, beepers_y and beepers_n must match. If you don't want beepers
#'     in the world, supply the value NULL.
#'     \item \code{beepers_y}: Numeric vector with the coordinates in the y-axis
#'     of the cells where there are beepers initially. The length of the vectors
#'     beepers_x, beepers_y and beepers_n must match. If you don't want beepers
#'     in the world, supply the value NULL.
#'     \item \code{beepers_n}: numeric vector with the number of beepers that
#'     are initially in each of the positions determined by the values of
#'     beepers_x and beepers_y. The length of the vectors beepers_x, beepers_y
#'     and beepers_n must match. If you don't want beepers in the world, supply
#'     the value NULL.
#'     \item \code{beepers_bag}: number of beepers that Karel has available in
#'     its bag at the beginning. Karel can put beepers if it has beepers in its
#'     bag. It can take the value Inf.
#'   }
#'
#' @importFrom ggplot2 ggplot geom_segment geom_point aes scale_x_continuous
#'   scale_y_continuous theme element_blank element_text geom_tile geom_text
#'   geom_rect coord_fixed element_rect
#' @importFrom dplyr tibble add_row slice mutate bind_rows n
#' @importFrom magrittr %>%
#'
#' @keywords internal
#'
.generate_world <- function(world, lang) {

  if (is.character(world)) {
  	# Load this world from internal data
  	world <- try(get(world), silent = TRUE)
  	if (inherits(world, "try-error")) {
  	  cli::cli_rule()
  	  cli::cli_abort(call = NULL,
  	                 message = c("x" = message_texts[[lang]]$world_doesnt_exist)
  	  )
  	}
	} else {
		# User has provided their own world as a list.
    check_user_world(world, lang)
	}

  # Clear the environment
  rm(list = ls(pkg_env), envir = pkg_env)

  # Append useful world info
  pkg_env$nx <- world$nx
  pkg_env$ny <- world$ny
  pkg_env$hor_walls <- world$hor_walls
  pkg_env$ver_walls <- world$ver_walls

  # Create Karel dataset
  pkg_env$karel <- tibble(x = world$karel_x, y = world$karel_y,
                          direction = world$karel_dir, moment = 1)

	# Current Karel's position and direction (1: east, 2: north, 3: west, 4:south)
	pkg_env$x_now <- world$karel_x
	pkg_env$y_now <- world$karel_y
	pkg_env$dir_now <- world$karel_dir

	# Current moment (for animation)
	pkg_env$moment <- 1

	# Array of open moves
	pkg_env$open_moves <- generate_open_moves(world$nx, world$ny, world$hor_walls,
	                                          world$ver_walls)

	# Create beepers datasets
	# beepers_now only has current state of beepers, beepers_all acummulates
	# all states for animation
	# beepers_any is the total number of beepers in the world (works even when
	# beepers_n is NULL, sum is 0)
	pkg_env$beepers_any <- sum(world$beepers_n)
	pkg_env$beepers_now <- create_beepers(world$nx, world$beepers_x,
	                                      world$beepers_y, world$beepers_n,
	                                      moment = 1)
	pkg_env$beepers_all <- pkg_env$beepers_now
	pkg_env$beepers_bag <- world$beepers_bag

	# Set variable to track if there was an error
	pkg_env$error <- FALSE

	# Plot the world
	plot_base_world()
	plot_static_world(1)

}

# -----------------------------------------------------------------------------

#' Run actions
#'
#' This function produces the animation that shows all actions performed by
#' Karel since its world was generated by \code{generate_world}. This is
#' internal, it's called by a wrapper depending on user's language
#'
#' @param loop A logical value TRUE or FALSE indicating if the animation should
#' repeat itself after finished or not (defaults to TRUE).
#'
#' @return Produces an animation with \code{gganimate}.
#'
#' @keywords internal
#'
.run_actions <- function(loop = FALSE, lang) {

  # Proceed if there was no mistake
  if (pkg_env$error) {
    cli::cli_rule()
    cli::cli_abort(call = NULL, message = c(
      "x" = message_texts[[lang]]$error_general,
      ">" = message_texts[[lang]]$start_again)
    )
  }

  if (pkg_env$moment == 1) {
    cli::cli_rule()
    cli::cli_abort(call = NULL, message = c(
      "x" = message_texts[[lang]]$do_sth_first_x,
      ">" = message_texts[[lang]]$do_sth_first_arrow)
    )
  }

  karel_for_drawing <- purrr::pmap_dfr(pkg_env$karel, draw_karel_df)

  p <-
    pkg_env$base_plot +
    geom_tile(data = pkg_env$beepers_all,
              aes(x = x - 0.5, y = y - 0.5, width = 0.4, height = 0.4),
              fill = "purple", color = "black", size = 0.5) +
    geom_text(data = pkg_env$beepers_all,
              aes(x = x - 0.5, y = y - 0.5, label = n), color = "white") +
    geom_rect(data = karel_for_drawing,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              alpha = karel_for_drawing$alpha,
              fill = karel_for_drawing$fill, color = "black") +
    gganimate::transition_manual(moment)

  # Choose frames per second according to the number of frames to animate
  nframes <- nrow(pkg_env$karel)

  fps <- dplyr::case_when(
    nframes < 40 ~ 2,
    nframes >= 40 & nframes < 70 ~ 3,
    nframes >= 70 & nframes < 100 ~ 4,
    nframes >= 100 ~ 5)

  suppressWarnings(
    gganimate::animate(p, nframes = nframes, fps = fps,
                       height = 800, width = 800,
                       renderer = gganimate::gifski_renderer(loop = loop))
  )
}

# -----------------------------------------------------------------------------

#' Generate array which shows moves that Karel can and can't make
#'
#' This function creates \code{open_moves}, a nx x ny x 4 array of TRUE/FALSE
#' values indicating if Karel can move to each direction from a given position.
#' For example, if Karel is in the bottom left corner, which is cell [1, 1], it
#' can't go south or left, so we have both open_moves[1, 1, 3] and open_moves[1,
#' 1, 4] set to FALSE. Depending on the existing walls it could move south or
#' north, so open_moves[1, 1, 1] and open_moves[1, 1, 2] could be TRUE or FALSE.
#'
#' Taking into account the size of the world and the walls, this function
#' properly defines the array open_moves.
#'
#' @param nx, ny size of the world
#' @param hor_walls, ver_walls dataset of horizontal and vertical walls as
#'   described in the details for the function \code{\link{.generate_world}}.
#'
#' @return A 6nx x ny x 4 array of TRUE/FALSE values
#'
#' @keywords internal
#'
generate_open_moves <- function(nx, ny, hor_walls, ver_walls) {
	open_moves <- array(TRUE, dim = c(nx, ny, 4))

	# Set world general boundaries (the rectangle contourn)
	# For example, when the robot is in the bottom street, it can't go south
	open_moves[1, , 3] <- FALSE
	open_moves[nx, , 1] <- FALSE
	open_moves[, 1, 4] <- FALSE
	open_moves[, ny, 2] <- FALSE

	# Build horizontal walls
	if (!is.null(hor_walls)) {
	  closed_pos <- purrr::pmap_dfr(hor_walls, put_hor_walls)
	  # Can't make it without for loop, but this is done only once,
	  # so I guess I'm fine
	  for (i in seq_len(nrow(closed_pos))) {
	    open_moves[closed_pos$pos_x[i], closed_pos$pos_y[i],
	               closed_pos$side[i]] <- FALSE
	  }
	}

	# Build vertical walls
	if (!is.null(ver_walls)) {
	  closed_pos <- purrr::pmap_dfr(ver_walls, put_ver_walls)
	  # Can't make it without for loop, but this is done only once,
	  # so I guess I'm fine
	  for (i in seq_len(nrow(closed_pos))) {
	    open_moves[closed_pos$pos_x[i], closed_pos$pos_y[i],
	               closed_pos$side[i]] <- FALSE
	  }
	}

	return(open_moves)
}

#' Put horizontal walls (streets)
#'
#' Helper function for \code{generate_open_moves()}. It takes the available data
#' for a piece of horizontal wall (x and y coordinates of its beginning point
#' and its length) and produces a dataset with the positions and directions of
#' prohibited moves for Karel because of this wall.
#'
#' @param x, y x and y coordinates of the beginning point of the wall
#' @param lgth length of the wall
#'
#' @return A lgth x 3 dataset with coordinates where Karel can't move and the
#'   direction (side) towards it can't move because of this piece of horizontal
#'   wall.
#'
#' @keywords internal
#'
put_hor_walls <- function(x, y, lgth) {
	tidyr::expand_grid(pos_x = (x + 1):(x + lgth), tibble(pos_y = y:(y + 1),
	                                                      side = c(2, 4)))
}

#' Put vertical walls (streets)
#'
#' Helper function for \code{generate_open_moves()}. It takes the available data
#' for a piece of vertical wall (x and y coordinates of its beginning point and
#' its length) and produces a dataset with the positions and directions of
#' prohibited moves for Karel because of this wall.
#'
#' @param x, y x and y coordinates of the beginning point of the wall
#' @param lgth length of the wall
#'
#' @return A lgth x 3 dataset with coordinates where Karel can't move and the
#'   direction (side) towards it can't move because of this piece of vertical
#'   wall.
#'
#' @keywords internal
#'
put_ver_walls <- function(x, y, lgth) {
	tidyr::expand_grid(pos_y = (y + 1):(y + lgth), tibble(pos_x = x:(x + 1),
	                                                      side = c(1, 3)))
}

#' Create dataset about beepers
#'
#' Given the elements provided in the world, this function generates a dataset
#' with info about the beepers present in the world. This function is called
#' from \code{generar_mundo()}.
#'
#' @param nx horizontal size of the world
#' @param pos_x, pos_y vectors of coordinates of cells with non-zero amount of
#'   beepers
#' @param n number of beepers in each cell indicated by coordinates in
#'   \code{pos_x} and \code{pos_y}
#' @param moment time
#'
#' @return A tibble with as many rows as cells with beepers in the world and 5
#'   columns: \code{x} and \code{y} for the coordinates of the cell, \code{cell}
#'   is the number of the cell counting as cell number 1 the cell in the bottom
#'   left corner and going upwards by row (meaning cell number 2 would be the
#'   cell in coordinates x=2 and y=1), \code{n} the number of beepers in this
#'   cell and \code{moment} the moment in which this state of the world
#'   corresponds to.
#'
#' @keywords internal
#'
create_beepers <- function(nx = NULL, pos_x = NULL, pos_y = NULL, n = NULL,
                           moment = 1) {
	if (is.null(pos_x)) {
		beepers <- tibble(x = NA, y = NA, cell = NA, n = NA, moment = moment)
	} else {
		beepers <- tibble(
			x = pos_x,
			y = pos_y,
			cell = pos_x + nx * pos_y - nx,
			n = n,
			moment = moment
		)
	}
	return(beepers)
}
