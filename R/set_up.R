# Solution for the "no visible binding for global variable" with variables in ggplot
utils::globalVariables(c("x", "y", "lgth", "n", "xmin", "xmax", "ymin", "ymax",
                         "moment", "."))

# Create environment
pkg_env <- new.env(parent = emptyenv())

#' Title
#'
#' @param world
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom ggplot2 ggplot geom_segment geom_point aes scale_x_continuous scale_y_continuous theme element_blank element_text geom_tile geom_text geom_rect coord_fixed
#' @importFrom dplyr tibble add_row slice mutate bind_rows n
#' @importFrom magrittr %>%
generar_mundo <- function(world) {
	if (is.character(world)) {
  	# Load this world from internal data
  	world <- try(get(world))
  	if (inherits(world, "try-error")) {
  	  stop("Required world doesn't exist.\nEl mundo pedido no existe.")
  	}
	} else {
		# User has provided their own world as a list.
	  # Todo: complete this check
	  elements <- names(world)
		if (!"nx" %in% elements) {
		  stop("Element nx missing in the world's definition.\nFalta el elemento nx en el mundo.")
		} else if (!is.numeric(world$nx) | length(world$nx) > 1) {
		  stop("nx must be a numeric vector of length 1 with the number of...")
		}
	}

	# Create environment
	# pkg_env <- new.env(parent = emptyenv())
  # Clear the environment
  rm(list = ls(pkg_env), envir = pkg_env)

  # Append useful world info
  pkg_env$nx <- world$nx
  pkg_env$ny <- world$ny
  pkg_env$hor_walls <- world$hor_walls
  pkg_env$ver_walls <- world$ver_walls

  # Create Karel dataset
  pkg_env$karel <- tibble(x = world$karel_x, y = world$karel_y, direction = world$karel_dir, moment = 1)

	# Current Karel's position and direction (1: east, 2: north, 3: west, 4: south)
	pkg_env$x_now <- world$karel_x
	pkg_env$y_now <- world$karel_y
	pkg_env$dir_now <- world$karel_dir

	# Current moment (for animation)
	pkg_env$moment <- 1

	# Array of open moves
	pkg_env$open_moves <- generate_open_moves(world$nx, world$ny, world$hor_walls, world$ver_walls)

	# Create beepers datasets
	# beepers_now only has current state of beepers, beepers_all acummulates all states for animation
	# beepers_total is the total number of beepers in the world (works even when beepers_n is NULL, sum is 0)
	pkg_env$beepers_any <- sum(world$beepers_n)
	pkg_env$beepers_now <- create_beepers(world$nx, world$beepers_x, world$beepers_y, world$beepers_n, moment = 1)
	pkg_env$beepers_all <- pkg_env$beepers_now
	pkg_env$beepers_bag <- world$beepers_bag

	# Plot the world
	plot_base_world()
	plot_static_world(1)

}

generate_open_moves <- function(nx, ny, hor_walls, ver_walls) {
	open_moves <- array(T, dim = c(nx, ny, 4))

	# Set world general boundaries (the rectangle contourn)
	# For example, when the robot is in the bottom street, it can't go south
	open_moves[1, , 3] <- F
	open_moves[nx, , 1] <- F
	open_moves[, 1, 4] <- F
	open_moves[, ny, 2] <- F

	# Build horizontal walls
	if (!is.null(hor_walls)) {
	  closed_pos <- purrr::pmap_dfr(hor_walls, put_hor_walls)
	  # Can't make it without for loop, but this is done only once, so I guess I'm fine
	  for (i in 1:nrow(closed_pos)) {
	    open_moves[closed_pos$pos_x[i], closed_pos$pos_y[i], closed_pos$side[i]] <- FALSE
	  }
	}

	# Build vertical walls
	if (!is.null(ver_walls)) {
	  closed_pos <- purrr::pmap_dfr(ver_walls, put_ver_walls)
	  # Can't make it without for loop, but this is done only once, so I guess I'm fine
	  for (i in 1:nrow(closed_pos)) {
	    open_moves[closed_pos$pos_x[i], closed_pos$pos_y[i], closed_pos$side[i]] <- FALSE
	  }
	}

	return(open_moves)
}


put_hor_walls <- function(x, y, lgth) {
	tidyr::expand_grid(pos_x = (x + 1):(x + lgth), tibble(pos_y = y:(y + 1), side = c(2, 4)))
}

put_ver_walls <- function(x, y, lgth) {
	tidyr::expand_grid(pos_y = (y + 1):(y + lgth), tibble(pos_x = x:(x + 1), side = c(1, 3)))
}

# test
# ver <- generar_mundo("world_001")

# pos_x, pos_y: vectors of indexes of cells with non-zero amount of beepers
# n: number of beepers in each cell indicated by x and y
create_beepers <- function(nx = NULL, pos_x = NULL, pos_y = NULL, n = NULL, moment = 1) {
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



#' Title
#'
#' @return
#' @export
#'
#' @examples
ejecutar_acciones <- function() {

  if (pkg_env$moment == 1) stop("Perform at least one action.\n Realizar al menos una actividad.")

  karel_for_drawing <- purrr::pmap_dfr(pkg_env$karel, draw_karel_df)

  p <-
    pkg_env$base_plot +
    geom_tile(data = pkg_env$beepers_all,
              aes(x = x - 0.5, y = y - 0.5, width = 0.4, height = 0.4),
              fill = "purple", color = "black", size = 0.5) +
    geom_text(data = pkg_env$beepers_all, aes(x = x - 0.5, y = y - 0.5, label = n), color = "white") +
    geom_rect(data = karel_for_drawing,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              alpha = karel_for_drawing$alpha,
              fill = karel_for_drawing$fill, color = "black") +
    gganimate::transition_manual(moment)

  # Choose frames per second according to the number of frames to animate
  nframes <- nrow(pkg_env$karel)

  fps <- dplyr::case_when(
    nframes < 40 ~ 2,
    nframes >= 40 & nframes < 100 ~ 3,
    nframes >= 100 ~ 4)

  suppressWarnings(
    gganimate::animate(p, nframes = nframes, fps = fps,
                       height = 800, width = 800,
                       renderer = gganimate::gifski_renderer(loop = FALSE))
  )

  # return(p)
}


