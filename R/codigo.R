# Solution for the "no visible binding for global variable" with variables in ggplot
utils::globalVariables(c("x", "y", "lgth", "n", "xmin", "xmax", "ymin", "ymax"))

temp_fun <- function() {
	print(world_001$hor_walls)
}

temp_fun2 <- function(mundo) {
	mundo <- get(mundo)
	print(mundo$hor_walls)
	return(mundo)
}

# Create environment
pkg_env <- new.env(parent = emptyenv())

generar_mundo <- function(world) {
	if (is.character(world)) {
		# Load this world from internal data
		world <- get(world)
		# Todo: check for errors (world has to be one of the available worlds)
	} else {
		# User has provided their own world as a list.
		# Todo: check this list is ok
	}

	# Create environment
	# pkg_env <- new.env(parent = emptyenv())
  # Clear the environment
  rm(list = ls(pkg_env), envir = pkg_env)

  # Append world size
  pkg_env$nx <- world$nx
  pkg_env$ny <- world$ny

  # Create Karel dataset
  pkg_env$karel <- tibble(x = 1, y = 1, direction = 1, moment = 1)

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
	pkg_env$beepers_now <- create_beepers(world$nx, world$beepers_x, world$beepers_y, world$beepers_n)
	pkg_env$beepers_all <- pkg_env$beepers_now

	# Plot the world
	p <- plot_world(world)
	graphics::plot(p)

	return(pkg_env$open_moves)

}

generate_open_moves <- function(nx, ny, hor_walls, ver_walls) {
	open_moves <- array(T, dim = c(nx, ny, 4))

	# Set world general boundaries (the rectangle contourn)
	# For example, when the robot is in the bottom street, it can't go south
	open_moves[1, , 3] <- F
	open_moves[nx, , 1] <- F
	open_moves[, 1, 4] <- F
	open_moves[, ny, 2] <- F

	closed_pos <- rbind(purrr::pmap_dfr(hor_walls, put_hor_walls), purrr::pmap_dfr(ver_walls, put_ver_walls))

	# Can't make it without for loop, but this is done only once, so I guess I'm fine
	for (i in 1:nrow(closed_pos)) {
		open_moves[closed_pos$pos_x[i], closed_pos$pos_y[i], closed_pos$side[i]] <- FALSE
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
create_beepers <- function(nx = NULL, pos_x = NULL, pos_y = NULL, n = NULL) {
	if (is.null(pos_x)) {
		beepers <- tibble(
			x = numeric(0),
			y = numeric(0),
			cell = numeric(0),
			n = numeric(0),
			moment = numeric(0))
	} else {
		beepers <- tibble(
			x = pos_x,
			y = pos_y,
			cell = pos_x + nx * pos_y - nx,
			n = n,
			moment = 1
		)
	}
	return(beepers)
}

#' Plot the world
#'
#' @param world
#' @importFrom ggplot2 ggplot geom_segment geom_point aes scale_x_continuous scale_y_continuous theme element_blank element_text geom_tile geom_text geom_rect
#' @importFrom dplyr tibble add_row slice mutate
plot_world <- function(world) {

	karel_for_drawing <- draw_karel_df(world$karel_x, world$karel_y, world$karel_dir, 1)

	p <-
		ggplot(NULL) +
		geom_segment(data = world$ver_walls, aes(x = x, y = y, xend = x, yend = y + lgth), size = 2) +
		geom_segment(data = world$hor_walls, aes(x = x, y = y, xend = x + lgth, yend = y), size = 2) +
		geom_point(data = tidyr::expand_grid(x = (1:world$nx) - 0.5, y = (1:world$ny) - 0.5),
							 aes(x = x, y = y), size = 2) +
		scale_x_continuous("", expand = c(0, 0), limits = c(0, world$nx),
											 breaks = 0.5:(world$nx - 0.5), labels = 1:world$nx) +
		scale_y_continuous("", expand = c(0, 0), limits = c(0, world$ny),
											 breaks = 0.5:(world$ny - 0.5), labels = 1:world$ny) +
		theme(
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank(),
			axis.ticks = element_blank(),
			axis.text = element_text(face = "bold")
		) +
		geom_tile(data = pkg_env$beepers_now,
							aes(x = x - 0.5, y = y - 0.5, width = 0.4, height = 0.4),
							fill = "purple", color = "black", size = 0.5) +
		geom_text(data = pkg_env$beepers_now, aes(x = x - 0.5, y = y - 0.5, label = n), color = "white") +
		geom_rect(data = karel_for_drawing,
							aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
							alpha = karel_for_drawing$alpha,
							fill = karel_for_drawing$fill, color = "black")
	return(p)
}


draw_karel_df <- function(x, y, direction, moment) {
	switch(direction,
				 # In the tibbles, this is the order of the coordinates:
				 # object = c("body", "left_foot", "right_foot", "left_eye", "right_eye", "mouth")
				 # Direction 1, going east
				 tibble(
				 	xmin = x - c(.9, .2, .2, .75, .75, .45),
				 	xmax = x - c(.2, .1, .1, .85, .85, .35),
				 	ymin = y - c(.85, .75, .45, .65, .45, .65),
				 	ymax = y - c(.15, .55, .25, .55, .35, .35),
				 	moment = moment,
				 	fill = c("orange", "black", "black", "brown", "brown", "red"),
				 	alpha = c(0.25, rep(1, 5))
				 ),
				 # Direction 2, going north
				 tibble(
				 	xmax = x - c(.85, .75, .45, .65, .45, .65),
				 	xmin = x - c(.15, .55, .25, .55, .35, .35),
				 	ymax = y - c(.8, .9, .9, .35, .35, .65),
				 	ymin = y - c(.1, .8, .8, .25, .25, .55),
				 	moment = moment,
				 	fill = c("orange", "black", "black", "brown", "brown", "red"),
				 	alpha = c(0.25, rep(1, 5))
				 ),
				 # Direction 3, going west
				 tibble(
				 	xmax = x - c(.9, .2, .2, .75, .75, .45),
				 	xmin = x - c(.2, .1, .1, .85, .85, .35),
				 	ymax = y - c(.85, .75, .45, .65, .45, .65),
				 	ymin = y - c(.15, .55, .25, .55, .35, .35),
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

