temp_fun <- function() {
	print(world_001$hor_walls)
}

temp_fun2 <- function(mundo) {
	mundo <- get(mundo)
	print(mundo$hor_walls)
	return(mundo)
}

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
	pkg_env <- new.env(parent = emptyenv())

	# Current Karel's position and direction (1: east, 2: north, 3: west, 4: south)
	pkg_env$x_now <- world$karel_x
	pkg_env$y_now <- world$karel_y
	pkg_env$dir_now <- world$karel_dir

	# Current moment (for animation)
	pkg_env$moment <- 1

	# Array of open moves COMPLETAR CON EL CODIGO DE ANTES
	pkg_env$open_moves <- generate_open_moves(world$nx, world$ny, world$hor_walls, world$ver_walls)

	print(ls(pkg_env))
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
	tidyr::expand_grid(pos_x = (x + 1):(x + lgth), dplyr::tibble(pos_y = y:(y + 1), side = c(2, 4)))
}
put_ver_walls <- function(x, y, lgth) {
	tidyr::expand_grid(pos_y = (y + 1):(y + lgth), dplyr::tibble(pos_x = x:(x + 1), side = c(1, 3)))
}

# test
# ver <- generar_mundo("world_001")
