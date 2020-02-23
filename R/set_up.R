# Solution for the "no visible binding for global variable" with variables in ggplot
utils::globalVariables(c("x", "y", "lgth", "n", "xmin", "xmax", "ymin", "ymax",
                         "moment", "."))

# Create environment
pkg_env <- new.env(parent = emptyenv())

#' Generar el mundo de Karel
#'
#' Esta función toma la definición de un mundo para Karel (tamaño, paredes,
#' "cosos" e incluso la ubicación y dirección de Karel) y lo muestra en un
#' gráfico. Además, prepara todo para que Karel pueda realizar sus acciones, de
#' modo que debe ser evaluada antes de que Karel empiece a cumplir sus
#' objetivos. En especial, si en algún momento hemos cometido un error, debemos
#' comenzar de nuevo corriendo esta función.
#'
#' Luego de correr \code{generar_mundo()}, se ejecutan las acciones de Karel y
#' se pueden visualizar con la función \code{ejecutar_acciones()}.
#'
#' @param mundo Un caracter de largo 1 indicando el nombre de uno de los mundos
#'   que ya vienen en el paquete o un objeto de tipo lista con todos los
#'   componentes que debe tener un mundo (ver más abajo en Detalles).
#'
#' @return Dibuja el estado inicial del mundo de Karel y deja todo preparado
#'   para comenzar a registrar sus acciones.
#'
#' @export
#'
#' @examples
#' generar_mundo("world_101")
#'
#' @seealso \code{\link{acciones}} \code{\link{ejecutar_acciones}}
#'
#' @details El argumento \code{mundo} puede consistir de un mundo creado (es
#'   decir, inventado) por cualquiera. En este caso, \code{mundo} debe ser una
#'   lista con los siguientes componentes:
#'
#'   \enumerate{
#'     \item \code{nx}: TODO
#'     \item \code{ny}:
#'     \item \code{hor_walls}:
#'     \item \code{ver_walls}:
#'     \item \code{karel_x}:
#'     \item \code{karel_y}:
#'     \item \code{karel_dir}:
#'     \item \code{beepers_x}:
#'     \item \code{beepers_y}:
#'     \item \code{beepers_n}:
#'     \item \code{beepers_bag}:
#'   }
#'
#' @importFrom ggplot2 ggplot geom_segment geom_point aes scale_x_continuous
#'   scale_y_continuous theme element_blank element_text geom_tile geom_text
#'   geom_rect coord_fixed
#' @importFrom dplyr tibble add_row slice mutate bind_rows n
#' @importFrom magrittr %>%
#'
generar_mundo <- function(mundo) {

  # I first programmed this argument with the word "world" but now I want the
  # argument to be in spanish here
  world <- mundo

  if (is.character(world)) {
  	# Load this world from internal data
  	world <- try(get(world))
  	if (inherits(world, "try-error")) {
  	  stop("\nRequired world doesn't exist.\nEl mundo pedido no existe.")
  	}
	} else {
		# User has provided their own world as a list.
    check_user_world(world)
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
#'   described in the details for the function \code{\link{generar_mundo}}.
#'
#' @return A 6nx x ny x 4 array of TRUE/FALSE values
#'
#' @keywords internal
#'
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
	tidyr::expand_grid(pos_x = (x + 1):(x + lgth), tibble(pos_y = y:(y + 1), side = c(2, 4)))
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
	tidyr::expand_grid(pos_y = (y + 1):(y + lgth), tibble(pos_x = x:(x + 1), side = c(1, 3)))
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


#' Ejecutar acciones
#'
#' Esta función produce la animación que muestra todas las acciones realizadas por Karel desde que su mundo fue generado con \code{generar_mundo}.
#'
#' @return Produce la animación con \code{gganimate}
#'
#' @examples
#' generar_mundo("world_101")
#' avanzar()
#' juntar_coso()
#' girar_izquierda()
#' poner_coso()
#' ejecutar_acciones()
#'
#' @seealso \code{\link{generar_mundo}}
#'
#' @export
ejecutar_acciones <- function() {

  if (pkg_env$moment == 1) stop("\nPerform at least one action.\n Realizar al menos una actividad.")

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
}


