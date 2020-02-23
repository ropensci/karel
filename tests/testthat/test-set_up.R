context("Set up")
library(karel)

# test_that("Checking provided nx and ny.", {
#   # Create one world
#   world_test <- list(nx = 6, ny = 4,
#                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                     karel_x = 1, karel_y = 1,
#                     # karel_dir = 1,
#                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), "\nkarel_dir is missing in the provided world.\nFalta karel_dir en el mundo provisto.")
#
#   world_test <- list(nx = 6.4, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), "\nnx, the number of avenues, must be numeric of length 1.
#          nx, el número de avenidas, debe ser númerico de largo 1.")
#
#   world_test <- list(nx = 6:8, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), "\nnx, the number of avenues, must be numeric of length 1.
#          nx, el número de avenidas, debe ser númerico de largo 1.")
#
#   world_test <- list(nx = 6, ny = "hola",
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), "\nny, the number of streets, must be numeric of length 1.
#          ny, el número de calles, debe ser númerico de largo 1.")
#
#   world_test <- list(nx = 6, ny = 4.5,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), "\nny, the number of streets, must be numeric of length 1.
#          ny, el número de calles, debe ser númerico de largo 1.")
# })

# test_that("Checking provided karel_x, karel_y, karel_dir and karel_bag", {
#
#   # karel_x
#   msg <- "\nkarel_x, the x-coordinate for Karel's initial position, must be numeric of length 1, and between 1 and nx.\nkarel_x, la coordenada en el eje x para la posición inicial de Karel, debe ser númerico de largo 1 y estar entre 1 y nx."
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1:2, karel_y = 1, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), msg)
#
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1.5, karel_y = 1, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), msg)
#
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 7, karel_y = 1, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), msg)
#
#   # karel_y
#   msg <- "\nkarel_y, the y-coordinate for Karel's initial position, must be numeric of length 1, and between 1 and ny.\nkarel_y, la coordenada en el eje y para la posición inicial de Karel, debe ser númerico de largo 1 y estar entre 1 y ny."
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1:2, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), msg)
#
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1.5, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), msg)
#
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = -1, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), msg)
#
#   # karel_dir
#   msg <- "\nkarel_dir, Karel's initial direction, must be numeric of length 1, either 1, 2, 3 or 4.\nkarel_dir, la direccion inicial de Karel, debe ser numerico de largo 1, puede ser 1, 2, 3 o 4."
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1, karel_dir = 1:4,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), msg)
#
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1, karel_dir = 1.5,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), msg)
#
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1, karel_dir = 5,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
#   expect_error(generar_mundo(world_test), msg)
#
#   # beepers_bag
# msg <- "\nbeepers_bag, the number of beepers in Karel's bag, must be numeric of length 1, greater or equal than zero, including Inf.\nbeepers_bag, el número de cosos en la mochila de Karel, debe ser númerico de largo 1 y mayor o igual a 0, incluyendo Inf."
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = 3:4)
#   expect_error(generar_mundo(world_test), msg)
#
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = 3.5)
#   expect_error(generar_mundo(world_test), msg)
#
#   world_test <- list(nx = 6, ny = 4,
#                      hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
#                      ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
#                      karel_x = 1, karel_y = 1, karel_dir = 1,
#                      beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = -1)
#   expect_error(generar_mundo(world_test), msg)
#
# })

test_that("Checking provided beepers_x, beepers_y and beepers_n", {

  msg <- "\nbeepers_x, beepers_y and beepers_n must be all NULL or numeric vectors of the same length.\nbeepers_x and beepers_y must be between 1 and nx or ny, respectively.\nbeepers_n must be greater or equal than 1.\nbeepers_x, beepers_y y beepers_n deben ser todos NULL o vectores numéricos del mismo largo.\nbeepers_x y beepers_y deben estar entre 1 y nx o ny, respectivamentey.\nbeepers_n debe ser mayor o igual a 1."

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = NULL, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), msg)

  world_test$beepers_x = 1:3
  expect_error(generar_mundo(world_test), msg)

  world_test$beepers_x = 1.3
  expect_error(generar_mundo(world_test), msg)

  world_test$beepers_x = -2
  expect_error(generar_mundo(world_test), msg)

  world_test$beepers_x = 10
  expect_error(generar_mundo(world_test), msg)

})

