library(karel)

test_that("Checking provided nx and ny.", {
  # Create one world
  world_test <- list(nx = 6, ny = 4,
                    hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                    ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                    karel_x = 1, karel_y = 1,
                    # karel_dir = 1,
                    beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), "\nkarel_dir is missing in the provided world.\nFalta karel_dir en el mundo provisto.")

  world_test <- list(nx = 6.4, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), "\nnx, the number of avenues, must be numeric of length 1.
         nx, el numero de avenidas, debe ser numerico de largo 1.")

  world_test <- list(nx = 6:8, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), "\nnx, the number of avenues, must be numeric of length 1.
         nx, el numero de avenidas, debe ser numerico de largo 1.")

  world_test <- list(nx = 6, ny = "hola",
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), "\nny, the number of streets, must be numeric of length 1.
         ny, el numero de calles, debe ser numerico de largo 1.")

  world_test <- list(nx = 6, ny = 4.5,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), "\nny, the number of streets, must be numeric of length 1.
         ny, el numero de calles, debe ser numerico de largo 1.")
})

test_that("Checking provided karel_x, karel_y, karel_dir and karel_bag", {

  # karel_x
  msg <- "\nkarel_x, the x-coordinate for Karel's initial position, must be numeric of length 1, and between 1 and nx.\nkarel_x, la coordenada en el eje x para la posicion inicial de Karel, debe ser numerico de largo 1 y estar entre 1 y nx."
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1:2, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), msg)

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1.5, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), msg)

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 7, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), msg)

  # karel_y
  msg <- "\nkarel_y, the y-coordinate for Karel's initial position, must be numeric of length 1, and between 1 and ny.\nkarel_y, la coordenada en el eje y para la posicion inicial de Karel, debe ser numerico de largo 1 y estar entre 1 y ny."
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1:2, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), msg)

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1.5, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), msg)

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = -1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), msg)

  # karel_dir
  msg <- "\nkarel_dir, Karel's initial direction, must be numeric of length 1, either 1, 2, 3 or 4.\nkarel_dir, la direccion inicial de Karel, debe ser numerico de largo 1, puede ser 1, 2, 3 o 4."
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1:4,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), msg)

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1.5,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), msg)

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 5,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)
  expect_error(generar_mundo(world_test), msg)

  # beepers_bag
msg <- "\nbeepers_bag, the number of beepers in Karel's bag, must be numeric of length 1, greater or equal than zero, including Inf.\nbeepers_bag, el numero de cosos en la mochila de Karel, debe ser numerico de largo 1 y mayor o igual a 0, incluyendo Inf."
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = 3:4)
  expect_error(generar_mundo(world_test), msg)

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = 3.5)
  expect_error(generar_mundo(world_test), msg)

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1, beepers_bag = -1)
  expect_error(generar_mundo(world_test), msg)

})

test_that("Checking provided beepers_x, beepers_y and beepers_n", {

  msg <- "\nbeepers_x, beepers_y and beepers_n must be all NULL or numeric vectors of the same length.\nbeepers_x and beepers_y must be between 1 and nx or ny, respectively.\nbeepers_n must be greater or equal than 1.\nbeepers_x, beepers_y y beepers_n deben ser todos NULL o vectores numericos del mismo largo.\nbeepers_x y beepers_y deben estar entre 1 y nx o ny, respectivamentey.\nbeepers_n debe ser mayor o igual a 1."

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

test_that("Checking provided hor_walls and ver_walls", {
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = data.frame(x = 3, y = 1, lgth = 3),
                     ver_walls = data.frame(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 1, beepers_y = 1, beepers_n = 1, beepers_bag = Inf)

  world_test$hor_walls <- "hello"
  expect_error(generar_mundo(world_test), "\nhor_walls must be either NULL of a data.frame.\nhor_walls debe ser NULL o un data.frame")

  world_test$hor_walls <- data.frame(x = numeric(0), y = numeric(0), lgth = numeric(0))
  expect_error(generar_mundo(world_test), "\nhor_walls has 0 rows.\nhor_walls tiene 0 filas.")

  world_test$hor_walls <- data.frame(x = 3, y = 1)
  expect_error(generar_mundo(world_test), "\nColumn lgth is missing in hor_walls data.frame.\nFalta la columna lgth en el data.frame hor_walls.")

  world_test$hor_walls <- data.frame(x = 3, y = 1, lgth = NA)
  expect_error(generar_mundo(world_test), "\nhor_walls can't have NA values.\nNo puede haber NAs en hor_walls.")

  world_test$hor_walls <- data.frame(x = 3, y = 1, lgth = "hola")
  expect_error(generar_mundo(world_test), "\nAll columns in hor_walls must be numeric.\nTodas las columnas de hor_walls deben ser numericas.")

  world_test$hor_walls <- data.frame(x = 3, y = 1, lgth = 2.3)
  expect_error(generar_mundo(world_test), "\nAll numbers in hor_walls must be integer, at least in the math sense, not necessarily of class integer.\nTodos los numeros en hor_walls deben ser enteros, al menos en el sentido matematico, no necesariamente de clase integer.")

  world_test$hor_walls <- data.frame(x = 7, y = 1, lgth = 1)
  expect_error(generar_mundo(world_test), "\nAll x values in hor_walls must lie between 0 and nx-1.\nTodos los valores x en hor_walls deben estar entre 0 y nx-1.")

  world_test$hor_walls <- data.frame(x = 1, y = 4, lgth = 1)
  expect_error(generar_mundo(world_test), "\nAll y values in hor_walls must lie between 0 and ny-1.\nTodos los valores y en hor_walls deben estar entre 0 y ny-1.")

  world_test$hor_walls <- data.frame(x = 3, y = 3, lgth = -1)
  expect_error(generar_mundo(world_test), "\nAll lgth values in hor_walls must be 1 or greater.\nTodos los valores lgth en hor_walls deben ser mayores o iguales a 1.")

  world_test$hor_walls <- data.frame(x = 4, y = 2, lgth = 5)
  expect_error(generar_mundo(world_test), "\nSome lengths in hor_walls are longer than allowed by nx.\nAlgunos largos en hor_walls exceden lo permitido por nx.")

  world_test$hor_walls <- data.frame(x = 4, y = 2, lgth = 1)
  world_test$ver_walls <- data.frame(x = 4, y = 2, lgth = 4)
  expect_error(generar_mundo(world_test), "\nSome lengths in ver_walls are longer than allowed by ny.\nAlgunos largos en ver_walls exceden lo permitido por ny.")


})

test_that("Other things about generar_mundo()", {
  expect_error(generar_mundo("world_1"), "\nRequired world doesn't exist.\nEl mundo pedido no existe.")

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = data.frame(x = 3, y = 1, lgth = 3),
                     ver_walls = data.frame(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = NULL, beepers_y = NULL, beepers_n = NULL, beepers_bag = Inf)
  generar_mundo(world_test)
  pkg_env <- get_pkg_env()
  expect_equal(pkg_env$beepers_now, dplyr::tibble(x = NA, y = NA, cell = NA, n = NA, moment = 1))
  expect_error(ejecutar_acciones(), "\nPerform at least one action.\nRealizar al menos una accion.")
})
