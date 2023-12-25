library(karel)

test_that("Checking provided nx and ny.", {
  # Create one world
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1,
                     # karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6.4, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6:8, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6, ny = "hola",
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6, ny = 4.5,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))
})

test_that("Checking provided karel_x, karel_y, karel_dir and karel_bag", {

  # karel_x
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1:2, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1.5, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 7, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  # karel_y
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1:2, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1.5, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = -1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  # karel_dir
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1:4,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1.5,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 5,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  # beepers_bag
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = 3:4)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = 3.5)
  expect_error(generar_mundo(world_test))

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = -1)
  expect_error(generar_mundo(world_test))

})

test_that("Checking provided beepers_x, beepers_y and beepers_n", {

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = NULL, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)
  expect_error(generar_mundo(world_test))

  world_test$beepers_x <- 1:3
  expect_error(generar_mundo(world_test))

  world_test$beepers_x <- 1.3
  expect_error(generar_mundo(world_test))

  world_test$beepers_x <- -2
  expect_error(generar_mundo(world_test))

  world_test$beepers_x <- 10
  expect_error(generar_mundo(world_test))

})

test_that("Checking provided hor_walls and ver_walls", {
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = data.frame(x = 3, y = 1, lgth = 3),
                     ver_walls = data.frame(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 1, beepers_y = 1, beepers_n = 1,
                     beepers_bag = Inf)

  world_test$hor_walls <- "hello"
  expect_error(generar_mundo(world_test))

  world_test$hor_walls <- data.frame(x = numeric(0), y = numeric(0),
                                     lgth = numeric(0))
  expect_error(generar_mundo(world_test))

  world_test$hor_walls <- data.frame(x = 3, y = 1)
  expect_error(generar_mundo(world_test))

  world_test$hor_walls <- data.frame(x = 3, y = 1, lgth = NA)
  expect_error(generar_mundo(world_test))

  world_test$hor_walls <- data.frame(x = 3, y = 1, lgth = "hola")
  expect_error(generar_mundo(world_test))

  world_test$hor_walls <- data.frame(x = 3, y = 1, lgth = 2.3)
  expect_error(generar_mundo(world_test))

  world_test$hor_walls <- data.frame(x = 7, y = 1, lgth = 1)
  expect_error(generar_mundo(world_test))

  world_test$hor_walls <- data.frame(x = 1, y = 4, lgth = 1)
  expect_error(generar_mundo(world_test))

  world_test$hor_walls <- data.frame(x = 3, y = 3, lgth = -1)
  expect_error(generar_mundo(world_test))

  world_test$hor_walls <- data.frame(x = 4, y = 2, lgth = 5)
  expect_error(generar_mundo(world_test))

  world_test$hor_walls <- data.frame(x = 4, y = 2, lgth = 1)
  world_test$ver_walls <- data.frame(x = 4, y = 2, lgth = 4)
  expect_error(generar_mundo(world_test))


})

test_that("Other things about generar_mundo()", {
  expect_error(generar_mundo("world_1"))

  world_test <- list(nx = 6, ny = 4,
                     hor_walls = data.frame(x = 3, y = 1, lgth = 3),
                     ver_walls = data.frame(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = NULL, beepers_y = NULL, beepers_n = NULL,
                     beepers_bag = Inf)
  generar_mundo(world_test)
  pkg_env <- get_pkg_env()
  expect_equal(pkg_env$beepers_now, dplyr::tibble(x = NA, y = NA, cell = NA,
                                                  n = NA, moment = 1))
  expect_error(ejecutar_acciones())
})
