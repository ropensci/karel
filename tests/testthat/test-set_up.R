# generic world template to be used in tests
world_test_template <- list(
  nx = 6, ny = 4,
  hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
  ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
  karel_x = 1, karel_y = 1,
  karel_dir = 1,
  beepers_x = 2, beepers_y = 1, beepers_n = 1,
  beepers_bag = Inf
)

test_that("The generic world template works correctly before modifying it
          in subsequent tests", {
 expect_no_error(generar_mundo(world_test_template))
 expect_no_error(generate_world(world_test_template))
})

test_that("Karel's initial direction is required when creating a new world
           provided as a list by the user.", {
   world_test <- world_test_template
   world_test$karel_dir <- NULL
   expect_error(generate_world(world_test), "karel_dir is missing")

   world_test$karel_dir <- 1:4
   expect_error(generate_world(world_test),
      "karel_dir, Karel's initial direction, must be numeric of length 1")

   world_test$karel_dir <- 5
   expect_error(generate_world(world_test), "either 1, 2, 3 or 4")

   world_test$karel_dir <- 1.5
   expect_error(generate_world(world_test),
            "karel_dir, Karel's initial direction, must be numeric of length 1")
})

test_that("The correct number of avenues (nx) is required when creating a new
           world provided as a list by the user.", {
  world_test <- world_test_template
  world_test$nx <- 6.4
  expect_error(
   generate_world(world_test),
   "nx, the number of avenues, must be numeric of length 1")

  world_test$nx <- 6:8
  expect_error(
    generate_world(world_test),
    "nx, the number of avenues, must be numeric of length 1")
})


test_that("The correct number of streets (ny) is required when creating a new
           world provided as a list by the user.", {
  world_test <- world_test_template
  world_test$ny <- "hola"
  expect_error(generate_world(world_test),
               "ny, the number of streets, must be numeric of length 1")

  world_test$ny <- 4.5
  expect_error(generate_world(world_test),
               "ny, the number of streets, must be numeric of length 1")
})

test_that("The correct initial position for Karel in x axis is required when
           creating a new world provided as a list by the user.", {
  # karel_x
  world_test <- world_test_template
  world_test$karel_x <- 1:2
  expect_error(generate_world(world_test),
               "must be numeric of length 1, and between 1 and nx")

  world_test$karel_x <- 1.5
  expect_error(generate_world(world_test),
               "must be numeric of length 1, and between 1 and nx")

  world_test$karel_x <- 7
  expect_error(generate_world(world_test),
               "must be numeric of length 1, and between 1 and nx")
})


test_that("The correct initial position for Karel in y axis is required when
           creating a new world provided as a list by the user.", {

  # karel_y
  world_test <- world_test_template
  world_test$karel_y <- 1:2
  expect_error(generate_world(world_test),
               "must be numeric of length 1, and between 1 and ny")

  world_test$karel_y <- 1.5
  expect_error(generate_world(world_test),
               "must be numeric of length 1, and between 1 and ny")

  world_test$karel_y <- -1
  expect_error(generate_world(world_test),
               "must be numeric of length 1, and between 1 and ny")
})

test_that("The correct number of beepers in Karel's bag (beepers_bag) is
           required when creating a new world provided as a list by the user", {

  world_test <- world_test_template
  world_test$beepers_bag <- 3:4
  expect_error(generate_world(world_test),
            "the number of beepers in Karel's bag, must be numeric of length 1")

  world_test$beepers_bag <- 3.5
  expect_error(generate_world(world_test),
            "the number of beepers in Karel's bag, must be numeric of length 1")

  world_test$beepers_bag <- -1
  expect_error(generate_world(world_test),
            "the number of beepers in Karel's bag, must be numeric of length 1")
})

test_that("The correct x-axis coordinates for beepers position (beepers_x) is
           required when creating a new world provided as a list by the user", {

  world_test <- world_test_template
  world_test$beepers_x <- NULL
  expect_error(generate_world(world_test),
               "beepers_x is missing in the provided world")

  world_test$beepers_x <- 1:3
  expect_error(generate_world(world_test),
      "beepers_x, beepers_y and beepers_n must be all NULL or numeric vectors")

  world_test$beepers_x <- 1.3
  expect_error(generate_world(world_test),
      "beepers_x, beepers_y and beepers_n must be all NULL or numeric vectors")

  world_test$beepers_x <- -2
  expect_error(generate_world(world_test),
      "beepers_x, beepers_y and beepers_n must be all NULL or numeric vectors")

  world_test$beepers_x <- 10
  expect_error(generate_world(world_test),
      "beepers_x, beepers_y and beepers_n must be all NULL or numeric vectors")

})

test_that("The correct y-axis coordinates for beepers position (beepers_y) is
           required when creating a new world provided as a list by the user", {

  world_test <- world_test_template
  world_test$beepers_y <- NULL
  expect_error(generate_world(world_test),
               "beepers_y is missing in the provided world.")

  world_test$beepers_y <- 1:3
  expect_error(generate_world(world_test),
      "beepers_x, beepers_y and beepers_n must be all NULL or numeric vectors")

  world_test$beepers_y <- 1.3
  expect_error(generate_world(world_test),
               "beepers_x, beepers_y and beepers_n must be all NULL or numeric")

  world_test$beepers_y <- -2
  expect_error(generate_world(world_test),
               "beepers_x, beepers_y and beepers_n must be all NULL or numeric")

  world_test$beepers_y <- 10
  expect_error(generate_world(world_test),
               "beepers_x, beepers_y and beepers_n must be all NULL or numeric")

})

test_that("The correct amounts of beepers (beepers_n) are
           required when creating a new world provided as a list by the user", {

  world_test <- world_test_template
  world_test$beepers_n <- NULL
  expect_error(generate_world(world_test),
               "beepers_n is missing in the provided world")

  world_test$beepers_n <- 1:3
  expect_error(generate_world(world_test),
               "beepers_x, beepers_y and beepers_n must be all NULL or numeric")

  world_test$beepers_n <- 1.3
  expect_error(generate_world(world_test),
               "beepers_x, beepers_y and beepers_n must be all NULL or numeric")

  world_test$beepers_n <- -2
  expect_error(generate_world(world_test),
               "beepers_x, beepers_y and beepers_n must be all NULL or numeric")

})

test_that("The correct data frame for the definition of horizontal walls is
           required when creating a new world provided as a list by the user", {

  world_test <- world_test_template

  world_test$hor_walls <- "hello"
  expect_error(generate_world(world_test),
               "hor_walls must be either NULL of a data.frame")

  world_test$hor_walls <- data.frame(x = numeric(0), y = numeric(0),
                                     lgth = numeric(0))
  expect_error(generate_world(world_test), "hor_walls has 0 rows")

  world_test$hor_walls <- data.frame(x = 3, y = 1)
  expect_error(generate_world(world_test),
               "Column lgth is missing in data.framehor_walls")

  world_test$hor_walls <- data.frame(x = 3, y = 1, lgth = NA)
  expect_error(generate_world(world_test), "hor_walls can't have NA values.")

  world_test$hor_walls <- data.frame(x = 3, y = 1, lgth = "hola")
  expect_error(generate_world(world_test),
               "hor_walls must have all numeric columns.")

  world_test$hor_walls <- data.frame(x = 3, y = 1, lgth = 2.3)
  expect_error(generate_world(world_test),
               "hor_walls must have all integer values")

  world_test$hor_walls <- data.frame(x = 7, y = 1, lgth = 1)
  expect_error(generate_world(world_test),
               "hor_walls must have in column x values between 0 and nx-1")

  world_test$hor_walls <- data.frame(x = 1, y = 4, lgth = 1)
  expect_error(generate_world(world_test),
               "hor_walls must have in column y values between 0 and ny-1")

  world_test$hor_walls <- data.frame(x = 3, y = 3, lgth = -1)
  expect_error(generate_world(world_test),
              "hor_walls must have in column lgth values greater or equal to 1")

  world_test$hor_walls <- data.frame(x = 4, y = 2, lgth = 5)
  expect_error(generate_world(world_test),
               "Some lengths in hor_walls are longer than allowed by nx.")
})

test_that("The correct data frame for the definition of vertical walls is
           required when creating a new world provided as a list by the user", {

  world_test <- world_test_template

  world_test$ver_walls <- "hello"
  expect_error(generate_world(world_test),
               "ver_walls must be either NULL of a data.frame.")

  world_test$ver_walls <- data.frame(x = numeric(0), y = numeric(0),
                                     lgth = numeric(0))
  expect_error(generate_world(world_test), "ver_walls has 0 rows")

  world_test$ver_walls <- data.frame(x = 3, y = 1)
  expect_error(generate_world(world_test),
               "Column lgth is missing in data.framever_walls.")

  world_test$ver_walls <- data.frame(x = 3, y = 1, lgth = NA)
  expect_error(generate_world(world_test), "ver_walls can't have NA values.")

  world_test$ver_walls <- data.frame(x = 3, y = 1, lgth = "hola")
  expect_error(generate_world(world_test),
               "ver_walls must have all numeric columns.")

  world_test$ver_walls <- data.frame(x = 3, y = 1, lgth = 2.3)
  expect_error(generate_world(world_test),
               "ver_walls must have all integer values")

  world_test$ver_walls <- data.frame(x = 7, y = 1, lgth = 1)
  expect_error(generate_world(world_test),
               "ver_walls must have in column x values between 0 and nx-1")

  world_test$ver_walls <- data.frame(x = 1, y = 4, lgth = 1)
  expect_error(generate_world(world_test),
               "ver_walls must have in column y values between 0 and ny-1")

  world_test$ver_walls <- data.frame(x = 3, y = 3, lgth = -1)
  expect_error(generate_world(world_test),
            "ver_walls must have in column lgth values greater or equal to 1.")

  world_test$ver_walls <- data.frame(x = 4, y = 2, lgth = 5)
  expect_error(generate_world(world_test),
               "Some lengths in ver_walls are longer than allowed by ny.")

})

test_that("The function the pkg environment is correctly returned, with the
          expected objects", {

  world_test <- world_test_template
  generate_world(world_test)
  pkg_env <- get_pkg_env()
  expect_equal(pkg_env$beepers_now, dplyr::tibble(x = NA, y = NA, cell = NA,
                                                  n = NA, moment = 1))

})

test_that("The package doesn't work when a world which doesn't exist is
          called", {
  expect_error(generar_mundo("world_1"), "El mundo pedido no existe.")
  expect_error(generate_world("world_1"), "Required world doesn't exist.")
})

test_that("You can't run actions without generating a world and actions
          first.", {
  expect_error(ejecutar_acciones(),
               "Karel no ha recibido ninguna instrucción aún")
  expect_error(run_actions(), "Karel hasn't received any instruction yet.")
})
