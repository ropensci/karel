test_that("Karel can't perform any other action once the user has made a
          mistake", {
  generate_world("mundo001")
  turn_right()
  # Karel tries to move through a wall:
  expect_error(move(), "Karel can't move south, because there's a wall")
  # can´t do anything else (until the world is regenerated):
  expect_error(move(), "You made a mistake before")
  expect_error(turn_right(), "You made a mistake before")
  expect_error(turn_left(), "You made a mistake before")
  expect_error(turn_around(), "You made a mistake before")
  expect_error(move(), "You made a mistake before")
  expect_error(put_beeper(), "You made a mistake before")
  expect_error(pick_beeper(), "You made a mistake before")
  expect_error(run_actions(), "You made a mistake before")
})

test_that("Karel can't move through walls when facing west", {
  generate_world("mundo001")
  turn_around()
  expect_error(move(), "Karel can't move west, because there's a wall")
})

test_that("Karel can't move through walls when facing east", {
  generate_world("mundo001")
  move()
  move()
  expect_error(move(), "Karel can't move east, because there's a wall")
})

test_that("Karel can't move through walls when facing south", {
  generate_world("mundo001")
  turn_right()
  expect_error(move(), "Karel can't move south, because there's a wall")
})

test_that("Karel can't move through walls when facing north", {
  generate_world("mundo001")
  turn_left()
  move()
  move()
  move()
  expect_error(move(), "Karel can't move north, because there's a wall")
})

test_that("Karel can't pick a beeper where none is present", {
  generate_world("mundo001")
  expect_error(pick_beeper(), "There are no beepers here to pick.")
})

test_that("Karel can't put a beeper if there is none in her bag", {
  # create a world where she has no beepers in her bag
  world_test <- list(nx = 6, ny = 4,
                     hor_walls = dplyr::tibble(x = 3, y = 1, lgth = 3),
                     ver_walls = dplyr::tibble(x = 3, y = 0, lgth = 1),
                     karel_x = 1, karel_y = 1, karel_dir = 1,
                     beepers_x = 2, beepers_y = 1, beepers_n = 1,
                     beepers_bag = 0)
  generar_mundo(world_test)
  expect_error(poner_coso(),
               "Karel no puede colocar un coso ya que no le queda ninguno en la mochila.")
})

