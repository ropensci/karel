test_that("Karel actually moves when the English wrapper move() is called", {
  generate_world("mundo001")
  karel_env <- get_pkg_env()
  move()
  expect_equal(karel_env$x_now, 2)
})

test_that("Karel actually picks a beeper when the English wrapper juntar_coso()
          is called", {
  generate_world("mundo001")
  karel_env <- get_pkg_env()
  move()
  pick_beeper()
  expect_equal(karel_env$beepers_any, 0)
})

test_that("Karel actually turns left when the English wrapper turn_left()
          is called", {
  generate_world("mundo001")
  karel_env <- get_pkg_env()
  turn_left()
  expect_equal(karel_env$dir_now, 2)
})

test_that("Karel actually puts a beeper when the English wrapper put_beeper()
          is called", {
  generate_world("mundo001")
  karel_env <- get_pkg_env()
  put_beeper()
  expect_equal(karel_env$beepers_any, 2)
  # there was one beeper and now there are 2
})
