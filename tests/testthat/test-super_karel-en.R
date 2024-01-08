test_that("Karel actually turns right when the English wrapper turn_right()
          is called", {
  generate_world("mundo001")
  karel_env <- get_pkg_env()
  turn_right()
  expect_equal(karel_env$dir_now, 4)
})

test_that("Karel actually turns arond when the English wrapper turn_around()
          is called", {
  generate_world("mundo001")
  karel_env <- get_pkg_env()
  turn_around()
  expect_equal(karel_env$dir_now, 3)
})
