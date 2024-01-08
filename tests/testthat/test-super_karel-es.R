test_that("Karel actually turns right when the Spanish wrapper girar_derecha()
          is called", {
  generar_mundo("mundo001")
  karel_env <- get_pkg_env()
  girar_derecha()
  expect_equal(karel_env$dir_now, 4)
})

test_that("Karel actually turns arond when the Spanish wrapper darse_vuelta()
          is called", {
  generar_mundo("mundo001")
  karel_env <- get_pkg_env()
  darse_vuelta()
  expect_equal(karel_env$dir_now, 3)
})
