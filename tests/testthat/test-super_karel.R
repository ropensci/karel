test_that("Super Karel's actions perform as expected", {
  generar_mundo("mundo001")
  karel_env <- get_pkg_env()
  girar_derecha()
  expect_equal(karel_env$dir_now, 4)
  darse_vuelta()
  expect_equal(karel_env$dir_now, 2)
})

