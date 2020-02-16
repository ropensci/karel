context("Actions")
library(karel)

test_that("Karels actions perform as expected", {
  generar_mundo("world_101")
  karel_env <- get_pkg_env()
  avanzar()
  juntar_coso()
  girar_izquierda()
  avanzar()
  poner_coso()
  girar_derecha()
  darse_vuelta()
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})

