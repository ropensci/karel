test_that("Karel actually moves when the Spanish wrapper avanzar() is called", {
  generar_mundo("mundo001")
  karel_env <- get_pkg_env()
  avanzar()
  expect_equal(karel_env$x_now, 2)
})

test_that("Karel actually picks a beeper when the Spanish wrapper juntar_coso()
          is called", {
  generar_mundo("mundo001")
  karel_env <- get_pkg_env()
  avanzar()
  juntar_coso()
  expect_equal(karel_env$beepers_any, 0)
})

test_that("Karel actually turns left when the Spanish wrapper girar_izquierda()
          is called", {
  generar_mundo("mundo001")
  karel_env <- get_pkg_env()
  girar_izquierda()
  expect_equal(karel_env$dir_now, 2)
})

test_that("Karel actually puts a beeper when the Spanish wrapper poner_coso()
          is called", {
  generar_mundo("mundo001")
  karel_env <- get_pkg_env()
  poner_coso()
  expect_equal(karel_env$beepers_any, 2)
  # there was one beeper and now there are 2
})


