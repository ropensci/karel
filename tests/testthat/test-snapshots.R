# Testing interactivity with vdiffr

# When a person using this package writes code to make Karel solve a problem,
# the result will be, provided everything goes well, a gif that shows Karel
# executing the programmed actions. The gif is created internally with gganimate
# from many ggplot2 plots. The package offers a function to access any of these
# graphs (plot_static_world), which I use to create examples and statements for
# new exercises, as well for debugging (it's not ment for students). The new
# units for testing interactivity execute pieces of code (simple or long) for
# Karel to solve various problems (taken from tutorials on the web) and use
# plot_static_world() to capture the graph that shows what Karel's world should
# be like at the end of each process. If the package works correctly and nothing
# has broken, these tests should indicate agreement between the captured image
# and the snapshot saved as reference.


# Simple test
test_that("basic generation of Karel's world works as expected
          and the world is displayed as a ggplot graphic", {

  vdiffr::expect_doppelganger("basic world is shown", generar_mundo("mundo001"))

})

# Longer test which uses several functions from the package
test_that("Karel can find her away to the exit in the maze problem", {

  generate_world("mundo009")
  while (no_beepers_present()) {
    turn_right()
    while (frente_cerrado()) {
      turn_left()
    }
    move()
  }
  final_plot <- plot_static_world(pkg_env$moment)

  vdiffr::expect_doppelganger("karel found the exit at top right corner",
                              final_plot)

})

# Even longer test to solve a problem which includes the definition of new
# functions
test_that("Karel solves the 'collect columns of beepers' problem", {

  collect_all <- function() {
    while (front_is_clear()) {
      collect_one_column()
      move()
    }
    collect_one_column()
  }

  collect_one_column <- function() {
    turn_left()
    collect_line()
    turn_around()
    move_forward_until_wall()
    turn_left()
  }

  return_to_start <- function() {
    turn_around()
    move_forward_until_wall()
    turn_around()
  }

  collect_line <- function() {
    while (beepers_present()) {
      pick_beeper()
      if (front_is_clear()) {
        move()
      }
    }
  }

  move_forward_until_wall <- function() {
    while (front_is_clear()) {
      move()
    }
  }

  put_all <- function() {
    while (karel_has_beepers()) {
      put_beeper()
    }
  }

  generate_world("mundo008")
  collect_all()
  put_all()
  return_to_start()
  final_plot <- plot_static_world(pkg_env$moment)

  vdiffr::expect_doppelganger("karel collect all the columns of beepers",
                              final_plot)

})

# Another interactivity test: solve Problem 6 (diamond)

test_that("Karel solves the 'collect columns of beepers' problem", {

  recorrer_diagonal <- function() {
    while (frente_abierto()) {
      avanzar()
      girar_izquierda()
      avanzar()
      girar_derecha()
    }
  }

  generar_mundo("mundo019")

  for (i in 1:4) {
    recorrer_diagonal()
    poner_coso()
    girar_derecha()
  }

  final_plot <- plot_static_world(pkg_env$moment)

  vdiffr::expect_doppelganger(
    "karel has put a beeper in each vertix of the diamond", final_plot)

})
