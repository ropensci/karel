test_that("it is correctly detected if Karel's front is opened or closed, with
          the Spanish wrappers for testing the world's condition", {
            generar_mundo("mundo001")
            expect_true(frente_abierto())
            expect_false(frente_cerrado())
})

test_that("it is correctly detected if Karel's left is opened or closed, with
          the Spanish wrappers for testing the world's condition", {
            generar_mundo("mundo001")
            expect_true(izquierda_abierto())
            expect_false(izquierda_cerrado())
          })

test_that("it is correctly detected if Karel's right is opened or closed, with
          the Spanish wrappers for testing the world's condition", {
            generar_mundo("mundo001")
            expect_false(derecha_abierto())
            expect_true(derecha_cerrado())
          })

test_that("it is correctly detected if there are any beepers at Karel's
          position, with the Spanish wrappers for testing the world's
          condition", {
            generar_mundo("mundo001")
            expect_false(hay_cosos())
            expect_true(no_hay_cosos())
          })

test_that("it is correctly detected if there are any beepers in Karel's bag,
          with the Spanish wrappers for testing the world's condition", {
            generar_mundo("mundo001")
            expect_true(karel_tiene_cosos())
            expect_false(karel_no_tiene_cosos())
          })

test_that("it is correctly detected to which direction Karel is facing, with the
          Spanish wrappers for testing the world's condition", {
            generar_mundo("mundo001")
            expect_true(mira_al_este())
            expect_false(mira_al_oeste())
            expect_false(mira_al_norte())
            expect_false(mira_al_sur())
          })
