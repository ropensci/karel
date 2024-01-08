test_that("it is correctly detected if Karel's front is opened or closed, with
          the English wrappers for testing the world's condition", {
            generar_mundo("mundo001")
            expect_true(front_is_clear())
            expect_false(front_is_blocked())
})

test_that("it is correctly detected if Karel's left is opened or closed, with
          the English wrappers for testing the world's condition", {
            generar_mundo("mundo001")
            expect_true(left_is_clear())
            expect_false(left_is_blocked())
          })

test_that("it is correctly detected if Karel's right is opened or closed, with
          the English wrappers for testing the world's condition", {
            generar_mundo("mundo001")
            expect_false(right_is_clear())
            expect_true(right_is_blocked())
          })

test_that("it is correctly detected if there are any beepers at Karel's
          position, with the English wrappers for testing the world's
          condition", {
            generar_mundo("mundo001")
            expect_false(beepers_present())
            expect_true(no_beepers_present())
          })

test_that("it is correctly detected if there are any beepers in Karel's bag,
          with the English wrappers for testing the world's condition", {
            generar_mundo("mundo001")
            expect_true(karel_has_beepers())
            expect_false(karel_has_no_beepers())
          })

test_that("it is correctly detected to which direction Karel is facing, with the
          English wrappers for testing the world's condition", {
            generar_mundo("mundo001")
            expect_true(facing_east())
            expect_false(facing_west())
            expect_false(facing_north())
            expect_false(facing_south())
          })
