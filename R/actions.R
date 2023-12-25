# Global list for messages. Each of its elements is a list with all the messages
# written in a differente language. There's a file for each language that adds
# to this list. The files are called "message_texts_*.R".
message_texts <- list()

#' Implementation of the action of moving forward
#'
#' @keywords internal
#'
.move <- function(lang) {
  # Proceed if there was no mistake
  if (pkg_env$error) {
    cli::cli_rule()
    cli::cli_abort(call = NULL, message = c(
      "x" = message_texts[[lang]]$error_general,
      ">" = message_texts[[lang]]$start_again)
    )
  }
  pkg_env$moment <- pkg_env$moment + 1
  switch(pkg_env$dir_now,

         # Current direction: east
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 1]) {
           pkg_env$x_now <- pkg_env$x_now + 1
         } else {
           pkg_env$error <- TRUE
           cli::cli_rule()
           cli::cli_abort(call = NULL, message = c(
             "x" = message_texts[[lang]]$cant_move_east,
             ">" = message_texts[[lang]]$start_again)
           )
         },

         # Current direction: north
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 2]) {
           pkg_env$y_now <- pkg_env$y_now + 1
         } else {
           pkg_env$error <- TRUE
           cli::cli_rule()
           cli::cli_abort(call = NULL, message = c(
             "x" = message_texts[[lang]]$cant_move_north,
             ">" = message_texts[[lang]]$start_again)
           )
         },

         # Current direction: west
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 3]) {
           pkg_env$x_now <- pkg_env$x_now - 1
         } else {
           pkg_env$error <- TRUE
           cli::cli_rule()
           cli::cli_abort(call = NULL, message = c(
             "x" = message_texts[[lang]]$cant_move_west,
             ">" = message_texts[[lang]]$start_again)
           )
         },

         # Current direction: south
         if (pkg_env$open_moves[pkg_env$x_now, pkg_env$y_now, 4]) {
           pkg_env$y_now <- pkg_env$y_now - 1
         } else {
           pkg_env$error <- TRUE
           cli::cli_rule()
           cli::cli_abort(call = NULL, message = c(
             "x" = message_texts[[lang]]$cant_move_south,
             ">" = message_texts[[lang]]$start_again)
           )
         }
  )
  pkg_env$karel <- add_row(pkg_env$karel, x = pkg_env$x_now, y = pkg_env$y_now,
                           direction = pkg_env$dir_now, moment = pkg_env$moment)

  # Even though moving doesn't change the amount of beepers, for the animation I
  # need to declare that in this new moment, beepers remain the same.
  pkg_env$beepers_now <- mutate(pkg_env$beepers_now, moment = moment + 1)
  pkg_env$beepers_all <- bind_rows(pkg_env$beepers_all, pkg_env$beepers_now)
}

#' Implementation of the action of turning left
#'
#' @keywords internal
#'
.turn_left <- function(lang) {
  # Proceed if there was no mistake
  if (pkg_env$error) {
    cli::cli_rule()
    cli::cli_abort(call = NULL, message = c(
      "x" = message_texts[[lang]]$error_general,
      ">" = message_texts[[lang]]$start_again)
    )
  }
  # Update moment and direction
  pkg_env$moment <- pkg_env$moment + 1
  pkg_env$dir_now <- switch(pkg_env$dir_now, 2, 3, 4, 1)

  # Update karel data set (only changes dir from last row)
  pkg_env$karel <- add_row(pkg_env$karel, x = pkg_env$x_now, y = pkg_env$y_now,
                           direction = pkg_env$dir_now, moment = pkg_env$moment)

  # Update beepers dataset, they remain the same but need to reflect this new
  # moment
  pkg_env$beepers_now$moment <- pkg_env$beepers_now$moment + 1
  pkg_env$beepers_all <- bind_rows(pkg_env$beepers_all, pkg_env$beepers_now)
}

#' Implementation of the action of putting a beeper
#'
#' @keywords internal
#'
.put_beeper <- function(lang) {

  # Proceed if there was no mistake
  if (pkg_env$error) {
    cli::cli_rule()
    cli::cli_abort(call = NULL, message = c(
      "x" = message_texts[[lang]]$error_general,
      ">" = message_texts[[lang]]$start_again)
    )
  }

  if (pkg_env$beepers_bag == 0) {
    pkg_env$error <- TRUE
    cli::cli_rule()
    cli::cli_abort(call = NULL, message = c(
      "x" = message_texts[[lang]]$cant_put_beeper,
      ">" = message_texts[[lang]]$start_again)
    )
  } else {

    # Update bag
    pkg_env$beepers_bag <- pkg_env$beepers_bag - 1
    # Update moment
    pkg_env$moment <- pkg_env$moment + 1

    if (pkg_env$beepers_any == 0) {
      # There are no beepers in the world now, create again the beepers_now
      # dataset
      pkg_env$beepers_now <- tibble(
        x = pkg_env$x_now,
        y = pkg_env$y_now,
        cell = pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx,
        n = 1,
        moment = pkg_env$moment
      )
    } else {
      pkg_env$beepers_now$moment <- pkg_env$moment
      # There are beepers, but I have to see if there are already here or not
      if (hay_cosos()) {
        # If there are beepers, add one more
        idx <- get_beepers_df_row()
        pkg_env$beepers_now$n[idx] <- pkg_env$beepers_now$n[idx] + 1
      } else {
        # If there arent any, add new row with one beeper to beepers dataset
        pkg_env$beepers_now <- add_row(
          pkg_env$beepers_now,
          x = pkg_env$x_now, y = pkg_env$y_now,
          cell = pkg_env$x_now + pkg_env$nx * pkg_env$y_now - pkg_env$nx,
          n = 1, moment = pkg_env$moment
        )
      }
    }
    pkg_env$beepers_any <- pkg_env$beepers_any + 1

    # Append this new state of beepers to beepers_all
    pkg_env$beepers_all <- bind_rows(pkg_env$beepers_all, pkg_env$beepers_now)

    # Even though putting beepers doesn't change karel position or direction I
    # need to add a row to karel dataset for the animation with this new moment
    pkg_env$karel <-
      pkg_env$karel %>%
      slice(n()) %>%
      mutate(moment = moment + 1) %>%
      bind_rows(pkg_env$karel, .)
  }
}

#' Implementation of the action of picking a beeper
#'
#' @keywords internal
#'
.pick_beeper <- function(lang) {

  # Proceed if there was no mistake
  if (pkg_env$error) {
    cli::cli_rule()
    cli::cli_abort(call = NULL, message = c(
      "x" = message_texts[[lang]]$error_general,
      ">" = message_texts[[lang]]$start_again)
    )
  }

  # We can only remove if there are no beepers there, otherwise it's an error
  if (hay_cosos()) {

    # Update beepers count
    pkg_env$beepers_any <- pkg_env$beepers_any - 1
    pkg_env$beepers_bag <- pkg_env$beepers_bag + 1

    # Update moment
    pkg_env$moment <- pkg_env$moment + 1
    pkg_env$beepers_now$moment <- pkg_env$moment

    # Get cell index and see if there are any beepers already there
    idx <- get_beepers_df_row()

    # Remove beeper
    pkg_env$beepers_now$n[idx] <- pkg_env$beepers_now$n[idx] - 1

    # Remove row from beepers dataset it there are no beepers left, so the box
    # disappears
    if (pkg_env$beepers_now$n[idx] == 0) {
      pkg_env$beepers_now <- dplyr::slice(pkg_env$beepers_now, -idx)
      if (nrow(pkg_env$beepers_now) == 0) {
        # This means that in this moment there were no beepers in the world
        # In order the plot to work, I will create a dataset with NA values
        pkg_env$beepers_now[1, ] <- list(NA, NA, NA, NA, pkg_env$moment)
      }
    }

    # Append this new state of beepers to beepers_all
    pkg_env$beepers_all <- bind_rows(pkg_env$beepers_all, pkg_env$beepers_now)

    # Even though removing beepers doesn't change karel position or direction I
    # need to add a row to karel dataset for the animation with this new moment
    pkg_env$karel <-
      pkg_env$karel %>%
      slice(n()) %>%
      mutate(moment = moment + 1) %>%
      bind_rows(pkg_env$karel, .)
  } else {
    pkg_env$error <- TRUE
    cli::cli_rule()
    cli::cli_abort(call = NULL, message = c(
      "x" = message_texts[[lang]]$cant_pick_beeper,
      ">" = message_texts[[lang]]$start_again)
    )
  }
}
