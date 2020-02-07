world_001 <- list(
    nx = 6,
    ny = 4,
    hor_walls = dplyr::tibble(
        x = c(3, 3),
        y = c(1, 2),
        lgth = c(3, 3)
    ),
    ver_walls = dplyr::tibble(
        x = 3,
        y = 0,
        lgth = 1
    ),
    karel_x = 1,
    karel_y = 1,
    karel_dir = 1,
    beepers_x = c(3, 4),
    beepers_y = c(1, 1),
    beepers_n = 4:5,
    beepers_bag = Inf
)

usethis::use_data(world_001, internal = TRUE, overwrite = TRUE)
