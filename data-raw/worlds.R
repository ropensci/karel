
mundo001 <- list(
  nx = 6,
  ny = 4,
  hor_walls = dplyr::tibble(
    x = 3,
    y = 1,
    lgth = 3
  ),
  ver_walls = dplyr::tibble(
    x = 3,
    y = 0,
    lgth = 1
  ),
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = 2,
  beepers_y = 1,
  beepers_n = 1,
  beepers_bag = Inf
)

mundo002 <- list(
  nx = 6,
  ny = 4,
  hor_walls = dplyr::tibble(
    x = c(0, 2),
    y = c(1, 1),
    lgth = c(1, 4)
  ),
  ver_walls = dplyr::tibble(
    x = c(1, 2),
    y = c(0, 0),
    lgth = c(1, 1)
  ),
  karel_x = 1,
  karel_y = 2,
  karel_dir = 1,
  beepers_x = NULL,
  beepers_y = NULL,
  beepers_n = NULL,
  beepers_bag = Inf
)


mundo003 <- list(
  nx = 11,
  ny = 3,
  hor_walls = dplyr::tibble(
    x = seq(0, 10, 2),
    y = 1,
    lgth = 1
  ),
  ver_walls = dplyr::tibble(
    x = 1:10,
    y = 0,
    lgth = 1
  ),
  karel_x = 1,
  karel_y = 2,
  karel_dir = 1,
  beepers_x = NULL,
  beepers_y = NULL,
  beepers_n = NULL,
  beepers_bag = Inf
)

mundo004 <- list(
  nx = 11,
  ny = 3,
  hor_walls = dplyr::tibble(
    x = seq(0, 10, 2),
    y = 1,
    lgth = 1
  ),
  ver_walls = dplyr::tibble(
    x = 1:10,
    y = 0,
    lgth = 1
  ),
  karel_x = 1,
  karel_y = 2,
  karel_dir = 1,
  beepers_x = 6,
  beepers_y = 1,
  beepers_n = 1,
  beepers_bag = Inf
)

mundo005 <- list(
  nx = 10,
  ny = 10,
  hor_walls = NULL,
  ver_walls = NULL,
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = NULL,
  beepers_y = NULL,
  beepers_n = NULL,
  beepers_bag = Inf
)

mundo006 <- list(
  nx = 7,
  ny = 3,
  hor_walls = dplyr::tibble(
    x = c(0, 4, 6),
    y = 1,
    lgth = c(2, 1, 1)
  ),
  ver_walls = dplyr::tibble(
    x = c(2, 3, 4, 5, 6),
    y = 0,
    lgth = 1
  ),
  karel_x = 1,
  karel_y = 2,
  karel_dir = 1,
  beepers_x = 3,
  beepers_y = 1,
  beepers_n = 1,
  beepers_bag = Inf
)

mundo007 <- list(
  nx = 7,
  ny = 3,
  hor_walls = dplyr::tibble(
    x = c(0, 4),
    y = 1,
    lgth = c(2, 1)
  ),
  ver_walls = dplyr::tibble(
    x = c(2, 3, 4, 5, 6),
    y = 0,
    lgth = 1
  ),
  karel_x = 1,
  karel_y = 2,
  karel_dir = 1,
  beepers_x = 3,
  beepers_y = 1,
  beepers_n = 1,
  beepers_bag = Inf
)

mundo008 <- list(
  nx = 9,
  ny = 7,
  hor_walls = NULL,
  ver_walls = NULL,
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = rep(2:8, times = c(3, 5, 2, 3, 6, 0, 2)),
  beepers_y = c(1:3, 1:5, 1:2, 1:3, 1:6, 1:2),
  beepers_n = rep(1, 21),
  beepers_bag = 0
)

mundo009 <- list(
  nx = 6,
  ny = 6,
  hor_walls = dplyr::tibble(
    x = c(1, 3, 1, 4, 2, 1, 5, 0, 2, 4),
    y = c(1, 1, 2, 2, 3, 4, 4, 5, 5, 5),
    lgth = c(1, 1, 2, 1, 2, 1, 1, 1, 1, 1)
  ),
  ver_walls = dplyr::tibble(
    x = c(1, 4, 2, 5, 1, 4, 3, 5, 2, 5),
    y = c(0, 0, 1, 1, 2, 2, 3, 3, 4, 5),
    lgth = c(1, 1, 1, 1, 2, 3, 1, 1, 1, 1)
  ),
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = 6,
  beepers_y = 6,
  beepers_n = 1,
  beepers_bag = Inf
)

mundo010 <- list(
  nx = 5,
  ny = 3,
  hor_walls = NULL,
  ver_walls = NULL,
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = 2,
  beepers_y = 1,
  beepers_n = 4,
  beepers_bag = Inf
)

mundo011 <- list(
  nx = 7,
  ny = 5,
  hor_walls = dplyr::tibble(
    x = c(2, 2),
    y = c(1, 4),
    lgth = c(3, 3)
  ),
  ver_walls = dplyr::tibble(
    x = c(2, 5, 5),
    y = c(1, 1, 3),
    lgth = c(3, 1, 1)
  ),
  karel_x = 3,
  karel_y = 4,
  karel_dir = 1,
  beepers_x = 6,
  beepers_y = 3,
  beepers_n = 1,
  beepers_bag = Inf
)

mundo012 <- list(
  nx = 6,
  ny = 3,
  hor_walls = NULL,
  ver_walls = NULL,
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = NULL,
  beepers_y = NULL,
  beepers_n = NULL,
  beepers_bag = Inf
)

mundo013 <- list(
  nx = 1,
  ny = 1,
  hor_walls = NULL,
  ver_walls = NULL,
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = NULL,
  beepers_y = NULL,
  beepers_n = NULL,
  beepers_bag = Inf
)

mundo014 <- list(
  nx = 20,
  ny = 1,
  hor_walls = NULL,
  ver_walls = NULL,
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = NULL,
  beepers_y = NULL,
  beepers_n = NULL,
  beepers_bag = Inf
)

mundo015 <- list(
  nx = 9,
  ny = 10,
  hor_walls = NULL,
  ver_walls = dplyr::tibble(
    x = c(1, 3, 4, 5, 6, 8),
    y = rep(0, 6),
    lgth = c(2, 4, 1, 1, 9, 3)
  ),
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = NULL,
  beepers_y = NULL,
  beepers_n = NULL,
  beepers_bag = Inf
)

mundo016 <- list(
  nx = 9,
  ny = 6,
  hor_walls = NULL,
  ver_walls = dplyr::tibble(
    x = c(1:3, 6:8),
    y = rep(0, 6),
    lgth = c(1, 2, 1, 2, 5, 2)
  ),
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = NULL,
  beepers_y = NULL,
  beepers_n = NULL,
  beepers_bag = Inf
)

set.seed(2)
pos <-
  data.frame(
    x = sample(1:9, 25, replace = T),
    y = sample(1:9, 25, replace = T)
  )
pos <- dplyr::distinct(pos)

mundo017 <- list(
  nx = 9,
  ny = 9,
  hor_walls = NULL,
  ver_walls = NULL,
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = pos$x,
  beepers_y = pos$y,
  beepers_n = rep(1, nrow(pos)),
  beepers_bag = Inf
)

mundo018 <- list(
  nx = 6,
  ny = 4,
  hor_walls = NULL,
  ver_walls = NULL,
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = c(1, 1, 6, 6, 2, 4, 5, 2),
  beepers_y = c(1, 4, 1, 4, 4, 2, 2, 3),
  beepers_n = rep(1, 8),
  beepers_bag = Inf
)


mundo019 <- list(
  nx = 7,
  ny = 7,
  hor_walls = data.frame(
    x = rep(1:5, each = 2),
    y = c(3, 4, 2, 5, 1, 6, 2, 5, 3, 4),
    lgth = rep(1, 10)
  ),
  ver_walls = data.frame(
    x = c(1, rep(2:5, each = 2), 6),
    y = c(3, 2, 4, 1, 5, 1, 5, 2, 4, 3),
    lgth = rep(1, 10)
  ),
  karel_x = 2,
  karel_y = 4,
  karel_dir = 1,
  beepers_x = NULL,
  beepers_y = NULL,
  beepers_n = NULL,
  beepers_bag = Inf
)


mundo020 <- list(
  nx = 11,
  ny = 11,
  hor_walls = data.frame(
    x = rep(1:9, each = 2),
    y = c(5, 6, 4, 7, 3, 8, 2, 9, 1, 10, 9, 2, 8, 3, 7, 4, 6, 5),
    lgth = rep(1, 18)
  ),
  ver_walls = data.frame(
    x = c(1, rep(2:9, each = 2), 10),
    y = c(5, 4, 6, 3, 7, 2, 8, 1, 9, 1, 9, 2, 8, 3, 7, 4, 6, 5),
    lgth = rep(1, 18)
  ),
  karel_x = 2,
  karel_y = 6,
  karel_dir = 1,
  beepers_x = NULL,
  beepers_y = NULL,
  beepers_n = NULL,
  beepers_bag = Inf
)



mundo021 <- list(
  nx = 5,
  ny = 5,
  hor_walls = NULL,
  ver_walls = NULL,
  karel_x = 2,
  karel_y = 3,
  karel_dir = 1,
  beepers_x = 3,
  beepers_y = 3,
  beepers_n = 8,
  beepers_bag = Inf
)



mundo022 <- list(
  nx = 13,
  ny = 8,
  hor_walls = data.frame(
    x = 0:12,
    y = c(5:7, 6, 5:7, 6, 5:7, 6, 5),
    lgth = rep(1, 13)
  ),
  ver_walls = data.frame(
    x = 1:12,
    y = c(5, 6, 6, 5, 5, 6, 6, 5, 5, 6, 6, 5),
    lgth = rep(1, 12)
  ),
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = c(1, 1, 5, 5, 5, 9, 9, 13, 13, 13),
  beepers_y = c(4, 5, 1, 2, 4, 3, 5, 1, 3, 5),
  beepers_n = rep(1, 10),
  beepers_bag = Inf
)


mundo023 <- list(
  nx = 13,
  ny = 9,
  hor_walls = data.frame(
    x = 0:12,
    y = c(4, 5, 6, 5, 3, 7, 8, 7, 5, 6, 7, 6, 1),
    lgth = rep(1, 13)
  ),
  ver_walls = data.frame(
    x = 1:12,
    y = c(4, 5, 5, 3, 3, 7, 7, 5, 5, 6, 6, 1),
    lgth = c(1, 1, 1, 2, 4, 1, 1, 2, 1, 1, 1, 5)
  ),
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = c(1, 1, 5, 9, 9),
  beepers_y = c(1, 4, 3, 2, 4),
  beepers_n = rep(1, 5),
  beepers_bag = Inf
)

mundo024 <- list(
  nx = 1,
  ny = 10,
  hor_walls = data.frame(
    x = 0,
    y = 8,
    lgth = 1
  ),
  ver_walls =NULL,
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = c(1, 1),
  beepers_y = c(2, 3),
  beepers_n = c(1, 1),
  beepers_bag = Inf
)

mundo025 <- list(
  nx = 6,
  ny = 4,
  hor_walls = dplyr::tibble(
    x = c(3, 3),
    y = c(1, 2),
    lgth = c(3, 3)
  ),
  ver_walls = dplyr::tibble(x = 3,
                            y = 0,
                            lgth = 1),
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = c(3, 4),
  beepers_y = c(1, 1),
  beepers_n = 4:5,
  beepers_bag = Inf
)


mundo026 <- list(
  nx = 6,
  ny = 4,
  hor_walls = dplyr::tibble(
    x = 3,
    y = 1,
    lgth = 3
  ),
  ver_walls = dplyr::tibble(
    x = 3,
    y = 0,
    lgth = 1
  ),
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = NULL,
  beepers_y = NULL,
  beepers_n = NULL,
  beepers_bag = Inf
)

mundo027 <- list(
  nx = 6,
  ny = 4,
  hor_walls = dplyr::tibble(
    x = 3,
    y = 1,
    lgth = 3
  ),
  ver_walls = dplyr::tibble(
    x = 3,
    y = 0,
    lgth = 1
  ),
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = 2,
  beepers_y = 1,
  beepers_n = 1,
  beepers_bag = Inf
)

mundo028 <- list(
  nx = 6,
  ny = 4,
  hor_walls = dplyr::tibble(
    x = 3,
    y = 1,
    lgth = 3
  ),
  ver_walls = dplyr::tibble(
    x = 3,
    y = 0,
    lgth = 1
  ),
  karel_x = 1,
  karel_y = 1,
  karel_dir = 1,
  beepers_x = 2,
  beepers_y = 1,
  beepers_n = 2,
  beepers_bag = Inf
)


usethis::use_data(mundo001,
                  mundo002,
                  mundo003,
                  mundo004,
                  mundo005,
                  mundo006,
                  mundo007,
                  mundo008,
                  mundo009,
                  mundo010,
                  mundo011,
                  mundo012,
                  mundo013,
                  mundo014,
                  mundo015,
                  mundo016,
                  mundo017,
                  mundo018,
                  mundo019,
                  mundo020,
                  mundo021,
                  mundo022,
                  mundo023,
                  mundo024,
                  mundo025,
                  mundo026,
                  mundo027,
                  mundo028,
                  internal = TRUE, overwrite = TRUE)

# for (i in 1:9) {
#   assign(paste0("mundo_10", i), get(paste0("world_10", i)))
# }
