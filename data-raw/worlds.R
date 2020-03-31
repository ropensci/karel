world_001 <- list(
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

# No beepers in the beginning
world_002 <- list(
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

# Beepers in one place in the beginning
world_003 <- list(
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

# Two  Beepers in one places in the beggining
world_004 <- list(
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

world_101 <- list(
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

world_102 <- list(
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


world_103 <- list(
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

world_104 <- list(
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

world_105 <- list(
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

world_106a <- list(
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

world_106b <- list(
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

world_107 <- list(
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

world_108 <- list(
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

world_109 <- list(
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

problema_1_1 <- list(
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

problema_1_3a <- list(
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

problema_1_3b <- list(
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

problema_1_3c <- list(
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

problema_1_4a <- list(
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

problema_1_4b <- list(
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

problema_1_5a <- list(
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

problema_1_5b <- list(
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

usethis::use_data(world_101,
                  world_102,
                  world_103,
                  world_104,
                  world_105,
                  world_106a,
                  world_106b,
                  world_107,
                  world_108,
                  world_109,
                  problema_1_1,
                  problema_1_3a,
                  problema_1_3b,
                  problema_1_3c,
                  problema_1_4a,
                  problema_1_4b,
                  problema_1_5a,
                  problema_1_5b,
                  internal = TRUE, overwrite = TRUE)

# for (i in 1:9) {
#   assign(paste0("mundo_10", i), get(paste0("world_10", i)))
# }
