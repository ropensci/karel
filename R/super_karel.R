#' Title
#'
#' Esta funcion no se exporta, no esta disponible para el usuario
#'
#' @return
#'
#' @examples
funcioninterna <- function() 2*3


#' Title
#'
#' Con esta queda visible en el globa env la funcion llamada funcioninterna y anda
#'
#' @return
#'
#' @export
#' @examples
subirfcn1 <- function() {
  assign("funcioninterna1", funcioninterna, envir = .GlobalEnv)
}

#' Title
#'
#' Con esta queda en el globa env la funcion llamada .funcioninterna,
#' pero como tiene el punto inicial no se, anda, corriendo .funcioninterna()
#'
#' @return
#'
#' @export
#' @examples
subirfcn2 <- function() {
  assign(".funcioninterna", funcioninterna, envir = .GlobalEnv)
}

#' Title
#'
#' No logro otra opcion donde no se vea listada en global environmet y se pueda
#' usar sin que empiece con punto
#'
#' @return
#'
#' @export
#' @examples
subirfcn3 <- function() {
  # assign("funcioninterna", funcioninterna, envir = karel)

  # environment(funcioninterna) <- asNamespace('karel')
  # assignInNamespace("funcioninterna", funcioninterna, ns = "karel")
#
  # esto solo anda pero devuelve la fcn
  # getFromNamespace("funcioninterna", "karel")

  # esto no anda
  # assign(".funcioninterna", getFromNamespace("funcioninterna", "karel"))

  environment(funcioninterna) <- asNamespace('karel')
}

