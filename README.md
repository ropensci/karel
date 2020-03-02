
<!-- README.md is generated from README.Rmd. Please edit that file -->

**NOTE: Work in progress**

**NOTA: En
construcción**

# La robot Karel enseña R <img src="man/figures/karel_hexsticker.png" align="right" alt="" width="160" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/karel)](https://CRAN.R-project.org/package=karel)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Downloads](https://cranlogs.r-pkg.org/badges/karel?color=blue)](https://cran.rstudio.com/package=karel)
<!-- badges: end -->

The goal of karel is to …

Página web: <https://mpru.github.io/karel/>

## Instalación

<!-- 
You can install the released version of karel from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("karel")
```
-->

Se puede instalar la versión en desarrollo del paquete `karel` desde
[GitHub](https://github.com/mpru/karel) con:

``` r
# install.packages("devtools")
devtools::install_github("mpru/karel")
```

## Ejemplo

Mientras construyo esta página, podés ir viendo un mini ejemplo. En las
viñetas hay algo más, podés visitarlas.

``` r
library(karel)
cargar_super_karel()

# Crear un mundo para Karel
generar_mundo("world_106")
```

<img src="man/figures/README-example-1.png" width="60%" style="display: block; margin: auto;" />

``` r

# Crear mi propia función
llenar_agujero <- function() {
  girar_derecha()
  avanzar()
  if (no_hay_cosos()) {
    poner_coso()
  }
  darse_vuelta()
  avanzar()
  girar_derecha()
}

# Usarla para que Karel pueda llenar todos los agujeros
while (frente_abierto()) {
  if (derecha_abierto()) {
    llenar_agujero()
  }
  avanzar()
}
if (derecha_abierto()) {
  llenar_agujero()
}
ejecutar_acciones()
```

<img src="man/figures/README-example-1.gif" width="60%" style="display: block; margin: auto;" />

``` r
# Laberinto
generar_mundo("world_108")
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="60%" style="display: block; margin: auto;" />

``` r
while (no_hay_cosos()) {
  girar_derecha()
  while (frente_cerrado()) {
    girar_izquierda()
  }
  avanzar()
}
ejecutar_acciones()
```

<img src="man/figures/README-unnamed-chunk-2-1.gif" width="60%" style="display: block; margin: auto;" />
