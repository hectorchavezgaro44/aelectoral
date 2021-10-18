

#' Title
#' Resultado de las diferencias entre elecciones, eligiendo una como contraste
#'
#' @param bd base de datos electoral
#' @param partido partidos o vector de partidos (ver base de datos de partidos)
#' @param eleccion_referencia elección con la que se contrastarán las elecciones.
#' @param eleccion_contraste elección o vector de elecciones contra los que se comparan
#' la elecciones de referencia.
#'
#' @return base con diferencias
#' @export
#' @import dplyr magrittr
#' @examples

calcular_diferencias <- function(bd, partido, eleccion_referencia, eleccion_contraste){
  res <- purrr::map(partido,
             ~   {
               eleccion_referencia <- paste("ele", .x, eleccion_referencia, sep = "_")
               eleccion_contraste <- paste("ele", .x, eleccion_contraste, sep="_")
               bd <- purrr::map(eleccion_contraste,
                         ~ {
                           bd %>%
                             mutate("dif_{stringr::str_remove(eleccion_referencia, 'ele_')}_{stringr::str_sub(string = .x, start = -5, end = -1)}":=!!sym(eleccion_referencia)-!!sym(.x))
                         }
               ) %>%
                 purrr::reduce(full_join)
               return(bd)

             })%>%
    purrr::reduce(full_join) %>%
    as_tibble()
  return(res)
}
