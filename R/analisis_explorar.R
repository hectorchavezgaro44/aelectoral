

#' Title
#' Resultado de las diferencias entre elecciones, eligiendo una como contraste
#'
#' @param bd base de datos con resultados electorales
#' @param partido partidos o vector de partidos (ver base de datos de partidos)
#' @param eleccion_referencia elección con la que se contrastarán las elecciones.
#' @param eleccion_contraste elección o vector de elecciones contra los que se comparan
#' la elecciones de referencia.
#'
#' @return base con una columna de diferencias y  con el prefijo "dif_"
#' @export
#' @import dplyr purrr
#' @examples
#' #NOT RUN
#' #calcular_diferencias(edomex::edomex_final, partido = c("pvem", "pt"),
#' #eleccion_referencia =  "df_21", eleccion_contraste = c("dl_21", "pm_21"))

calcular_diferencias <- function(bd, partido, eleccion_referencia, eleccion_contraste){
  res <- map(partido,
             ~   {
               eleccion_referencia <- paste("ele", .x, eleccion_referencia, sep = "_")
               eleccion_contraste <- paste("ele", .x, eleccion_contraste, sep="_")
               bd <- map(eleccion_contraste,
                         ~ {
                           bd %>%
                             mutate("dif_{stringr::str_remove(eleccion_referencia, 'ele_')}_{stringr::str_sub(string = .x, start = -5, end = -1)}":=!!sym(eleccion_referencia)-!!sym(.x))
                         }
               ) %>%
                reduce(full_join)
               return(bd)

             })%>%
    reduce(full_join) %>%
    as_tibble()
  return(res)
}



#' Title
#' Obtiene el porcentaje de votos obtenidos por partidos con respecto a la lista nominal
#'
#' @param bd base de datos con resultados electorales
#' @param partido partidos o vector de partidos (ver base de datos de partidos)
#' @param eleccion elección o vector de elecciones de la cual se van a obtener los totales
#' @param grupo unidad de análisis que se desea observar (sección, distrito, municipio)
#'
#' @return base de datos con cada una de las votaciones totales por partido en las elecciones solicitadas
#' @export
#' @import dplyr purrr
#' @examples
calcular_votos_relativos <- function(bd, partido, eleccion, grupo){
  res  <- map(eleccion,
              ~{
                sufijo <- paste("ele",partido, .x, sep = "_")
                nominal <-  paste("ele_nominal", .x, sep = "_")
                bd %>% group_by({{grupo}}) %>%
                  summarise(across(sufijo,
                                   ~sum(.x, na.rm = T)/sum(!!sym(nominal),
                                                           na.rm=T))) %>%
                  filter(!is.na({{grupo}}))
              }) %>% reduce(full_join)
  return(res)
}


#' Title
#'
#' @param bd base de datos con resultados electorales
#' @param partido partidos o vector de partidos (ver base de datos de partidos)
#' @param eleccion elección o vector de elecciones de la cual se van a obtener los totales
#' @param grupo
#'
#' @return base de datos con cada una de las votaciones totales por partido en las elecciones solicitadas
#' @export
#' @import dplyr purrr
#' @examples
calcular_votos_totales <- function(bd, partido, eleccion, grupo=NULL){
  res <- bd %>% {if(!is_null(grupo)) bd %>% group_by({{grupo}}) else .} %>%
    summarise(across(matches(cross(list(partido, eleccion)) %>%
                               map_chr(.f = ~.x %>% unlist() %>% paste(collapse="_")) %>%
                               paste("ele",., sep="_")),
                     ~sum(.x, na.rm = T)))

  return(res)
}

