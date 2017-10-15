#' Draw the development of multiple first names in France since 1900 using dygraph
#'
#' @param name_vector vector of names to be drawn
#'
#' @import prenoms
#' @import dplyr
#' @import dygraphs
#' @import tidyr
#' @import assertthat
#'
#' @return dygraph plot of the names
#' @export
#'
#' @examples
#' draw_names_dygraph(c("Diane","Vincent"))

draw_names_dygraph <- function(name_vector){
  assert_that(is.character(name_vector))

  tmp <- prenoms::prenoms %>%
    filter(name %in% name_vector) %>%
    group_by(year,name) %>%
    summarize(n=sum(n)) %>%
    spread(key=name,value=n) %>%
    dygraph()

  return(tmp)
}
