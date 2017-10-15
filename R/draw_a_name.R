#' Draw the development of first names in France since 1900
#'
#' @param the_name a character
#' @param the_sex a character
#'
#' @import prenoms
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @import assertthat
#'
#' @return The ggplot of the name
#' @export
#'
#' @examples
#' draw_a_name("Vincent", "M")
#'

draw_a_name <- function(the_name, the_sex){
  assert_that(the_sex %in% c("M", "F"))
  assert_that(is.string(the_name))

  tmp <- prenoms::prenoms %>%
    filter(sex==the_sex, name==the_name) %>%
    group_by(year,sex,name) %>%
    summarize(n=sum(n)) %>%
    ggplot(aes(x=year,y=n)) +
      geom_line() +
      labs(title=the_name) +
      theme_gdocs()
  return(tmp)
}
