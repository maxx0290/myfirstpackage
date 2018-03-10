#' Function using dplyr
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' data(farmData)
#' myselect(farmData, names(farmData)[3])

library("magrittr")
myselect <- function(x, choice){
  x%>%
    dplyr::select(dplyr::starts_with(choice)) %>%
    head()
}
