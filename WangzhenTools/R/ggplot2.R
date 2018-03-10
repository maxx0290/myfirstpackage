#' Wrapper for ggplot2 for data d
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plot1(d$x,d$p)

plot1<-function(x,p){
  devtools::use_package("magrittr")
  ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
}


#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plot2(d$x,d$p)

plot2<-function(x,p){
  devtools::use_package("magrittr")
  #devtools::use_package("ggplot2")
  ggplot2::ggplot(mapping = ggplot2::aes(x = x,y = p)) + ggplot2::geom_point(alpha = 0.2)
}

#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plot3(d$x,d$p)
#'
plot3<-function(x,p){
  devtools::use_package("magrittr")
  ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = p)) + ggplot2::geom_jitter(width = 30, height = 30)
}
