## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(WangzhenTools)

## ------------------------------------------------------------------------
data(d)
plot1(d$x,d$p)
plot2(d$x,d$p)
plot3(d$x,d$p)

## ------------------------------------------------------------------------
func1(rnorm(10))
func2(rnorm(10))
func3(rnorm(10))
data(d)
func4(d)
d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
func5(d)
func6(NA)
x1 <- rgamma(100,3)
func1 = function(theta, x1) dgamma(x1, shape = theta, log = TRUE)
result7_gamma <- func7(x1,func1,c(0,3))
result7_gamma
#'

