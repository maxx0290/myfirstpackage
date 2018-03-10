
#' Question 1
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' func8(a,x)
func8 <- function(a,x)
{
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(x))

  stopifnot(is.finite(x))
  stopifnot(!is.na(x))
  stopifnot(!is.nan(x))
  stopifnot(is.finite(a))
  stopifnot(!is.na(a))
  stopifnot(!is.nan(a))

  stopifnot(nrow(a) == ncol(a))
  stopifnot(nrow(a) == length(x))

  a.inverse <- solve(a)
  t(x) %*% a.inverse %*% x
}


#' Question 2
#'
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p1.rda"))
#' different version
"%alice%"<-function(a,x){if(is.matrix(a) & is.numeric(a) & is.numeric(x)
                            & is.finite(x) & !is.na(x) & !is.nan(x) & is.finite(a)
                            & !is.na(a) & !is.nan(a) & nrow(a) == ncol(a) & nrow(a) == length(x))
{return(t(x) %*% (solve(a)) %*% x)};cat(a," ",x)}


#' Question 3 and 4
#'@param x vector
#'
#' @return list
#' @export
#' @examples
#' load(url("http://www.stat.umn.edu/geyer/3701/data/q2p3.rda"))
#' stand(A3)
A3<-matrix(rnorm(16),nc=4,nr=4)

stand <- function(A3)
{
  #error check
  if(nrow(A3) == 1)
  {
    stop("There is only one row!")
  }
  A3.mean <- apply(A3, 2, mean)
  A3.sd <- apply(A3, 2, sd)
  A3.stand <- ((A3 - A3.mean) / A3.sd)
  A3.stand
}



#' Question 5
#'@param x vector
#'
#' @return list
#' @export
#' @examples
#'fred <-matrix(1:6, ncol = 2)
#'myapply(fred,2,"quantile",prob = 0.75)
#'myapply(fred,1,"quantile",prob = (1:3)/4)
myapply <- function(X, MARGIN, FUN, ...)
{
  #stopifnot(length(dim(X)) ==2)

  if(length(dim(X)) !=2 )
  {
    stop("matrix is not 2D")
  }

  if(!(MARGIN %in% c(1,2)))
  {
    stop("margin is not i 1 or 2")
  }

  R = dim(X)[1]
  C = dim(X)[2]
  f = match.fun(FUN)

  if(MARGIN == 1)
  {
    result = list()
    for(i in 1:R)
    {
      result[[i]] = f(X[i,],...)
    }

  }else if(MARGIN == 2)
  {
    result = list()
    for(j in 1:C)
    {
      result[[j]] = f(X[,j],...)
    }
  }
  return(simplify2array(result))
}



