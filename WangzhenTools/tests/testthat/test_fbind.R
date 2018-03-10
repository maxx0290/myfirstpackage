
context("Binding factors")

           test_that("fbind binds factor (or character)", {
           x <- c('a', 'b')
           x_fact <- factor(x)
           y <- c('c', 'd')
           z <- factor(c('a', 'b', 'c', 'd'))

           expect_identical(fbind(x, y), z)
           expect_identical(fbind(x_fact, y), z)
           })

           context("Homework functions")

           test_that("func1 computes mean, var, sd", {
             x <- 1:10
             var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
             x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
             expect_identical(func1(x), x_list)
           })

           test_that("func2 computes mean, var, sd", {
             x <- 1:10
             var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
             x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
             expect_identical(func2(x), x_list)
             save<-try(func2(NA),silent=TRUE)
             expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
           })

