library(mice)

n <- 50
set.seed(2020)
x1 <- sample(0:1,n, replace=TRUE, )
x2 <- rnorm(n, x1*3 + 2, 6)
x3 <- 6 + 10*x2 + rnorm(n,0,5)

rat <- data.frame(x1, x2, x3)

mod <- lm(x3 ~x1 + x2 , data = rat)
summary(mod)


#Create missingness.
rat$x1[runif(n) < 0.2] <- NA
rat$x2[rat$x3 > 50 & runif(n) < .25] <- NA

#Visualizing Missing data.  
library(VIM)
aggr(rat)
matrixplot(rat)
matrixplot(rat[order(rat$x1),])


#Running MICE
m = 5
mouse_droppings <- mice(rat, m = m, method = c("cart","cart","cart"))

#Pull out a completer data set
mickey <- list()
for (i in 1:m){
mickey[[i]] <- complete(mouse_droppings,i)
}

mod_imp <- list()
for (i in 1:m){
mod_imp[[i]] <- summary(lm( x3 ~ x2 + x1, mickey[[i]]))$coefficients[,1:2]
}





#Now not by hand
some_imps <- with(mouse_droppings, exp = lm(x3 ~ x1 + x2))


pool(some_imps)












