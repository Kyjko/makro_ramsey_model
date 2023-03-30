library(tidyverse)
library(ggplot2)

T <- 75

a <- 0.33000
b <- 0.95000
d <- 0.10000
A <- 1
b0 <- 0

Y <- function(K, L) {
  return(A*K**a*L**(1-a))
}

MYk <- function(K, L) {
  return(A*a*K**(a-1)*L**(1-a))
}

InvY <- function(X, L) {
  return((X/(A*L**(1-a)))**(1/a))
}

InvMYk <- function(X, L) {
  return((X/(a*A*L**(1-a)))**(1/(a-1)))
}

###### perturbs ####
e_k <- -0.000
e_c <- 0.000

###### !!!!!!! #####

k <- c()
c <- c()
#k[1] <- 3.1608602 + e_k
#c[1] <- 0.632006
#c[1] <- 1.1458748 + e_c
k[1] <- 1.000000
c[1] <- 0.63200640655
#c[1] <- 0.5

for(i in 1:T) {
  k[i+1] <- k[i] + Y(k[i],1) - c[i] - d*k[i]
  c[i+1] <- b*(1 + MYk(k[i+1],1) - d) * c[i]
}

k_star <- InvMYk(d + 1/b - 1,1)
c_star <- Y(k_star,1) - d*k_star

bc <- c()
j <- 1
for(i in seq(0, k_star*10, 0.01)) {
  bc[j] <- Y(i,1) - d*i
  j <- j + 1
}

dbc <- data.frame(cbind(seq(0, k_star*10, 0.01), bc))
colnames(dbc) <- c("k", "bc")


d <- data.frame(cbind(k, c))
colnames(d) <- c("k", "c")

ggplot(d) + geom_point(aes(x=k,y=c),color="red") + 
  geom_vline(mapping=aes(xintercept=k_star),color="black") + 
  geom_line(data=dbc, mapping=aes(x=k,y=bc),color="black")

# konvergencia check
tail(k, 20)
tail(c, 20)

