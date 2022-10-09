### [그림2] 통제변수 구성에 따른 회귀계수 추정치 
sim <- function(n = 1000, a = .4){
  X1 <- rnorm(n, 0, 1)
  U1 <- rnorm(n, 0, 1)
  U2 <- rnorm(n, 0, 1)
  U3 <- rnorm(n, 0, 1)
  e2 <- rnorm(n, 0, sqrt(1 - 3*a^2))
  e3 <- rnorm(n, 0, sqrt(1 - 1*a^2))
  e4 <- rnorm(n, 0, sqrt(1 - 2*a^2))
  eY <- rnorm(n, 0, sqrt(1 - 4*a^2 - 4*a^3 - 2*a^4))
  
  X2 <- a*X1 + a*U1 + a*U3 + e2
  X3 <- a*X2 + e3
  X4 <- a*U1 + a*U2 + e4
  Y <- a*X2 + a*X3 + a*U3 + a*U2 + eY
  
  out <- lm(Y ~ X1 + X2 + X3 + X4)
  
  c(coef(lm(Y ~ X1))['X1'],
    coef(lm(Y ~ X1 + X2))['X1'],
    coef(lm(Y ~ X1 + X3))['X1'],
    coef(lm(Y ~ X1 + X4))['X1'],
    coef(lm(Y ~ X1 + X2 + X3))['X1'],
    coef(lm(Y ~ X1 + X2 + X4))['X1'],
    coef(lm(Y ~ X1 + X3 + X4))['X1'],
    coef(lm(Y ~ X1 + X2 + X3 + X4))['X1'])}
rslt <- replicate(100000, sim(a = .4, n = 1000), simplify = T)
m.t <- apply(rslt, 1, mean)
s.t <- apply(rslt, 1, sd)