### <표4> 변수별 인과적 경로와 회귀계수의 관련성 경로
sim <- function(n = 1000, a = .4){
  IV <- rnorm(n, 0, 1)
  U1 <- rnorm(n, 0, 1)
  U2 <- rnorm(n, 0, 1)
  U3 <- rnorm(n, 0, 1)
  eX <- rnorm(n, 0, sqrt(1 - 3*a^2))
  eM <- rnorm(n, 0, sqrt(1 - 1*a^2))
  eC <- rnorm(n, 0, sqrt(1 - 2*a^2))
  eY <- rnorm(n, 0, sqrt(1 - 4*a^2 - 4*a^3 - 2*a^4))
  
  X <- a*IV + a*U1 + a*U3 + eX
  M <- a*X + eM
  C <- a*U1 + a*U2 + eC
  Y <- a*X + a*M + a*U3 + a*U2 + eY
  
  out <- lm(Y ~ IV + X + M + C)
  
  c(sd(IV), sd(X), sd(M), sd(C), sd(Y),
    coef(lm(Y ~ IV))['IV'],
    coef(lm(Y ~ X + U3))['X'],
    coef(lm(Y ~ M + X))['M'],
    coef(lm(Y ~ C + U1 + U2))['C'],
    coef(out)[-1])}
rslt <- replicate(100000, sim(n = 1000, a = .4), simplify = T)
round(apply(rslt, 1, mean), 3)
