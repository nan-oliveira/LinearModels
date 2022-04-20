
set.seed(21)

n <- 50
x1 <- rnorm(n, 20, 5)
x2 <- rgamma(n, 10, 9)
x3 <- runif(n, 1, 60)
x4 <- rbinom(n, 20, 0.6)
x5 <- rpois(n, 18)
sigma2 <- 30
erro <- rnorm(n, 0, sqrt(sigma2))

y <- 10 + 3.97 * x1 - 9 * x2 + 4.04 * x3 + 1.2 * x4 + 7 * x5 + erro

matrixX <- function(x1, x2, x3, x4, x5) {
  out <- cbind(1, x1, x2, x3, x4, x5)
  colnames(out) <- c("Intercept", paste0("x", 1:5))
  return(out)
}

estbeta <- function(X, y) solve(t(X) %*% X) %*% t(X) %*% y

X <- matrixX(x1, x2, x3, x4, x5)
estbeta(X, y)

estbetaRestr <- function(X, y, A, c) {
  beta_hat <- estbeta(X, y )
  
  beta_hat_h <- beta_hat + 
    solve(t(X) %*% X) %*% t(A) %*% 
    solve(A %*% solve(t(X) %*% X) %*% t(A)) %*% 
    (c - A %*% beta_hat)
  
}
