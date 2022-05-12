
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


matrixX <- function(...) {
  out <- cbind(1, ...)
  n_var <- ncol(out) - 1
  colnames(out) <- c("Intercept", paste0("x", seq(1, n_var)))
  return(out)
}


matrixX(c(1,2), c(4,5))


funcShaded <- function(x, estF = 0.0978698) {
  y <- df(x, 1, 44)
  y[x < estF] <- NA
  return(y)
}

p9 <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
  stat_function(fun = df, args = list(1, 44), size = 1.5) +
  stat_function(fun = funcShaded, geom = "area", fill = "#000917", alpha = 0.2) +
  geom_vline(xintercept = 1) +
  scale_x_continuous(
    name = "x", breaks = seq(0, 3, 0.5), limits=c(0, 3)
  ) +
  scale_y_continuous(name = "f(x)") +
  ggtitle("Normal function curves of probabilities") +
  scale_colour_brewer(palette = "Accent") +
  theme_bw() +
  theme(axis.line = element_line(size = 1, colour = "black"),
        panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank(),
        plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="black", size = 9),
        axis.text.y=element_text(colour="black", size = 9),
        legend.position = "bottom")
p9



t(beta_hat - beta_res) %*% t(X) %*% X %*% (beta_hat - beta_res) 
