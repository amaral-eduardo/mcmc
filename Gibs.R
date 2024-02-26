## Define constantes
N <- 10000
## Burnin
burn <- 1000
## Matriz para armazenar as amostras
X <- matrix(0, N, 2)

## Define parametros da Normal bivariada
rho <- -.75
mu1 <- 0
mu2 <- 2
sigma1 <- 1
sigma2 <- .5
s1 <- sqrt(1 - rho^2) * sigma1
s2 <- sqrt(1 - rho^2) * sigma2

## Valores iniciais: propositalmente valores discrepantes
X[1, ] <- c(10, 15)

## Gera a cadeia
for (i in 2:N) {
  x2 <- X[i-1, 2]
  m1 <- mu1 + rho * (x2 - mu2) * sigma1/sigma2
  X[i, 1] <- rnorm(1, m1, s1)
  x1 <- X[i, 1]
  m2 <- mu2 + rho * (x1 - mu1) * sigma2/sigma1
  X[i, 2] <- rnorm(1, m2, s2)
}

## Cadeias (mude os valores iniciais para ver convergencia)
matplot(X, type = "l")

## Correlacao entre os valores
par(mfrow = c(1, 2))
acf(X[,1])
acf(X[,2])

par(mfrow = c(1, 1))
## Conjunta
plot(X, main = "", xlab = bquote(X[1]),
     ylab = bquote(X[2]), ylim = range(X[, 2]))

## Descarta os primeiros 1000 valores
b <- burn + 1
x <- X[b:N, ]
matplot(x, type = "l")

## Nao elimina o problema de autocorrelacao...
par(mfrow = c(1, 2))
acf(x[,1])
acf(x[,2])

par(mfrow = c(1, 1))
## ... mas elimina o problema dos valores iniciais discrepantes
plot(x, main = "", xlab = bquote(X[1]),
     ylab = bquote(X[2]), ylim = range(x[, 2]))

## Tamanho da amostra após o burnin
dim(x)

## Faz o thinning
x <- x[seq(1, nrow(x), 5), ]
dim(x)

## Confere novamente
matplot(x, type = "l")

## Agora elimina o problema de autocorrelacao
par(mfrow = c(1, 2))
acf(x[,1])
acf(x[,2])

par(mfrow = c(1, 1))
## Conjunta
plot(x, main = "", xlab = bquote(X[1]),
     ylab = bquote(X[2]), ylim = range(x[, 2]))

## Compara com as marginais
## NOTE que no caso da normal bivariada, as marginais são normais com os
## respectivos parâmetros mu e sigma
par(mfrow = c(1, 2))
hist(x[, 1], freq = FALSE)
curve(dnorm(x, mu1, sigma1), col = 2, add = TRUE)
hist(x[, 2], freq = FALSE)
curve(dnorm(x, mu2, sigma2), col = 2, add = TRUE)

par(mfrow = c(1, 1))
## Compara estatisticas
colMeans(x)

cov(x)

cor(x)