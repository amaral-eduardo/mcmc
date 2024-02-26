##### Implementação do algoritmo Metropolis-Hastings #######

# priori
prior <- function(p) {
  return(dbeta(p, 2, 2))
}
#verossimilhança
likelihood <- function(p, k, n) {
  return(p^k * (1-p)^(n - k))
}
#posteriori
posterior <- function(p, k, n) {
  return(likelihood(p, k, n) * prior(p))
}

# Metropolis-Hastings
metropolis_hastings <- function(n_iter, k, n) {
  p_current <- runif(1)  # Inicialização
  samples <- numeric(n_iter)
  
  for (i in 1:n_iter) {
    p_proposed <- p_current + rnorm(1, mean = 0, sd = 0.1)  # Proposta de novo estado
    acceptance_prob <- min(1, posterior(p_proposed, k, n) / posterior(p_current, k, n))  # Razão de aceitação
    if (runif(1) < acceptance_prob) {
      p_current <- p_proposed  # Aceitação do novo estado
    }
    samples[i] <- p_current
  }
  
  return(samples)
}

# Dados observados
k <- 77  # Número de caras
n <- 100  # Número total de lançamentos

# Amostragem da distribuição a posteriori
samples <- metropolis_hastings(100000, k, n)

# Plot
hist(samples, breaks = 30, main = "Distribuicao a Posteriori de p", xlab = "p", prob = TRUE)
lines(density(samples), col = "red", lwd = 2)
legend(x = 0.6, y = 6, legend = c(paste("Lancamentos", n), paste("Caras", k)),
       fill = c("white", "white"), border = NA, bty = "n")


mean(samples)
