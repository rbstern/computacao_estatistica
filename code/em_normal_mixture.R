# Criando dados simulados
## Parâmetro desconhecido
theta <- c(0, 10)
## Constantes conhecidas
p <- 0.5
sigma <- 1
## Variável latente
z <- rbinom(100, 1, p)
## Dados
x <- z*rnorm(100, theta[1], sigma) + 
  (1-z)*rnorm(100, theta[2], sigma)

# Código do EM
## Função de pesos
h <- function(x, theta_iter)
{
  a = p*dnorm(x, theta_iter[1], sigma)
  b = (1-p)*dnorm(x, theta_iter[2], sigma)
  a/(a+b)
}

B <- 100
theta_hat <- matrix(NA, nrow = B, ncol = 2)
theta_hat[1,] <- c(-1, 0) # Chute inicial
for(ii in 2:B)
{
  # Passo E
  pesos <- h(x, theta_hat[ii-1,]) 
  # Passo M
  theta_hat[ii,] <- c(sum(pesos*x)/sum(pesos),
                      sum((1-pesos)*x)/sum(1-pesos))
}

# Estimativa
theta_hat[B,]
