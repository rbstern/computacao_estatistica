# Criando dados simulados
## Parâmetro desconhecido
theta <- 0.2
probs = c(theta^2, 2*theta*(1-theta), (1-theta)^2)
## Variável latente
n <- 500
z <- t(rmultinom(n, 1, probs))
## Dados
x <- z[,1]
# Código do EM
## Função de pesos
S1 <- function(x, t) sum(x)
S2 <- function(x, t)
{
  p2 = 2*t*(1-t)/(2*t*(1-t) + (1-t)^2)  
  sum(p2*(1-x))
}
S3 <- function(x, t)
{
  p3 = (1-t)^2/(2*t*(1-t) + (1-t)^2)
  sum(p3*(1-x))
}

B <- 100
theta_hat <- rep(NA, B)
theta_hat[1] <- 0.5 # Chute inicial
for(ii in 2:B)
{
  # Passo E
  S <- c(S1(x, theta_hat[ii-1]),
         S2(x, theta_hat[ii-1]),
         S3(x, theta_hat[ii-1]))  
  # Passo M
  theta_hat[ii] <- (2*S[1] + S[2])/(2*n)
}

# Estimativa
theta_hat[B]
