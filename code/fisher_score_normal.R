quasi_newton <- function(init, gradient, A, eps, B = 10^2)
{
  diff = Inf
  t = init
  i = 0
  while((i < B) & diff > 10^(-10))
  {
    aux = t + eps * A(t) %*% gradient(t)
    diff = sum(abs(aux - t))
    t = aux
    i = i + 1
  }
  t
}

mu <- 10
lambda <- 4
n <- 10
X <- rnorm(n, mu, sqrt(lambda))
c(mean(X), mean((X-mean(X))^2))

score <- function(param)
 c(sum(-(param[1]-X)/param[2]), 
   sum(-0.5/param[2] + 0.5*(X-param[1])^2/param[2]^2))

fisher_info <- function(param)
  diag(c(n/param[2], n/(2*param[2]^2)))

quasi_newton(
  c(0, 1), 
  score, 
  fisher_info, 
  0.001, 
  10^5
)
