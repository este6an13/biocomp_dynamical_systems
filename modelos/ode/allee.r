library(deSolve)

params <- c(b=0.5, d=0.49, K=1000, a=500)

state <- c(N=550)

Cr_Allee <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dN <- (b - d) * N * (1 - N/K) * ((N - a) / K)
    list(c(dN))
  })
}

times <- seq(0, 1000, by=0.01)

out <- ode(y = state, times=times, func=Cr_Allee, parms=params)

plot(out, xlab='tiempo', ylab='n(t)')