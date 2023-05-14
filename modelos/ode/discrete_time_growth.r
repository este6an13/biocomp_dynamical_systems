library(deSolve)

params <- c(r=0.08)

state <- c(N = 5) # poblaciones

Cr_DTG <- function(t, state, params) {
  with(as.list(c(state, params)), {
    N <- (1 + r) * N
    list(c(N))
  })
}

times <- 0:50

out <- ode(y = state, times=times, func=Cr_DTG, parms=params, method="iteration")

plot(out, xlab='tiempo', ylab='n(t)')