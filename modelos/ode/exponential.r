library(deSolve)

params <- c(b=0.5, d=0.42)

state <- c(N=10)

Cr_Expo <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dN <- (b - d) * N
    list(c(dN))
  })
}

times <- seq(0, 200, by=0.01)

out <- ode(y = state, times=times, func=Cr_Expo, parms=params)

plot(out, xlab='tiempo', ylab='n(t)')