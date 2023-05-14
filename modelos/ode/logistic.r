library(deSolve)

params <- c(b=0.5, d=0.49, K=1000) # asintóticamente, la población tiende a K

state <- c(N=1050) # N = K, punto de equilibrio

# los puntos de equilibrio pueden ser estables e inestables

Cr_Logist <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dN <- (b - d) * N * (1 - N/K)
    list(c(dN))
  })
}

times <- seq(0, 1000, by=0.01)

out <- ode(y = state, times=times, func=Cr_Logist, parms=params)

plot(out, xlab='tiempo', ylab='n(t)')