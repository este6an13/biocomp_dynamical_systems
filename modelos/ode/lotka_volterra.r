library(deSolve)

params <- c(a=0.1, b=0.002, c=0.2, d=0.0025)

state <- c(X=80, Y=20)

Cr_LV <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dX = a * X - b * X * Y
    dY = -c * Y + d * X * Y
    list(c(dX, dY))
  })
}

times <- seq(0, 200, by=0.01)

out <- ode(y = state, times=times, func=Cr_LV, parms=params)

plot(out, xlab='tiempo', ylab='n(t)')

# PDF tinylatex

# https://teaching.smp.uq.edu.au/scims/Appl_analysis/Lotka_Volterra.html