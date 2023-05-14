library(deSolve)

params <- c(r1=0.7, r2=0.5, a1=0.2, a2=0.2, K1=60, K2=10)

state <- c(X=80, Y=20)

Cr_LVG <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dX = r1 * X * (1 - ((X + a1 * X) / K1))
    dY = r2 * Y * (1 - ((Y + a2 * Y) / K2))
    list(c(dX, dY))
  })
}

times <- seq(0, 100, by=0.01)

out <- ode(y = state, times=times, func=LGV, parms=params)

plot(out, xlab='tiempo', ylab='n(t)')

# PDF tinylatex

# https://hankstevens.github.io/Primer-of-Ecology/mutualisms.html