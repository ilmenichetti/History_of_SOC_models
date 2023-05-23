
library(deSolve)

##function (differential form)
ODEfun <- function(time=seq(1:times_range), state=state, parms=parms) {
  with(as.list(c(state, parms)), {
    .Y=I-ky*r*Y
    .O=h*ky*r*Y-ko*r*O
    return(list(c(.Y, .O)))
  })
}


init=c(Y=2, O=10)
parameters=c(ky=0.8, ko=0.0065, h=0.15, r=1, I=1)
sim_length=100

simulation <- ode(y = init, time = seq(1:sim_length), func = ODEfun, parms = parameters)
total_SOC=simulation[,2]+simulation[,3]


plot(simulation[,1], total_SOC,  type="l", ylim=c(0, max(total_SOC)*1.2), xlab="time")
lines(simulation[,2], col="darkgreen")
lines(simulation[,3], col="darkorange")
