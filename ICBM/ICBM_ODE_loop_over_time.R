
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




ICBM_variable<-function(ky=0.8, ko=0.0065, h=0.15, r=1, I=1, init, sim_length){

  simulation_variable<-mat.or.vec(sim_length, 3)
  simulation_variable[,1]=seq(1, sim_length)

  if(length(r)==1){r_long=rep(r, sim_length)}else if(length(r)!=sim_length){message ("you supplied an r that's not the same lenght as the desired sim lenght")}
  if(length(I)==1){I_long=rep(I, sim_length)}else if(length(r)!=sim_length){message ("you supplied an I that's not the same lenght as the desired sim lenght")}

  simulation_variable<-mat.or.vec(sim_length, 3)
  simulation_variable[,1]=seq(1, sim_length)

  #first sim step
  parameters_loop=c(ky=ky, ko=ko, h=h, r=r_long[1], I=I_long[1])
  simulation_variable[1:2,]<-ode(y = init, time = c(1,2), func = ODEfun, parms = parameters_loop)

  #iterating the simulation
  for(i in 3:sim_length){
    parameters_loop=c(ky=ky, ko=ko, h=h, r=r_long[i], I=I_long[i])
    simulation_variable[i,2:3]<-ode(y = c(Y=simulation_variable[i-1,2], O=simulation_variable[i-1,3]), time = c(0,1), func = ODEfun, parms = parameters_loop)[2,2:3]
  }

  return(simulation_variable)
}

ICBM_out<-ICBM_variable(ky=0.8, ko=0.0065, h=0.15, r=1, I=1, init, sim_length)


plot(simulation[,1], total_SOC,  type="l", ylim=c(0, max(total_SOC)*1.2), xlab="time", lty=2)
lines(simulation[,2], col="darkgreen", lty=2)
lines(simulation[,3], col="darkorange", lty=2)
lines(ICBM_out[,1], total_SOC)
lines(ICBM_out[,2], col="darkgreen")
lines(ICBM_out[,3], col="darkorange")


