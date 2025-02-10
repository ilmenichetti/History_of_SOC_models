
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




ICBM_variable<-function(ky=0.8, ko=0.0065, h=0.15, r=1, I=1){

  simulation_variable<-mat.or.vec(sim_length, 3)
  simulation_variable[,1]=seq(1, sim_length)
  simulation_variable[1:2,]<-ode(y = init, time = c(1,2), func = ODEfun, parms = parameters)
  if(length(r)==1){r_long=rep(r, sim_length)}else if(length(r)!=sim_length){message ("you supplied an r that's not the same lenght as the desired sim lenght")}
  if(length(I)==1){I_long=rep(I, sim_length)}else if(length(r)!=sim_length){message ("you supplied an I that's not the same lenght as the desired sim lenght")}

  simulation_variable<-mat.or.vec(sim_length, 3)
  simulation_variable[,1]=seq(1, sim_length)

  #first sim step
  parameters=c(ky, ko, h, r=r_long[1], I=I_long[1])
  simulation_variable[1:2,]<-ode(y = init, time = c(1,2), func = ODEfun, parms = parameters)
  #iterating the simulation
  for(i in 3:sim_length){
    parameters=c(ky, ko, h, r=r_long[i], I=I_long[i])
    simulation_variable[i,2:3]<-ode(y = c(Y=simulation_variable[i-1,2], O=simulation_variable[i-1,3]), time = c(0,1), func = ODEfun, parms = parameters)[2,2:3]
  }

  return(simulation_variable)
}

ICBM_out<-ICBM_variable(ky=0.8, ko=0.0065, h=0.15, r=1, I=1)


plot(simulation[,1], total_SOC,  type="l", ylim=c(0, max(total_SOC)*1.2), xlab="time", lty=2)
lines(simulation[,2], col="darkgreen", lty=2)
lines(simulation[,3], col="darkorange", lty=2)
lines(ICBM_out[,1], total_SOC)
lines(ICBM_out[,2], col="darkgreen")
lines(ICBM_out[,3], col="darkorange")



measured_SOC= data.frame(
  c(1956, 1967,1974, 1975,1977, 1979,1983,1985,1987,1989,1991,1993,1995,1997,1999,2001,
    2005,2007,2009,2011,2013,2015,2017,2019),
  c(43315.2,46099.1354432654,54402.0514037605,54041.8051533193,55034.1640770398,54615.8930041639,52681.7594782169,53624.496505942,
    54005.0133116738,60017.1966448382,61107.5003781939,51574.6724815113,54275.1183294204,57965.4787115247,57956.4965618206,56938.4828493794,
    55910.6109731281,56683.1588144316,58995.1969362421,55438.5423266927,56744.1691233292,56825.0174199966,57116.9017798636,54074.4482017914)
)

colnames(measured_SOC)=c("year", "SOC")
measured_SOC$year=(measured_SOC$year-measured_SOC$year[1])+1

library(hydroGOF)
#define some parameters fo pass to the optimize function to run the model
parameters=c( 0.0065, 0.1500, 1.0000, 840+1770)

ICBM_cost_univariate<-function(x, pars, inputs){
  #assuming an initialization where Y is 5% and O is 95% of the total SOC
  initialization=c(Y=measured_SOC$SOC[1]*0.05, O=measured_SOC$SOC[1]*0.95)
  #defining the parameter list for the ODE functino
  parms=c(ky=x, ko=pars[1], h=pars[2], r=pars[3], I=pars[4])
  #running the ODE solver
  simulation <- ode(y = initialization, time = seq(1:sim_length), func = ODEfun, parms)
  #summing up the pools and calculating the RMSE
  total_SOC=simulation[,2]+simulation[,3]
  RMSE<-rmse(total_SOC[measured_SOC$year], measured_SOC$SOC)
  return(RMSE)
}


xmin <- optimize(ICBM_cost_univariate, c(0, 1), tol = 0.0001, pars=parameters)
xmin



library(hydroGOF)
ICBM_cost<-function(x){
  initialization=c(Y=measured_SOC$SOC[1]*0.05, O=measured_SOC$SOC[1]*0.95)
  parms=c(ky=x[1], ko=x[2], h=x[3], r=1, I=840+1770)
  simulation <- ode(y = initialization, time = seq(1:sim_length), func = ODEfun, parms = parms)
  total_SOC=simulation[,2]+simulation[,3]
  RMSE<-rmse(total_SOC[measured_SOC$year], measured_SOC$SOC)
return(RMSE)
}


library(dfoptim)
hjkb(par=c(0.8, 0.0065, 0.15), fn=ICBM_cost, lower=c(0.1,0,0), upper=c(1,0.1, 0.4))
optim(par=c(0.8, 0.0065, 0.15), fn=ICBM_cost)



