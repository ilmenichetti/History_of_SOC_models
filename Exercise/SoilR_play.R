library(SoilR)


t_start=0
t_end=10
t_step=1/12
t=seq(t_start,t_end,t_step)

t=seq(0,20,by=0.1)
k=0.8 # 1/time
C0=80 # mass
In = 40 # mass/time

Model1=OnepModel(t,k,C0,In)

ks=c(k1=0.99,k2=0.8,k3=0.55)

Model2=ThreepSeriesModel(t,ks,
                        a21=0.15,
                        a32=0.06,
                        C0=c(20,20,40),In)


Ct1=getC(Model1)
Ct2=getC(Model2)

png("1vs3pools.png", width = 2000, height = 800, res=200)
par(mar=c(4,4,2,2))
plot(Ct1, type="l", col="darkorange", ylab="SOC", xlab="Time", lty=2, ylim=c(40,80))
Ct2_tot=rowSums(Ct2)
lines(Ct2_tot, col="red")
legend("topright", c("1 pool", "3 pools"), lty=c(2,1), col=c("darkorange", "red"), bty="n")
dev.off()

