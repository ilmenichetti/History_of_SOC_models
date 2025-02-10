library(vistime)
vistime(device.data, events = "DeviceName", groups = "DeviceManufacturer",
        start = "start_date", end = "end_date")


X_0=50
k=0.1
tim=seq(1:100)
L=2
X=c(X_0,(L/k)*(1-exp(-k*tim))+X_0*exp(-k*tim))

plot(c(0,tim), X, type="l", ylim=c(0,55), ylab="C stocks", xlab="Time", yaxt="n", xaxt="n")
abline(h=X_0, lty=2, col="red")
text(10,X_0+1.2, "Initial steady state", col="red", cex=0.6)
abline(h=L/k, lty=2, col="darkgreen")
text(10,L/k+1.2, "Final steady state", col="darkgreen", cex=0.6)

