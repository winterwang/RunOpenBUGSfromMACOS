library(R2OpenBUGS)
data(schools)

nummodel <- function(){
  for (j in 1:J){
    y[j] ~ dnorm (theta[j], tau.y[j])
    theta[j] ~ dnorm (mu.theta, tau.theta)
    tau.y[j] <- pow(sigma.y[j], -2)}
  mu.theta ~ dnorm (0.0, 1.0E-6)
  tau.theta <- pow(sigma.theta, -2)
  sigma.theta ~ dunif (0, 1000)
}
write.model(nummodel, "nummodel.txt")
model.file1 = paste(getwd(),"nummodel.txt", sep="/")
file.show("nummodel.txt")


J <- nrow(schools)
y <- schools$estimate
sigma.y <- schools$sd
data <- list ("J", "y", "sigma.y")


inits <- function(){
  list(theta = rnorm(J, 0, 100), mu.theta = rnorm(1, 0, 100), sigma.theta = runif(1, 0, 100))}

WINE="/usr/local/bin/wine"
WINEPATH="/usr/local/bin/winepath"
OpenBUGS.pgm="/Users/wangcc.me/Wine\ Files/drive_c/Program\ Files/OpenBUGS/OpenBUGS323/OpenBUGS.exe"


parameters = c("theta", "mu.theta", "sigma.theta")

schools.sim <- bugs(data, inits, model.file = model.file1,parameters=parameters,n.chains = 3, n.iter = 1000, OpenBUGS.pgm=OpenBUGS.pgm, WINE=WINE, WINEPATH=WINEPATH,useWINE=T)
print(schools.sim)
