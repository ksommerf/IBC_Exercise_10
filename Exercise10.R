#Kyle Sommerfield and Farai Musariri

#Set parameters
rN=-0.1
rM=0.05
K=1000000
timesteps=500

#Store values and set initial value
Ns=numeric(length=timesteps)
Ns[1]=100
Ms=numeric(length=timesteps)
Ms[1]=100

#Run simulations
for(t in 1:(timesteps-1)){
  Ns[t+1] <- Ns[t]+rN*Ns[t]*(1-(Ns[t]+Ms[t])/K)
  Ms[t+1] <- Ms[t]+rM*Ms[t]*(1-(Ns[t]+Ms[t])/K)
}

#Create plot
library(ggplot2)
nonmutant <- data.frame(time=1:timesteps,N=Ns)
mutant <- data.frame(time=1:timesteps,M=Ms)
ggplot() + geom_line(data=nonmutant,aes(x=time,y=N),color="blue") + geom_line(data=mutant,aes(x=time,y=M),color="red") + xlab("Time") + ylab("Cell Count")
