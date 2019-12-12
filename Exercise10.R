#Kyle Sommerfield and Farai Musariri

#Set parameters (before mutation)
rN=0.1
rM=0.1
N0=99
M0=1
K=1000000
timesteps=1000

#Store values and set initial value
Ns=numeric(length=timesteps)
Ns[1]=N0
Ms=numeric(length=timesteps)
Ms[1]=M0

#Run simulation to find equilibrium point
for(t in 1:(timesteps-1)){
  Ns[t+1] <- Ns[t]+rN*Ns[t]*(1-(Ns[t]+Ms[t])/K)
  Ms[t+1] <- Ms[t]+rM*Ms[t]*(1-(Ns[t]+Ms[t])/K)
}

#Create plot
library(ggplot2)
nonmutant <- data.frame(time=1:timesteps,N=Ns)
mutant <- data.frame(time=1:timesteps,M=Ms)
ggplot() + geom_line(data=nonmutant,aes(x=time,y=N),color="blue") + geom_line(data=mutant,aes(x=time,y=M),color="red") + xlab("Time") + ylab("Cell Count")

#assume equilibrium is occuring at timestep=330for (t in 1:(timesteps-1)){
if (t<330)
{
  Ns[t+1] <- Ns[t]+rN*Ns[t]*(1-(Ns[t]+Ms[t])/K)
  Ms[t+1] <- Ms[t]+rM*Ms[t]*(1-(Ns[t]+Ms[t])/K) 
} else {
  rNm=-0.1 
  rMm=0.05
  Ns[t+1] <- Ns[t]+rNm*Ns[t]*(1-(Ns[t]+Ms[t])/K)
  Ms[t+1] <- Ms[t]+rMm*Ms[t]*(1-(Ns[t]+Ms[t])/K)
}
}

#note that rNm and rMm = new parameters after the mutation.

#Create final plot
nonmutant <- data.frame(time=1:timesteps,N=Ns)
mutant <- data.frame(time=1:timesteps,M=Ms)
ggplot() + geom_line(data=nonmutant,aes(x=time,y=N),color="blue") + geom_line(data=mutant,aes(x=time,y=M),color="red") + xlab("Time") + ylab("Cell Count")
