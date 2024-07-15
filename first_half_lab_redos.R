########## Practical 3 ##########

# monte carlo simulation 
# get the ecdf, sample the probabilities, then we can simulate growth rates 
# and describe the population sizes by plugging these into the bird formula

rcumul<- function(n,x,p,min,max){
  y<-c()
  U<-runif(n,0,1)
  for(j in 1:n){
    i<-1;
    while((p[i]<=U[j])&(i<=length(x))){i<-i+1}
    if(i==1) y[j]<-(U[j]*(x[i]-min))/p[i]+min
    else if(i>length(x)) y[j]<-((U[j]-p[i-1])*
                                  (max-x[i-1]))/(1-p[i-1])+x[i-1]
    else y[j]<-((U[j]-p[i-1])*(x[i]-x[i-1]))/
        (p[i]-p[i-1])+x[i-1]}
  y
}

m<-10   
c<-1000 
y0<-10

gr = c(0.038,0.02,0.027,0.023,0.042,0.018,0.031,0.036,0.048,0.032,0.027,0.037,0.032,0.033,0.035,0.031,0.034,0.02,0.03,0.039)

min.gr = 0.01
max.gr = 0.05
ordered.gr = sort(gr)

getecdf<-function(x){
  i<-seq(1,length(x),1)
  cdf.rawdata<-i/(length(x)+1)
  cdf.rawdata
}

gr.cdf = getecdf(gr)

plot(ordered.gr,gr.cdf,type = "l", main = "Cumulative Density Function for Growth Rates")

sampdist<-rcumul(n=5000,x=gr,p=gr.cdf,min=min.gr,max=max.gr)

y5 = y0+c*exp(-exp(-sampdist*(5-m)))

quantile(y5,c(0.025,0.5,0.975))
mean(y5)

par(mfrow=c(1,1))
plot(density(y5), xlab="Population Size", main="Density of Growth at 5 years")
plot(density(y5,bw = 10), xlab="Population Size", main="Smoothed Density of Growth at 5 years")

# we can see that the mean population size after five years was 320 birds
# A 95% confidence interval for the population size is (293,341)
# We can be 95% confident upon repeated sampling, that the true population size 
# lies in this interval. This is corroborated in the density of growth plot as 
# it has a peak at approximately 320

y0<-2
y5_2<-y0+c*exp(-exp(-sampdist*(5-m))) 
quantile(y5_2,c(0.025,0.5,0.975))
mean(y5_2)
plot(density(y5_2,bw = 10), xlab="Population Size", main="Smoothed Density of Growth at 5 years")


# if the population size starts with 2 birds, after five years it is estimated that 
# there is a mean population size of 312 birds
# A 95% confidence interval for the population size starting from 2 birds is (285,333)
# We can be 95% confident upon repeated sampling, that the true population size 
# lies in this interval. This is corroborated in the density of growth plot as 
# it has a peak at approximately 315

y0<-6
y5_6<-y0+c*exp(-exp(-sampdist*(5-m))) 
plot(density(y5_6,bw = 10), xlab="Population Size", main="Smoothed Density of Growth at 5 years")
quantile(y5_6,c(0.025,0.5,0.975))
mean(y5_6)

# if the population size starts with 6 birds, after five years it is estimated that 
# there is a mean population size of 316 birds
# A 95% confidence interval for the population size starting from 6 birds is (289,377)
# We can be 95% confident upon repeated sampling, that the true population size 
# lies in this interval. This is corroborated in the density of growth plot as 
# it has a peak at approximately 320





########## Practical 4 ##########

# cost of new bridge follows a normal distribution
# but our prior is a uniform distribution as we are not given any other information
# experts say that mean cost is between 3.5 and 5 - limits of our uniform prior
# plug the values generated from this into the density function for the normal distribution
# we can then plot these 

explicit.pdf<-function(n1){
  x.range = seq(2.5,6,0.001)
  standev = 0.25
  y<-array(dim=c(n1,length(x.range)))
  for(i in 1:n1){
    mu<-runif(1,3.5,5)
    for(j in 1:length(x.range)){
      y[i,j]<-dnorm(x.range[j],mu,standev)}
  }
  y }
# Run the function for 10 iterations
cexpsamp<-explicit.pdf(10)
# Plot the results, first defining the sample space of variability distribution num<-c(seq(0,100))
plot(x.range,cexpsamp[1,],type="n",main="Distribution of Cost of New Bridge - Explicit Method",
     xlab="Number of smokers in sample of 100", ylab="Density", ylim=range(as.numeric(cexpsamp)))
for (i in 1:10) lines(x.range,cexpsamp[i,],lty=i)
leg<-c("Sample 1", "Sample 2","Sample 3", "Sample 4", "Sample 5","Sample 6","Sample 7",
       "Sample 8","Sample 9","Sample 10")
legend("topright",leg,lty=c(seq(1:10)))




########## Practical 5 ##########

### 1 ###

# cost of new bridge follows a normal distribution
# but our prior is a uniform distribution as we are not given a shape parameter
# experts say that mean cost is between 3.5 and 5 - limits of our uniform prior
# standard deviation is 0.25 for when we simulate normal distributions
# we can derive from many samples a distribution for the cost

simul = function(n1,n2){
  standev = 0.25;
  y = array(dim = c(n1,n2));
  for (i in 1:n1) {
    mu = runif(1,3.5,5);
    for (j in 1:n2) {
      y[i,j] = rnorm(1,mu,standev)
    }
  }
y}

simulsamp<-simul(10,1000)

plot(density(simulsamp[1,]), ylim = c(0,1.8), main = "Density of New Bridge Prices Simulation", 
     xlab = "Cost of New Bridge",xlim = range(simulsamp), ylab = "Density of Costs")

for (i in 2:10)  lines(density(simulsamp[i,]),lty=i)

leg<-c("Sample 1", "Sample 2","Sample 3", "Sample 4","Sample 5","Sample 6","Sample 7","Sample 8","Sample 9","Sample 10")
legend("topright",leg,lty=c(seq(1:10)))

mean(simulsamp)
for (i in 1:10) {
  print(mean(simulsamp[i,]))
}
for (i in 1:10) {
  print(quantile(simulsamp[i,],c(0.025,0.5,0.975)))
  print(quantile(simulsamp[i,],0.975)-quantile(simulsamp[i,],0.025))
}

# we can see that the mean cost of building a new bridge is 4.132418 

# the mean cost of building differs between samples 
# the confidence intervals are all approximately the same width

# each curve has a peak which represents the mean cost of building a new bridge
# for each of the 10 samples
# the locations of the peak along the x-axis varies between simulations 
# reflecting variability in the mean cost of a bridge
# this is due to the uncertainty in the cost

# the width of each curve shows the dispersion of the expected costs of buildinga bridge. 
# a wider curve represents more variability in the cost

### 2 ###

# we need to create a distribution which models each expert's likelihood 
# of a correct guess

e1 = 10
e2 = 13
n = 15

e1.dist=rbeta(1000, e1+1, n-e1+1)
e2.dist=rbeta(1000, e2+1, n-e2+1)

b = c()
for (i in 1:length(e1.dist)) {
  if (e2.dist[i]>e1.dist[i]) b[i] = 1 else b[i] = 0
}

mean(b)
table(b)/length(b)

plot(density(e1.dist,bw=0.035),xlim=c(0,1),ylim=c(0,5),type="l",xlab="Probability of correct guess",ylab="Probability density", main="Comparison of Expert 1 and Expert 2 (using rbeta)")
lines(density(e2.dist,bw=0.035), lty=2)
legend("topleft",c("Expert 1","Expert 2"),lty=c(1,2))

distribution_expert_1<-dbeta(seq(0,1,0.0001),e1+1,n-e1+1)
distribution_expert_2<-dbeta(seq(0,1,0.0001),e2+1,n-e2+1)

plot(seq(0,1,0.0001),distribution_expert_1,xlim=c(0,1),ylim=c(0,5),type="l",
     xlab="Probability of correct guess",ylab="Probability density", main="Comparison of Expert 1 and Expert 2 (using dbeta)")
lines(seq(0,1,0.0001),distribution_expert_2, lty=2)
legend("topleft",c("Expert 1","Expert 2"),lty=c(1,2))








