#################### 1 ####################

set.seed(250)
# use calc.b function to find b based on a
# we used trial and error to iterate towards an appropriate b

calc.b<-function(a, mode){
  b <- 2-a+(a-1)/mode
  print(b)
}

xmin<-0.00001
xmax<- 0.0007
step.size<-0.000001

a.plot<- 6 #choose value of a
b.plot<- calc.b(a=a.plot, mode=0.000187) #choose value of mode
plot(seq(xmin,xmax,step.size),dbeta(seq(xmin,xmax,step.size), a.plot, b.plot), type="l", main=paste("Beta with a=",round(a.plot,1)," and b=", round(b.plot, 1), sep=""),ylab="Density", xlab="Proportion")
abline(v=0.000187)
x.range <- seq(0.00001,0.0006,0.000001)

# given mu and t
# decided upon a beta distribution to describe v
# we need to use the beta distribution as:
# proportion is between 0 and 1 - same as beta distribution
# it is flexible - can change it so it reflects the range and modal values given in the question
# then used this to derive a distribution for p(t)

mu <- 0.078
t <- 2
v <- rbeta(100000, a.plot, b.plot)
p <- ((v*(1-exp(-(mu+v)*t)))/(v+mu))

# plot  distribution of v and p(t) on the same plot

plot(density(p),ylim=c(0,5000),lwd=2, col="black",main="Density of probability of Infection p(t)", xlab = "Probability of Dogs being infected with E. multilocularis")
lines(density(v), lwd=2, col="blue")
legend("topright", c("Distribution describing v","Distribution describing p(t)"),col=c("blue", "black"), cex=0.8, lwd=2)

# Summary Stats

Summary.v <- summary(v)
Summary.p <- summary(p)
quantiles.v <- quantile(v,c(0.025,0.5,0.975))
quantiles.p <- quantile(p,c(0.025,0.5,0.975))
Summary.v;Summary.p;quantiles.v;quantiles.p

#################### 2 ####################

treatment.times <- c(105,73,140,53,100,39,129,41,115,55,145,89,45,35,7,36,29,109,70,31,76,90,64,39,76)

# assume treatment time follows a normal distribution
# use bootstrapping to model mean and sd of treatment time

pbs<-function(x,b){
  y<-array(dim=c(b,2))
  samp <- c()
  m<-mean(x)
  s<-sd(x)
  for(i in 1:b){
    samp<- rnorm(length(x), m, s)
    y[i,1]<- mean(samp)
    y[i,2]<-sd(samp)
  }
  y}

bs.time <- pbs(treatment.times, 100000)

# finding the probability that it is outside 24-120 hours
prob.Outside <- function(mean, sd){
  p.below <- pnorm(24, mean, sd)
  p.above <- 1- pnorm(120, mean, sd)
  p.below + p.above
}

total.prob <- prob.Outside(bs.time[,1],bs.time[,2])

plot(density(total.prob),lwd=2, col="black", main="Distribution of probabilities of treatment time outside 24-120 Hours", xlab="Probablilty")

# Summary
Summary.total.prob <- summary(total.prob)
quantiles.total.prob <- quantile(total.prob,c(0.025,0.5,0.975))
Summary.total.prob
quantiles.total.prob

#################### 3 ####################

# calculating a distribution that describes one individual dog being infected
combined.prob <- p * total.prob
plot(density(combined.prob),lwd=2, col="black", main ="Distribution of the probality that any dog returning to GB after a 2 week holiday is infected", xlab="Probability")

# Summary
quantile.combined.prob <- quantile(combined.prob,c(0.025,0.5,0.975))
summary.combined.prob <- summary(combined.prob)
quantile.combined.prob
summary.combined.prob

#################### 4 ####################

annualDogsOnHoliday = 1000

# using non-parametric bootstrapping to derive the annual risk distribution

dogsOnHolidayProbabilityOfInfection = sample(x = combined.prob, size = annualDogsOnHoliday, replace = TRUE)

# using this method as no data was given

npbs<-function(x,b){
  y<-c()
  for(i in 1:b){
    y[i]<-mean(sample(x,length(x),replace=TRUE))
  }
  y}

npDogs = npbs(dogsOnHolidayProbabilityOfInfection,10000)

plot(density(npDogs), xlab="Mean Probabilities of Infection", main="Bootstrap Distribution of Mean Probabilities of Infection", lwd = 3)

mean(npDogs)
quantile(npDogs, probs = c(0.025,0.5,0.975))
summary(npDogs)

#################### 5 ####################

# Define mu (recovery rate) ranging from 0 weeks to 1 week 
# with small increments of 0.02 weeks

mu_range = seq(0, 1, 0.02)

# store results for each mu value

results_list = list()
density_data_list = list()

# Define a function to calculate combined probability of infection 
# for a given mu

calc.combined.prob <- function(mu){
  p <- ((v*(1-exp(-(mu+v)*t)))/(v+mu))
  combined.prob <- p * total.prob
  return(combined.prob)
}

# Loop over each mu value in the defined range

for (mu in mu_range) {
  
  # Calculate the combined probability of infection for the current mu value
  
  combined_prob_for_mu = calc.combined.prob(mu)
  
  # Sample the calculated probability to simulate the probability of infection 
  # for dogs on holiday, with replacement
  
  dogsOnHolidayProbabilityOfInfection = sample(x = combined_prob_for_mu, size = annualDogsOnHoliday, replace = TRUE)
  
  # Applying NPBS to the simulated probabilities to estimate the distribution
  
  npDogs = npbs(dogsOnHolidayProbabilityOfInfection, 10000)
  
  # Calculate the density of the bootstrap sample estimates
  
  densitiesData = density(npDogs)
  
  # Store the relevant information associated with the current mu value
  
  density_data_list[[paste("mu =", mu)]] = densitiesData
  mean_probability <- mean(npDogs)
  quantiles_probability <- quantile(npDogs, probs = c(0.025, 0.5, 0.975))
  results_list[[paste("mu =", mu)]] <- list(mean = mean_probability, quantiles = quantiles_probability)
}

head(results_list)
head(density_data_list)

# Plot the density of mean probabilities for the first mu value

plot(density_data_list[[1]]$x, density_data_list[[1]]$y, 
     type = 'l', main = "Density of Mean Probabilities for Different Recovery Rates", 
     xlab = "Mean Probability", ylab = "Density", 
     ylim = c(0, max(sapply(density_data_list, function(x) max(x$y)))), xlim = c(0.000035,0.0001), lwd = 0.2)

colours <- rainbow(length(density_data_list))

# Plot the densities for all mu values, except the first one, using different colours

for (i in 2:length(density_data_list)) {
  lines(density_data_list[[i]]$x, density_data_list[[i]]$y, col = colours[i], lwd = 0.5)
}


# Create labels for the legend, sampling every 10th mu value

legend_labels <- sapply(seq(1, length(mu_range), by = 10), function(i) paste("mu ~", round(mu_range[i], 3), "Weeks"))

# Create a subset of colours for the legend

legend_colours <- colours[seq(1, length(colours), by = 10)]

# Add the legend to the plot

legend("topright", legend = legend_labels, col = legend_colours, lty = 1)

