########## Revision 23/04/24 ##########

###### 2022 Past Paper #####

### 1 ###

# bootstrapping allows us to quantify the uncertainty associated with the mean number of
# births the distribution of the parameters is unknown but we are given
# data so we use non parametric bootstrapping
# we will perform a large number n, bootstrap resamples of the given data (with replacement)


no_births = c(18, 18, 17, 15, 17, 26, 23, 11, 22, 14, 19, 18, 17, 17, 16, 14, 14, 11, 21, 19)

npbs<-function(x,b){
  y<-array(dim = c(b,2))
  for(i in 1:b){
    y[i,1]<-mean(sample(x,length(x),replace=TRUE))
    y[i,2]<-sd(sample(x,length(x),replace = TRUE))
  }
  y}

# Run model and plot results 
non_para_births <-npbs(no_births,1000)

plot(density(non_para_births[,1]), xlab="Mean Number of Births", main="Distribution of mean number of births \n Non-parametric bootstrap")
plot(density(non_para_births[,2]), xlab="standard deviation of Number of Births", main="Distribution of standard deviation of number of births \n Non-parametric bootstrap")



mean(non_para_births[,1])
mean(non_para_births[,2])

quantile(non_para_births[,1], probs=seq(0,1,0.025))
quantile(non_para_births[,2], probs=seq(0,1,0.025))

# we can see that the estimated mean number of births is 17.346
# the 95% confidence interval for the mean number of births
# is (15.84875, 18.90125)
# we can be 95% confident that upon repeated sampling it is highly likely 
# that the true mean number of births lies in this interval
# the plot shows this as it has a peak at approximately 17.5

# we can see that the estimated standard deviation of number of births is 3.642
# the 95% confidence interval for the standard deviation of number of births
# is (2.491815 , 4.887302)
# we can be 95% confident that upon repeated sampling it is highly likely 
# that the true standard deviation of number of births lies in this interval

### 2 ### 

# we are given a rate of births per month so the gamma prior is the only 
# one we can use for rates
# it has a shape parameter and a rate parameter
# shape parameter alpha = 540 and rate of t = 30

alpha<-540
t<-30

rlambda<-rgamma(1000,alpha+1,t)
plot(density(rlambda), main = "Distribution of mean number of births (Bayesian Inference)", xlab = "Mean number of births")
mean(rlambda)
quantile(rlambda,seq(0,1,0.025))

# the mean number of babies born per day from the more recent survey is 18.036
# the 95% CI for the mean number born per day is (16.50809 , 19.64673)
# we can be 95% confident that the true mean number of babies born per day
# is in this interval upon repeated sampling
# the mean was lower in the previous study, i.e. the mean number of births has increased
# since 2015

### 3 ###

simul<-function(n1,n2){
  y<-array(dim=c(n1,n2));
  for(i in 1:n1){
    p<-rgamma(1,404,20);
    for(j in 1:n2){
      y[i,j]<-rpois(1,p)
    }
  }
  y
}

simulsamp = simul(10,1000)
plot(density(simulsamp[1,]), main=" ", xlim = range(simulsamp), xlab="Number of births")
for (i in 2:10){
  lines(density(simulsamp[i,]),lty=i)
}
leg<-c("Sample 1", "Sample 2","Sample 3", "Sample 4",
       "Sample 5", "Sample 6", "Sample 7", "Sample 8", "Sample 9", "Sample 10")
legend("topright",leg,lty=c(seq(1:10)))
abline(v=18.12)

for (i in 1:10) {
  print(quantile(simulsamp[i,],seq(0,1,0.025)))
  print(mean(simulsamp[i,]))
}

# the mean number of births are all around the same ~19
# the confidence intervals are all approximately the same width

# each curve has a peak which represents the mean number of births on any given day
# for each of the 10 simulations
# the locations of the peak along the x-axis varies between simulations 
# reflecting variability in the mean number of births
# this is due to the uncertainty in the birth rate

# the width of each curve shows the dispersion of the expected number of 
# births. a wider curve represents more variability in the number of births
# the curves further along the axis have a higher birth rate

### 4 ###

# we need to find the number of times the births each day exceed 29 for 
# each simulated 1000 days. 

greater_than_twenty_nine<-c()
for(i in 1:10){
  greater_than_twenty_nine[i]<-sum(simulsamp[i,]>29)/ncol(simulsamp)
}

greater_than_twenty_nine
mean(greater_than_twenty_nine)

# the average number of times the daily births exceed 29 over each of the
# ten 1000 day simulations is 2.222% 

### 5 ###

# need to have a prior of beta and a posterior of binomial
# so we need most of the values to be >0.22 with a peak in 
# the distribution at ~0.26

calc.b<-function(a, mode){
  b<-2-a+(a-1)/mode
  print(b)
}

P_B.a<-100
P_B.b<-calc.b(a=P_B.a, mode=0.28)

P_B<-rbeta(1000,P_B.a,P_B.b)

plot(density(P_B),xlab="Probability of Number of Staff Absent on Any Given Day", main = "Distribution of the uncertainty for the proportion of \n hospital staff absent on any given day")
qts<-seq(0,1,0.025)
quantile(P_B,qts)
mean(P_B)

# the mean proportion of hospital staff absent on any given day is 28.022%
# a 95% CI for the proportion of hospital staff absent on any given day is (0.231 , 0.328)
# we can be 95% confident that the true proportion of hospital staff absent on any given day
# is in this interval upon repeated sampling

# the hospital will not be able to handle as many newborns if the proportion of absentees is higher
# i.e. capacity is reduced
# the capacity is the affected parameter

### 6 ###

# a limitation is that the data given to us in Q1 could have been unrepresentative of the target population
# npbs might not characterise he tails of the distribution well given the small sample size
# the sample from q1 was quite small relative to the target population
# this would have meant the mean might not have been very accurate
# in q2 we simulated over 1000 days but not every day might have an equal chance of a child being born
# expert opinion might not be accurate
# 




###### 2021 Past Paper #####

### 1 ###

# in this case we will use non parametric bootstrapping to estimate the reproduction number 
# and its uncertainty. this is because we are given data but no distribution to sample from.
# we resample with replacement from the observed data.

covid_cases = c(4,5,2,1,0,5,8,2,2,3,5,9,0,1,3,2,6,2,3,5)

npbs<-function(x,b){
  y<-array(dim = c(b,2))
  for(i in 1:b){
    y[i,1]<-mean(sample(x,length(x),replace=TRUE))
    y[i,2]<-sd(sample(x,length(x),replace=TRUE))
  }
  y}

# Run model and plot results 
np_cov<-npbs(covid_cases,1000)

plot(density(np_cov[,1]), xlab="Mean R number", main="Distribution of mean R number \n Non-parametric bootstrap")
plot(density(np_cov[,2]), xlab="standard deviation of R number", main="Distribution of SD of R number \n Non-parametric bootstrap")

mean(np_cov[,1])
quantile(np_cov[,1], probs=seq(0,1,0.025))
mean(np_cov[,2])
quantile(np_cov[,2], probs=seq(0,1,0.025))


# we can see that the estimated mean R number is 3.4
# the estimated median R number is also 3.4
# 95% confidence interval for the distribution of estimated R0 values 
# across the bootstrap samples is from (2.44875,4.45125)
# we can be 95% confident that upon repeated sampling the true R number lies 
# in this interval
# we can see that the estimated standard deviation of R number values is 2.363202
# the median standard deviation is estimated at 2.364207
# 95% confidence interval for the estimated standard deviation of R0 values
# across the bootstrap samples is from (1.650139,3.084799)
# we can be 95% confident that upon repeated sampling the true standard deviation 
# of the R number lies in this interval

### 2 ###

# we will get the growth rate by substituting in our reproduction number into the 
# formula that is given
# then we will use this vector to evaluate at each of the given days 1:30
# by putting the number of infections into the I equation and evaluating

r = (np_cov[,1]-1)/12
quantile(r,c(0.025,0.5,0.975))

# we can see that the estimated growth rate has a 95% confidence interval going
# from 0.1207292 to 0.2876042
# we can be 95% confident that upon repeated sampling the true growth rate lies 
# in this interval

I = array(dim=c(length(r),30))
t = 1:30
for (i in 1:length(np_cov[,1])) {
  I[i,] = 20*exp(r[i]*t) #twenty initial cases, calculate I at each r, for 30 time points
}

I.day.10<-floor(I[,10])
quantile(I.day.10, seq(0,1,0.025))
median(I.day.10)

# the median number of infections at day 10 is estimated to be 147
# the 95% CI for the estimated number of infections at day 10 goes from 66 to 385
# we can be 95% confident that upon repeated sampling the true number of infections 
# at ten days lies in this interval

I.day.20<-floor(I[,20])
quantile(I.day.20, seq(0,1,0.025))
median(I.day.20)

# the median number of infections at day 20 is estimated to be 1091
# the 95% CI for the estimated number of infections at day 20 goes from 224 to 4893
# we can be 95% confident that upon repeated sampling the true number of infections 
# at 20 days lies in this interval

I.day.30<-floor(I[,30])
quantile(I.day.30, seq(0,1,0.025))
median(I.day.30)

# the median number of infections at day 30 is estimated to be 8068
# the 95% CI for the estimated number of infections at day 30 goes from 750 to 143018
# we can be 95% confident that upon repeated sampling the true number of infections 
# at 30 days lies in this interval

plot(I[1,], xlab="time", main = "number of infections of \n first 10 random values of R0", ylab="Number infected", type="l", ylim=c(0,max(I[1:10,30])))
for(i in 2:10){
  lines(I[i,], lty=i)
}

# as we can see there are multiple different lines representing different R0's
# we can see that the infection number grows exponentially with time
# but for higher R0's the infection rate is also higher meaning that the number of 
# infections grows faster with increasingly large R0's

### 3 ###

# prior distribution needed where the curve goes from 0.25 to 0.35 
# with each value equally as likely as any other value to be the the true proportion

simul<-function(n1,n2){
  y<-array(dim=c(n1,n2));
  for(i in 1:n1){
    p<-runif(1,0.25,0.35);
    for(j in 1:n2){
      y[i,j]<-rbinom(1,median(I.day.30),p)
    }
  }
  y
}
simulsamp = simul(10,1000)

plot(density(simulsamp[1,]), main=" ", ylim = c(0,0.0105), xlim = range(simulsamp), xlab="Number of hospitalisations")
for (i in 2:10){
  lines(density(simulsamp[i,]),lty=i)
}
leg<-c("Sample 1", "Sample 2","Sample 3", "Sample 4",
       "Sample 5", "Sample 6", "Sample 7", "Sample 8", "Sample 9", "Sample 10")
legend("topright", leg,lty=c(seq(1:10)))

mean(simulsamp)

p.greater.than = c()
for (i in 1:10) {
  p.greater.than[i] = sum(simulsamp[i,]>2400)/ncol(simulsamp)
}
p.greater.than

mean(simulsamp)

### 4 ###

# the proportion of hospitalizations might be different in European countries
# than in Scotland

# the sample size to estimate R0 is quite wee so when we simulate R0
# the sample mean may be unrepresentative of the true R0 mean

# non-parametric bootstrapping doesn't accurately capture the tails of the true distribution

# seasonality might affect the R number so if the cases used are not recent 
# they may not be representative of the current R0

# the growth rate equation does not take into account preventative measures that
# the government might impose to stop the spread of the virus

# might be able to plan precautionary measures from this study

# they could get an estimate for the number of beds from this study

# doesnt take into account the bed turnover rate













