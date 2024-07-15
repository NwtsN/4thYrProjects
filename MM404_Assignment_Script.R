##### Read in data #####

DATA <- read.csv("Parkinsons.csv")
set.seed(202002962) 
z_sd <- sd(DATA$Vocal.Freq)/5
z_sim <- rnorm(n=nrow(DATA), mean=0, sd=z_sd)
pk <- DATA
pk$Vocal.Freq <- pk$Vocal.Freq + z_sim

##### View data #####

View(pk)
str(pk)
summary(pk)

pk$Patient=factor(pk$Patient,levels = 1:24)
pk$Visit=factor(pk$Visit,levels = 1:6)
pk$Parkinsons=factor(pk$Parkinsons,levels = c("No","Yes","Awaiting"))
table(pk$Visit)
table(pk$Patient)
table(pk$Parkinsons)
str(pk)
summary(pk)

# can see that Vocal.Freq is the response
# From question can see that Parkinsons is a fixed effect - "3 diagnosis groups of patients were selected"
# From question can see that Patients are a random effect - "Patients at a local health center were randomly invited to take part in the study"
# From question can see that Visit is a fixed effect - "The participants were invited to return to the clinic at pre-defined time points" 

# so we have a nested design as patients are nested within Parkinsons
# but it must be a repeated measures design as we have repeated observations on the same subjects over time.
# In this case patient is random, Parkinsons and Visit are fixed.

##### Visualise data #####

par(mfrow=c(1,2))
boxplot(Vocal.Freq~Parkinsons,data = pk, 
        ylab="Vocal Frequency (Hz)",
        xlab="Parkinsons Group",
        main="Boxplot of Parkinsons Group vs Vocal Frequency",
        sub="Figure 1")
boxplot(Vocal.Freq~Visit,data=pk,
        xlab = "Visit Number",
        ylab = "Vocal Frequency (Hz)",
        main="Boxplot of Visit Number vs Vocal Frequency",
        sub="Figure 2")
par(mfrow=c(1,1))
boxplot(Vocal.Freq~Parkinsons:Visit,data = pk,
        ylab="Vocal Frequency (Hz)", 
        xlab="Parkinsons Group and Visit Number Combination",
        main="Boxplot of Parkinsons Group and Visit Number Combinations vs Vocal Frequency",
        cex.axis=0.8,sub="Figure 3")

# the Parkinsons Group vs Vocal frequency boxplot shows that the No Parkinsons diagnosis group has the largest variation in responses of the three groups 
# as we can see the No Parkinsons diagnosis group has a response IQR which spans approximately 100 Hz
# the Yes and Awaiting groups both have a similar level of variability in their responses
# their IQRs are approximately 50 Hz and 40 Hz respectively
# the No Parkinsons diagnosis group has a considerably larger variation in responses than the other two groups
# the No Parkinsons diagnosis group also has a higher median response than the other two groups
# the approximate median value of the No Parkinsons diagnosis group is 200 Hz 
# whereas the Yes and Awaiting groups medians are approximately 130 Hz and 150 Hz respectively
# it seems that the No Parkinsons diagnosis group has a higher vocal frequency than the other 2 groups

# the Visit Number vs Vocal frequency boxplot shows that the median vocal frequency does not differ much as the patient progresses through their 6 visits
# the median vocal frequency is approximately in the range 140 Hz to 150 Hz for all Visits
# the all of the Visits have a similar level of response variation
# approximately 55 Hz for every Visit
# it seems that the the vocal frequencies do not appear to differ over time

# the highest median vocal responses come from the 2 combinations, that being No Parkinsons diagnosis and Visit 3 and No Parkinsons diagnosis and Visit 6
# the lowest median vocal response is from the Yes and Visit 1
# the lowest variability seems to be in the groups, Yes and Visit 1, Awaiting and visit 1,
# Awaiting and visit 2, Awaiting and visit 4
# the highest variability seems to come from the combinations, No parkinsons diagnosis and visit 2,
# No parkinsons diagnosis and visit 3, No parkinsons diagnosis and visit 6
# there seems to be no clear trends over time

##### Assumptions #####

pk.assumptions=aov(Vocal.Freq~Parkinsons*Visit+Patient,data=pk)

par(mfrow=c(3,2))
plot(pk.assumptions)
plot(residuals(pk.assumptions),type="b",main="Time series/Versus Order plot of Residuals",ylab="Residuals",xlab="Order")
plot(density(rstandard(pk.assumptions)),main="Density Plot of Standardised Residuals - pk.assumptions model")
mean(rstandard(pk.assumptions))
mean(residuals(pk.assumptions))

# the assumption that residuals are normally distributed is satisfied
# the qq-plot shows the points are mostly on/around the qq-line
# the assumption that the residuals have constant variance is satisfied
# there appears to be a random scattering of points in the versus fits plot
# the assumption that the mean of the residuals is zero is satisfied
# the density of residuals plot shows a peak at 0 and the mean of the residuals is also a really small number
# the assumption that the residuals are independent from one another is satisfied
# the Time Series/Versus Order plot shows no trends/runs or patterns

##### Model #####

# using sum to 0 constraint

contrasts(pk$Parkinsons)=contr.sum(length(levels(pk$Parkinsons)))
contrasts(pk$Visit)=contr.sum(length(levels(pk$Visit)))

pk.aov=aov(Vocal.Freq~Parkinsons*Visit+Error(Patient),data=pk)
summary(pk.aov)

# We can see from the model above that the F statistic is 2.466 with a p-value 0.0108 
# below our significance level of 95%
# can reject the null hypothesis
# suggest that there is a significant interaction between Parkinsons group and Visit on the Vocal Frequency levels. 
# So let’s look at the interaction plot.

par(mfrow=c(1,1))
with(pk, interaction.plot(Visit, Parkinsons, Vocal.Freq,
                          lwd=3,sub="Figure 5",col=c("forestgreen","red","blue3"),lty=c(1,5,3),
                          main="Interaction Plot - Parkinsons Group and Visit Number",
                          xlab="Visit Number",
                          ylab = "Mean of Vocal Frequency (Hz)"))

require(nlme)
pk.lme=lme(Vocal.Freq~Parkinsons*Visit,random = ~1|Patient,data = pk)
anova(pk.lme)
z=summary(pk.lme)
z

sig.interactions=data.frame(EffectSize=z$tTable[9:18,1],Pvalue=round(z$tTable[9:18,5],digits = 3),Significant=z$tTable[9:18,5]<0.05)
sig.interactions

# from the output Parkinsons1 is the difference in Vocal.Freq between patients with Parkinson's disease (level "Yes") and those without (level "No")
# from the output Parkinsons2 is the difference in Vocal.Freq between patients awaiting Parkinson's diagnosis (level "Awaiting") and those without (level "No")
# Visit1 is the difference in vocal frequency between the 1st and 2nd visits
# Visit2 is the difference between 1st and 3rd visits etc.

# we can see that the interactions between Parkinsons2:Visit1, Parkinsons1:Visit4 and Parkinsons2:Visit5 
# have statistically significant effects on vocal frequency
# i.e. they have the largest effect sizes
# that is, the 3 interactions which have the largest effect are between Awaiting and Visit 2, Yes (diagnosed) and Visit 5, Awaiting and Visit 6

# the residual variance is 12.71006^2 = 161.5 and the residual variance between patients is 37.88264^2 = 1435.1
# total variance is 161.5 + 1435.1 = 1596.6
# the proportion of variance accounted for by the error is
# 161.5/1596.6 = 0.1011524*100 = 10.12%
# the proportion of variance accounted for by the differences between patients is
# 1435.1/1596.6 = 0.8988476*100 = 89.88%

##### Planned Comparisons #####

# we have a planned comparison 
# compare diagnosed to the average of undiagnosed and awaiting diagnosis

# check order of variables

table(pk$Parkinsons)

# order is Awaiting, Yes, No

mat=matrix(c(-1/2,1,-1/2), nrow=1) 
cMat=MASS::ginv(mat)
require(lme4)
pk.lmer.planned=lmer(Vocal.Freq ~ Parkinsons*Visit+(1|Patient), data=pk,
               contrasts=list(Parkinsons=cMat)) # fit model with the contrast
coef(summary(pk.lmer.planned))

# We can see that the estimate is -34.737664
# which would mean that the average of No and Awaiting is 34.737664 higher than the average of Yes. 

confint.merMod(pk.lmer.planned)

# can see that the 95% confidence interval for this estimate is:
# ( -70.0262184,0.5508910 )
# which is not significant as it crosses zero
# This suggests that there is no difference between the Vocal Frequency 
# in those diagnosed with Parkinson’s and the average Vocal Frequency of the other two diagnosis groups
# but it is really close to not crossing 0 so if a slightly lower significance level was used 
# we would say there would be a difference
