# can margin of error be not in percent

################################################################################
# LAB 10 R CODE
# YULIIA HELEVERIA
# MATH 240 - SPRING 2025
################################################################################

################################################################################
# Load Libraries
################################################################################
library(tidyverse)
library(ggplot2)

################################################################################
# TASK 1: Basic Simulation
################################################################################
#generate 10k polls for satisfaction with US world position
n.serveyed <- 1004 #sample size
probability <- 0.39
n.repeat <- 10000

#generate the poll completion 10,000 times
poll <- rbinom(n.repeat, n.serveyed, probability)

#put data into tibble for plotting
ggpoll <- tibble(poll)|>
  mutate(poll = poll/n.serveyed)#convert the data into proportion

#plot a histogram of sample proportion with superimposed density
plot.poll <- ggplot(data = ggpoll)+
  geom_histogram(aes(x = poll, y = after_stat(density)),
                 color="grey",
                 bins = 25)+ #histogram of generated polls
  geom_density(aes(x = poll),
               color = "red")+ #superimposed density
  scale_x_continuous(breaks = seq(0.3, 0.5, by = 0.03))+ #put ticks on y-axis
  geom_hline(yintercept = 0)+ #add y-intercept
  theme_bw()+
  xlab("Proportion of satisfied adult sample (n = 1004)")+ #add x and y label
  ylab("Density")

#calculate the range of middle 95%
lower <- quantile(ggpoll$poll, probs = 0.025)
upper <- quantile(ggpoll$poll, probs = 0.975)
middle.range <- upper-lower

#approximate the margin of error
error.margin <- middle.range/2 #divide the range by 2
error.percentage <- error.margin*100 #convert error into percentage

#double the sample size and perform the same computation
double.serveyed <- 2008 #new sample size

#generate the poll completion 10,000 times
poll.double <- rbinom(n.repeat, double.serveyed, probability)

#put data into tibble for plotting
ggpoll.double <- tibble(poll.double)|>
  mutate(poll = poll.double/double.serveyed) #convert the data into proportion

#plot a histogram of sample proportion with superimposed density
plot.poll.double <- ggplot(data = ggpoll.double)+
  geom_histogram(aes(x = poll, y = after_stat(density)),
                 color="grey",
                 bins = 25)+ #histogram of generated polls
  geom_density(aes(x = poll),
               color = "red")+ #superimposed density
  scale_x_continuous(breaks = seq(0.3, 0.5, by = 0.03))+ #put ticks on y-axis
  geom_hline(yintercept = 0)+ #add y-intercept
  theme_bw()+
  xlab("Proportion of satisfied adult sample (n = 2008)")+ #add x and y label
  ylab("Density")

#calculate the range of middle 95%
lower2 <- quantile(ggpoll.double$poll, probs = 0.025)
upper2 <- quantile(ggpoll.double$poll, probs = 0.975)
middle.range2 <- upper2-lower2

#approximate the margin of error
error.margin2 <- middle.range2/2 #divide the range by 2
error2.percentage <-error.margin2*100 #convert error into percentage

################################################################################
# TASK 2: Resampling
################################################################################
#create a data frame with data from Gallup survey
Gallup.survey <- tibble(id = 1:n.serveyed,
                        response = rep(NA, n.serveyed))

#input 39% satisfied, 59% dissatisfied, 2% no opinion
num.satisfied <- round(n.serveyed*0.39)
num.dissatisfied <- round(n.serveyed*0.59)
num.noop <- round(n.serveyed*0.02)

#input the satisfaction data 
for (i in 1:n.serveyed){
  if (i <= num.satisfied){ #input satisfied people
    Gallup.survey$response[i] = 1
  } else{ #input dissatisfied and no opinion people
    Gallup.survey$response[i] = 0
  }
}

#perform resampling
R <- 1000
resamples <- tibble(mean = numeric(R))

for (i in 1:R){ #do R resamples
  #collect a random resample
  curr.resample <- sample(x = Gallup.survey$response,
                     size = n.serveyed,
                     replace = T)
  #compute statistics on the resample
  resamples$mean[i] <- mean(curr.resample)
}

#plot a histogram of resulting proportions from resampling
resample.hist <- ggplot(data = resamples)+
  geom_histogram(aes(x=mean, y = after_stat(density)),
                 color="grey",
                 bins = 20)+ #histogram of resample statistics
  geom_density(aes(x= mean), color = "red")+ #superimpose density
  theme_bw()+ #remove gray background
  geom_hline(yintercept = 0)+
  ylab("Density")+ #add x and y labels 
  xlab("Means calculated by resampling")
      
#calculate the range of middle 95%
lower.resample <- quantile(resamples$mean, probs = 0.025)
upper.resample <- quantile(resamples$mean, probs = 0.975)
middle.range.resample <- upper.resample-lower.resample

#approximate the margin of error
error.margin.resample <- middle.range.resample/2 #divide the range by 2
error.resamp.percent <- error.margin.resample*100 #convert error to percentage

################################################################################
# TASK 3: Simulation over n and p
################################################################################
#specify range for n and p
n.range <- seq(100, 3000, by = 10)
p.range <- seq(0.01, 0.99, by = 0.01)

#vector to store error value for each n and p
error.storage <- tibble(
  "P" = rep(NA, length(n.range)*length(p.range)),
  "N" = rep(NA, length(n.range)*length(p.range)),
  "Error" = rep(NA, length(n.range)*length(p.range))
)

#run 10000 simulations for each n and p
curr.index = 0
for (n.curr in n.range){
  for (p.curr in p.range){
    curr.index = curr.index +1
    #generate the poll completion 10,000 times
    poll.curr <- rbinom(n.repeat, n.curr, p.curr)
    
    #calculate the range of middle 95%
    lower.curr <- quantile((poll.curr/n.curr)*100, probs = 0.025)
    upper.curr <- quantile((poll.curr/n.curr)*100, probs = 0.975)
    middle.range.curr <- upper.curr-lower.curr
    
    #approximate and store the margin of error
    error.storage$P[curr.index] = p.curr
    error.storage$N[curr.index] = n.curr
    error.storage$Error[curr.index] <- middle.range.curr/2 #divide the range by 2
  }
}

#summarize the results with a plot
error.plot <- ggplot(data = error.storage)+
  geom_raster(aes(x = N, y = P, fill = Error))+ #plot error based on r and n
  theme_bw()+
  labs(fill = "Margin of Error (%)")+ #label the legend
  theme(legend.position = "bottom") #move legend to the bottom

ggsave("error.pdf", error.plot, width = 5, height = 5)

################################################################################
# TASK 4: Actual margin of error calculation
################################################################################
#alpha for the confidence interval 
alpha <- 0.05
#z value of standard normal distribution
z.critical <- qnorm(1-alpha/2)
z.squared <- z.critical^2

#vector to store margin of error for each n and p
error.margin.storage <- tibble(
  "P" = rep(NA, length(n.range)*length(p.range)),
  "N" = rep(NA, length(n.range)*length(p.range)),
  "Margin.of.Error" = rep(NA, length(n.range)*length(p.range))
)

#compute Wilson margin of error for each n and p
curr.index = 0
for (n.curr in n.range){
  for (p.curr in p.range){
    curr.index = curr.index + 1  #increment index
    #plug values into the formula for Wilson margin of error
    wilson.curr <- (z.critical*sqrt(n.curr*p.curr*(1-p.curr) +
                                      z.squared/4))/
      (n.curr + z.squared)
    
    #store values in the tibble
    error.margin.storage$P[curr.index] = p.curr
    error.margin.storage$N[curr.index] = n.curr
    error.margin.storage$Margin.of.Error[curr.index] <- wilson.curr*100
  }
}

#summarize the results with a plot
error.wilson.plot <- ggplot(data = error.margin.storage)+
  geom_raster(aes(x = N, y = P, fill = Margin.of.Error))+ #plot margin of error based on r and n
  theme_bw()+
  labs(fill = "Wilson margin of Error (%)")+ #label the legend
  theme(legend.position = "bottom") #move legend to the bottom

ggsave("error.wilson.pdf", error.wilson.plot, width = 5, height = 5)

