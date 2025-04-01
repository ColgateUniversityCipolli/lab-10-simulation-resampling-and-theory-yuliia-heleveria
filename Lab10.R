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





