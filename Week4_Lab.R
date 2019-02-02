# Lab 4: Introduction to stochastic exponential growth

# Install/load the PVA package
devtools::install_github("brucekendall/PVA")
library(PVA)

# Simulate a dataset from the stochastic exponential growth (SEG) model
pop.data <- data.frame(Year = 2000:2010, 
                       Nt = c(1000, simulateSEG(-0.0105, 0.2, 1000, 10)))

#-0.015 is the log of the geometric mean of lambad
# 0.2 is in log of lambda


# Plot Nt against year
#  1. Is the population growing or declining?

ggplot(data = pop.data) +
  geom_point(aes(x=Year, y=Nt))+
  geom_line(aes(x=Year, y=Nt))

#  2. Does the population appear to be growing/declining exponentially? 
#     HINT: plot Nt on a log scale!

pop.data.log <- pop.data %>%
  mutate(Ntlog= log(Nt))

ggplot(data = pop.data.log) +
  geom_point(aes(x=Year, y=Ntlog))+
  geom_line(aes(x=Year, y=Ntlog))

# Use a linear regression of Nt vs. Year to estimate the average trend in 
#   the population.

pop.data.reg <- lm(data=pop.data, Nt ~ Year)
summary(pop.data.reg)
pop.data.reg
##-15.64 slope

# Use the confint function to get a 95% CI for the trend estimate

confint(pop.data.reg)
#-116.7035 to 85.4306 95% CI

# Use a linear regression of log(Nt) vs. Year to estimate the average trend in the log abundance

pop.data.log.reg <- lm(data=pop.data.log, Ntlog ~ Year)
summary(pop.data.log.reg)
#slope = -0.006619


# Come to board and record, for your population:
# a. Final population size
# b. The trend estimate
# c. The 95% CI of the trend estimate
# d. Estimated trend in log abundance

##demo_sim_SEG()
