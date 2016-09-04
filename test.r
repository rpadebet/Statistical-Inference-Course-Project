
## -------------------------------------------------------------------------
#                           Project Code                                    
## -------------------------------------------------------------------------
                   
#load libraries

library(ggplot2)
library(knitr)
library(scales)


# Exponential Distribution Simulation
lambda <- 0.2
sims<-1000
pop <-40

#Theoretical Stats
mean_theory <- (1/lambda)
sd_theory <- (1/lambda)

# sample sims*pop random numbers from the distribution 
exp_data<-rexp(sims*pop,lambda)

#Store it in a matrix with dimensions 1000 x 40 with each of 1000 rows of 40 elements each representing a sample
samp_exp_m <- matrix(exp_data,sims,pop)

# Compute mean and standard deviation for each of the 1000 simulations
means_samp_vect <- apply(samp_exp_m,MARGIN = 1,FUN = mean)
sd_samp_vect <- apply(samp_exp_m,MARGIN = 1,FUN = sd)

# Stats for the distribution of means
mean_simulate <- mean(means_samp_vect)
sd_simulate <- mean(sd_samp_vect)

# Plot the Distribution of means
MeanDist <- ggplot(data = as.data.frame(means_samp_vect),mapping = aes(x = means_samp_vect, y=..density..))+
    geom_histogram(fill = 'grey',bins = 100, color= 'black') +
    geom_density(size =2, color="blue")+
    labs(title = " Distribution of Sample Means of Exponential Distribution",x = 'Sample Means', y='Density')+
    geom_vline(aes(xintercept = mean_simulate, colour= "sample.mean"), size = 2,show.legend = TRUE)+
    geom_vline(aes(xintercept = mean_theory,color= "theory.mean"), size = 2,show.legend = TRUE)+
    scale_color_manual(name="Means", values=c(sample.mean='red',theory.mean='green'))

print(MeanDist)


# Plot the Distribution of Standard Errors of the Mean
SEDist <- ggplot(data = as.data.frame(sd_samp_vect),mapping = aes(x = sd_samp_vect,y=..density..))+
    geom_histogram(fill = 'grey',bins = 100, color= 'black') +
    geom_density(size =2, color="blue")+
    labs(title = " Distribution of Standard Errors of Mean of Exponential Distribution",x = 'Standard Errors', y='Density')+
    geom_vline(aes(xintercept = sd_simulate, colour= "sample.sd"), size = 2,show.legend = TRUE)+
    geom_vline(aes(xintercept = sd_theory,color= "theory.sd"), size = 2,show.legend = TRUE)+
    scale_color_manual(name="Means", values=c(sample.sd='red',theory.sd='green'))

print(SEDist)

# Generate 1000 random exponentials. Compute mean and sd of sample. 
rexp_sample <- rexp(1000,rate = lambda)
rexp_sample_mean <- mean(rexp_sample)
rexp_sample_sd <- sd(rexp_sample)

#Plot the exponential distribution
ExDist <- ggplot(data = as.data.frame(rexp_sample),mapping = aes(x = rexp_sample, y =..density..))+
    geom_histogram(fill = 'blue',bins = 100, color= 'black') +
    geom_density(size =2, color="salmon")+
    labs(title = " Sample Mean vs Theoretical Mean of Exponential Distribution",x = 'Sample Values', y='Density')+
    geom_vline(aes(xintercept = rexp_sample_mean, colour= "sample.mean"), size = 2,show.legend = TRUE)+
    geom_vline(aes(xintercept = mean_theory,color= "theory.mean"), size = 2,show.legend = TRUE)+
    scale_color_manual(name="Means", values=c(sample.mean='red',theory.mean='green'))

print(ExDist)

Mean<-c(rexp_sample_mean,mean_simulate,mean_theory)
StdError <- c(rexp_sample_sd,sd_simulate,sd_theory)
Variance<-StdError^2
Distribution <- c("Random Exponential Distribution","Simulated Distribution of Means","Theoretically Expected Result")

SummaryDF <- data.frame(Distribution,Mean,StdError,Variance)

SummaryDF







