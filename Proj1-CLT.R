
## -------------------------------------------------------------------------
#                           Project Code                                    
## -------------------------------------------------------------------------
                   
#load libraries
library(ggplot2)
# Exponential Distribution Simulation
lambda <- 0.2
sims<-1000
pop <-40
#Distribution Stats
mean_dist <- (1/lambda)
sd_dist <- (1/lambda)
var_dist <- sd_dist^2

# sample sims*pop random numbers from the distribution 
exp_data<-rexp(sims*pop,lambda)

#Store it in a matrix with dimensions 1000 x 40 with each of 1000 rows of 40 elements each representing a sample
samp_exp_m <- matrix(exp_data,sims,pop)

# Generate Distribution of sample means - 1000 such means computed from 40 element samples
means_samp_vect <- apply(samp_exp_m,MARGIN = 1,FUN = mean)


# Stats for the distribution of sample means
mean_simulate <- mean(means_samp_vect)
sd_simulate  <- sd(means_samp_vect)
var_simulate <-sd_simulate^2

# Mean, Standard Deviation and Variance based on CLT
mean_clt_theory <-1/lambda              # Expected Value of mean of samples approximates mean of distribution
sd_clt_theory <- 1/(lambda*sqrt(pop))   # Standard Deviation of sample is sigma/sqrt(N) where N is sample size 
var_clt_theory <- 1/((lambda^2)*pop)

# Plot the Distribution of Sample Means and Expected Value of such Mean
# Show the sample mean and compare it to the theoretical mean of the distribution.
MeanDist <- ggplot(data = as.data.frame(means_samp_vect),mapping = aes(x = means_samp_vect, y=..density..))+
    geom_histogram(fill = 'grey',bins = 100, color= 'black') +
    geom_density(size =2, color="blue")+
    labs(title = " Distribution of Sample Means,Expected Value of Sample Mean,Distribution Mean from Exponential Distribution",x = 'Sample Means', y='Density')+
    geom_vline(aes(xintercept = mean_simulate, colour= "exp.sample.mean"), size = 2,show.legend = TRUE)+
    geom_vline(aes(xintercept = mean_clt_theory,color= "dist.mean"), size = 2,show.legend = TRUE)+
    scale_color_manual(name="Means", values=c(exp.sample.mean='red',dist.mean='green'))

print(MeanDist)         # plot of distribution
print("Expected Value of Distribution of Sample Means")
print(mean_simulate)    # expected value of sample means
print("Expected Value of Distribution of Sample Means as predicted by CLT")
print(mean_clt_theory)  # expected value of the sample means as predicted by CLT
print("Mean of underlying Exponential Distribution")
print(mean_dist)        # mean of exponential distribution

# Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
print("Variance of distribution of sample means")
print(var_simulate)   # Variance of distribution of sample means
print("Variance of distribution of sample means as predicted by CLT")
print(var_clt_theory) # Variance of distribution of sample means as predicted by CLT
print("Variance of underlying Exponential Distribution")
print(var_dist)       # Variance of exponential distribution


# Generate 1000 random exponentials. Compute mean and sd . 
rexp_sample <- rexp(1000,rate = lambda)
rexp_sample_mean <- mean(rexp_sample)
#rexp_sample_sd <- sd(rexp_sample)
#rexp_sample_var <-rexp_sample_sd^2

#Plot the exponential distribution
ExDist <- ggplot(data = as.data.frame(rexp_sample),mapping = aes(x = rexp_sample, y =..density..))+
    geom_histogram(fill = 'blue',bins = 100, color= 'black') +
    geom_density(size =2, color="salmon")+
    labs(title = " Sample Mean vs Theoretical Mean of Exponential Distribution",x = 'Sample Values', y='Density')+
    geom_vline(aes(xintercept = rexp_sample_mean, colour= "sample.mean"), size = 2,show.legend = TRUE)+
    geom_vline(aes(xintercept = mean_clt_theory,color= "theory.mean"), size = 2,show.legend = TRUE)+
    scale_color_manual(name="Means", values=c(sample.mean='red',theory.mean='green'))

print(ExDist)

#Plot the exponential distribution vs Distribution of Means
CompDist <- ggplot()+
    geom_histogram(data = as.data.frame(rexp_sample),mapping = aes(x = rexp_sample, y =..density..),fill = 'blue',bins = 100, color= 'black') +
    geom_density( data = as.data.frame(means_samp_vect),mapping = aes(x = means_samp_vect, y =..density..),size =1, color="red")+
    labs(title = " Sample Distribution vs Distribution of Means vs Theoretical Mean",x = 'Values', y='Density')+
    geom_vline(aes(xintercept = mean_clt_theory),color= "black", size = 2)+
    coord_cartesian(xlim=c(0:10))

print(CompDist)
















