library(nicheROVER)
citation("nicheROVER")


data.raw <- read.csv("Data/PCA scores_123.csv") # raw PCA scores
data.raw$Species <- as.factor(data.raw$Species)

data.raw <- data.raw %>% relocate(Species, .before = PC1)
data.raw <- data.raw %>% select(Species:PC3)


data <- read.csv("Data/PCA scores_123_adjusted.csv") # PCA scores x prop. variance explained
data$Species <- as.factor(data$Species)

str(data)


# generate parameter draws from the 'default' posteriors of each fish
nsamples <- 1000
system.time({
  data.par <- tapply(1:nrow(data), data$Species, function(ii) niw.post(nsamples = nsamples, 
                                                                       X = data[ii, 2:4]))
})

#user  system elapsed 
#0.67    0.00    0.68

# various parameter plots
clrs <- c("#525252", "#9ecae1", "#fb6a4a", "#6a51a3")  # colors for each species

# mu1 (del15N), mu2 (del13C), and Sigma12
par(mar = c(4, 4, 0.5, 0.1) + 0.1, mfrow = c(1, 3))
niche.par.plot(data.par, col = clrs, plot.index = 1)
niche.par.plot(data.par, col = clrs, plot.index = 2)
niche.par.plot(data.par, col = clrs, plot.index = 1:2)
legend("topright", legend = names(data.par), fill = clrs)



# all mu (PC1, PC2, PC3)
niche.par.plot(data.par, col = clrs, plot.mu = TRUE, plot.Sigma = FALSE)
legend("topright", legend = names(data.par), fill = clrs)

# all mu and sigma (PC1, PC2, PC3)
par(mar = c(4.2, 4.2, 2, 1) + 0.1)
niche.par.plot(data.par, col = clrs, plot.mu = TRUE, plot.Sigma = TRUE)



# 2-d projections of 10 niche regions
nsamples <- 10
data.par <- tapply(1:nrow(data), data$Species, function(ii) niw.post(nsamples = nsamples, 
                                                                     X = data[ii, 2:4]))

# format data for plotting function
soil.data <- tapply(1:nrow(data), data$Species, function(ii) X = data[ii, 2:4])

niche.plot(niche.par = data.par, niche.data = soil.data, pfrac = 0.05, 
           iso.names = expression(PC1, PC2, PC3), 
           col = clrs, xlab = expression("Environmental Variables"))

legend("topright", legend = names(data.par), fill = clrs)


# niche overlap plots for 95% niche region sizes
nsamples <- 1000
data.par <- tapply(1:nrow(data), data$Species, function(ii) niw.post(nsamples = nsamples, 
                                                                     X = data[ii, 2:4]))

# Overlap calculation.  use nsamples = nprob = 10000 (1e4) for higher
# accuracy.  the variable over.stat can be supplied directly to the
# overlap.plot function

over.stat <- overlap(data.par, nreps = nsamples, nprob = 1000, alpha = c(0.95, 
                                                                         0.99))

# The mean overlap metrics calculated across iteratations for both niche
# region sizes (alpha = .95 and alpha = .99) can be calculated and displayed
# in an array.
over.mean <- apply(over.stat, c(1:2, 4), mean) * 100
round(over.mean, 2)

#, , alpha = 95%

#                               Species B
#Species A       Calamagrostis Carex Phragmites Typha
#Calamagrostis            NA 32.21       6.29  3.59
#Carex                  2.67    NA      69.09 30.20
#Phragmites             0.32 56.46         NA 42.23
#Typha                  0.17 20.12      52.44    NA

#, , alpha = 99%

#                             Species B
#Species A       Calamagrostis Carex Phragmites Typha
#Calamagrostis            NA 58.80      22.02 14.59
#Carex                  4.72    NA      81.76 49.69
#Phragmites             0.64 71.80         NA 58.30
#Typha                  0.31 34.25      69.79    NA

over.cred <- apply(over.stat * 100, c(1:2, 4), quantile, prob = c(0.025, 0.975), 
                   na.rm = TRUE)
round(over.cred[, , , 1])  # display alpha = .95 niche region

#, , Species B = Calamagrostis

#Species A
#           Calamagrostis Carex Phragmites Typha
#2.5%             NA     0          0     0
#97.5%            NA    10          2     1

#, , Species B = Carex

#Species A
#Calamagrostis Carex Phragmites Typha
#2.5%              0    NA         30     4
#97.5%            87    NA         84    54

#, , Species B = Phragmites

#Species A
#           Calamagrostis Carex Phragmites Typha
#2.5%              0    44         NA    25
#97.5%            45    93         NA    84

#, , Species B = Typha

#Species A
#           Calamagrostis Carex Phragmites Typha
#2.5%              0     6         17    NA
#97.5%            46    74         77    NA


# Overlap plot with 95% alpha

over.stat <- overlap(data.par, nreps = nsamples, nprob = 1000, alpha = 0.95)
overlap.plot(over.stat, col = clrs, mean.cred.col = "black", equal.axis = TRUE, 
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")