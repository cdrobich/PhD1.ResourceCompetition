citation("nicheROVER")

# for Niche Size

library(devtools)

devtools::install_github("mlysy/nicheROVER", ref = "master")

# Updated Vignette
# http://htmlpreview.github.io/?https://github.com/mlysy/nicheROVER/master/doc/ecol-vignette.html
## for niche.size

library(nicheROVER)

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
                                                                       X = data[ii, 2:3]))
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
                                                                     X = data[ii, 2:3]))

# format data for plotting function
soil.data <- tapply(1:nrow(data), data$Species, function(ii) X = data[ii, 2:3])

niche.plot(niche.par = data.par, niche.data = soil.data, pfrac = 0.05, 
           iso.names = expression(PC1, PC2), 
           col = clrs, xlab = expression(""))





####### Estimating Niche Size ##########

# posterior distribution of (mu, Sigma) for each species
nsamples <- 1000
data.par <- tapply(1:nrow(data), data$Species, 
                   function(ii) niw.post(nsamples = nsamples, X = data[ii, 2:3]))


# posterior distribution of niche size by species
data.size <- sapply(data.par, function(spec) {
  apply(spec$Sigma, 3, niche.size, alpha = .95)
})

# point estimate and standard error
rbind(est = colMeans(data.size),
      se = apply(data.size, 2, sd))

#    Calamagrostis      Carex Phragmites      Typha
#est    0.07769107 0.18014774  0.3716952 0.30575692
#se     0.02152039 0.04817866  0.0989603 0.08162212

# boxplots
boxplot(data.size, col = clrs, pch = 16, cex = .5,
        ylab = "Niche Size", xlab = "Species")


###### Niche Overlap ##########

# niche overlap plots for 95% niche region sizes
nsamples <- 1000
data.par <- tapply(1:nrow(data), data$Species, 
                   function(ii) niw.post(nsamples = nsamples, X = data[ii, 2:3]))

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

#                                Species B
#Species A       Calamagrostis Carex Phragmites Typha
#Calamagrostis            NA 26.29      78.48  2.62
#Carex                  6.07    NA      93.09 32.33
#Phragmites            10.24 63.54         NA 51.18
#Typha                  0.36 18.25      62.77    NA

#, , alpha = 99%

#                             Species B
#Species A       Calamagrostis Carex Phragmites Typha
#Calamagrostis            NA 53.64      94.73 11.14
#Carex                 10.19    NA      97.71 54.33
#Phragmites            14.56 77.59         NA 69.13
#Typha                  0.61 32.68      80.36    NA


over.median <- apply(over.stat, c(1:2, 4), median) * 100
round(over.median, 2)

#, , alpha = 95%

#Species B
#Species A       Calamagrostis Carex Phragmites Typha
#Calamagrostis            NA  16.9      86.15  0.00
#Carex                   4.4    NA      95.60 30.80
#Phragmites              9.7  62.8         NA 52.45
#Typha                   0.1  16.2      64.55    NA

#, , alpha = 99%

#Species B
#Species A       Calamagrostis Carex Phragmites Typha
#Calamagrostis            NA 51.55       98.4  1.10
#Carex                   8.1    NA       99.2 57.70
#Phragmites             14.0 77.85         NA 71.65
#Typha                   0.2 30.25       84.0    NA


over.cred <- apply(over.stat * 100, c(1:2, 4), quantile, prob = c(0.025, 0.975), 
                   na.rm = TRUE)
round(over.cred[, , , 1])  # display alpha = .95 niche region

#, , Species B = Calamagrostis

#               Species A
#      Calamagrostis Carex Phragmites Typha
#2.5%             NA     0          2     0
#97.5%            NA    21         23     3

#, , Species B = Carex

#             Species A
#       Calamagrostis Carex Phragmites Typha
#2.5%              0    NA         40     3
#97.5%            88    NA         88    49

#, , Species B = Phragmites

#Species A
#      Calamagrostis Carex Phragmites Typha
#2.5%             20    74         NA    26
#97.5%           100   100         NA    93

#, , Species B = Typha

#Species A
#      Calamagrostis Carex Phragmites Typha
#2.5%              0     5         22    NA
#97.5%            32    80         86    NA

# Overlap plot with 95% alpha

over.stat <- overlap(data.par, nreps = nsamples, nprob = 1000, alpha = 0.95)
overlap.plot(over.stat, col = clrs, mean.cred.col = "black", equal.axis = TRUE, 
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")





