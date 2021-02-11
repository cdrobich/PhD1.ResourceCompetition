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


# multiply scores by variance explained
data.raw$PCA1.ad <- (data.raw$PC1 * 0.648)
data.raw$PCA2.ad <- (data.raw$PC2 * 0.08576)

data <- data.raw %>% 
  select(X,Species, PCA1.ad,PCA2.ad)

data$Species <- as.factor(data$Species)

str(data)


# generate parameter draws from the 'default' posteriors of each fish
nsamples <- 1000
system.time({
  data.par <- tapply(1:nrow(data), data$Species, function(ii) niw.post(nsamples = nsamples, 
                                                                       X = data[ii, 3:4]))
})

#user  system elapsed 
#0.08    0.00    0.08

# various parameter plots
clrs <- c("#084594", "#9ecae1", "#fb6a4a", "#6e016b")  # colors for each species

colours = c("Calamagrostis" = "#084594", 
            "Typha" = "#6e016b", 
            "Carex" = "#9ecae1", 
            "Phragmites" = "#fb6a4a")


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
                                                                     X = data[ii, 3:4]))

# format data for plotting function
soil.data <- tapply(1:nrow(data), data$Species, function(ii) X = data[ii, 3:4])

niche.plot(niche.par = data.par, niche.data = soil.data, pfrac = 0.05, 
           iso.names = expression(PC1, PC2), 
           col = clrs, xlab = expression(""))





####### Estimating Niche Size ##########

# posterior distribution of (mu, Sigma) for each species
nsamples <- 1000
data.par <- tapply(1:nrow(data), data$Species, 
                   function(ii) niw.post(nsamples = nsamples, X = data[ii, 3:4]))


# posterior distribution of niche size by species
data.size <- sapply(data.par, function(spec) {
  apply(spec$Sigma, 3, niche.size, alpha = .95)
})

# point estimate and standard error
rbind(est = colMeans(data.size),
      se = apply(data.size, 2, sd))

#    Calamagrostis      Carex Phragmites     Typha
#est    0.07721194 0.18603562 0.35979881 0.30447676
#se     0.02134153 0.04774974 0.09810626 0.08312643

# boxplots
boxplot(data.size, col = clrs, pch = 16, cex = .5,
        ylab = "Niche Size", xlab = "Species")


###### Niche Overlap ##########

# niche overlap plots for 95% niche region sizes
nsamples <- 1000
data.par <- tapply(1:nrow(data), data$Species, 
                   function(ii) niw.post(nsamples = nsamples, X = data[ii, 3:4]))

# Overlap calculation.  use nsamples = nprob = 10000 (1e4) for higher
# accuracy.  the variable over.stat can be supplied directly to the
# overlap.plot function

over.stat <- overlap(data.par, nreps = nsamples, 
                     nprob = 1000, alpha = c(0.95, 0.99))

# The mean overlap metrics calculated across iteratations for both niche
# region sizes (alpha = .95 and alpha = .99) can be calculated and displayed
# in an array.
over.mean <- apply(over.stat, c(1:2, 4), mean) * 100
round(over.mean, 2)


over.median <- apply(over.stat, c(1:2, 4), median) * 100
round(over.median, 2)

#, , alpha = 95%

#                Species B
#Species A     Calamagrostis Carex Phragmites Typha
#Calamagrostis            NA 20.15       83.6  0.00
#Carex                   4.7    NA       94.9 31.90
#Phragmites              8.3 64.30         NA 53.45
#Typha                   0.1 16.50       61.3    NA

over.cred <- apply(over.stat * 100, c(1:2, 4), quantile, prob = c(0.025, 0.975), 
                   na.rm = TRUE)
round(over.cred[, , , 1])  # display alpha = .95 niche region

#, , Species B = Calamagrostis

#Species A
#      Calamagrostis Carex Phragmites Typha
#2.5%             NA     0          2     0
#97.5%            NA    18         22     2

#, , Species B = Carex

#Species A
#          Calamagrostis Carex Phragmites Typha
#2.5%              0    NA         42     3
#97.5%            83    NA         87    51

#, , Species B = Phragmites

#Species A
#      Calamagrostis Carex Phragmites Typha
#2.5%             15    74         NA    28
#97.5%           100   100         NA    94

#, , Species B = Typha

#Species A
#Calamagrostis Carex Phragmites Typha
#2.5%              0     6         25    NA
#97.5% 

# Overlap plot with 95% alpha

over.stat <- overlap(data.par, nreps = nsamples, nprob = 1000, alpha = 0.95)
overlap.plot(over.stat, col = clrs, mean.cred.col = "black", equal.axis = TRUE, 
             xlab = "Overlap Probability (%) -- Niche Region Size: 95%")





