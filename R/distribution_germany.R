library(fitdistrplus)
source("R/preprocess_germany.R")
# create the cullen and frey graph
descdist(newest_numbers$CumNumberTestedIll, discrete = TRUE)
# try poission, negative binomial and normal distribution
fit_poisson <- fitdist(newest_numbers$CumNumberTestedIll, "pois")
fit_nbinomial <- fitdist(newest_numbers$CumNumberTestedIll, "nbinom")
fit_normal <- fitdist(newest_numbers$CumNumberTestedIll, "norm")
# plot the fits
plot(fit_poisson)
plot(fit_nbinomial)
plot(fit_normal)
# compare aic
fit_poisson$aic
fit_nbinomial$aic
fit_normal$aic
# https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best