library(fitdistrplus)
library(qqplotr)
library(patchwork)
source("R/preprocess_germany.R")
# create the cullen and frey graph
set.seed(420)
descdist(newest_numbers$value, discrete = TRUE)
# try poission, negative binomial and normal distribution
fit_poisson <- fitdist(newest_numbers$value, "pois")
fit_nbinomial <- fitdist(newest_numbers$value, "nbinom")
fit_normal <- fitdist(newest_numbers$value, "norm")
ecdf_value <- ecdf(newest_numbers$value)
newest_numbers$ecdf <- ecdf_value(newest_numbers$value)
x_nbinom <- rnbinom(nrow(newest_numbers), size = fit_nbinomial$estimate[1], mu = fit_nbinomial$estimate[2])
x_normal <- rnorm(nrow(newest_numbers), mean = fit_normal$estimate[1], sd = fit_normal$estimate[2])
x_poisson <- rpois(nrow(newest_numbers), lambda = fit_poisson$estimate[1])
ecdf_nbinom <- ecdf(x_nbinom)
ecdf_normal <- ecdf(x_normal)
ecdf_poisson <- ecdf(x_poisson)
qqplot_nbinom <- ggplot(
  data = newest_numbers,
  mapping = aes(
    sample = value
  )
) +
  stat_qq_band(
    distribution = "nbinom",
    dparams = list(
      size = fit_nbinomial$estimate[1],
      mu = fit_nbinomial$estimate[2]
    )
  ) +
  stat_qq_line(
    distribution = "nbinom",
    dparams = list(
      size = fit_nbinomial$estimate[1],
      mu = fit_nbinomial$estimate[2]
    )
  ) +
  stat_qq_point(
    distribution = "nbinom",
    dparams = list(
      size = fit_nbinomial$estimate[1],
      mu = fit_nbinomial$estimate[2]
    )
  ) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() +
  ggtitle("QQ-Plot for Germany", "Negative binomial distribution")

qqplot_normal <- ggplot(
  data = newest_numbers,
  mapping = aes(
    sample = value
  )
) +
  stat_qq_band(
    distribution = "norm",
    dparams = list(
      mean = fit_normal$estimate[1],
      sd = fit_normal$estimate[2]
    )
  ) +
  stat_qq_line(
    distribution = "norm",
    dparams = list(
      mean = fit_normal$estimate[1],
      sd = fit_normal$estimate[2]
    )
  ) +
  stat_qq_point(
    distribution = "norm",
    dparams = list(
      mean = fit_normal$estimate[1],
      sd = fit_normal$estimate[2]
    )
  ) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() +
  ggtitle("QQ-Plot for Germany", "Normal distribution")

qqplot_poisson <- ggplot(
  data = newest_numbers,
  mapping = aes(
    sample = value
  )
) +
  stat_qq_band(
    distribution = "pois",
    dparams = list(
      lambda = fit_poisson$estimate[1]
    )
  ) +
  stat_qq_line(
    distribution = "pois",
    dparams = list(
      lambda = fit_poisson$estimate[1]
    )
  ) +
  stat_qq_point(
    distribution = "pois",
    dparams = list(
      lambda = fit_poisson$estimate[1]
    )
  ) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() +
  ggtitle("QQ-Plot for Germany", "Poisson distribution")

cdf_plot_nbinom <- ggplot() +
  geom_point(
    aes(
      x = newest_numbers$value,
      y = newest_numbers$ecdf
    )
  ) +
  geom_line(
    aes(
      x = sort(newest_numbers$value),
      y = sort(ecdf_nbinom(newest_numbers$value))
    ),
    colour = "red",
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF") +
  theme_minimal() +
  ggtitle("Emp. and theo. CDFs for Germany", "Negative binomial distribution")

cdf_plot_normal <- ggplot() +
  geom_point(
    aes(
      x = newest_numbers$value,
      y = newest_numbers$ecdf
    )
  ) +
  geom_line(
    aes(
      x = sort(newest_numbers$value),
      y = sort(ecdf_normal(newest_numbers$value))
    ),
    colour = "red",
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF") +
  theme_minimal() +
  ggtitle("Emp. and theo. CDFs for Germany", "Normal distribution")

cdf_plot_poisson <- ggplot() +
  geom_point(
    aes(
      x = newest_numbers$value,
      y = newest_numbers$ecdf
    )
  ) +
  geom_line(
    aes(
      x = sort(x_poisson),
      y = sort(ecdf_poisson(newest_numbers$value))
    ),
    colour = "red",
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF") +
  theme_minimal() +
  ggtitle("Emp. and theo. CDFs for Germany", "Poisson distribution")


qqplot_nbinom + cdf_plot_nbinom
qqplot_normal + cdf_plot_normal
qqplot_poisson + cdf_plot_poisson
# plot the fits
# compare aic
fit_poisson$aic
fit_nbinomial$aic
fit_normal$aic
# https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best