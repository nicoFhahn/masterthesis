library(fitdistrplus)
library(qqplotr)
library(patchwork)
library(tibble)
source("R/preprocess_norge.R")
# create the cullen and frey graph
set.seed(420)
descdist(newest_numbers$value, discrete = TRUE)
# try poission, negative binomial and normal distribution
fit_poisson <- fitdist(newest_numbers$value, "pois")
fit_nbinomial <- fitdist(newest_numbers$value, "nbinom")
fit_normal <- fitdist(newest_numbers$value, "norm")
# plot the fits
ecdf_value <- ecdf(newest_numbers$value)
newest_numbers$ecdf <- ecdf_value(newest_numbers$value)
x_nbinom <- rnbinom(
  nrow(newest_numbers),
  size = fit_nbinomial$estimate[1],
  mu = fit_nbinomial$estimate[2]
)
x_normal <- rnorm(
  nrow(newest_numbers),
  mean = fit_normal$estimate[1],
  sd = fit_normal$estimate[2]
)
x_poisson <- rpois(
  nrow(newest_numbers),
  lambda = fit_poisson$estimate[1]
)
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
  ggtitle("QQ-Plot for Norway", "Negative binomial distribution")

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
  ggtitle("QQ-Plot for Norway", "Normal distribution")

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
  ggtitle("QQ-Plot for Norway", "Poisson distribution")

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
  ggtitle("Emp. and theo. CDFs for Norway", "Negative binomial distribution")

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
  ggtitle("Emp. and theo. CDFs for Norway", "Normal distribution")

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
  ggtitle("Emp. and theo. CDFs for Norway", "Poisson distribution")


qqplot_nbinom + cdf_plot_nbinom
qqplot_normal + cdf_plot_normal
qqplot_poisson + cdf_plot_poisson
# compare aic
fit_poisson$aic
fit_nbinomial$aic
fit_normal$aic
x_nbinom <- rnbinom(
  10 * nrow(newest_numbers),
  size = fit_nbinomial$estimate[1],
  mu = fit_nbinomial$estimate[2]
)
x_normal <- rnorm(
  10 * nrow(newest_numbers),
  mean = fit_normal$estimate[1],
  sd = fit_normal$estimate[2]
)
x_poisson <- rpois(
  10 * nrow(newest_numbers),
  lambda = fit_poisson$estimate[1]
)

distr <- tibble(
  value = c(x_nbinom, x_normal, x_poisson),
  distribution = c(
    rep("Negative binomial", 3560),
    rep("Normal", 3560),
    rep("Poisson", 3560)
  )
)
distrplot_1 <- ggplot() +
  geom_histogram(
    data = newest_numbers,
    aes(value, y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = 200
  ) +
  geom_density(
    data = distr,
    aes(
      value,
      colour = distribution
    ),
    fill = "#FF6666",
    alpha = 0.1,
    size = 1
  ) +
  theme_minimal() +
  xlab(
    "Number of infections"
  ) +
  ylab(
    "Density"
  ) +
  scale_colour_manual(
    values = c("#DA4167", "#19323C", "#85CB33")
  ) +
  guides(
    colour = guide_legend(
      title = "Distribution"
    )
  ) +
  xlim(c(-5000, 10000))
distrplot_2 <- ggplot() +
  geom_histogram(
    data = newest_numbers,
    aes(value, y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = 200
  ) +
  geom_density(
    data = distr[distr$distribution != "Poisson", ],
    aes(
      value,
      colour = distribution
    ),
    fill = "#FF6666",
    alpha = 0.1,
    size = 1
  ) +
  theme_minimal() +
  xlab(
    "Number of infections"
  ) +
  ylab(
    "Density"
  ) +
  scale_colour_manual(
    values = c("#DA4167", "#19323C", "#85CB33")
  ) +
  guides(
    colour = guide_legend(
      title = "Distribution"
    )
  ) +
  xlim(c(-5000, 10000)) +
  theme(
    legend.position = "none"
  )

distrplot_1 + distrplot_2 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Distribution fits for the number of infections",
    subtitle = "Norway"
  )
