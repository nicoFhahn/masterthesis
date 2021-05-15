library(fitdistrplus)
library(qqplotr)
library(patchwork)
source("R/preprocess_timeseries.R")
ts_norway$id_date <- seq_len(nrow(ts_norway))
ts_norway$population <- sum(read_csv("wrangled_data/norge_features.csv")$population_total[!duplicated(read_csv("wrangled_data/norge_features.csv")$population_total)])
expected <- expected(
  ts_norway$population,
  ts_norway$new_cases,
  n.strata = 1
)
ts_norway$e <- expected
# create the cullen and frey graph
set.seed(420)
descdist(ts_norway$new_cases, discrete = TRUE)
# try poission, negative binomial and normal distribution
fit_poisson <- fitdist(ts_norway$new_cases, "pois")
fit_nbinomial <- fitdist(ts_norway$new_cases, "nbinom")
fit_normal <- fitdist(ts_norway$new_cases, "norm")
fit_logistic <- fitdist(ts_norway$new_cases, "logis")
ecdf_new_cases <- ecdf(ts_norway$new_cases)
ts_norway$ecdf <- ecdf_new_cases(ts_norway$new_cases)
x_nbinom <- rnbinom(
  nrow(ts_norway),
  size = fit_nbinomial$estimate[1],
  mu = fit_nbinomial$estimate[2]
)
x_normal <- rnorm(
  nrow(ts_norway),
  mean = fit_normal$estimate[1],
  sd = fit_normal$estimate[2]
)
x_poisson <- rpois(
  nrow(ts_norway),
  lambda = fit_poisson$estimate[1]
)
x_logistic <- rlogis(
  nrow(ts_norway),
  location = fit_logistic$estimate[1],
  scale = fit_logistic$estimate[2]
)
ecdf_nbinom <- ecdf(x_nbinom)
ecdf_normal <- ecdf(x_normal)
ecdf_poisson <- ecdf(x_poisson)
ecdf_logistic <- ecdf(x_logistic)
qqplot_nbinom <- ggplot(
  data = ts_norway,
  mapping = aes(
    sample = new_cases
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
  data = ts_norway,
  mapping = aes(
    sample = new_cases
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
  data = ts_norway,
  mapping = aes(
    sample = new_cases
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

qqplot_logistic <- ggplot(
  data = ts_norway,
  mapping = aes(
    sample = new_cases
  )
) +
  stat_qq_band(
    distribution = "logis",
    dparams = list(
      location = fit_logistic$estimate[1],
      scale = fit_logistic$estimate[2]
    )
  ) +
  stat_qq_line(
    distribution = "logis",
    dparams = list(
      location = fit_logistic$estimate[1],
      scale = fit_logistic$estimate[2]
    )
  ) +
  stat_qq_point(
    distribution = "logis",
    dparams = list(
      location = fit_logistic$estimate[1],
      scale = fit_logistic$estimate[2]
    )
  ) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal() +
  ggtitle("QQ-Plot for Norway", "Logistic distribution")

cdf_plot_nbinom <- ggplot() +
  geom_point(
    aes(
      x = ts_norway$new_cases,
      y = ts_norway$ecdf
    )
  ) +
  geom_line(
    aes(
      x = sort(ts_norway$new_cases),
      y = sort(ecdf_nbinom(ts_norway$new_cases))
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
      x = ts_norway$new_cases,
      y = ts_norway$ecdf
    )
  ) +
  geom_line(
    aes(
      x = sort(ts_norway$new_cases),
      y = sort(ecdf_normal(ts_norway$new_cases))
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
      x = ts_norway$new_cases,
      y = ts_norway$ecdf
    )
  ) +
  geom_line(
    aes(
      x = sort(x_poisson),
      y = sort(ecdf_poisson(ts_norway$new_cases))
    ),
    colour = "red",
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF") +
  theme_minimal() +
  ggtitle("Emp. and theo. CDFs for Norway", "Poisson distribution")

cdf_plot_logistic <- ggplot() +
  geom_point(
    aes(
      x = ts_norway$new_cases,
      y = ts_norway$ecdf
    )
  ) +
  geom_line(
    aes(
      x = sort(x_logistic),
      y = sort(ecdf_poisson(ts_norway$new_cases))
    ),
    colour = "red",
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF") +
  theme_minimal() +
  ggtitle("Emp. and theo. CDFs for Norway", "Logistic distribution")


qqplot_nbinom + cdf_plot_nbinom
qqplot_normal + cdf_plot_normal
qqplot_poisson + cdf_plot_poisson
qqplot_logistic + cdf_plot_logistic
# plot the fits
# compare aic
fit_poisson$aic
fit_nbinomial$aic
fit_normal$aic
x_nbinom <- rnbinom(
  10 * nrow(ts_norway),
  size = fit_nbinomial$estimate[1],
  mu = fit_nbinomial$estimate[2]
)
x_normal <- rnorm(
  10 * nrow(ts_norway),
  mean = fit_normal$estimate[1],
  sd = fit_normal$estimate[2]
)
x_poisson <- rpois(
  10 * nrow(ts_norway),
  lambda = fit_poisson$estimate[1]
)

distr <- tibble(
  new_cases = c(x_nbinom, x_normal, x_poisson),
  distribution = c(
    rep("Negative binomial", 4990),
    rep("Normal", 4990),
    rep("Poisson", 4990)
  )
)
distrplot_1 <- ggplot() +
  geom_histogram(
    data = ts_norway,
    aes(new_cases, y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = 750
  ) +
  geom_density(
    data = distr,
    aes(
      new_cases,
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
  xlim(c(-20000, 40000)) +
  theme(
    legend.position = "none"
  )
ggplot() +
  geom_histogram(
    data = ts_norway,
    aes(new_cases, y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = 1250
  ) +
  geom_density(
    data = distr[distr$distribution != "Poisson", ],
    aes(
      new_cases,
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
  xlim(c(-20000, 40000))

distrplot_1 + distrplot_2 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Distribution fits for the number of infections",
    subtitle = "Norway"
  )
distrplot_2 +
  ggtitle(
    label = "Distribution fits for the number of infections",
    subtitle = "Norway"
  )
