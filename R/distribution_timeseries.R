# this is the script used for fitting distributions to the german numbers
library(fitdistrplus)
library(patchwork)
library(qqplotr)
source("R/preprocess_timeseries.R")
# create the cullen and frey graph
set.seed(420)
descdist(ts_germany$new_cases, discrete = TRUE)
# try poission, negative binomial, logistic and normal distribution
fit_poisson <- fitdist(ts_germany$new_cases, "pois")
fit_nbinomial <- fitdist(ts_germany$new_cases, "nbinom")
fit_normal <- fitdist(ts_germany$new_cases, "norm")
fit_logistic <- fitdist(ts_germany$new_cases, "logis")
# get the ecdf function
ecdf_new_cases <- ecdf(ts_germany$new_cases)
ts_germany$ecdf <- ecdf_new_cases(ts_germany$new_cases)
# draw a sample based on the estimates
x_nbinom <- rnbinom(
  nrow(ts_germany),
  size = fit_nbinomial$estimate[1],
  mu = fit_nbinomial$estimate[2]
)
x_normal <- rnorm(
  nrow(ts_germany),
  mean = fit_normal$estimate[1],
  sd = fit_normal$estimate[2]
)
x_poisson <- rpois(
  nrow(ts_germany),
  lambda = fit_poisson$estimate[1]
)
x_logistic <- rlogis(
  nrow(ts_germany),
  location = fit_logistic$estimate[1],
  scale = fit_logistic$estimate[2]
)
# get the ecdf functions
ecdf_nbinom <- ecdf(x_nbinom)
ecdf_normal <- ecdf(x_normal)
ecdf_poisson <- ecdf(x_poisson)
# create the qq plots
ecdf_nbinom <- ecdf(x_nbinom)
ecdf_normal <- ecdf(x_normal)
ecdf_poisson <- ecdf(x_poisson)
ecdf_logistic <- ecdf(x_logistic)
qqplot_nbinom <- ggplot(
  data = ts_germany,
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
  ggtitle("QQ-Plot for Germany", "Negative binomial distribution")

qqplot_normal <- ggplot(
  data = ts_germany,
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
  ggtitle("QQ-Plot for Germany", "Normal distribution")

qqplot_poisson <- ggplot(
  data = ts_germany,
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
  ggtitle("QQ-Plot for Germany", "Poisson distribution")

qqplot_logistic <- ggplot(
  data = ts_germany,
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
  ggtitle("QQ-Plot for Germany", "Logistic distribution")
# create the cdf plots
cdf_plot_nbinom <- ggplot() +
  geom_point(
    aes(
      x = ts_germany$new_cases,
      y = ts_germany$ecdf,
      colour = "Emp. CDF"
    )
  ) +
  geom_line(
    aes(
      x = sort(ts_germany$new_cases),
      y = sort(ecdf_nbinom(ts_germany$new_cases)),
      colour = "Theo. CDF"
    ),
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF", colour = "Type") +
  theme_minimal() +
  scale_colour_manual(
    values = c("red", "#2D3047")
  ) +
  ggtitle("Emp. and theo. CDFs for Germany", "Negative binomial distribution")

cdf_plot_normal <- ggplot() +
  geom_point(
    aes(
      x = ts_germany$new_cases,
      y = ts_germany$ecdf,
      colour = "Emp. CDF"
    )
  ) +
  geom_line(
    aes(
      x = sort(ts_germany$new_cases),
      y = sort(ecdf_normal(ts_germany$new_cases)),
      colour = "Theo. CDF"
    ),
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF", colour = "Type") +
  theme_minimal() +
  scale_colour_manual(
    values = c("red", "#2D3047")
  ) +
  ggtitle("Emp. and theo. CDFs for Germany", "Normal distribution")

cdf_plot_poisson <- ggplot() +
  geom_point(
    aes(
      x = ts_germany$new_cases,
      y = ts_germany$ecdf,
      colour = "Emp. CDF"
    )
  ) +
  geom_line(
    aes(
      x = sort(x_poisson),
      y = sort(ecdf_poisson(ts_germany$new_cases)),
      colour = "Theo. CDF"
    ),
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF", colour = "Type") +
  theme_minimal() +
  scale_colour_manual(
    values = c("red", "#2D3047")
  ) +
  ggtitle("Emp. and theo. CDFs for Germany", "Poisson distribution")

cdf_plot_logistic <- ggplot() +
  geom_point(
    aes(
      x = ts_germany$new_cases,
      y = ts_germany$ecdf,
      colour = "Emp. CDF"
    )
  ) +
  geom_line(
    aes(
      x = sort(x_logistic),
      y = sort(ecdf_poisson(ts_germany$new_cases)),
      colour = "Theo. CDF"
    ),
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF", colour = "Type") +
  theme_minimal() +
  scale_colour_manual(
    values = c("red", "#2D3047")
  ) +
  ggtitle("Emp. and theo. CDFs for Germany", "Logistic distribution")

# combine both plots
qqplot_nbinom + cdf_plot_nbinom
qqplot_normal + cdf_plot_normal
qqplot_poisson + cdf_plot_poisson
qqplot_logistic + cdf_plot_logistic
# get the aics
fit_poisson$aic
fit_nbinomial$aic
fit_normal$aic
# draw distribution based on estimates
x_nbinom <- rnbinom(
  10 * nrow(ts_germany),
  size = fit_nbinomial$estimate[1],
  mu = fit_nbinomial$estimate[2]
)
x_normal <- rnorm(
  10 * nrow(ts_germany),
  mean = fit_normal$estimate[1],
  sd = fit_normal$estimate[2]
)
x_poisson <- rpois(
  10 * nrow(ts_germany),
  lambda = fit_poisson$estimate[1]
)
# tibble for plotting
distr <- tibble(
  new_cases = c(x_nbinom, x_normal, x_poisson),
  distribution = c(
    rep("Negative binomial", 10 * nrow(ts_germany)),
    rep("Normal", 10 * nrow(ts_germany)),
    rep("Poisson", 10 * nrow(ts_germany))
  )
)
# plot the distributions
distrplot_1 <- ggplot() +
  geom_histogram(
    data = ts_germany,
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
  xlim(c(-20000, 40000)) +
  theme(
    legend.position = "none"
  )
distrplot_2 <- ggplot() +
  geom_histogram(
    data = ts_germany,
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
    subtitle = "Germany"
  )
distrplot_2 +
  ggtitle(
    label = "Distribution fits for the number of infections",
    subtitle = "Germany"
  )

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
      y = ts_norway$ecdf,
      colour = "Emp. CDF"
    )
  ) +
  geom_line(
    aes(
      x = sort(ts_norway$new_cases),
      y = sort(ecdf_nbinom(ts_norway$new_cases)),
      colour = "Theo. CDF"
    ),
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF", colour = "Type") +
  theme_minimal() +
  scale_colour_manual(
    values = c("red", "#2D3047")
  ) +
  ggtitle("Emp. and theo. CDFs for Norway", "Negative binomial distribution")

cdf_plot_normal <- ggplot() +
  geom_point(
    aes(
      x = ts_norway$new_cases,
      y = ts_norway$ecdf,
      colour = "Emp. CDF"
    )
  ) +
  geom_line(
    aes(
      x = sort(ts_norway$new_cases),
      y = sort(ecdf_normal(ts_norway$new_cases)),
      colour = "Theo. CDF"
    ),
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF", colour = "Type") +
  theme_minimal() +
  scale_colour_manual(
    values = c("red", "#2D3047")
  ) +
  ggtitle("Emp. and theo. CDFs for Norway", "Normal distribution")

cdf_plot_poisson <- ggplot() +
  geom_point(
    aes(
      x = ts_norway$new_cases,
      y = ts_norway$ecdf,
      colour = "Emp. CDF"
    )
  ) +
  geom_line(
    aes(
      x = sort(x_poisson),
      y = sort(ecdf_poisson(ts_norway$new_cases)),
      colour = "Theo. CDF"
    ),
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF", colour = "Type") +
  theme_minimal() +
  scale_colour_manual(
    values = c("red", "#2D3047")
  ) +
  ggtitle("Emp. and theo. CDFs for Norway", "Poisson distribution")

cdf_plot_logistic <- ggplot() +
  geom_point(
    aes(
      x = ts_norway$new_cases,
      y = ts_norway$ecdf,
      colour = "Emp. CDF"
    )
  ) +
  geom_line(
    aes(
      x = sort(x_logistic),
      y = sort(ecdf_poisson(ts_norway$new_cases)),
      colour = "Theo. CDF"
    ),
    size = 0.8
  ) +
  labs(x = "Number of infections", y = "CDF", colour = "Type") +
  theme_minimal() +
  scale_colour_manual(
    values = c("red", "#2D3047")
  ) +
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
    rep("Negative binomial", 10 * nrow(ts_norway)),
    rep("Normal", 10 * nrow(ts_norway)),
    rep("Poisson", 10 * nrow(ts_norway))
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
    binwidth = 750
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
