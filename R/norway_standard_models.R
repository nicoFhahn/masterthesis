library(ggplot2)
library(INLA)
source("R/preprocess_norge.R")
#####################################################
# basic model
# specify penalized prior
prior_1 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
prior_2 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)
  )
)
# formula for standard model
formula_1 <- value ~ f(idarea_1, model = "iid", hyper = prior_1) +
  pop_dens + urb_dens
formula_2 <- value ~ f(idarea_1, model = "iid", hyper = prior_2) +
  pop_dens + urb_dens
set.seed(345)
# calculate the models
res_1 <- inla(
  formula_1,
  data = newest_numbers,
  family = "nbinomial",
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE)
)
res_2 <- inla(
  formula_2,
  data = newest_numbers,
  family = "nbinomial",
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE)
)
# compare the DIC
res_1$dic$dic
res_2$dic$dic
summary(res_1)
plot(res_1)
res_1$summary.fixed
res_1$summary.random
res_1$summary.hyperpar
res_1$summary.fitted.values
res_1$summary.fitted.values[order(res_1$summary.fitted.values$mean, decreasing = TRUE), ][1:5, ]
newest_numbers[c(1, 112, 117, 237, 280), c("kommune_name", "value", "population", "sir")]
# get spline smoothing of the marginal density
alpha <- res_1$marginals.fixed[[1]]
ggplot(data.frame(inla.smarginal(alpha)), aes(x, y)) +
  geom_line() +
  theme_bw()


quant <- inla.qmarginal(0.05, alpha)
# plot the probability of alpha being lower than 0.05
ggplot(data.frame(inla.smarginal(alpha)), aes(x, y)) +
  geom_line() +
  geom_area(
    data = subset(data.frame(inla.smarginal(alpha)), x < quant),
    fill = "black") +
  theme_bw()
# plot the posterior distribution of alpha at 3.2
ggplot(data.frame(inla.smarginal(alpha)), aes(x, y)) +
  geom_line() +
  geom_vline(xintercept = 3.2, linetype = "dashed") +
  theme_bw()
marg.variance <- inla.tmarginal(
  function(x) 1/x,
  res_1$marginals.hyperpar$`Precision for idarea_1`
)
# plot the posterior distribution of the variance of the random effect
ggplot(data.frame(inla.smarginal(marg.variance)), aes(x, y)) +
  geom_line() +
  theme_bw()
# get summary of the marginal
inla.zmarginal(marg.variance)
list_marginals <- res_1$marginals.fitted.values
marginals <- data.frame(do.call(rbind, list_marginals))
marginals$kommune <- rep(
  names(list_marginals),
  times = sapply(list_marginals, nrow)
  )
ggplot(marginals[1:150, ], aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(~ kommune) +
  labs(x = "", y = "Density") +
  theme_bw()
rm(list=setdiff(ls(), "newest_numbers"))

