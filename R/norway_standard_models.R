library(dplyr)
library(fitdistrplus)
library(ggplot2)
library(INLA)
library(leaflet)
library(MASS)
library(patchwork)
library(readr)
library(sf)
library(SpatialEpi)
library(spdep)
library(units)
#####################################################
# prepare the data
norge_features <- read_csv("wrangled_data/norge_features.csv")
norge_sf <- read_sf("wrangled_data/shapes_norge.shp")
norge <- merge(
  norge_features,
  norge_sf,
  by = "kommune_no"
)
norway_municipality_confirmed <- read_csv(
  "https://raw.githubusercontent.com/thohan88/covid19-nor-data/master/data/01_infected/msis/municipality_wide.csv"
)
norway_municipality_confirmed_long <- melt(
  setDT(norway_municipality_confirmed),
  id.vars = colnames(norway_municipality_confirmed)[1:6],
  variable.name = "date"
)
norway_municipality_confirmed_long$date <- as.Date(as.character(norway_municipality_confirmed_long$date))
newest_numbers <- norway_municipality_confirmed_long[norway_municipality_confirmed_long$date == max(norway_municipality_confirmed_long$date), ]
norge_demo <- read_delim("norge_data/Personer1.csv", ";")
norge_demo$age <- as.numeric(str_extract(norge_demo$age, "[0-9]{1,}"))
norge_demo$kommune_no <- str_extract(norge_demo$region, "[0-9]{4}")
newest_numbers <- merge(
  newest_numbers,
  norge_demo,
  by = "kommune_no"
)
# order them
newest_numbers <- newest_numbers[order(
  newest_numbers$kommune_no,
  newest_numbers$sex,
  newest_numbers$age
), ]
# group them
d <- newest_numbers %>%
  group_by(kommune_no) %>%
  summarise(
    count = sum(value)
  )
# calculate the expected count
expected_count <- expected(
  population = newest_numbers$population,
  cases = newest_numbers$value,
  n.strata = 212
)
d$expected_count <- expected_count
# calculate the SIR
d$sir <- d$count / d$expected_count
# use only the newest numbers
newest_numbers <- norge[norge$date == max(norge$date), ]
newest_numbers <- merge(
  newest_numbers,
  d,
  by = "kommune_no"
)
newest_numbers <- st_as_sf(newest_numbers)
st_crs(newest_numbers) <- 4326
# calculate the number of infected people
newest_numbers$inf_rate <- newest_numbers$value / newest_numbers$population
# add id area variables
newest_numbers$idarea_1 <- seq_len(nrow(newest_numbers))
newest_numbers$idarea_2 <- seq_len(nrow(newest_numbers))
# add the expected count
newest_numbers$area <- as.numeric(set_units(st_area(newest_numbers), km^2))
newest_numbers$pop_dens <- newest_numbers$population / newest_numbers$area
rm(list=setdiff(ls(), "newest_numbers"))
#####################################################
# check whether the data follows exponential distribution
descdist(newest_numbers$value, discrete = TRUE)
fit_poisson <- fitdist(newest_numbers$value, "pois")
fit_nbinomial <- fitdist(newest_numbers$value, "nbinom")
fit_normal <- fitdist(newest_numbers$value, "norm")
plot(fit_poisson)
plot(fit_nbinomial)
plot(fit_normal)
fit_poisson$aic
fit_nbinomial$aic
fit_normal$aic
rm(list=setdiff(ls(), "newest_numbers"))
# https://stats.stackexchange.com/questions/132652/how-to-determine-which-distribution-fits-my-data-best
#####################################################
# basic model
# specify penalized prior
prior_prec_1 <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
# formula for standard model
formula_1 <- value ~ f(idarea_1, model = "iid", hyper = prior_prec_1) +
  pop_dens
set.seed(345)
# calculate the model
res_1 <- inla(
  formula_1,
  data = newest_numbers,
  family = "nbinomial",
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE)
)
# get the summary
summary(res_1)
plot(res_1)
res_1$summary.fixed
res_1$summary.random
res_1$summary.hyperpar
res_1$summary.fitted.values
res_1$summary.fitted.values[order(res_1$summary.fitted.values$mean, decreasing = TRUE), ][1:5, ]
newest_numbers[c(1, 117, 112, 237, 96), c("kommune_name", "value", "population", "sir")]
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
#####################################################
head(newest_numbers)
newest_numbers$sex <- newest_numbers$population_female / newest_numbers$population_total
# create the neighbordhood matrix
nb <- poly2nb(newest_numbers)
head(nb)
nb2INLA("maps/map_1.adj", nb)
g <- inla.read.graph(filename = "maps/map_1.adj")
# specify the model formula
# we will start with demographic variables and pop density
formula_2 <- value ~ 
  # add the demographic vars and pop density
  pop_dens + sex + median_age +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE) +
  # random effects term
  f(idarea_2, model = "iid")
  
res_2 <- inla(
  formula_2,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)

prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01))
)
# now the same again with a pc prior
formula_3 <- value ~ 
  # add the demographic vars and pop density
  pop_dens + sex + median_age +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior) +
  # random effects term
  f(idarea_2, model = "iid")

res_3 <- inla(
  formula_3,
  family = "nbinomial",
  data = newest_numbers,
  E = expected_count,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)
res_2$dic$dic
res_3$dic$dic
summary(res_2)
summary(res_3)
options(scipen = 10)
res_3$summary.fixed
# look at the marginal
marginal_intercept <- data.frame(
  inla.smarginal(res_3$marginals.fixed$`(Intercept)`)
)
marginal_pop_dens <- data.frame(
  inla.smarginal(res_3$marginals.fixed$pop_dens)
)
marginal_sex <- data.frame(
  inla.smarginal(res_3$marginals.fixed$sex)
)
marginal_median_age <- data.frame(
  inla.smarginal(res_3$marginals.fixed$median_age)
)
marginal_plot_1 <- ggplot(marginal_intercept, aes(x = x, y = y)) +
  geom_line() +
  labs(x = expression(beta[9]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") +
  ggtitle(
    label = "Posterior distribution of the intercept"
  ) +
  theme_minimal()
marginal_plot_2 <- ggplot(marginal_pop_dens, aes(x = x, y = y)) +
  geom_line() +
  labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") +
  ggtitle(
    label = "Posterior distribution of the coefficient",
    subtitle = "Variable: 'Population density'"
  ) +
  theme_minimal()
marginal_plot_3 <- ggplot(marginal_sex, aes(x = x, y = y)) +
  geom_line() +
  labs(x = expression(beta[2]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") +
  ggtitle(
    label = "Posterior distribution of the coefficient",
    subtitle = "Variable: 'Proportion of females in population'"
  ) +
  theme_minimal()
marginal_plot_4 <- ggplot(marginal_median_age, aes(x = x, y = y)) +
  geom_line() +
  labs(x = expression(beta[3]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") +
  ggtitle(
    label = "Posterior distribution of the coefficient",
    subtitle = "Variable: 'Median age'"
  ) +
  theme_minimal()
(marginal_plot_1 +
  marginal_plot_2) /
  (marginal_plot_3 +
     marginal_plot_4)
head(res_3$summary.fitted.values[order(res_3$summary.fitted.values$mean, decreasing = TRUE), ])
newest_numbers[c(1, 265, 117, 115, 96, 96), ]
results_frame <- newest_numbers
results_frame$rr <- res_3$summary.fitted.values$mean
results_frame$q025 <- res_3$summary.fitted.values$`0.025quant`
results_frame$q5 <- res_3$summary.fitted.values$`0.5quant`
results_frame$q975 <- res_3$summary.fitted.values$`0.975quant`
pal_1 <- colorNumeric(
  c("#f4f4f9", "#b8dbd9", "#586f7c", "#2f4550", "#000000"),
  # colors depend on the count variable
  domain = results_frame$rr
)
leaflet(results_frame) %>%
  addTiles() %>%
  addPolygons(
    weight = 1,
    fillColor = ~pal_1(rr),
    fillOpacity = 0.7,
    color = "black",
    group = "Relative risk",
    label = paste(
      "Kommune: ", results_frame$kommune_name, "<br>",
      "Population: ", results_frame$population, "<br>",
      "Population density: ", results_frame$pop_dens, "<br>",
      "Proportion of females: ", results_frame$sex, "<br>",
      "Median age: ", results_frame$median_age, "<br>",
      "Number of infections: ", results_frame$value, "<br>",
      "Expected number of infections: ", round(results_frame$expected), "<br>",
      "SIR: ", round(results_frame$sir, 3), "<br>",
      "Relative risk: ", round(results_frame$rr, 3)
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = results_frame,
    pal = pal_1,
    values = ~rr,
    title = "RR",
    group = "RR"
  )
marg <- res_3$marginals.fitted.values[[1]]
1 - inla.pmarginal(q = 0.1, marginal = marg)
# now the same again with a pc prior
formula_4 <- value ~ 
  # add the mobility vars and pop density
  pop_dens + groc_pha + parks + resident + ret_recr + transit + workplace +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_prec_1) +
  # random effects term
  f(idarea_2, model = "iid")

res_4 <- inla(
  formula_4,
  family = "nbinomial",
  data = newest_numbers,
  E = expected,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)
res_2$dic$dic
res_4$dic$dic
summary(res_4)
options(scipen = 10)
res_4$summary.fixed
# higher population density doesn't matter
# higher workplace mobility leads to higher risk
# lower retail & recr leads to lower risk
# lower residential leads to lower risk
# lower
marginal_pop_dens <- inla.smarginal(res_4$marginals.fixed$pop_dens)
marginal_workplace <- inla.smarginal(res_4$marginals.fixed$workplace)
marginal_ret_recr <- inla.smarginal(res_4$marginals.fixed$ret_recr)
marginal_resident <- inla.smarginal(res_4$marginals.fixed$resident)
marginal_pop_dens <- data.frame(marginal_pop_dens)
marginal_workplace <- data.frame(marginal_workplace)
marginal_ret_recr <- data.frame(marginal_ret_recr)
marginal_resident <- data.frame(marginal_resident)
marginal_plot_1 <- ggplot(marginal_pop_dens, aes(x = x, y = y)) +
  geom_line() +
  labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") +
  ggtitle(
    label = "Posterior distribution of the coefficient",
    subtitle = "Variable: 'Population density'"
  ) +
  theme_minimal()
marginal_plot_2 <- ggplot(marginal_workplace, aes(x = x, y = y)) +
  geom_line() +
  labs(x = expression(beta[7]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") +
  ggtitle(
    label = "Posterior distribution of the coefficient",
    subtitle = "Variable: 'Workplace mobility'"
  ) +
  theme_minimal()
marginal_plot_3 <- ggplot(marginal_ret_recr, aes(x = x, y = y)) +
  geom_line() +
  labs(x = expression(beta[5]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") +
  ggtitle(
    label = "Posterior distribution of the coefficient",
    subtitle = "Variable: 'Retail & recreation mobility'"
  ) +
  theme_minimal()
marginal_plot_4 <- ggplot(marginal_resident, aes(x = x, y = y)) +
  geom_line() +
  labs(x = expression(beta[4]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") +
  ggtitle(
    label = "Posterior distribution of the coefficient",
    subtitle = "Variable: 'Residential mobility'"
  ) +
  theme_minimal()
(marginal_plot_1 +
    marginal_plot_4) /
  (marginal_plot_3 +
     marginal_plot_2)
head(res_4$summary.fitted.values[order(res_4$summary.fitted.values$mean, decreasing = TRUE), ])
newest_numbers[c(265, 174, 117, 1, 95, 172), ]
results_frame <- newest_numbers
results_frame$rr <- res_4$summary.fitted.values$mean
results_frame$q025 <- res_4$summary.fitted.values$`0.025quant`
results_frame$q5 <- res_4$summary.fitted.values$`0.5quant`
results_frame$q975 <- res_4$summary.fitted.values$`0.975quant`
pal_1 <- colorNumeric(
  c("#f4f4f9", "#b8dbd9", "#586f7c", "#2f4550", "#000000"),
  # colors depend on the count variable
  domain = results_frame$rr
)
leaflet(results_frame) %>%
  addTiles() %>%
  addPolygons(
    weight = 1,
    fillColor = ~pal_1(rr),
    fillOpacity = 0.7,
    color = "black",
    group = "Relative risk",
    label = paste(
      "Kommune: ", results_frame$kommune_name, "<br>",
      "Population: ", results_frame$population, "<br>",
      "Grocery & pharmacy mobility: ", results_frame$groc_pha, "<br>",
      "Residential mobility: ", results_frame$resident, "<br>",
      "Parks mobility: ", results_frame$parks, "<br>",
      "Retail & recreation mobility: ", results_frame$ret_recr, "<br>",
      "Transit mobility: ", results_frame$transit, "<br>",
      "Workplace mobility: ", results_frame$workplace, "<br>",
      "Number of infections: ", results_frame$value, "<br>",
      "Expected number of infections: ", round(results_frame$expected), "<br>",
      "SIR: ", round(results_frame$sir, 3), "<br>",
      "Relative risk: ", round(results_frame$rr, 3)
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = results_frame,
    pal = pal_1,
    values = ~rr,
    title = "RR",
    group = "RR"
  )
marg <- res_4$marginals.fitted.values[[1]]
1 - inla.pmarginal(q = 0.1, marginal = marg)
# now the same again with a pc prior
formula_5 <- value ~ 
  # add the mobility vars and pop density
  pop_dens + groc_pha + parks + resident + ret_recr + transit + workplace +
  sex + age_1 + age_2 + age_3 + age_4 + age_5 + age_6 + age_7 + age_8 + age_9 + age_10 +
  # specify the model with neighborhood matrix
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior_prec_1) +
  # random effects term
  f(idarea_2, model = "iid")

res_5 <- inla(
  formula_5,
  family = "nbinomial",
  data = newest_numbers,
  E = expected,
  control.predictor = list(
    compute = TRUE
  ),
  control.compute = list(dic = TRUE)
)
res_2$dic$dic
res_5$dic$dic
summary(res_5)
options(scipen = 10)
res_5$summary.fixed
# higher population density doesn't matter
# higher workplace mobility leads to higher risk
# lower retail & recr leads to lower risk
# lower residential leads to lower risk
# lower
# marginal_pop_dens <- inla.smarginal(res_5$marginals.fixed$pop_dens)
# marginal_workplace <- inla.smarginal(res_5$marginals.fixed$workplace)
# marginal_ret_recr <- inla.smarginal(res_5$marginals.fixed$ret_recr)
# marginal_resident <- inla.smarginal(res_5$marginals.fixed$resident)
# marginal_pop_dens <- data.frame(marginal_pop_dens)
# marginal_workplace <- data.frame(marginal_workplace)
# marginal_ret_recr <- data.frame(marginal_ret_recr)
# marginal_resident <- data.frame(marginal_resident)
# marginal_plot_1 <- ggplot(marginal_pop_dens, aes(x = x, y = y)) +
#   geom_line() +
#   labs(x = expression(beta[1]), y = "Density") +
#   geom_vline(xintercept = 0, col = "black") +
#   ggtitle(
#     label = "Posterior distribution of the coefficient",
#     subtitle = "Variable: 'Population density'"
#   ) +
#   theme_minimal()
# marginal_plot_2 <- ggplot(marginal_workplace, aes(x = x, y = y)) +
#   geom_line() +
#   labs(x = expression(beta[7]), y = "Density") +
#   geom_vline(xintercept = 0, col = "black") +
#   ggtitle(
#     label = "Posterior distribution of the coefficient",
#     subtitle = "Variable: 'Workplace mobility'"
#   ) +
#   theme_minimal()
# marginal_plot_3 <- ggplot(marginal_ret_recr, aes(x = x, y = y)) +
#   geom_line() +
#   labs(x = expression(beta[5]), y = "Density") +
#   geom_vline(xintercept = 0, col = "black") +
#   ggtitle(
#     label = "Posterior distribution of the coefficient",
#     subtitle = "Variable: 'Retail & recreation mobility'"
#   ) +
#   theme_minimal()
# marginal_plot_4 <- ggplot(marginal_resident, aes(x = x, y = y)) +
#   geom_line() +
#   labs(x = expression(beta[4]), y = "Density") +
#   geom_vline(xintercept = 0, col = "black") +
#   ggtitle(
#     label = "Posterior distribution of the coefficient",
#     subtitle = "Variable: 'Residential mobility'"
#   ) +
#   theme_minimal()
# (marginal_plot_1 +
#     marginal_plot_4) /
#   (marginal_plot_3 +
#      marginal_plot_2)
head(res_5$summary.fitted.values[order(res_5$summary.fitted.values$mean, decreasing = TRUE), ])
newest_numbers[c(265, 174, 117, 1, 95, 96), ]
results_frame <- newest_numbers
results_frame$rr <- res_5$summary.fitted.values$mean
results_frame$q025 <- res_5$summary.fitted.values$`0.025quant`
results_frame$q5 <- res_5$summary.fitted.values$`0.5quant`
results_frame$q975 <- res_5$summary.fitted.values$`0.975quant`
pal_1 <- colorNumeric(
  c("#f4f4f9", "#b8dbd9", "#586f7c", "#2f4550", "#000000"),
  # colors depend on the count variable
  domain = results_frame$rr
)
leaflet(results_frame) %>%
  addTiles() %>%
  addPolygons(
    weight = 1,
    fillColor = ~pal_1(rr),
    fillOpacity = 0.7,
    color = "black",
    group = "Relative risk",
    label = paste(
      "Kommune: ", results_frame$kommune_name, "<br>",
      "Population: ", results_frame$population, "<br>",
      "Grocery & pharmacy mobility: ", results_frame$groc_pha, "<br>",
      "Residential mobility: ", results_frame$resident, "<br>",
      "Parks mobility: ", results_frame$parks, "<br>",
      "Retail & recreation mobility: ", results_frame$ret_recr, "<br>",
      "Transit mobility: ", results_frame$transit, "<br>",
      "Workplace mobility: ", results_frame$workplace, "<br>",
      "Number of infections: ", results_frame$value, "<br>",
      "Expected number of infections: ", round(results_frame$expected), "<br>",
      "SIR: ", round(results_frame$sir, 3), "<br>",
      "Relative risk: ", round(results_frame$rr, 3)
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = results_frame,
    pal = pal_1,
    values = ~rr,
    title = "RR",
    group = "RR"
  )
marg <- res_5$marginals.fitted.values[[1]]
1 - inla.pmarginal(q = 0.1, marginal = marg)
  #####
nb <- poly2nb(newest_numbers)
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3))
)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
formula_bym2 <- inf_rate ~ f(idarea_1, model = "bym2", graph = g, hyper = prior)
newest_numbers$expected_count <- expected_count
res <- inla(
  formula_bym2,
  family = "exponential", data = newest_numbers,
  E = expected_count, control.predictor = list(compute = TRUE)
)
summary(res)

formula_bym <- inf_rate ~
  f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior) +
  f(idarea_2, model = "iid")
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01))
)
res <- inla(
  formula_bym,
  family = "exponential", data = newest_numbers,
  E = expected_count, control.predictor = list(compute = TRUE)
)
summary(res)
