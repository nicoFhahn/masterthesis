library(sf)
library(dplyr)
library(readr)
library(SpatialEpi)
library(stringr)
library(leaflet)
library(tmap)
library(ggplot2)
library(patchwork)
library(INLA)
library(MASS)
library(spdep)
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

###########################################
newest_numbers_grouped <- newest_numbers %>%
  group_by(
    kommn_n,
    date
  ) %>%
  summarise(
    fylke_n = unique(fylke_n),
    fylk_nm = unique(fylk_nm),
    kmmn_nm = unique(kmmn_nm),
    value = unique(value),
    groc_ph = unique(groc_ph),
    parks = unique(parks),
    residnt = unique(residnt),
    ret_rcr = unique(ret_rcr),
    transit = unique(transit),
    workplc = unique(workplc),
    sex = sum(Prs2020[sex == "Females"]) / (sum(Prs2020)),
    age_1 = sum(Prs2020[age == "0-17 years"]) / (sum(Prs2020)),
    age_2 = sum(Prs2020[age == "18-49 years"]) / (sum(Prs2020)),
    age_3 = sum(Prs2020[age == "50-66 years"]) / (sum(Prs2020)),
    age_4 = sum(Prs2020[age == "67-79 years"]) / (sum(Prs2020)),
    age_5 = sum(Prs2020[age == "80-89 years"]) / (sum(Prs2020)),
    age_6 = sum(Prs2020[age == "90 years or older"]) / (sum(Prs2020)),
    popultn = sum(Prs2020),
    sprmrkt = unique(sprmrkt),
    plc_f_w = unique(plc_f_w),
    retail = unique(retail),
    nrsng_h = unique(nrsng_h),
    restrnt = unique(restrnt),
    terminl = unique(terminl),
    aerodrm = unique(aerodrm),
    office = unique(office),
    platfrm = unique(platfrm),
    unvrsty = unique(unvrsty),
    college = unique(college),
    kndrgrt = unique(kndrgrt),
    schools = unique(schools)
  )
newest_numbers_grouped$inf_rate <- newest_numbers_grouped$value / newest_numbers_grouped$popultn
newest_numbers_grouped$idarea_1 <- seq_len(nrow(newest_numbers_grouped))
newest_numbers_grouped$idarea_2 <- seq_len(nrow(newest_numbers_grouped))
fit1 <- fitdistr(newest_numbers_grouped$inf_rate, "exponential")
ks.test(newest_numbers_grouped$inf_rate, "pexp", fit1$estimate)
hist(newest_numbers_grouped$inf_rate, freq = FALSE, breaks = 75, xlim = c(0, quantile(newest_numbers_grouped$inf_rate, 0.99)))
curve(dexp(x, fit1$estimate), from = 0, col = "red", add = TRUE)
prior_prec <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
  )
)
formula <- inf_rate ~ f(idarea, model = "iid", hyper = prior_prec)
# to try: gaussian, skewnormal, exponential, stochvol, stochvolt, stochvolnig
set.seed(345)
res <- inla(
  formula,
  data = newest_numbers_grouped,
  family = "exponential",
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE)
)
summary(res)
plot(res)
res$summary.fixed
res$summary.random
res$summary.hyperpar
res$summary.fitted.values
nb <- poly2nb(newest_numbers_grouped)
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)
  ),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3)
  )
)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")
formula_bym2 <- inf_rate ~ f(idarea_1, model = "bym2", graph = g, hyper = prior)
newest_numbers_grouped$expected_count <- expected_count
res <- inla(
  formula_bym2,
  family = "exponential", data = newest_numbers_grouped,
  E = expected_count, control.predictor = list(compute = TRUE)
)
summary(res)

formula_bym <- inf_rate ~
f(idarea_1, model = "besag", graph = g, scale.model = TRUE, hyper = prior) +
  f(idarea_2, model = "iid")
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)
  )
)
res <- inla(
  formula_bym,
  family = "exponential", data = newest_numbers_grouped,
  E = expected_count, control.predictor = list(compute = TRUE)
)
summary(res)
