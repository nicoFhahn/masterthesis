library(ggplot2)
library(patchwork)
library(readr)
library(sf)
library(spdep)
library(INLA)
library(tibble)
library(latex2exp)
library(pbapply)
newest_numbers <- read_csv("eval_data/newest_numbers_norway_march24.csv")
norge_sf <- read_sf("wrangled_data/shapes_norge.shp")
newest_numbers <- merge(
  newest_numbers,
  norge_sf,
  by = "kommune_no"
)
newest_numbers <- st_as_sf(newest_numbers)
set.seed(7918)
test <- sample(
  seq_len(nrow(newest_numbers)),
  size = floor(0.2 * nrow(newest_numbers))
)
test_value <- newest_numbers$value[test]
newest_numbers$value[test] <- NA
link <- rep(NA, nrow(newest_numbers))
link[which(is.na(newest_numbers$value))] <- 1
nb <- poly2nb(newest_numbers)
# save the matrix
nb2INLA("maps/map_1.adj", nb)
g <- inla.read.graph(filename = "maps/map_1.adj")
Q <- Diagonal(x = sapply(nb, length))
for (i in 2:nrow(newest_numbers)) {
  Q[i - 1, i] <- -1
  Q[i, i - 1] <- -1
}

C <- Diagonal(x = 1, n = nrow(newest_numbers)) - Q
formula_besag <- value ~
  urb_dens + median_age + unemp_tot + unemp_immg + immigrants_total + sex +
  marketplace + place_of_worship + nursing_home + aerodrome +
  office + platform + higher_education +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior)
formula_bym2 <- value ~
  urb_dens + median_age + unemp_tot + unemp_immg + immigrants_total + sex +
  marketplace + place_of_worship + nursing_home + aerodrome +
  office + platform + higher_education +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior)
formula_leroux <- value ~
  urb_dens + median_age + unemp_tot + unemp_immg + immigrants_total + sex +
  marketplace + place_of_worship + nursing_home + aerodrome +
  office + platform + higher_education +
  f(idarea_1, model = "generic1", Cmatrix = C, hyper = prior)
models <- pblapply(
  seq(0.1, 5, 0.01),
  function(x, ...) {
    prior <- list(
      prec = list(
        prior = "pc.prec",
        param = c(x, 0.01)
      )
    )
    res_besag <- inla(
      formula_besag,
      family = "nbinomial",
      data = newest_numbers,
      E = expected_count,
      control.predictor = list(
        compute = TRUE,
        link = link
      ),
      Ntrials = newest_numbers$population,
      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
    )
    res_bym2 <- inla(
      formula_bym2,
      family = "nbinomial",
      data = newest_numbers,
      E = expected_count,
      control.predictor = list(
        compute = TRUE,
        link = link
      ),
      Ntrials = newest_numbers$population,
      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
    )
    res_leroux <- inla(
      formula_leroux,
      family = "nbinomial",
      data = newest_numbers,
      E = expected_count,
      control.predictor = list(
        compute = TRUE,
        link = link
      ),
      Ntrials = newest_numbers$population,
      control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE)
    )
    predicted_besag <- c()
    predicted_bym2 <- c()
    predicted_leroux <- c()
    for (i in seq_len(nrow(newest_numbers))) {
      predicted_besag[i] <- inla.emarginal(
        function(x) x * newest_numbers$population[i],
        res_besag$marginals.fitted.values[[i]]
      )
      predicted_bym2[i] <- inla.emarginal(
        function(x) x * newest_numbers$population[i],
        res_bym2$marginals.fitted.values[[i]]
      )
      predicted_leroux[i] <- inla.emarginal(
        function(x) x * newest_numbers$population[i],
        res_leroux$marginals.fitted.values[[i]]
      )
    }
    mae <- c(list(
      mean(abs(predicted_besag[test] - test_value)),
      mean(abs(predicted_bym2[test] - test_value)),
      mean(abs(predicted_leroux[test] - test_value))
    ))
    dic <- c(list(
      res_besag$dic$dic,
      res_bym2$dic$dic,
      res_leroux$dic$dic
    ))
    waic <- c(list(
      res_besag$waic$waic,
      res_bym2$waic$waic,
      res_leroux$waic$waic
    ))
    cpo <- c(list(
      sum(log(res_besag$cpo$cpo), na.rm = TRUE),
      sum(log(res_bym2$cpo$cpo), na.rm = TRUE),
      sum(log(res_leroux$cpo$cpo), na.rm = TRUE)
    ))
    results <- tibble(
      dic = unlist(dic),
      waic = unlist(waic),
      cpo = unlist(cpo),
      mae = unlist(mae),
      model = c("Besag", "BYM2", "Leroux"),
      U = rep(x, 3),
      alpha = rep(0.01, 3)
    )
    hyperpar_frame <- tibble(
      precision = c(
        res_besag$summary.hyperpar$mean[2],
        res_bym2$summary.hyperpar$mean[2],
        res_leroux$summary.hyperpar$mean[2]
      ),
      phi = c(
        res_besag$summary.hyperpar$mean[3],
        res_bym2$summary.hyperpar$mean[3],
        res_leroux$summary.hyperpar$mean[3]
      ),
      model = c("Besag", "BYM2", "Leroux"),
      U = rep(x, 3),
      alpha = rep(0.01, 3)
    )
    marginal_frame <- tibble(
      lower = c(
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$`(Intercept)`
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$median_age
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$aerodrome
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$office
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$platform
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$sex
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$higher_education
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$nursing_home
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$marketplace
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$urb_dens
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$unemp_immg
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$place_of_worship
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$unemp_tot
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$immigrants_total
          )
        )[1]
      ),
      mean = c(
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$`(Intercept)`
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$median_age
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$aerodrome
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$office
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$platform
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$sex
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$higher_education
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$nursing_home
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$marketplace
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$urb_dens
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$unemp_immg
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$place_of_worship
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$unemp_tot
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$immigrants_total
        )
      ),
      upper = c(
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$`(Intercept)`
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$median_age
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$aerodrome
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$office
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$platform
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$sex
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$higher_education
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$nursing_home
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$marketplace
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$urb_dens
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$unemp_immg
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$place_of_worship
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$unemp_tot
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$immigrants_total
          )
        )[2]
      ),
      variable = c(
        rep("Intercept", 1),
        rep("median_age", 1),
        rep("aerodrome", 1),
        rep("office", 1),
        rep("platform", 1),
        rep("sex", 1),
        rep("higher_education", 1),
        rep("nursing_home", 1),
        rep("marketplace", 1),
        rep("urb_dens", 1),
        rep("unemp_immg", 1),
        rep("place_of_worship", 1),
        rep("unemp_total", 1),
        rep("immigrants_total", 1)
      ),
      U = x
    )
    marginal_frame$variable <- ordered(marginal_frame$variable, levels = unique(marginal_frame$variable))
    list(
      results = results,
      hyperpar_frame = hyperpar_frame,
      marginal_frame = marginal_frame
    )
  }
)