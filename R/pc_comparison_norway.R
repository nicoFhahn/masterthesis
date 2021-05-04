library(ggplot2)
library(patchwork)
library(readr)
library(sf)
library(spdep)
library(INLA)
library(tibble)
library(latex2exp)
library(pbapply)
source("R/preprocess_norge.R")
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
  office + platform + higher_education + vaccine_shots +
  f(idarea_1, model = "besagproper", graph = g, hyper = prior)
formula_bym2 <- value ~
  urb_dens + median_age + unemp_tot + unemp_immg + immigrants_total + sex +
  marketplace + place_of_worship + nursing_home + aerodrome +
  office + platform + higher_education + vaccine_shots +
  f(idarea_1, model = "bym2", graph = g, scale.model = TRUE, hyper = prior)
formula_leroux <- value ~ vaccine_shots +
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
    res_bym2 <- try(inla(
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
    ), silent = TRUE
    )
    res_leroux <- try(inla(
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
    ), silent = TRUE)
    while (class(res_bym2) != "inla") {
      prior <- list(
        prec = list(
          prior = "pc.prec",
          param = c(x + runif(1, -0.01, 0.01), 0.01)
        )
      )
      res_bym2 <- try(inla(
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
      ), silent = TRUE
      )
    }
    while (class(res_leroux) != "inla") {
      prior <- list(
        prec = list(
          prior = "pc.prec",
          param = c(x + runif(1, -0.01, 0.01), 0.01)
        )
      )
      res_leroux <- try(inla(
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
      ), silent = TRUE
      )
    }
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
      predicted_leroux[i] <- try(inla.emarginal(
        function(x) x * newest_numbers$population[i],
        res_leroux$marginals.fitted.values[[i]]
      ), silent = TRUE)
      if (class(predicted_leroux[i]) != "numeric") {
        predicted_leroux[i] <- NA
      }
    }
    mae <- c(list(
      mean(abs(predicted_besag[test] - test_value)),
      mean(abs(predicted_bym2[test] - test_value)),
      ifelse(
        any(is.na(predicted_leroux)),
        NA,
        mean(abs(predicted_leroux[test] - test_value))
      )
    ))
    dic <- c(list(
      res_besag$dic$dic,
      res_bym2$dic$dic,
      ifelse(
        any(is.na(predicted_leroux)),
        NA,
        res_leroux$dic$dic
      )
    ))
    waic <- c(list(
      res_besag$waic$waic,
      res_bym2$waic$waic,
      ifelse(
        any(is.na(predicted_leroux)),
        NA,
        res_leroux$waic$waic
      )
    ))
    cpo <- c(list(
      sum(log(res_besag$cpo$cpo), na.rm = TRUE),
      sum(log(res_bym2$cpo$cpo), na.rm = TRUE),
      ifelse(
        any(is.na(predicted_leroux)),
        NA,
        sum(log(res_leroux$cpo$cpo), na.rm = TRUE)
      )
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
            exp, res_bym2$marginals.fixed$sex
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
            exp, res_bym2$marginals.fixed$median_age
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
            exp, res_bym2$marginals.fixed$vaccine_shots
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
            exp, res_bym2$marginals.fixed$platform
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
            exp, res_bym2$marginals.fixed$unemp_immg
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$immigrants_total
          )
        )[1],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$urb_dens
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
          res_bym2$marginals.fixed$sex
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
          res_bym2$marginals.fixed$median_age
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$place_of_worship
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$vaccine_shots
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
          res_bym2$marginals.fixed$platform
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$unemp_tot
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$unemp_immg
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$immigrants_total
        ),
        inla.emarginal(
          exp,
          res_bym2$marginals.fixed$urb_dens
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
            exp, res_bym2$marginals.fixed$sex
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
            exp, res_bym2$marginals.fixed$median_age
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
            exp, res_bym2$marginals.fixed$vaccine_shots
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
            exp, res_bym2$marginals.fixed$platform
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
            exp, res_bym2$marginals.fixed$unemp_immg
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$immigrants_total
          )
        )[2],
        inla.qmarginal(
          c(0.025, 0.975),
          inla.tmarginal(
            exp, res_bym2$marginals.fixed$urb_dens
          )
        )[2]
      ),
      variable = c(
        rep("Intercept", 1),
        rep("sex", 1),
        rep("aerodrome", 1),
        rep("office", 1),
        rep("median_age", 1),
        rep("place_of_worship", 1),
        rep("vaccine_shots", 1),
        rep("higher_education", 1),
        rep("nursing_home", 1),
        rep("marketplace", 1),
        rep("platform", 1),
        rep("unemp_total", 1),
        rep("unemp_immg", 1),
        rep("immigrants_total", 1),
        rep("urb_dens", 1)
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
save(models, file = "priors/priors_norway.Rda")
results <- do.call(rbind, lapply(models, function(x) x$results))
hyperpar_frame <- do.call(rbind, lapply(models, function(x) x$hyperpar_frame))
marginal_frame <- do.call(rbind, lapply(models, function(x) x$marginal_frame))
plot_1 <- ggplot(
  results,
  aes(
    U,
    dic,
    colour = model
  )
) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = TeX("$ DIC, \\alpha = 0.01, country = Norway$")
  ) +
  labs(
    x = TeX("$\\sigma_0$"),
    y = "DIC"
  ) +
  theme(
    legend.position = "none"
  )
plot_2 <- ggplot(
  results,
  aes(
    U,
    waic,
    colour = model
  )
) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = TeX("$ WAIC, \\alpha = 0.01, country = Norway$")
  ) +
  labs(
    x = TeX("$\\sigma_0$"),
    y = "WAIC"
  )
plot_3 <- ggplot(
  results,
  aes(
    U,
    cpo,
    colour = model
  )
) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = TeX("$ CPO, \\alpha = 0.01, country = Norway$")
  ) +
  labs(
    x = TeX("$\\sigma_0$"),
    y = "CPO"
  ) +
  theme(
    legend.position = "none"
  )
plot_4 <- ggplot(
  results,
  aes(
    U,
    mae,
    colour = model
  )
) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = 1) +
  theme_minimal() +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  ) +
  ggtitle(
    label = "Comparison of performance measures",
    subtitle = TeX("$ MAE, \\alpha = 0.01, country = Norway$")
  ) +
  labs(
    x = TeX("$\\sigma_0$"),
    y = "MAE"
  )
plot_1 + plot_2
plot_3 + plot_4
plot_2
plot_4
marginal_frame$U <- as.factor(marginal_frame$U)
ggplot(
  data = marginal_frame[marginal_frame$U %in% c(0.1, 1, 5), ]
) +
  geom_errorbar(
    aes(
      y = variable,
      xmin = lower,
      xmax = upper,
      colour = U
    ),
    position = "dodge",
    size = 0.5
  ) +
  geom_pointrange(
    aes(
      y = variable,
      xmin = lower,
      xmax = upper,
      x = mean,
      colour = U
    ),
    position = position_dodge2(width = 0.9, padding = 0),
    size = 0.5
  ) +
  theme_minimal() +
  xlab(
    "Marginals"
  ) +
  ylab(
    "Variable"
  ) +
  ggtitle(
    "Credibility intervals of the coefficients"
  ) +
  scale_colour_manual(
    values = c("#1B998B", "#ED217C", "#2D3047"),
    labels = c(
      expression(
        sigma[0]==0.1,
        sigma[0]==1,
        sigma[0]==5,
      )
    )
  ) +
  geom_vline(xintercept = 1) +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  )
color_low <- "#20A4F3"
color_high <- "#FF206E"
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(1, 0.01)
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
newest_numbers$random_besag <- res_besag$summary.random$idarea_1$mean
newest_numbers$random_bym2_unstructured <- res_bym2$summary.random$idarea_1$mean[1:356]
newest_numbers$random_bym2_structured <- res_bym2$summary.random$idarea_1$mean[357:712]
newest_numbers$random_leroux <- res_leroux$summary.random$idarea_1$mean
color_low <- "#20A4F3"
color_high <- "#FF206E"
plot_7 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = random_besag)) +
  ggtitle(
    label = "Spatial field for the Besag model",
  ) +
  scale_fill_gradient2(
    "Post mean",
    low = color_low,
    high = color_high,
    midpoint = 0,
    limits = c(-3, 3)
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
plot_8 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = random_leroux)) +
  ggtitle(
    label = "Spatial field for the Leroux model"
  ) +
  scale_fill_gradient2(
    "Post mean",
    low = color_low,
    high = color_high,
    midpoint = 0,
    limits = c(-3, 3)
  ) +
  theme_minimal()
plot_9 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = random_bym2_unstructured)) +
  ggtitle(
    label = "Spatial field for the BYM2 model",
    subtitle = "Unstructured component"
  ) +
  scale_fill_gradient2(
    "Post mean",
    low = color_low,
    high = color_high,
    midpoint = 0,
    limits = c(-3, 3)
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
plot_10 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = random_bym2_structured)) +
  ggtitle(
    label = "Spatial field for the BYM2 model",
    subtitle = "Structured component"
  ) +
  scale_fill_gradient2(
    "Post mean",
    low = color_low,
    high = color_high,
    midpoint = 0,
    limits = c(-3, 3)
  ) +
  theme_minimal()
plot_7 + plot_8
plot_9 + plot_10
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.1, 0.01)
  )
)
res_bym2_01 <- inla(
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
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(5, 0.01)
  )
)
res_bym2_5 <- inla(
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
plot_11 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = res_bym2_01$summary.random$idarea_1$mean[357:712])) +
  ggtitle(
    label = "Spatial field for the BYM2 model",
    subtitle = TeX("$Structured\\,component, \\sigma_0 = 0.1$")
  ) +
  scale_fill_gradient2(
    "Post mean",
    low = color_low,
    high = color_high,
    midpoint = 0,
    limits = c(-3, 3)
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

plot_12 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = res_bym2_5$summary.random$idarea_1$mean[357:712])) +
  ggtitle(
    label = "Spatial field for the BYM2 model",
    subtitle = TeX("$Structured\\,component, \\sigma_0 = 5$")
  ) +
  scale_fill_gradient2(
    "Post mean",
    low = color_low,
    high = color_high,
    midpoint = 0,
    limits = c(-3, 3)
  ) +
  theme_minimal()
plot_11 + plot_12
ggplot(data = newest_numbers) +
  geom_sf(aes(fill = res_bym2_01$summary.random$idarea_1$mean[357:712])) +
  ggtitle(
    label = "Spatial field for the BYM2 model",
    subtitle = TeX("$Structured\\,component, \\sigma_0 = 0.1$")
  ) +
  scale_fill_gradient2(
    "Post mean",
    low = color_low,
    high = color_high,
    midpoint = 0
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )
