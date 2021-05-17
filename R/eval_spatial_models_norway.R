library(ggplot2)
library(patchwork)
library(readr)
library(sf)
library(tibble)
library(INLA)
# source("R/nontemporal_models_norway.R")
newest_numbers <- read_csv("eval_data/newest_numbers_norway_may2.csv")
norge_sf <- read_sf("wrangled_data/shapes_norge.shp")
newest_numbers <- merge(
  newest_numbers,
  norge_sf,
  by = "kommune_no"
)
newest_numbers <- st_as_sf(newest_numbers)
load("models/nontemporal_norway.RDa")
# show the 5 municipalities with the most infections
newest_numbers[
  order(newest_numbers$value, decreasing = TRUE),
][1:5, c("kommune_name", "population", "value", "vaccine_shots")]
####################### Model with no spatial component
# get the dic
models_final[[2]][[1]]$dic
# get the waic
models_final[[2]][[1]]$waic
# get the cpo
models_final[[2]][[1]]$cpo
# get the mae
models_final[[3]][[1]]
# get the summary
models_final[[1]][[1]]$summary.fixed[
  order(models_final[[1]][[1]]$summary.fixed$mean),
]
sapply(
  models_final[[1]][[1]]$marginals.fixed[
    rownames(models_final[[1]][[1]]$summary.fixed[
      order(models_final[[1]][[1]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_final[[1]][[1]]$marginals.fixed[
    rownames(models_final[[1]][[1]]$summary.fixed[
      order(models_final[[1]][[1]]$summary.fixed$mean),
    ])
  ],
  function(x) {
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, x
      )
    )
  }
)
# now the models with a spatial component
# get the dic values
# first besag, then bym2, then leroux
models_final[[2]][[2]]$dic
models_final[[2]][[3]]$dic
models_final[[2]][[4]]$dic
# now the waic
models_final[[2]][[2]]$waic
models_final[[2]][[3]]$waic
models_final[[2]][[4]]$waic
# now the cpo
models_final[[2]][[2]]$cpo
models_final[[2]][[3]]$cpo
models_final[[2]][[4]]$cpo
# now the mae
models_final[[3]][[2]]
models_final[[3]][[3]]
models_final[[3]][[4]]
options(scipen = 10)
# create a tibble with the credibility intervals and posterior coefficients
# for the bym2 model and the model without the spatial component
marginal_frame <- tibble(
  lower = c(
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$`(Intercept)`
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$`(Intercept)`
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$sex
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$sex
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$office
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$office
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$aerodrome
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$aerodrome
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$median_age
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$median_age
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$place_of_worship
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$place_of_worship
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$nursing_home
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$nursing_home
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$higher_education
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$higher_education
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$platform
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$platform
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$marketplace
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$marketplace
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$vaccine_shots
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$vaccine_shots
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$unemp_tot
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$unemp_tot
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$immigrants_total
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$immigrants_total
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$unemp_immg
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$unemp_immg
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$urb_dens
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$urb_dens
      )
    )[1]
  ),
  mean = c(
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$`(Intercept)`
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$`(Intercept)`
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$sex
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$sex
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$office
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$office
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$aerodrome
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$aerodrome
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$median_age
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$median_age
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$place_of_worship
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$place_of_worship
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$nursing_home
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$nursing_home
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$higher_education
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$higher_education
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$platform
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$platform
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$marketplace
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$marketplace
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$vaccine_shots
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$vaccine_shots
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$unemp_tot
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$unemp_tot
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$immigrants_total
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$immigrants_total
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$unemp_immg
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$unemp_immg
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$urb_dens
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$urb_dens
    )
  ),
  upper = c(
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$`(Intercept)`
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$`(Intercept)`
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$sex
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$sex
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$office
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$office
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$aerodrome
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$aerodrome
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$median_age
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$median_age
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$place_of_worship
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$place_of_worship
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$nursing_home
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$nursing_home
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$higher_education
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$higher_education
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$platform
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$platform
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$marketplace
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$marketplace
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$vaccine_shots
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$vaccine_shots
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$unemp_tot
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$unemp_tot
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$immigrants_total
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$immigrants_total
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$unemp_immg
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$unemp_immg
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$urb_dens
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$urb_dens
      )
    )[2]
  ),
  variable = c(
    rep("Intercept", 2),
    rep("Sex", 2),
    rep("Office", 2),
    rep("Aerodrome", 2),
    rep("Median age", 2),
    rep("Place of worship", 2),
    rep("Nursing home", 2),
    rep("Higher education", 2),
    rep("Platform", 2),
    rep("Marketplace", 2),
    rep("Vaccinations", 2),
    rep("Total unemployment", 2),
    rep("Total immigrants", 2),
    rep("Unemployed immigrants", 2),
    rep("Urban density", 2)
  ),
  model = rep(
    c("No spatial", "BYM2"), 15
  )
)
# order the variable
marginal_frame$variable <- ordered(
  marginal_frame$variable,
  levels = unique(marginal_frame$variable)
)
# plot the intervals and mean
ggplot(
  data = marginal_frame
) +
  geom_errorbar(
    aes(
      y = variable,
      xmin = lower,
      xmax = upper,
      colour = model
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
      colour = model
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
    values = c("#ED217C", "#2D3047")
  ) +
  geom_vline(xintercept = 1) +
  guides(
    colour = guide_legend(
      title = "Model"
    )
  )
# get the summary of the bym2 model
models_final[[1]][[3]]$summary.fixed[
  order(models_final[[1]][[3]]$summary.fixed$mean),
]
# get the exponentiated coefficients
sapply(
  models_final[[1]][[3]]$marginals.fixed[
    rownames(models_final[[1]][[3]]$summary.fixed[
      order(models_final[[1]][[3]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
# and the credibility intervals
sapply(
  models_final[[1]][[3]]$marginals.fixed[
    rownames(models_final[[1]][[3]]$summary.fixed[
      order(models_final[[1]][[3]]$summary.fixed$mean),
    ])
  ],
  function(x) {
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, x
      )
    )
  }
)
# add new variable for the relative risk
newest_numbers$rr <- models_final[[1]][[3]]$summary.fitted.values$mean
# now calculate the posterior probability
csi <- models_final[[1]][[3]]$marginals.random$idarea_1[
  seq_len(nrow(newest_numbers))
]
a <- 0
prob_csi <- lapply(csi, function(x) {
  1 - inla.pmarginal(a, x)
})
csi_cutoff <- c(0, 0.25, 0.5, 0.75, 1)
cat_csi <- cut(
  unlist(prob_csi),
  breaks = csi_cutoff,
  include.lowest = TRUE
)
# calculate the posterior mean of the relative risk (also log)
zeta <- lapply(csi, function(x) inla.emarginal(exp, x))
zeta_log <- lapply(csi, function(x) log10(inla.emarginal(exp, x)))
zeta_cutoff <- c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
zeta_log_cutoff <- c(-0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5, 0.7)
# group it
cat_zeta <- cut(
  unlist(zeta),
  breaks = zeta_cutoff,
  include.lowest = TRUE
)
cat_zeta_log <- cut(
  unlist(zeta_log),
  breaks = zeta_log_cutoff,
  include.lowest = TRUE
)
newest_numbers$cat_zeta <- cat_zeta
newest_numbers$cat_zeta_log <- cat_zeta_log
newest_numbers$prob_csi <- cat_csi
marg_hyper < -inla.hyperpar.sample(100000, models_final[[1]][[3]])
perc_var_u1 <- mean(marg_hyper[, 1] / ( marg_hyper[, 1] + marg_hyper[, 2]))
perc_var_u1
color_low <- "#20A4F3"
color_high <- "#FF206E"

plot_1 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = rr)) +
  ggtitle(
    label = "Relative risk",
    subtitle = "Norway"
  ) +
  scale_fill_gradient2(
    "RR",
    low = color_low,
    high = color_high,
    midpoint = 1
  ) +
  theme_minimal()
plot_1
plot_2 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = cat_zeta)) +
  ggtitle(
    label = "Posterior mean of the random effects",
    subtitle = "Norway"
  ) +
  scale_fill_viridis_d(option = "B", direction = -1, drop = FALSE) +
  theme_minimal() +
  guides(
    fill = guide_legend(
      title = "Mean"
    )
  )
plot_2
plot_3 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = prob_csi)) +
  ggtitle(
    label = "Exceedance probability",
    subtitle = "Norway"
  ) +
  scale_fill_viridis_d(option = "B", direction = -1, drop = FALSE) +
  theme_minimal() +
  guides(
    fill = guide_legend(
      title = "Probability"
    )
  )
plot_3
plot_1
plot_2 + plot_3
plot_4 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = cat_zeta_log)) +
  ggtitle(
    label = "Log10 Posterior mean of the random effects",
    subtitle = "Norway"
  ) +
  scale_fill_viridis_d(option = "B", direction = -1, drop = FALSE) +
  theme_minimal() +
  guides(
    fill = guide_legend(
      title = "Log10 mean"
    )
  )
plot_4 + plot_3
# get the summary of the hyperparameters
models_final[[1]][[3]]$summary.hyperpar
newest_numbers$zeta <- unlist(zeta)
pal <- colorNumeric(
  "YlOrRd",
  newest_numbers$rr
)
newest_numbers$csi <- unlist(prob_csi)
threshold <- 0.75
library(leaflet)
leaflet(data = newest_numbers[newest_numbers$csi >= threshold, ]) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(
    color = "black",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.8,
    fillColor = ~pal(rr),
    label = paste(
      "Kommune: ", newest_numbers[newest_numbers$csi >= threshold, ]$kommune_name, "<br>",
      "Zeta: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$zeta, 2), "<br>",
      "Xi: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$csi, 2), "<br>",
      "RR: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$rr, 2), "<br>",
      "Number of infections: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$value, 2), "<br>",
      "Urban density: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$urb_dens, 2), "<br>",
      "Unemployed immigrants: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$unemp_immg, 2), "<br>",
      "Total immigrants: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$immigrants_total, 2), "<br>",
      "Expected count: ", newest_numbers[newest_numbers$csi >= threshold, ]$expected_count
    ) %>%
      lapply(htmltools::HTML)
  )
threshold <- 0
pal <- colorFactor(
  "inferno",
  newest_numbers$cat_zeta_log
)
threshold <- 0
leaflet(data = newest_numbers[newest_numbers$csi >= threshold, ]) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(
    color = "black",
    weight = 1,
    opacity = 1,
    fillOpacity = 0.8,
    fillColor = ~pal(cat_zeta_log),
    label = paste(
      "Kommune: ", newest_numbers[newest_numbers$csi >= threshold, ]$kommune_name, "<br>",
      "Zeta: ", newest_numbers[newest_numbers$csi >= threshold, ]$cat_zeta_log, "<br>",
      "Xi: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$csi, 2), "<br>",
      "RR: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$rr, 2), "<br>",
      "Number of infections: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$value, 2), "<br>",
      "Urban density: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$urb_dens, 2), "<br>",
      "Unemployed immigrants: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$unemp_immg, 2), "<br>",
      "Total immigrants: ", round(newest_numbers[newest_numbers$csi >= threshold, ]$immigrants_total, 2), "<br>",
      "Expected count: ", newest_numbers[newest_numbers$csi >= threshold, ]$expected_count
    ) %>%
      lapply(htmltools::HTML)
  )
