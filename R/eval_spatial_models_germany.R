library(readr)
library(sf)
library(INLA)
library(ggplot2)
library(patchwork)
# source("R/nontemporal_models_germany.R")
newest_numbers <- read_csv("eval_data/newest_numbers_germany_march24.csv")
germany_sf <- read_sf("wrangled_data/shapes_germany.shp")
newest_numbers <- merge(
  newest_numbers,
  germany_sf,
  by.x = "municipality_id",
  by.y = "Kennziffer"
)
newest_numbers <- st_as_sf(newest_numbers)
# load the models
load("models/nontemporal_germany.Rda")
# show the 5 municipalities with the most infections
newest_numbers[
  order(newest_numbers$value, decreasing = TRUE),
][1:5, c("municipality", "population", "value")]
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
        exp, models_final[[1]][[1]]$marginals.fixed$Gruene
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$Gruene
      )
    )[1],
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
        exp, models_final[[1]][[1]]$marginals.fixed$SPD
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$SPD
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$`log(trade_tax)`
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$`log(trade_tax)`
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$clinic
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$clinic
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
        exp, models_final[[1]][[1]]$marginals.fixed$urb_dens
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$urb_dens
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
        exp, models_final[[1]][[1]]$marginals.fixed$die_linke
        
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$die_linke
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$FDP
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$FDP
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
        exp, models_final[[1]][[1]]$marginals.fixed$retail
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$retail
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$pop_dens
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$pop_dens
      )
    )[1]
  ),
  mean = c(
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$Gruene
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$Gruene
    ),
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
      models_final[[1]][[1]]$marginals.fixed$higher_education
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$higher_education
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$SPD
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$SPD
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$`log(trade_tax)`
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$`log(trade_tax)`
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$clinic
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$clinic
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
      models_final[[1]][[1]]$marginals.fixed$sex
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$sex
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$urb_dens
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$urb_dens
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
      models_final[[1]][[1]]$marginals.fixed$aerodrome
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$aerodrome
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$die_linke
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$die_linke
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$FDP
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$FDP
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
      models_final[[1]][[1]]$marginals.fixed$retail
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$retail
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[1]]$marginals.fixed$pop_dens
    ),
    inla.emarginal(
      exp,
      models_final[[1]][[3]]$marginals.fixed$pop_dens
    )
  ),
  upper = c(
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$Gruene
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$Gruene
      )
    )[2],
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
        exp, models_final[[1]][[1]]$marginals.fixed$SPD
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$SPD
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$`log(trade_tax)`
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$`log(trade_tax)`
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$clinic
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$clinic
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
        exp, models_final[[1]][[1]]$marginals.fixed$urb_dens
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$urb_dens
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
        exp, models_final[[1]][[1]]$marginals.fixed$die_linke
        
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$die_linke
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$FDP
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$FDP
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
        exp, models_final[[1]][[1]]$marginals.fixed$retail
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$retail
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[1]]$marginals.fixed$pop_dens
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_final[[1]][[3]]$marginals.fixed$pop_dens
      )
    )[2]
  ),
  variable = c(
    rep("Greens", 2),
    rep("Intercept", 2),
    rep("higher_education", 2),
    rep("SPD", 2),
    rep("log(trade_tax)", 2),
    rep("clinic", 2),
    rep("place_of_worship", 2),
    rep("sex", 2),
    rep("urb_dens", 2),
    rep("platform", 2),
    rep("aerodrome", 2),
    rep("die_linke", 2),
    rep("FDP", 2),
    rep("nursing_home", 2),
    rep("retail", 2),
    rep("pop_dens", 2)
  ),
  model = rep(
    c("No spatial", "BYM2"), 16
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
# calculate the posterior mean of the relative risk
zeta <- lapply(csi, function(x) inla.emarginal(exp, x))
zeta_cutoff <- c(0.1, 0.5, 0.9, 1, 1.4, 1.8, 2.2, 2.6)
# group it
cat_zeta <- cut(
  unlist(zeta),
  breaks = zeta_cutoff,
  include.lowest = TRUE
)
newest_numbers$cat_zeta <- cat_zeta
newest_numbers$prob_csi <- cat_csi
mat_marg <- matrix(NA, nrow = nrow(newest_numbers), ncol = 100000)
m <- models_final[[1]][[3]]$marginals.random$idarea_1
for (i in seq_len(nrow(newest_numbers))) {
  u <- m[[i]]
  mat_marg[i, ] <- inla.rmarginal(100000, u)
}
var_u <- apply(mat_marg, 2, var)
var_v <- inla.rmarginal(
  100000,
  inla.tmarginal(
    function(x) 1 / x,
    models_final[[1]][[3]]$marginals.hyperpar$`Precision for idarea_1`
  )
)
perc_var_u <- mean(var_u / (var_u + var_v))
perc_var_u

color_low <- "#20A4F3"
color_high <- "#FF206E"
library(ggplot2)
plot_1 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = rr)) +
  ggtitle(
    label = "Relative risk based on all variables",
    subtitle = "Germany"
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
    label = "Posterior mean of the relative risk",
    subtitle = "Germany"
  ) +
  scale_fill_viridis_d(option = "B", direction = -1, drop = FALSE) +
  theme_minimal() +
  guides(
    fill = guide_legend(
      title = "Relative risk"
    )
  )
plot_2
plot_3 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = prob_csi)) +
  ggtitle(
    label = "Posterior probability",
    subtitle = "Germany"
  ) +
  scale_fill_viridis_d(option = "B", direction = -1, drop = FALSE) +
  theme_minimal() +
  guides(
    fill = guide_legend(
      title = "Posterior probability"
    )
  )
plot_3
plot_2 + plot_3
# get the summary of the hyperparameters
models_final[[1]][[3]]$summary.hyperpar
