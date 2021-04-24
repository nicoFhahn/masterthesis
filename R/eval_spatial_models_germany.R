library(readr)
library(sf)
library(INLA)
# source("R/germany_leroux_models.R")
# source("R/germany_besagproper_models.R")
# source("R/germany_bym2_models.R")
# source("R/germany_nospatial_models.R")
newest_numbers <- read_csv("eval_data/newest_numbers_germany_march24.csv")
germany_sf <- read_sf("wrangled_data/shapes_germany.shp")
newest_numbers <- merge(
  newest_numbers,
  germany_sf,
  by.x = "municipality_id",
  by.y = "Kennziffer"
)
newest_numbers <- st_as_sf(newest_numbers)
load("models/leroux_germany.Rda")
models_final_leroux <- models_final
load("models/besagproper_germany.Rda")
models_final_besag <- models_final
load("models/bym2_germany.Rda")
models_final_bym2 <- models_final
load("models/nospatial_germany.Rda")
models_final_nospatial <- models_final
rm(models_final)
models_leroux <- models_final_leroux[[1]]
models_besag <- models_final_besag[[1]]
models_bym2 <- models_final_bym2[[1]]
models_nospatial <- models_final_nospatial[[1]]
results_leroux <- models_final_leroux[[2]]
results_besag <- models_final_besag[[2]]
results_bym2 <- models_final_bym2[[2]]
results_nospatial <- models_final_nospatial[[2]]
mae_leroux <- models_final_leroux[[3]]
mae_besag <- models_final_besag[[3]]
mae_bym2 <- models_final_bym2[[3]]
mae_nospatial <- models_final_nospatial[[3]]
newest_numbers[
  order(newest_numbers$value, decreasing = TRUE),
][1:5, c("municipality", "population", "value")]
####################### Models with no spatial component
all_results <- c(results_nospatial[7])
all_dic <- unlist(lapply(all_results, function(x) x$dic))
all_waic <- unlist(lapply(all_results, function(x) x$waic))
all_cpo <- unlist(lapply(all_results, function(x) x$cpo))
id_nospatial <- which(
  unlist(mae_nospatial[12:13]) %in% min(unlist(mae_nospatial[12:13]))
) + 11
all_dic[id_nospatial - 11]
all_waic[id_nospatial - 11]
all_cpo[id_nospatial - 11]
mae_nospatial[id_nospatial]
models_nospatial[[id_nospatial]]$summary.fixed[
  order(models_nospatial[[id_nospatial]]$summary.fixed$mean),
]
sapply(
  models_nospatial[[id_nospatial]]$marginals.fixed[
    rownames(models_nospatial[[id_nospatial]]$summary.fixed[
      order(models_nospatial[[id_nospatial]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_nospatial[[id_nospatial]]$marginals.fixed[
    rownames(models_nospatial[[id_nospatial]]$summary.fixed[
      order(models_nospatial[[id_nospatial]]$summary.fixed$mean),
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
# models with spatial
all_results <- c(
  results_besag[7], results_bym2[7],
  results_leroux[7]
)
all_dic <- unlist(lapply(all_results, function(x) x$dic))
all_waic <- unlist(lapply(all_results, function(x) x$waic))
all_cpo <- unlist(lapply(all_results, function(x) x$cpo))
id_nospatial_2 <- which(
  unlist(mae_nospatial[12:13]) %in% min(unlist(mae_nospatial[12:13]))
)
all_dic[id_nospatial_2]
all_waic[id_nospatial_2]
all_cpo[id_nospatial_2]
mae_besag[id_nospatial_2 + 11]
all_dic[id_nospatial_2 + 2]
all_waic[id_nospatial_2 + 2]
all_cpo[id_nospatial_2 + 2]
mae_bym2[id_nospatial_2 + 11]
all_dic[id_nospatial_2 + 4]
all_waic[id_nospatial_2 + 4]
all_cpo[id_nospatial_2 + 4]
mae_leroux[id_nospatial_2 + 11]
options(scipen = 10)
marginal_frame <- tibble(
  lower = c(
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$Gruene
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$Gruene
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$`(Intercept)`
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$`(Intercept)`
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$higher_education
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$higher_education
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$SPD
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$SPD
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$`log(trade_tax)`
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$`log(trade_tax)`
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$clinic
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$clinic
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$place_of_worship
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$place_of_worship
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$sex
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$sex
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$urb_dens
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$urb_dens
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$platform
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$platform
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$aerodrome
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$aerodrome
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$die_linke
        
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$die_linke
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$FDP
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$FDP
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$nursing_home
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$nursing_home
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$retail
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$retail
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$pop_dens
      )
    )[1],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$pop_dens
      )
    )[1]
  ),
  mean = c(
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$Gruene
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$Gruene
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$`(Intercept)`
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$`(Intercept)`
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$higher_education
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$higher_education
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$SPD
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$SPD
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$`log(trade_tax)`
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$`log(trade_tax)`
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$clinic
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$clinic
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$place_of_worship
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$place_of_worship
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$sex
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$sex
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$urb_dens
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$urb_dens
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$platform
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$platform
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$aerodrome
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$aerodrome
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$die_linke
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$die_linke
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$FDP
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$FDP
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$nursing_home
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$nursing_home
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$retail
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$retail
    ),
    inla.emarginal(
      exp,
      models_nospatial[[id_nospatial]]$marginals.fixed$pop_dens
    ),
    inla.emarginal(
      exp,
      models_bym2[[id_nospatial]]$marginals.fixed$pop_dens
    )
  ),
  upper = c(
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$Gruene
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$Gruene
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$`(Intercept)`
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$`(Intercept)`
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$higher_education
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$higher_education
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$SPD
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$SPD
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$`log(trade_tax)`
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$`log(trade_tax)`
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$clinic
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$clinic
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$place_of_worship
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$place_of_worship
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$sex
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$sex
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$urb_dens
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$urb_dens
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$platform
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$platform
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$aerodrome
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$aerodrome
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$die_linke
        
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$die_linke
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$FDP
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$FDP
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$nursing_home
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$nursing_home
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$retail
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$retail
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_nospatial[[id_nospatial]]$marginals.fixed$pop_dens
      )
    )[2],
    inla.qmarginal(
      c(0.025, 0.975),
      inla.tmarginal(
        exp, models_bym2[[id_nospatial]]$marginals.fixed$pop_dens
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
marginal_frame$variable <- ordered(marginal_frame$variable, levels = unique(marginal_frame$variable))
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

models_bym2[[id_nospatial_2 + 11]]$summary.fixed[
  order(models_bym2[[id_nospatial_2 + 11]]$summary.fixed$mean),
]
sapply(
  models_bym2[[id_nospatial_2 + 11]]$marginals.fixed[
    rownames(models_bym2[[id_nospatial_2 + 11]]$summary.fixed[
      order(models_bym2[[id_nospatial_2 + 11]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_bym2[[id_nospatial_2 + 11]]$marginals.fixed[
    rownames(models_bym2[[id_nospatial_2 + 11]]$summary.fixed[
      order(models_bym2[[id_nospatial_2 + 11]]$summary.fixed$mean),
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
newest_numbers$rr <- models_bym2[[id_nospatial_2 + 11]]$summary.fitted.values$mean
csi <- models_bym2[[id_nospatial_2 + 11]]$marginals.random$idarea_1[
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
zeta <- lapply(csi, function(x) inla.emarginal(exp, x))
zeta_cutoff <- c(0.1, 0.5, 0.9, 1, 1.4, 1.8, 2.2, 2.6)
cat_zeta <- cut(
  unlist(zeta),
  breaks = zeta_cutoff,
  include.lowest = TRUE
)
newest_numbers$cat_zeta <- cat_zeta
newest_numbers$prob_csi <- cat_csi
mat_marg <- matrix(NA, nrow = nrow(newest_numbers), ncol = 100000)
m <- models_bym2[[id_nospatial_2 + 11]]$marginals.random$idarea_1
for (i in seq_len(nrow(newest_numbers))) {
  u <- m[[i]]
  mat_marg[i, ] <- inla.rmarginal(100000, u)
}
var_u <- apply(mat_marg, 2, var)
var_v <- inla.rmarginal(
  100000,
  inla.tmarginal(
    function(x) 1 / x,
    models_bym2[[id_nospatial_2 + 11]]$marginals.hyperpar$`Precision for idarea_1`
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
library(patchwork)
plot_2 + plot_3
models_bym2[[id_nospatial]]$summary.hyperpar
