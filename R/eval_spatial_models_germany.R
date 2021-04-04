library(readr)
library(sf)
library(INLA)
source("R/germany_leroux_models.R")
source("R/germany_besagproper_models.R")
source("R/germany_bym2_models.R")
source("R/germany_nospatial_models.R")
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
####################### DEMOGRAPHIC MODELS
demo_results <- c(
  results_besag[1:6], results_bym2[1:6],
  results_leroux[1:6], results_nospatial[1:6]
)
demo_dic <- unlist(lapply(demo_results, function(x) x$dic))
demo_waic <- unlist(lapply(demo_results, function(x) x$waic))
demo_cpo <- unlist(lapply(demo_results, function(x) x$cpo))
min(unlist(mae_besag[1:11]))
min(unlist(mae_bym2[1:11]))
min(unlist(mae_leroux[1:11]))
min(unlist(mae_nospatial[1:11]))
id_besag <- which(unlist(mae_besag[1:11]) %in% min(unlist(mae_besag[1:11])))
demo_dic[id_besag]
demo_waic[id_besag]
demo_cpo[id_besag]
mae_besag[id_besag]
id_bym2 <- which(unlist(mae_bym2[1:11]) %in% min(unlist(mae_bym2[1:11])))
demo_dic[id_bym2 + 11]
demo_waic[id_bym2 + 11]
demo_cpo[id_bym2 + 11]
mae_bym2[id_bym2]
id_leroux <- which(unlist(mae_leroux[1:11]) %in% min(unlist(mae_leroux[1:11])))
demo_dic[id_leroux + 22]
demo_waic[id_leroux + 22]
demo_cpo[id_leroux + 22]
mae_leroux[id_leroux]
id_nospatial <- which(unlist(mae_nospatial[1:11]) %in% min(unlist(mae_nospatial[1:11])))
demo_dic[id_nospatial + 33]
demo_waic[id_nospatial + 33]
demo_cpo[id_nospatial + 33]
mae_nospatial[id_nospatial]
options(scipen = 10)
models_leroux[[7]]$summary.fixed[
  order(models_leroux[[7]]$summary.fixed$mean),
]
sapply(
  models_leroux[[7]]$marginals.fixed[
    rownames(models_leroux[[7]]$summary.fixed[
      order(models_leroux[[7]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_leroux[[7]]$marginals.fixed[
    rownames(models_leroux[[7]]$summary.fixed[
      order(models_leroux[[7]]$summary.fixed$mean),
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
newest_numbers$rr <- models_leroux[[7]]$summary.fitted.values$mean
csi <- models_leroux[[7]]$marginals.random$idarea_1[
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
m <- models_leroux[[7]]$marginals.random$idarea_1
for (i in seq_len(nrow(newest_numbers))) {
  u <- m[[i]]
  mat_marg[i, ] <- inla.rmarginal(100000, u)
}
var_u <- apply(mat_marg, 2, var)
var_v <- inla.rmarginal(
  100000,
  inla.tmarginal(
    function(x) 1 / x,
    models_leroux[[7]]$marginals.hyperpar$`Precision for idarea_1`
  )
)
perc_var_u <- mean(var_u / (var_u + var_v))
perc_var_u

color_low <- "#002FA7"
color_high <- "#F50039"
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
  scale_fill_viridis_d(option = "B", direction = -1) +
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
  scale_fill_viridis_d(option = "B", direction = -1) +
  theme_minimal() +
  guides(
    fill = guide_legend(
      title = "Posterior probability"
    )
  )
plot_3
library(patchwork)
plot_2 + plot_3
################## INFRASTRUCTURAL MODELS
infra_results <- c(
  results_besag[7:8], results_bym2[7:8],
  results_leroux[7:8], results_nospatial[7:8]
)
infra_dic <- unlist(lapply(infra_results, function(x) x$dic))
infra_waic <- unlist(lapply(infra_results, function(x) x$waic))
infra_cpo <- unlist(lapply(infra_results, function(x) x$cpo))
min(unlist(mae_besag[12:15]))
min(unlist(mae_bym2[12:15]))
min(unlist(mae_leroux[12:15]))
min(unlist(mae_nospatial[12:15]))
id_besag <- which(unlist(mae_besag[12:15]) %in% min(unlist(mae_besag[12:15])))
infra_dic[id_besag]
infra_waic[id_besag]
infra_cpo[id_besag]
mae_besag[id_besag + 11]
id_bym2 <- which(unlist(mae_bym2[12:15]) %in% min(unlist(mae_bym2[12:15])))
infra_dic[id_bym2 + 4]
infra_waic[id_bym2 + 4]
infra_cpo[id_bym2 + 4]
mae_bym2[id_bym2 + 11]
id_leroux <- which(
  unlist(mae_leroux[12:15]) %in% min(unlist(mae_leroux[12:15]))
)
infra_dic[id_leroux + 8]
infra_waic[id_leroux + 8]
infra_cpo[id_leroux + 8]
mae_leroux[id_leroux + 11]
id_nospatial <- which(
  unlist(mae_nospatial[12:15]) %in% min(unlist(mae_nospatial[12:15]))
)
infra_dic[id_nospatial + 12]
infra_waic[id_nospatial + 12]
infra_cpo[id_nospatial + 12]
mae_nospatial[id_nospatial + 11]
options(scipen = 10)
models_nospatial[[14]]$summary.fixed[
  order(models_nospatial[[14]]$summary.fixed$mean),
]
sapply(
  models_nospatial[[14]]$marginals.fixed[
    rownames(models_nospatial[[14]]$summary.fixed[
      order(models_nospatial[[14]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_nospatial[[14]]$marginals.fixed[
    rownames(models_nospatial[[14]]$summary.fixed[
      order(models_nospatial[[14]]$summary.fixed$mean),
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
  ######################################### ALL MODELS
all_results <- c(results_besag[9], results_bym2[9], results_leroux[9], results_nospatial[9])
all_dic <- unlist(lapply(all_results, function(x) x$dic))
all_waic <- unlist(lapply(all_results, function(x) x$waic))
all_cpo <- unlist(lapply(all_results, function(x) x$cpo))
min(unlist(mae_besag[16:17]))
min(unlist(mae_bym2[16:17]))
min(unlist(mae_leroux[16:17]))
min(unlist(mae_nospatial[16:17]))
id_besag <- which(unlist(mae_besag[16:17]) %in% min(unlist(mae_besag[16:17])))
all_dic[id_besag]
all_waic[id_besag]
all_cpo[id_besag]
mae_besag[id_besag + 15]
id_bym2 <- which(unlist(mae_bym2[16:17]) %in% min(unlist(mae_bym2[16:17])))
all_dic[id_bym2 + 2]
all_waic[id_bym2 + 2]
all_cpo[id_bym2 + 2]
mae_bym2[id_bym2 + 15]
id_leroux <- which(
  unlist(mae_leroux[16:17]) %in% min(unlist(mae_leroux[16:17]))
)
all_dic[id_leroux + 4]
all_waic[id_leroux + 4]
all_cpo[id_leroux + 4]
mae_leroux[id_leroux + 15]
id_nospatial <- which(
  unlist(mae_nospatial[16:17]) %in% min(unlist(mae_nospatial[16:17]))
)
all_dic[id_nospatial + 6]
all_waic[id_nospatial + 6]
all_cpo[id_nospatial + 6]
mae_nospatial[id_nospatial + 15]

options(scipen = 10)
models_nospatial[[16]]$summary.fixed[
  order(models_nospatial[[16]]$summary.fixed$mean),
]
sapply(
  models_nospatial[[16]]$marginals.fixed[
    rownames(models_nospatial[[16]]$summary.fixed[
      order(models_nospatial[[16]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_nospatial[[16]]$marginals.fixed[
    rownames(models_nospatial[[16]]$summary.fixed[
      order(models_nospatial[[16]]$summary.fixed$mean),
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
  pal <- colorFactor(
  viridis(6, direction = -1, option = "B"),
  domain = newest_numbers$cat_zeta
)
leaflet(newest_numbers) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~ pal(cat_zeta),
    fillOpacity = 0.7,
    color = "black",
    group = "Relative risk",
    label = paste(
      "Municipality: ", newest_numbers$Landkreis
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = newest_numbers,
    pal = pal,
    values = ~cat_zeta,
    group = "RR"
  )
