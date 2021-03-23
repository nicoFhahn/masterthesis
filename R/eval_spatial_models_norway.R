library(readr)
library(sf)
library(INLA)
# source("R/germany_leroux_models.R")
# source("R/germany_besagproper_models.R")
# source("R/germany_bym2_models.R")
newest_numbers <- read_csv("eval_data/newest_numbers_norway_march20.csv")
norge_sf <- read_sf("wrangled_data/shapes_norge.shp")
newest_numbers <- merge(
  newest_numbers,
  norge_sf,
  by = "kommune_no"
)
newest_numbers <- st_as_sf(newest_numbers)
load("models/leroux_norway.Rda")
models_final_leroux <- models_final
load("models/besagproper_norway.Rda")
models_final_besag <- models_final
load("models/bym2_norway.Rda")
models_final_bym2 <- models_final
rm(models_final)
models_leroux <- models_final_leroux[[1]]
models_besag <- models_final_besag[[1]]
models_bym2 <- models_final_bym2[[1]]
results_leroux <- models_final_leroux[[2]]
results_besag <- models_final_besag[[2]]
results_bym2 <- models_final_bym2[[2]]
mae_leroux <- models_final_leroux[[3]]
mae_besag <- models_final_besag[[3]]
mae_bym2 <- models_final_bym2[[3]]
newest_numbers[order(newest_numbers$value, decreasing = TRUE), ][1:5, c("kommune_name", "population", "value")]
####################### DEMOGRAPHIC MODELS
demo_results <- c(results_besag[1:6], results_bym2[1:6], results_leroux[1:6])
demo_dic <- unlist(lapply(demo_results, function(x) x$dic))
demo_waic <- unlist(lapply(demo_results, function(x) x$waic))
demo_cpo <- unlist(lapply(demo_results, function(x) x$cpo))
min(unlist(mae_besag[1:22]))
min(unlist(mae_bym2[1:22]))
min(unlist(mae_leroux[1:22]))
id_besag <- which(unlist(mae_besag[1:22]) %in% min(unlist(mae_besag[1:22])))
demo_dic[id_besag]
demo_waic[id_besag]
demo_cpo[id_besag]
mae_besag[id_besag]
id_bym2 <- which(unlist(mae_bym2[1:22]) %in% min(unlist(mae_bym2[1:22])))
demo_dic[id_bym2 + 22]
demo_waic[id_bym2 + 22]
demo_cpo[id_bym2 + 22]
mae_bym2[id_bym2]
id_leroux <- which(unlist(mae_leroux[1:22]) %in% min(unlist(mae_leroux[1:22])))
demo_dic[id_leroux + 44]
demo_waic[id_leroux + 44]
demo_cpo[id_leroux + 44]
mae_leroux[id_leroux]
options(scipen = 10)
models_leroux[[13]]$summary.fixed[order(models_leroux[[13]]$summary.fixed$mean), ]

sapply(
  models_leroux[[13]]$marginals.fixed[
    rownames(models_leroux[[13]]$summary.fixed[
      order(models_leroux[[13]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_leroux[[13]]$marginals.fixed[
    rownames(models_leroux[[13]]$summary.fixed[
      order(models_leroux[[13]]$summary.fixed$mean),
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
inla.emarginal(exp, models_leroux[[22]]$marginals.fixed$pop_dens)^100
csi <- models_leroux[[22]]$marginals.random$idarea_1[1:nrow(newest_numbers)]
zeta <- lapply(csi, function(x) inla.emarginal(exp, x))
zeta_cutoff <- c(0.6, 0.9, 1.0, 1.1, 1.8)
cat_zeta <- cut(
  unlist(zeta),
  breaks = zeta_cutoff,
  include.lowest = TRUE
)
maps_cat_zeta <- data.frame(
  ID = newest_numbers$idarea_1,
  cat_zeta = cat_zeta
)
newest_numbers$cat_zeta <- cat_zeta
################## INFRASTRUCTURAL MODELS
infra_results <- c(results_leroux[7:8], results_besag[7:8], results_bym2[7:8])
infra_dic <- unlist(lapply(infra_results, function(x) x$dic))
infra_waic <- unlist(lapply(infra_results, function(x) x$waic))
infra_cpo <- unlist(lapply(infra_results, function(x) x$cpo))
infra_dic_rank <- unlist(lapply(infra_dic, function(x, ...) which(sort(infra_dic) %in% x)))
infra_waic_rank <- unlist(lapply(infra_waic, function(x, ...) which(sort(infra_waic) %in% x)))
infra_cpo_rank <- unlist(lapply(infra_cpo, function(x, ...) which(sort(infra_cpo) %in% x)))
infra_ranks <- tibble(
  dic = infra_dic_rank,
  waic = infra_waic_rank,
  cpo = infra_cpo_rank
)
infra_ranks$total <- rowSums(infra_ranks)
infra_ranks[1:8, ]
infra_ranks[9:16, ]
infra_ranks[17:24, ]
infra_dic[which(infra_ranks[1:8, ]$total %in% min(infra_ranks[1:8, ]$total))]
infra_dic[which(infra_ranks[9:16, ]$total %in% min(infra_ranks[9:16, ]$total)) + 8]
infra_dic[which(infra_ranks[17:24, ]$total %in% min(infra_ranks[17:24, ]$total)) + 16]
infra_waic[which(infra_ranks[1:8, ]$total %in% min(infra_ranks[1:8, ]$total))]
infra_waic[which(infra_ranks[9:16, ]$total %in% min(infra_ranks[9:16, ]$total)) + 8]
infra_waic[which(infra_ranks[17:24, ]$total %in% min(infra_ranks[17:24, ]$total)) + 16]
infra_cpo[which(infra_ranks[1:8, ]$total %in% min(infra_ranks[1:8, ]$total))]
infra_cpo[which(infra_ranks[9:16, ]$total %in% min(infra_ranks[9:16, ]$total)) + 8]
infra_cpo[which(infra_ranks[17:24, ]$total %in% min(infra_ranks[17:24, ]$total)) + 16]
options(scipen = 10)
models_leroux[[30]]$summary.fixed[order(models_leroux[[30]]$summary.fixed$mean), ]
sapply(
  models_leroux[[30]]$marginals.fixed[
    rownames(models_leroux[[30]]$summary.fixed[
      order(models_leroux[[30]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_leroux[[30]]$marginals.fixed[
    rownames(models_leroux[[30]]$summary.fixed[
      order(models_leroux[[30]]$summary.fixed$mean),
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
range(newest_numbers$nursing_home)
range(newest_numbers$retail)
range(newest_numbers$kindergarten)
range(newest_numbers$pop_dens)
inla.emarginal(exp, models_leroux[[30]]$marginals.fixed$nursing_home)^0.1
inla.emarginal(exp, models_leroux[[30]]$marginals.fixed$retail)^0.1
inla.emarginal(exp, models_leroux[[30]]$marginals.fixed$kindergarten)^0.1
inla.emarginal(exp, models_leroux[[30]]$marginals.fixed$pop_dens)^100
sum(models_leroux[[30]]$cpo$failure)
csi <- models_leroux[[30]]$marginals.random$idarea_1[1:nrow(newest_numbers)]
zeta <- lapply(csi, function(x) inla.emarginal(exp, x))
zeta_cutoff <- c(0.6, 0.9, 1.0, 1.1, 1.8)
cat_zeta <- cut(
  unlist(zeta),
  breaks = zeta_cutoff,
  include.lowest = TRUE
)
maps_cat_zeta <- data.frame(
  ID = newest_numbers$idarea_1,
  cat_zeta = cat_zeta
)
newest_numbers$cat_zeta <- cat_zeta
var_v <- inla.rmarginal(
  100000,
  inla.tmarginal(
    function(x) 1 / x,
    models_leroux[[30]]$marginals.hyperpar$`Precision for idarea_1`
  )
)
######################################### ALL MODELS
all_results <- c(results_leroux[9], results_besag[9], results_bym2[9])
all_dic <- unlist(lapply(all_results, function(x) x$dic))
all_waic <- unlist(lapply(all_results, function(x) x$waic))
all_cpo <- unlist(lapply(all_results, function(x) x$cpo))
all_dic_rank <- unlist(lapply(all_dic, function(x, ...) which(sort(all_dic) %in% x)))
all_waic_rank <- unlist(lapply(all_waic, function(x, ...) which(sort(all_waic) %in% x)))
all_cpo_rank <- unlist(lapply(all_cpo, function(x, ...) which(sort(all_cpo) %in% x)))
all_ranks <- tibble(
  dic = all_dic_rank,
  waic = all_waic_rank,
  cpo = all_cpo_rank
)
all_ranks$total <- rowSums(all_ranks)
all_ranks[1:4, ]
all_ranks[5:8, ]
all_ranks[9:12, ]
all_dic[which(all_ranks[1:4, ]$total %in% min(all_ranks[1:4, ]$total))]
all_dic[which(all_ranks[5:8, ]$total %in% min(all_ranks[5:8, ]$total)) + 4]
all_dic[which(all_ranks[9:12, ]$total %in% min(all_ranks[9:12, ]$total)) + 8]
all_waic[which(all_ranks[1:4, ]$total %in% min(all_ranks[1:4, ]$total))]
all_waic[which(all_ranks[5:8, ]$total %in% min(all_ranks[5:8, ]$total)) + 4]
all_waic[which(all_ranks[9:12, ]$total %in% min(all_ranks[9:12, ]$total)) + 8]
all_cpo[which(all_ranks[1:4, ]$total %in% min(all_ranks[1:4, ]$total))]
all_cpo[which(all_ranks[5:8, ]$total %in% min(all_ranks[5:8, ]$total)) + 4]
all_cpo[which(all_ranks[9:12, ]$total %in% min(all_ranks[9:12, ]$total)) + 8]
options(scipen = 10)
models_bym2[[31]]$summary.fixed[order(models_bym2[[31]]$summary.fixed$mean), ]
sapply(
  models_bym2[[31]]$marginals.fixed[
    rownames(models_bym2[[31]]$summary.fixed[
      order(models_bym2[[31]]$summary.fixed$mean),
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_bym2[[31]]$marginals.fixed[
    rownames(models_bym2[[31]]$summary.fixed[
      order(models_bym2[[31]]$summary.fixed$mean),
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
range(newest_numbers$marketplace)
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$marketplace)^0.1
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$entertainment)^0.1
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$bakeries)^0.1
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$clinic)^0.1
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$retail)^0.1
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$nursing_home)^0.1
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$kindergarten)^0.1
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$place_of_worship)^0.1
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$marketplace)^0.1
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$platform)^0.1
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$unemp_immg)^0.5
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$mining_pt_com)^0.5
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$mining_ft_com)^0.5
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$workers_pt_com)^0.5
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$construction_pt_com)^0.5
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$pop_dens)^100
inla.emarginal(exp, models_bym2[[31]]$marginals.fixed$urb_dens)^10
newest_numbers$rr <- models_bym2[[31]]$summary.fitted.values$mean
csi <- models_bym2[[31]]$marginals.random$idarea_1[1:356]
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
zeta_cutoff <- c(0.1, 0.5, 0.9, 1, 1.4, 1.8, 2.2, 2.6, 3, 5, 10, 15)
cat_zeta <- cut(
  unlist(zeta),
  breaks = zeta_cutoff,
  include.lowest = TRUE
)
newest_numbers$cat_zeta <- cat_zeta
newest_numbers$prob_csi <- cat_csi
mat_marg <- matrix(NA, nrow = 356, ncol = 100000)
m <- models_bym2[[31]]$marginals.random$idarea_1
for (i in seq_len(356)) {
  u <- m[[i]]
  mat_marg[i, ] <- inla.rmarginal(100000, u)
}
var_u <- apply(mat_marg, 2, var)
var_v <- inla.rmarginal(
  100000,
  inla.tmarginal(
    function(x) 1 / x,
    models_bym2[[31]]$marginals.hyperpar$`Precision for idarea_1`
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
    subtitle = "Norway"
  ) +
  scale_fill_gradient2(
    "SIR",
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
    subtitle = "Norway"
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
    subtitle = "Norway"
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
library(leaflet)
pal <- colorFactor(
  viridis(4, direction = -1, option = "B"),
  domain = newest_numbers$prob_csi
)
leaflet(newest_numbers) %>%
  addMapboxGL(
    style = "mapbox://styles/mapbox/streets-v9",
    accessToken = "pk.eyJ1Ijoibmljb2hhaG4iLCJhIjoiY2p2YzU4ZWNiMWY4ZTQ2cGZsZHB5cDJzZiJ9.Sg3fJKvEhfkuhKx7aBBjZA"
  ) %>%
  addPolygons(
    weight = 1,
    fillColor = ~ pal(prob_csi),
    fillOpacity = 0.7,
    color = "black",
    group = "Relative risk",
    label = paste(
      "Municipality: ", newest_numbers$kommune_name
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = newest_numbers,
    pal = pal,
    values = ~prob_csi,
    group = "RR"
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
      "Municipality: ", newest_numbers$kommune_name, "<br>",
      "Zeta: ", newest_numbers$cat_zeta
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  addLegend(
    data = newest_numbers,
    pal = pal,
    values = ~cat_zeta,
    group = "RR"
  )
