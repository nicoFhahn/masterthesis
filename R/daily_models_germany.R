# source("R/germany_leroux_models.R")
# source("R/germany_besagproper_models.R")
# source("R/germany_bym2_models.R")
newest_numbers <- read_csv("eval_data/newest_numbers_germany_march10.csv")
germany_sf <- read_sf("wrangled_data/shapes_germany.shp")
newest_numbers <- merge(
  newest_numbers,
  germany_sf,
  by.x = "Kreis",
  by.y = "Kennziffer"
)
newest_numbers <- st_as_sf(newest_numbers)
load("models/leroux_germany.Rda")
models_final_leroux <- models_final
load("models/besagproper_germany.Rda")
models_final_besag <- models_final
load("models/bym2_germany.Rda")
models_final_bym2 <- models_final
rm(models_final)
models_leroux <- models_final_leroux[[1]]
models_besag <- models_final_besag[[1]]
models_bym2 <- models_final_bym2[[1]]
results_leroux <- models_final_leroux[[2]]
results_besag <- models_final_besag[[2]]
results_bym2 <- models_final_bym2[[2]]
newest_numbers[order(newest_numbers$CumNumberTestedIll, decreasing = TRUE), ][1:5, c("Landkreis", "PopulationTotal", "CumNumberTestedIll")]
####################### DEMOGRAPHIC MODELS
demo_results <- c(results_leroux[1:6], results_besag[1:6], results_bym2[1:6])
demo_dic <- unlist(lapply(demo_results, function(x) x$dic))
demo_waic <- unlist(lapply(demo_results, function(x) x$waic))
demo_cpo <- unlist(lapply(demo_results, function(x) x$cpo))
demo_dic_rank <- unlist(lapply(demo_dic, function(x, ...) which(sort(demo_dic) %in% x)))
demo_waic_rank <- unlist(lapply(demo_waic, function(x, ...) which(sort(demo_waic) %in% x)))
demo_cpo_rank <- unlist(lapply(demo_cpo, function(x, ...) which(sort(demo_cpo) %in% x)))
demo_ranks <- tibble(
  dic = demo_dic_rank,
  waic = demo_waic_rank,
  cpo = demo_cpo_rank
)
demo_ranks$total <- rowSums(demo_ranks)
demo_ranks[1:11, ]
demo_ranks[12:22, ]
demo_ranks[23:33, ]
demo_ranks[34:44, ]
demo_ranks[45:55, ]
demo_ranks[56:66, ]
demo_dic[which(demo_ranks[1:22, ]$total %in% min(demo_ranks[1:22, ]$total))]
demo_dic[which(demo_ranks[23:44, ]$total %in% min(demo_ranks[23:44, ]$total)) + 22]
demo_dic[which(demo_ranks[45:66, ]$total %in% min(demo_ranks[45:66, ]$total)) + 44]
demo_waic[which(demo_ranks[1:22, ]$total %in% min(demo_ranks[1:22, ]$total))]
demo_waic[which(demo_ranks[23:44, ]$total %in% min(demo_ranks[23:44, ]$total)) + 22]
demo_waic[which(demo_ranks[45:66, ]$total %in% min(demo_ranks[45:66, ]$total)) + 44]
demo_cpo[which(demo_ranks[1:22, ]$total %in% min(demo_ranks[1:22, ]$total))]
demo_cpo[which(demo_ranks[23:44, ]$total %in% min(demo_ranks[23:44, ]$total)) + 22]
demo_cpo[which(demo_ranks[45:66, ]$total %in% min(demo_ranks[45:66, ]$total)) + 44]
options(scipen = 10)
models_leroux[[9]]$summary.fixed
sapply(
  models_leroux[[9]]$marginals.fixed[
    rownames(models_leroux[[9]]$summary.fixed[
      order(models_leroux[[9]]$summary.fixed$mean), 
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_leroux[[9]]$marginals.fixed[
    rownames(models_leroux[[9]]$summary.fixed[
      order(models_leroux[[9]]$summary.fixed$mean), 
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
inla.emarginal(exp, models_leroux[[9]]$marginals.fixed$Gewerbesteuer) ^ 25000
inla.emarginal(exp, models_leroux[[9]]$marginals.fixed$einkuenfte_gesamt) ^ 1000
csi <- models_leroux[[9]]$marginals.random$idarea_1[1:nrow(newest_numbers)]
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
models_bym2[[26]]$summary.fixed[order(models_bym2[[26]]$summary.fixed$mean), ]
sapply(
  models_bym2[[26]]$marginals.fixed[
    rownames(models_bym2[[26]]$summary.fixed[
      order(models_bym2[[26]]$summary.fixed$mean), 
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_bym2[[26]]$marginals.fixed[
    rownames(models_bym2[[26]]$summary.fixed[
      order(models_bym2[[26]]$summary.fixed$mean), 
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
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$marketplace)^ 0.1
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$entertainment)^ 0.1
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$hairdresser)^ 0.1
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$shops)^ 0.1
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$bakeries)^ 0.1
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$nursing_home)^ 0.1
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$aerodrome)^ 0.1
csi <- models_bym2[[26]]$marginals.random$idarea_1[1:nrow(newest_numbers)]
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
    models_bym2[[26]]$marginals.hyperpar$`Precision for idarea_1`
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
models_leroux[[34]]$summary.fixed[order(models_leroux[[34]]$summary.fixed$mean), ]
sapply(models_leroux[[34]]$marginals.fixed, inla.emarginal, fun = exp)
sapply(models_leroux[[34]]$marginals.fixed, inla.qmarginal, p = c(0.025, 0.975))
sapply(
  models_leroux[[34]]$marginals.fixed[
    rownames(models_leroux[[34]]$summary.fixed[
      order(models_leroux[[34]]$summary.fixed$mean), 
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_leroux[[34]]$marginals.fixed[
    rownames(models_leroux[[34]]$summary.fixed[
      order(models_leroux[[34]]$summary.fixed$mean), 
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
inla.emarginal(exp, models_leroux[[34]]$marginals.fixed$afd)^ 0.01
inla.emarginal(exp, models_leroux[[34]]$marginals.fixed$sonstige)^ 0.001
inla.emarginal(exp, models_leroux[[34]]$marginals.fixed$schools)^ 0.1
inla.emarginal(exp, models_leroux[[34]]$marginals.fixed$arbeitslose_auslaender)^ 1
inla.emarginal(exp, models_leroux[[34]]$marginals.fixed$pop_dens)^ 250
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
newest_numbers$rr <- models_leroux[[34]]$summary.fitted.values$mean
csi <- models_leroux[[34]]$marginals.random$idarea_1[1:399]
a <- 0
prob_csi <- lapply(csi, function(x) {1 - inla.pmarginal(a, x)})
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
mat_marg <- matrix(NA, nrow = 399, ncol = 100000)
m <- models_leroux[[34]]$marginals.random$idarea_1
for (i in seq_len(399)) {
  u <- m[[i]]
  mat_marg[i, ] <- inla.rmarginal(100000, u)
}
var_u <- apply(mat_marg, 2, var)
var_v <- inla.rmarginal(
  100000,
  inla.tmarginal(
    function(x) 1/x,
    models_leroux[[34]]$marginals.hyperpar$`Precision for idarea_1`
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
      "Municipality: ", newest_numbers$Landkreis
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
