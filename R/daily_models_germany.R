source("R/germany_leroux_models.R")
source("R/germany_besagproper_models.R")
source("R/germany_bym2_models.R")
source("R/preprocess_germany.R")
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
inla.emarginal(exp, models_leroux[[9]]$marginals.fixed$Gewerbesteuer)
range(newest_numbers$Gewerbesteuer)
inla.emarginal(exp, models_leroux[[9]]$marginals.fixed$Gewerbesteuer)^25000
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_leroux[[9]]$marginals.fixed$Gewerbesteuer)
)
inla.emarginal(exp, models_leroux[[9]]$marginals.fixed$einkuenfte_gesamt)
range(newest_numbers$einkuenfte_gesamt)
inla.emarginal(exp, models_leroux[[9]]$marginals.fixed$einkuenfte_gesamt)^1000
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_leroux[[9]]$marginals.fixed$einkuenfte_gesamt)
)
inla.emarginal(exp, models_leroux[[9]]$marginals.fixed$`(Intercept)`)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_leroux[[9]]$marginals.fixed$`(Intercept)`)
)
sum(models_leroux[[9]]$cpo$failure)
predicted.p.value <- c()
n <- nrow(newest_numbers)
for (i in (1:n)) {
  predicted.p.value[i] <- inla.pmarginal(
    q = newest_numbers$CumNumberTestedIll[i],
    marginal = models_leroux[[9]]$marginals.fitted.values[[i]]
  )
}
plot(
  newest_numbers$CumNumberTestedIll,
  models_leroux[[9]]$summary.fitted.values$mean,
  xlab = "Observed Values",
  ylab = "Mean Post. Pred. Distr."
)
hist(predicted.p.value, main = "", xlab = "Posterior predictive p-value")
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
var_v <- inla.rmarginal(
  100000,
  inla.tmarginal(
    function(x) 1 / x,
    models_leroux[[9]]$marginals.hyperpar$`Precision for idarea_1`
  )
)

perc_var_u <- mean(var_u / (var_u + var_v))
perc_var_u
marg_hyper <- inla.hyperpar.sample(100000, models_leroux[[10]])
perc_var_u1 <- mean(marg_hyper[, 1] / (marg_hyper[, 1] + marg_hyper[, 2]))
perc_var_u1
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
sapply(models_bym2[[26]]$marginals.fixed, inla.emarginal, fun = exp)
sapply(models_bym2[[26]]$marginals.fixed, inla.qmarginal, p = c(0.025, 0.975))
range(newest_numbers$marketplace)
range(newest_numbers$bakeries)
range(newest_numbers$hairdresser)
range(newest_numbers$shops)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$marketplace)^ 0.01
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$bakeries)^ 0.1
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$hairdresser)^ 0.1
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$shops)^ 0.1
sum(models_bym2[[26]]$cpo$failure)
predicted.p.value <- c()
n <- nrow(newest_numbers)
for (i in (1:n)) {
  predicted.p.value[i] <- inla.pmarginal(
    q = newest_numbers$CumNumberTestedIll[i],
    marginal = models_bym2[[26]]$marginals.fitted.values[[i]]
  )
}
plot(
  newest_numbers$CumNumberTestedIll,
  models_bym2[[26]]$summary.fitted.values$mean,
  xlab = "Observed Values",
  ylab = "Mean Post. Pred. Distr."
)
hist(predicted.p.value, main = "", xlab = "Posterior predictive p-value")
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
  inla.qmarginal,
  p = c(0.025, 0.975)
)
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
  inla.qmarginal,
  p = c(0.025, 0.975)
)
range(newest_numbers$marketplace)
range(newest_numbers$bakeries)
range(newest_numbers$hairdresser)
range(newest_numbers$shops)
inla.emarginal(exp, models_leroux[[34]]$marginals.fixed$afd)^ 0.01
inla.emarginal(exp, models_leroux[[34]]$marginals.fixed$sonstige)^ 0.001
inla.emarginal(exp, models_leroux[[34]]$marginals.fixed$schools)^ 0.1
range(newest_numbers$schools)
sum(models_leroux[[34]]$cpo$failure)
predicted.p.value <- c()
n <- nrow(newest_numbers)
for (i in (1:n)) {
  predicted.p.value[i] <- inla.pmarginal(
    q = newest_numbers$CumNumberTestedIll[i],
    marginal = models_leroux[[34]]$marginals.fitted.values[[i]]
  )
}
plot(
  newest_numbers$CumNumberTestedIll,
  models_leroux[[34]]$summary.fitted.values$mean,
  xlab = "Observed Values",
  ylab = "Mean Post. Pred. Distr."
)
hist(predicted.p.value, main = "", xlab = "Posterior predictive p-value")
csi <- models_leroux[[34]]$marginals.random$idarea_1[1:nrow(newest_numbers)]
zeta <- lapply(csi, function(x) inla.emarginal(exp, x))
zeta_cutoff <- c(0.1, 0.5, 0.9, 1.3, 1.7, 2.1, 2.5)
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
    models_leroux[[34]]$marginals.hyperpar$`Precision for idarea_1`
  )
)
newest_numbers$rr <- models_leroux[[34]]$summary.fitted.values$mean
csi <- models_leroux[[34]]$marginals.random$idarea_1[1:399]
a <- 0
prob.csi <- lapply(csi, function(x) {1 - inla.pmarginal(a, x)})
newest_numbers$prob_csi <- unlist(prob.csi)
color_low <- "#002FA7"
color_high <- "#F50039"
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
    label = "Relative risk based on all variables",
    subtitle = "Germany"
  ) +
  scale_fill_viridis(option = "B", direction = -1) +
  theme_minimal()
plot_3
