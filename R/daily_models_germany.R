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
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$`(Intercept)`)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$marketplace)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$entertainment)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$hairdresser)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$shops)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$bakeries)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$place_of_worship)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$platform)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$nursing_home)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$schools)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$kindergarten)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$aerodrome)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$retail)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$restaurant)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$higher_education)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$office)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$clinic)
inla.emarginal(exp, models_bym2[[26]]$marginals.fixed$sport)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$`(Intercept)`)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$marketplace)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$entertainment)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$hairdresser)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$shops)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$bakeries)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$place_of_worship)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$platform)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$nursing_home)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$schools)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$kindergarten)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$aerodrome)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$retail)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$restaurant)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$higher_education)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$office)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$clinic)
)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_bym2[[26]]$marginals.fixed$sport)
)
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
plot_1 <- ggplot(data = newest_numbers) +
  geom_sf(aes(fill = rr)) +
  ggtitle(
    label = "Relative risk based on demographic variables",
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


models[[9]]
models[11:18][unlist(results_sum[6]) == min(unlist(results_sum[6]))][[1]]$call
results[6]
models[[11]]
models[13:14][unlist(results_sum[7]) == min(unlist(results_sum[7]))][[1]]$call
results[[7]]
models[[14]]
models[15:16][unlist(results_sum[8]) == min(unlist(results_sum[8]))][[1]]$call
results[[8]]
models[[16]]
models[unlist(results_sum) == min(unlist(results_sum))][[1]]$call
