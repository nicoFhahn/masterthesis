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
demo_ranks[1:20, ]
demo_ranks[21:40, ]
demo_ranks[41:60, ]
demo_dic[which(demo_ranks[1:20, ]$total %in% min(demo_ranks[1:20, ]$total))]
demo_dic[which(demo_ranks[21:40, ]$total %in% min(demo_ranks[21:40, ]$total)) + 20]
demo_dic[which(demo_ranks[41:60, ]$total %in% min(demo_ranks[41:60, ]$total)) + 40]
demo_waic[which(demo_ranks[1:20, ]$total %in% min(demo_ranks[1:20, ]$total))]
demo_waic[which(demo_ranks[21:40, ]$total %in% min(demo_ranks[21:40, ]$total)) + 20]
demo_waic[which(demo_ranks[41:60, ]$total %in% min(demo_ranks[41:60, ]$total)) + 40]
demo_cpo[which(demo_ranks[1:20, ]$total %in% min(demo_ranks[1:20, ]$total))]
demo_cpo[which(demo_ranks[21:40, ]$total %in% min(demo_ranks[21:40, ]$total)) + 20]
demo_cpo[which(demo_ranks[41:60, ]$total %in% min(demo_ranks[41:60, ]$total)) + 40]
options(scipen = 10)
models_leroux[[10]]$summary.fixed
inla.emarginal(exp, models_leroux[[10]]$marginals.fixed$Gewerbesteuer)
range(newest_numbers$Gewerbesteuer)
inla.emarginal(exp, models_leroux[[10]]$marginals.fixed$Gewerbesteuer)^25000
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_leroux[[10]]$marginals.fixed$Gewerbesteuer)
)
inla.emarginal(exp, models_leroux[[10]]$marginals.fixed$einkuenfte_gesamt)
range(newest_numbers$einkuenfte_gesamt)
inla.emarginal(exp, models_leroux[[10]]$marginals.fixed$einkuenfte_gesamt)^1000
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_leroux[[10]]$marginals.fixed$einkuenfte_gesamt)
)
inla.emarginal(exp, models_leroux[[10]]$marginals.fixed$`(Intercept)`)
inla.qmarginal(
  c(0.025, 0.975),
  inla.tmarginal(exp, models_leroux[[10]]$marginals.fixed$`(Intercept)`)
)
sum(models_leroux[[10]]$cpo$failure)
predicted.p.value <- c()
n <- nrow(newest_numbers)
for (i in (1:n)) {
  predicted.p.value[i] <- inla.pmarginal(
    q = newest_numbers$CumNumberTestedIll[i],
    marginal = models_leroux[[10]]$marginals.fitted.values[[i]]
  )
}
plot(
  newest_numbers$CumNumberTestedIll,
  models_leroux[[10]]$summary.fitted.values$mean,
  xlab = "Observed Values",
  ylab = "Mean Post. Pred. Distr."
)
hist(predicted.p.value, main = "", xlab = "Posterior predictive p-value")
csi <- models_leroux[[10]]$marginals.random$idarea_1[1:nrow(newest_numbers)]
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
a <- 0
prob_csi <- lapply(csi, function(x) 1 - inla.pmarginal(a, x))
newest_numbers$prob_csi <- unlist(prob_csi)
mat_marg <- matrix(
  NA,
  nrow(newest_numbers),
  ncol = 100000
)
m <- models_leroux[[10]]$marginals.random$idarea_1
for (i in seq_len(nrow(newest_numbers))) {
  u <- m[[i]]
  mat_marg[i, ] <- inla.rmarginal(100000, u)
}
var_u <- apply(mat_marg, 2, var)
var_v <- inla.rmarginal(
  100000,
  inla.tmarginal(
    function(x) 1 / x,
    models_leroux[[10]]$marginals.hyperpar$`Precision for idarea_1`
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
infra_ranks[1:6, ]
infra_ranks[7:12, ]
infra_ranks[13:18, ]
infra_dic[which(infra_ranks[1:6, ]$total %in% min(infra_ranks[1:6, ]$total))]
infra_dic[which(infra_ranks[7:12, ]$total %in% min(infra_ranks[7:12, ]$total)) + 6]
infra_dic[which(infra_ranks[13:18, ]$total %in% min(infra_ranks[13:18, ]$total)) + 12]
infra_waic[which(infra_ranks[1:6, ]$total %in% min(infra_ranks[1:6, ]$total))]
infra_waic[which(infra_ranks[7:12, ]$total %in% min(infra_ranks[7:12, ]$total)) + 6]
infra_waic[which(infra_ranks[13:18, ]$total %in% min(infra_ranks[13:18, ]$total)) + 12]
infra_cpo[which(infra_ranks[1:6, ]$total %in% min(infra_ranks[1:6, ]$total))]
infra_cpo[which(infra_ranks[7:12, ]$total %in% min(infra_ranks[7:12, ]$total)) + 6]
infra_cpo[which(infra_ranks[13:18, ]$total %in% min(infra_ranks[13:18, ]$total)) + 12]

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
