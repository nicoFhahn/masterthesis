source("R/norway_leroux_models.R")
source("R/norway_besagproper_models.R")
source("R/norway_bym2_models.R")
source("R/preprocess_norge.R")
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
models_leroux[[22]]$summary.fixed
sapply(
  models_leroux[[22]]$marginals.fixed[
    rownames(models_leroux[[22]]$summary.fixed[
      order(models_leroux[[22]]$summary.fixed$mean), 
    ])
  ],
  inla.emarginal,
  fun = exp
)
sapply(
  models_leroux[[22]]$marginals.fixed[
    rownames(models_leroux[[22]]$summary.fixed[
      order(models_leroux[[22]]$summary.fixed$mean), 
    ])
  ],
  inla.qmarginal,
  p = c(0.025, 0.975)
)
predicted.p.value <- c()
n <- nrow(newest_numbers)
for (i in (1:n)) {
  predicted.p.value[i] <- inla.pmarginal(
    q = newest_numbers$CumNumberTestedIll[i],
    marginal = models_leroux[[22]]$marginals.fitted.values[[i]]
  )
}
plot(
  newest_numbers$CumNumberTestedIll,
  models_leroux[[22]]$summary.fitted.values$mean,
  xlab = "Observed Values",
  ylab = "Mean Post. Pred. Distr."
)
hist(predicted.p.value, main = "", xlab = "Posterior predictive p-value")
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
var_v <- inla.rmarginal(
  100000,
  inla.tmarginal(
    function(x) 1 / x,
    models_leroux[[22]]$marginals.hyperpar$`Precision for idarea_1`
  )
)

perc_var_u <- mean(var_u / (var_u + var_v))
perc_var_u
marg_hyper <- inla.hyperpar.sample(100000, models_leroux[[22]])
perc_var_u1 <- mean(marg_hyper[, 1] / (marg_hyper[, 1] + marg_hyper[, 2]))
perc_var_u1