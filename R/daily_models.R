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
demo_results <- c(results_leroux[1:5], results_besag[1:5], results_bym2[1:5])
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
demo_ranks[1:18, ]
demo_ranks[19:36, ]
demo_ranks[37:54, ]
demo_dic[which(demo_ranks[1:18, ]$total %in% min(demo_ranks[1:18, ]$total))]
demo_dic[which(demo_ranks[19:36, ]$total %in% min(demo_ranks[19:36, ]$total)) + 18]
demo_dic[which(demo_ranks[37:54, ]$total %in% min(demo_ranks[37:54, ]$total)) + 36]
demo_waic[which(demo_ranks[1:18, ]$total %in% min(demo_ranks[1:18, ]$total))]
demo_waic[which(demo_ranks[19:36, ]$total %in% min(demo_ranks[19:36, ]$total)) + 18]
demo_waic[which(demo_ranks[37:54, ]$total %in% min(demo_ranks[37:54, ]$total)) + 36]
demo_cpo[which(demo_ranks[1:18, ]$total %in% min(demo_ranks[1:18, ]$total))]
demo_cpo[which(demo_ranks[19:36, ]$total %in% min(demo_ranks[19:36, ]$total)) + 18]
demo_cpo[which(demo_ranks[37:54, ]$total %in% min(demo_ranks[37:54, ]$total)) + 36]
infra_results <- c(results_leroux[6], results_besag[6], results_bym2[6])
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
infra_ranks[1:4, ]
infra_ranks[5:8, ]
infra_ranks[9:12, ]
infra_dic[which(infra_ranks[1:4, ]$total %in% min(infra_ranks[1:4, ]$total))]
infra_dic[which(infra_ranks[5:8, ]$total %in% min(infra_ranks[5:8, ]$total)) + 4]
infra_dic[which(infra_ranks[9:12, ]$total %in% min(infra_ranks[9:12, ]$total)) + 8]
infra_waic[which(infra_ranks[1:4, ]$total %in% min(infra_ranks[1:4, ]$total))]
infra_waic[which(infra_ranks[5:8, ]$total %in% min(infra_ranks[5:8, ]$total)) + 4]
infra_waic[which(infra_ranks[9:12, ]$total %in% min(infra_ranks[9:12, ]$total)) + 8]
infra_cpo[which(infra_ranks[1:4, ]$total %in% min(infra_ranks[1:4, ]$total))]
infra_cpo[which(infra_ranks[5:8, ]$total %in% min(infra_ranks[5:8, ]$total)) + 4]
infra_cpo[which(infra_ranks[9:12, ]$total %in% min(infra_ranks[9:12, ]$total)) + 8]
inla.emarginal(exp, models_leroux[[16]]$marginals.fixed$`(Intercept)`)
inla.emarginal(exp, models_leroux[[7]]$marginals.fixed$pop_dens)
inla.emarginal(exp, models_leroux[[7]]$marginals.fixed$urb_dens)
inla.emarginal(exp, models_leroux[[7]]$marginals.fixed$sex)
inla.emarginal(exp, models_leroux[[7]]$marginals.fixed$Gewerbesteuer)
inla.emarginal(exp, models_leroux[[7]]$marginals.fixed$einkuenfte_gesamt)
newest_numbers$rr <- models_leroux[[7]]$summary.fitted.values$mean
color_low <- "#002FA7"
color_high <- "#F50039"
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
models[11:12][unlist(results_sum[6]) == min(unlist(results_sum[6]))][[1]]$call
results[6]
models[[11]]
models[13:14][unlist(results_sum[7]) == min(unlist(results_sum[7]))][[1]]$call
results[[7]]
models[[14]]
models[15:16][unlist(results_sum[8]) == min(unlist(results_sum[8]))][[1]]$call
results[[8]]
models[[16]]
models[unlist(results_sum) == min(unlist(results_sum))][[1]]$call