library(iml)
library(mlr)
load("models/predictive_norway.Rda")
results_tibble <- model_list[[1]]
models <- model_list[[2]]
newest_numbers_test <- model_list[[3]]
newest_numbers_train <- model_list[[4]]
feature_names <- model_list[[5]]
predictor_rf <- Predictor$new(
  models[[3]],
  data = newest_numbers_train[, 2:14],
  y = newest_numbers_train$value
)
predictor_fnn <- Predictor$new(
  models[[1]],
  data = newest_numbers_train[, 2:14],
  y = newest_numbers_train$value
)
predictor_xgboost <- Predictor$new(
  models[[5]],
  data = newest_numbers_train[, 2:14],
  y = newest_numbers_train$value
)
predictor_nnet <- Predictor$new(
  models[[2]],
  data = newest_numbers_train[, 2:14],
  y = newest_numbers_train$value
)
predictor_rpart <- Predictor$new(
  models[[4]],
  data = newest_numbers_train[, 2:14],
  y = newest_numbers_train$value
)
imp_rf <- FeatureImp$new(predictor_rf, loss = "mae")
imp_fnn <- FeatureImp$new(predictor_fnn, loss = "mae")
imp_xgboost <- FeatureImp$new(predictor_xgboost, loss = "mae")
imp_nnet <- FeatureImp$new(predictor_nnet, loss = "mae")
imp_rpart <- FeatureImp$new(predictor_rpart, loss = "mae")
imp_rf$results$feature <- feature_names$noice[match(imp_rf$results$feature, feature_names$ugly)]
plot(imp_rf) +
  theme_minimal() +
  ggtitle("Feature importance of the variables", subtitle = "Random forest")
imp_fnn$results$feature <- feature_names$noice[match(imp_fnn$results$feature, feature_names$ugly)]
plot(imp_fnn) +
  theme_minimal() +
  ggtitle("Feature importance of the variables", subtitle = "K nearest neighbours")
imp_xgboost$results$feature <- feature_names$noice[match(imp_xgboost$results$feature, feature_names$ugly)]
plot(imp_xgboost) +
  theme_minimal() +
  ggtitle("Feature importance of the variables", subtitle = "XGBoost")
imp_nnet$results$feature <- feature_names$noice[match(imp_nnet$results$feature, feature_names$ugly)]
plot(imp_nnet) +
  theme_minimal() +
  ggtitle("Feature importance of the variables", subtitle = "Neural net")
imp_rpart$results$feature <- feature_names$noice[match(imp_rpart$results$feature, feature_names$ugly)]
plot(imp_rpart) +
  theme_minimal() +
  ggtitle("Feature importance of the variables", subtitle = "Regression tree")
pdp_rf_1 <- FeatureEffect$new(predictor_rf, feature = "place_of_worship", method = "pdp")
pdp_rf_2 <- FeatureEffect$new(predictor_rf, feature = "office", method = "pdp")
pdp_rf_3 <- FeatureEffect$new(predictor_rf, feature = "platform", method = "pdp")
pdp_rf_4 <- FeatureEffect$new(predictor_rf, feature = "higher_education", method = "pdp")
pdp_rf_5 <- FeatureEffect$new(predictor_rf, feature = "urb_dens", method = "pdp")
pdp_rf_6 <- FeatureEffect$new(predictor_rf, feature = "vaccine_shots", method = "pdp")
ice_rf_1 <- FeatureEffect$new(predictor_rf, feature = "place_of_worship", method = "ice")
ice_rf_2 <- FeatureEffect$new(predictor_rf, feature = "platform", method = "ice")
ice_rf_3 <- FeatureEffect$new(predictor_rf, feature = "higher_education", method = "ice")
ice_rf_4 <- FeatureEffect$new(predictor_rf, feature = "urb_dens", method = "ice")
ice_rf_5 <- FeatureEffect$new(predictor_rf, feature = "platform", method = "ice")
ice_rf_6 <- FeatureEffect$new(predictor_rf, feature = "vaccine_shots", method = "ice")
plot_rf_1 <- plot(pdp_rf_1) +
  theme_minimal() +
  labs(x = "Place of worship") +
  plot(pdp_rf_2) +
  theme_minimal() +
  labs(x = "Office")
plot_rf_1 +
  plot_annotation(
    title = "Partial dependence plot of the features",
    subtitle = "Random forest"
  )
plot_rf_2 <- plot(pdp_rf_3) +
  theme_minimal() +
  labs(x = "Platform") +
  plot(pdp_rf_4) +
  theme_minimal() +
  labs(x = "Higher education")
plot_rf_2 +
  plot_annotation(
    title = "Partial dependence plot of the features",
    subtitle = "Random forest"
  )
plot_rf_3 <- plot(pdp_rf_5) +
  theme_minimal() +
  labs(x = "Urban density") +
  plot(pdp_rf_6) +
  theme_minimal() +
  labs(x = "Vaccinations")
plot_rf_3 +
  plot_annotation(
    title = "Partial dependence plot of the features",
    subtitle = "Random forest"
  )
plot_rf_1 <- plot(ice_rf_1) +
  theme_minimal() +
  labs(x = "Place of worship") +
  plot(ice_rf_2) +
  theme_minimal() +
  labs(x = "Office")
plot_rf_1 +
  plot_annotation(
    title = "ICE plot of the features",
    subtitle = "Random forest"
  )
plot_rf_2 <- plot(ice_rf_3) +
  theme_minimal() +
  labs(x = "Platform") +
  plot(ice_rf_4) +
  theme_minimal() +
  labs(x = "Higher education")
plot_rf_2 +
  plot_annotation(
    title = "ICE plot of the features",
    subtitle = "Random forest"
  )
plot_rf_3 <- plot(ice_rf_5) +
  theme_minimal() +
  labs(x = "Urban density") +
  plot(ice_rf_6) +
  theme_minimal() +
  labs(x = "Vaccinations")
plot_rf_3 +
  plot_annotation(
    title = "ICE plot of the features",
    subtitle = "Random forest"
  )
pdp_fnn_1 <- FeatureEffect$new(predictor_fnn, feature = "urb_dens", method = "pdp")
pdp_fnn_2 <- FeatureEffect$new(predictor_fnn, feature = "place_of_worship", method = "pdp")
ice_fnn_1 <- FeatureEffect$new(predictor_fnn, feature = "urb_dens", method = "ice")
ice_fnn_2 <- FeatureEffect$new(predictor_fnn, feature = "place_of_worship", method = "ice")
plot_fnn_1 <- plot(pdp_fnn_1) +
  theme_minimal() +
  labs(x = "Urban density") +
  plot(pdp_fnn_2) +
  theme_minimal() +
  labs(x = "Place of worship")
plot_fnn_1 +
  plot_annotation(
    title = "Partial dependence plot of the features",
    subtitle = "K nearest neighbours"
  )
plot_fnn_1 <- plot(ice_fnn_1) +
  theme_minimal() +
  labs(x = "Urban density") +
  plot(ice_fnn_2) +
  theme_minimal() +
  labs(x = "Place of worship")
plot_fnn_1 +
  plot_annotation(
    title = "ICE plot of the features",
    subtitle = "K nearest neighbours"
  )
pdp_xgboost_1 <- FeatureEffect$new(predictor_xgboost, feature = "place_of_worship", method = "pdp")
pdp_xgboost_2 <- FeatureEffect$new(predictor_xgboost, feature = "urb_dens", method = "pdp")
ice_xgboost_1 <- FeatureEffect$new(predictor_xgboost, feature = "place_of_worship", method = "ice")
ice_xgboost_2 <- FeatureEffect$new(predictor_xgboost, feature = "urb_dens", method = "ice")
plot_xgboost_1 <- plot(pdp_xgboost_1) +
  theme_minimal() +
  labs(x = "Place of worship") +
  plot(pdp_xgboost_2) +
  theme_minimal() +
  labs(x = "Urban density")
plot_xgboost_1 +
  plot_annotation(
    title = "Partial dependence plot of the features",
    subtitle = "XGBoost"
  )
plot_xgboost_1 <- plot(ice_xgboost_1) +
  theme_minimal() +
  labs(x = "Place of worship") +
  plot(ice_xgboost_2) +
  theme_minimal() +
  labs(x = "Urban density")
plot_xgboost_1 +
  plot_annotation(
    title = "ICE plot of the features",
    subtitle = "XGBoost"
  )
pdp_nnet_1 <- FeatureEffect$new(predictor_nnet, feature = "urb_dens", method = "pdp")
pdp_nnet_2 <- FeatureEffect$new(predictor_nnet, feature = "sex", method = "pdp")
ice_nnet_1 <- FeatureEffect$new(predictor_nnet, feature = "urb_dens", method = "ice")
ice_nnet_2 <- FeatureEffect$new(predictor_nnet, feature = "sex", method = "ice")
plot_nnet_1 <- plot(pdp_nnet_1) +
  theme_minimal() +
  labs(x = "Urban density") +
  plot(pdp_nnet_2) +
  theme_minimal() +
  labs(x = "Sex")
plot_nnet_1 +
  plot_annotation(
    title = "Partial dependence plot of the features",
    subtitle = "Neural net"
  )
plot_nnet_1 <- plot(ice_nnet_1) +
  theme_minimal() +
  labs(x = "Urban density") +
  plot(ice_nnet_2) +
  theme_minimal() +
  labs(x = "Sex")
plot_nnet_1 +
  plot_annotation(
    title = "ICE plot of the features",
    subtitle = "Neural net"
  )
pdp_rpart_1 <- FeatureEffect$new(predictor_rpart, feature = "office", method = "pdp")
pdp_rpart_2 <- FeatureEffect$new(predictor_rpart, feature = "vaccine_shots", method = "pdp")
ice_rpart_1 <- FeatureEffect$new(predictor_rpart, feature = "office", method = "ice")
ice_rpart_2 <- FeatureEffect$new(predictor_rpart, feature = "vaccine_shots", method = "ice")
plot_rpart_1 <- plot(pdp_rpart_1) +
  theme_minimal() +
  labs(x = "Office") +
  plot(pdp_rpart_2) +
  theme_minimal() +
  labs(x = "Vaccinations")
plot_rpart_1 +
  plot_annotation(
    title = "Partial dependence plot of the features",
    subtitle = "Regression tree"
  )
plot_rpart_1 <- plot(ice_rpart_1) +
  theme_minimal() +
  labs(x = "Office") +
  plot(ice_rpart_2) +
  theme_minimal() +
  labs(x = "Vaccinations")
plot_rpart_1 +
  plot_annotation(
    title = "ICE plot of the features",
    subtitle = "Regression tree"
  )
interact_rf_1 <- Interaction$new(predictor_rf)
interact_rf_2 <- Interaction$new(predictor_rf, feature = "urb_dens")
interact_rf_1$results$.feature <- feature_names$noice[match(interact_rf_1$results$.feature, feature_names$ugly)]
for (i in seq_len(14)) {
  interact_rf_2$results$.feature <- str_replace(
    interact_rf_2$results$.feature,
    feature_names$ugly[i],
    feature_names$noice[i]
  )
}
plot(interact_rf_1) +
  theme_minimal() +
  plot(interact_rf_2) +
  theme_minimal() +
  plot_annotation(
    title = "Feature interactions",
    subtitle = "Random forest"
  )
interact_fnn_1 <- Interaction$new(predictor_fnn)
interact_fnn_2 <- Interaction$new(predictor_fnn, feature = "urb_dens")
interact_fnn_1$results$.feature <- feature_names$noice[match(interact_fnn_1$results$.feature, feature_names$ugly)]
for (i in seq_len(14)) {
  interact_fnn_2$results$.feature <- str_replace(
    interact_fnn_2$results$.feature,
    feature_names$ugly[i],
    feature_names$noice[i]
  )
}
plot(interact_fnn_1) +
  theme_minimal() +
  plot(interact_fnn_2) +
  theme_minimal() +
  plot_annotation(
    title = "Feature interactions",
    subtitle = "K nearest neighbours"
  )
interact_xgboost_1 <- Interaction$new(predictor_xgboost)
interact_xgboost_2 <- Interaction$new(predictor_xgboost, feature = "urb_dens")
interact_xgboost_1$results$.feature <- feature_names$noice[match(interact_xgboost_1$results$.feature, feature_names$ugly)]
for (i in seq_len(14)) {
  interact_xgboost_2$results$.feature <- str_replace(
    interact_xgboost_2$results$.feature,
    feature_names$ugly[i],
    feature_names$noice[i]
  )
}
plot(interact_xgboost_1) +
  theme_minimal() +
  plot(interact_xgboost_2) +
  theme_minimal() +
  plot_annotation(
    title = "Feature interactions",
    subtitle = "XGBoost"
  )
interact_nnet_1 <- Interaction$new(predictor_nnet)
interact_nnet_2 <- Interaction$new(predictor_nnet, feature = "urb_dens")
interact_nnet_1$results$.feature <- feature_names$noice[match(interact_nnet_1$results$.feature, feature_names$ugly)]
for (i in seq_len(14)) {
  interact_nnet_2$results$.feature <- str_replace(
    interact_nnet_2$results$.feature,
    feature_names$ugly[i],
    feature_names$noice[i]
  )
}
plot(interact_nnet_1) +
  theme_minimal() +
  plot(interact_nnet_2) +
  theme_minimal() +
  plot_annotation(
    title = "Feature interactions",
    subtitle = "Neural net"
  )
interact_rpart_1 <- Interaction$new(predictor_rpart)
interact_rpart_2 <- Interaction$new(predictor_rpart, feature = "urb_dens")
interact_rpart_1$results$.feature <- feature_names$noice[match(interact_rpart_1$results$.feature, feature_names$ugly)]
for (i in seq_len(14)) {
  interact_rpart_2$results$.feature <- str_replace(
    interact_rpart_2$results$.feature,
    feature_names$ugly[i],
    feature_names$noice[i]
  )
}
plot(interact_rpart_1) +
  theme_minimal() +
  plot(interact_rpart_2) +
  theme_minimal() +
  plot_annotation(
    title = "Feature interactions",
    subtitle = "Regression tree"
  )
shapley_rf_nf <- Shapley$new(predictor_rf, x.interest = newest_numbers_test[66, 2:14])
fv <- str_split(shapley_rf_nf$results$feature.value, "=")
shapley_rf_nf$results$feature.value <- unlist(lapply(
  fv,
  function(x) {
    paste(feature_names$noice[which(feature_names$ugly %in% x[1])], round(as.numeric(x[2]), 3), sep = "=")
  }
))
shapley_1_rf <- shapley_rf_nf$plot() +
  theme_minimal() +
  ggtitle(
    paste(
      "Actual value: ", newest_numbers_test$value[66],
      "\nActual prediction: ", round(shapley_rf_nf$y.hat.interest, 2),
      "\nAverage prediction: ", round(shapley_rf_nf$y.hat.average, 2),
      sep = ""
    ),
    subtitle = paste("Municipality:", newest_numbers_test$kommune_name[66], "(Test)")
  )
shapley_rf_tr <- Shapley$new(predictor_rf, x.interest = newest_numbers_train[251, 2:14])
fv <- str_split(shapley_rf_tr$results$feature.value, "=")
shapley_rf_tr$results$feature.value <- unlist(lapply(
  fv,
  function(x) {
    paste(feature_names$noice[which(feature_names$ugly %in% x[1])], round(as.numeric(x[2]), 3), sep = "=")
  }
))
shapley_2_rf <- shapley_rf_tr$plot() +
  theme_minimal() +
  ggtitle(
    paste(
      "Actual value: ", newest_numbers_train$value[251],
      "\nActual prediction: ", round(shapley_rf_tr$y.hat.interest, 2),
      "\nAverage prediction: ", round(shapley_rf_tr$y.hat.average, 2),
      sep = ""
    ),
    subtitle = paste("Municipality:", newest_numbers_train$kommune_name[251], "(Train)")
  )
shapley_2_rf +
  shapley_1_rf +
  plot_annotation(
    title = "Explaining predictions using Shapley values",
    subtitle = "Random forest"
  )
shapley_fnn_nf <- Shapley$new(predictor_fnn, x.interest = newest_numbers_test[66, 2:14])
fv <- str_split(shapley_fnn_nf$results$feature.value, "=")
shapley_fnn_nf$results$feature.value <- unlist(lapply(
  fv,
  function(x) {
    paste(feature_names$noice[which(feature_names$ugly %in% x[1])], round(as.numeric(x[2]), 3), sep = "=")
  }
))
shapley_1_fnn <- shapley_fnn_nf$plot() +
  theme_minimal() +
  ggtitle(
    paste(
      "Actual value: ", newest_numbers_test$value[66],
      "\nActual prediction: ", round(shapley_fnn_nf$y.hat.interest, 2),
      "\nAverage prediction: ", round(shapley_fnn_nf$y.hat.average, 2),
      sep = ""
    ),
    subtitle = paste("Municipality:", newest_numbers_test$kommune_name[66], "(Test)")
  )
shapley_fnn_tr <- Shapley$new(predictor_fnn, x.interest = newest_numbers_train[251, 2:14])
fv <- str_split(shapley_fnn_tr$results$feature.value, "=")
shapley_fnn_tr$results$feature.value <- unlist(lapply(
  fv,
  function(x) {
    paste(feature_names$noice[which(feature_names$ugly %in% x[1])], round(as.numeric(x[2]), 3), sep = "=")
  }
))
shapley_2_fnn <- shapley_fnn_tr$plot() +
  theme_minimal() +
  ggtitle(
    paste(
      "Actual value: ", newest_numbers_train$value[251],
      "\nActual prediction: ", round(shapley_fnn_tr$y.hat.interest, 2),
      "\nAverage prediction: ", round(shapley_fnn_tr$y.hat.average, 2),
      sep = ""
    ),
    subtitle = paste("Municipality:", newest_numbers_train$kommune_name[251], "(Train)")
  )
shapley_2_fnn +
  shapley_1_fnn +
  plot_annotation(
    title = "Explaining predictions using Shapley values",
    subtitle = "K nearest neighbours"
  )

shapley_xgboost_nf <- Shapley$new(predictor_xgboost, x.interest = newest_numbers_test[66, 2:14])
fv <- str_split(shapley_xgboost_nf$results$feature.value, "=")
shapley_xgboost_nf$results$feature.value <- unlist(lapply(
  fv,
  function(x) {
    paste(feature_names$noice[which(feature_names$ugly %in% x[1])], round(as.numeric(x[2]), 3), sep = "=")
  }
))
shapley_1_xgboost <- shapley_xgboost_nf$plot() +
  theme_minimal() +
  ggtitle(
    paste(
      "Actual value: ", newest_numbers_test$value[66],
      "\nActual prediction: ", round(shapley_xgboost_nf$y.hat.interest, 2),
      "\nAverage prediction: ", round(shapley_xgboost_nf$y.hat.average, 2),
      sep = ""
    ),
    subtitle = paste("Municipality:", newest_numbers_test$kommune_name[66], "(Test)")
  )
shapley_xgboost_tr <- Shapley$new(predictor_xgboost, x.interest = newest_numbers_train[251, 2:14])
fv <- str_split(shapley_xgboost_tr$results$feature.value, "=")
shapley_xgboost_tr$results$feature.value <- unlist(lapply(
  fv,
  function(x) {
    paste(feature_names$noice[which(feature_names$ugly %in% x[1])], round(as.numeric(x[2]), 3), sep = "=")
  }
))
shapley_2_xgboost <- shapley_xgboost_tr$plot() +
  theme_minimal() +
  ggtitle(
    paste(
      "Actual value: ", newest_numbers_train$value[251],
      "\nActual prediction: ", round(shapley_xgboost_tr$y.hat.interest, 2),
      "\nAverage prediction: ", round(shapley_xgboost_tr$y.hat.average, 2),
      sep = ""
    ),
    subtitle = paste("Municipality:", newest_numbers_train$kommune_name[251], "(Train)")
  )
shapley_2_xgboost +
  shapley_1_xgboost +
  plot_annotation(
    title = "Explaining predictions using Shapley values",
    subtitle = "XGBoost"
  )

shapley_nnet_nf <- Shapley$new(predictor_nnet, x.interest = newest_numbers_test[66, 2:14])
fv <- str_split(shapley_nnet_nf$results$feature.value, "=")
shapley_nnet_nf$results$feature.value <- unlist(lapply(
  fv,
  function(x) {
    paste(feature_names$noice[which(feature_names$ugly %in% x[1])], round(as.numeric(x[2]), 3), sep = "=")
  }
))
shapley_1_nnet <- shapley_nnet_nf$plot() +
  theme_minimal() +
  ggtitle(
    paste(
      "Actual value: ", newest_numbers_test$value[66],
      "\nActual prediction: ", round(shapley_nnet_nf$y.hat.interest, 2),
      "\nAverage prediction: ", round(shapley_nnet_nf$y.hat.average, 2),
      sep = ""
    ),
    subtitle = paste("Municipality:", newest_numbers_test$kommune_name[66], "(Test)")
  )
shapley_nnet_tr <- Shapley$new(predictor_nnet, x.interest = newest_numbers_train[251, 2:14])
fv <- str_split(shapley_nnet_tr$results$feature.value, "=")
shapley_nnet_tr$results$feature.value <- unlist(lapply(
  fv,
  function(x) {
    paste(feature_names$noice[which(feature_names$ugly %in% x[1])], round(as.numeric(x[2]), 3), sep = "=")
  }
))
shapley_2_nnet <- shapley_nnet_tr$plot() +
  theme_minimal() +
  ggtitle(
    paste(
      "Actual value: ", newest_numbers_train$value[251],
      "\nActual prediction: ", round(shapley_nnet_tr$y.hat.interest, 2),
      "\nAverage prediction: ", round(shapley_nnet_tr$y.hat.average, 2),
      sep = ""
    ),
    subtitle = paste("Municipality:", newest_numbers_train$kommune_name[251], "(Train)")
  )
shapley_2_nnet +
  shapley_1_nnet +
  plot_annotation(
    title = "Explaining predictions using Shapley values",
    subtitle = "Neural net"
  )
shapley_rpart_nf <- Shapley$new(predictor_rpart, x.interest = newest_numbers_test[66, 2:14])
fv <- str_split(shapley_rpart_nf$results$feature.value, "=")
shapley_rpart_nf$results$feature.value <- unlist(lapply(
  fv,
  function(x) {
    paste(feature_names$noice[which(feature_names$ugly %in% x[1])], round(as.numeric(x[2]), 3), sep = "=")
  }
))
shapley_1_rpart <- shapley_rpart_nf$plot() +
  theme_minimal() +
  ggtitle(
    paste(
      "Actual value: ", newest_numbers_test$value[66],
      "\nActual prediction: ", round(shapley_rpart_nf$y.hat.interest, 2),
      "\nAverage prediction: ", round(shapley_rpart_nf$y.hat.average, 2),
      sep = ""
    ),
    subtitle = paste("Municipality:", newest_numbers_test$kommune_name[66], "(Test)")
  )
shapley_rpart_tr <- Shapley$new(predictor_rpart, x.interest = newest_numbers_train[251, 2:14])
fv <- str_split(shapley_rpart_tr$results$feature.value, "=")
shapley_rpart_tr$results$feature.value <- unlist(lapply(
  fv,
  function(x) {
    paste(feature_names$noice[which(feature_names$ugly %in% x[1])], round(as.numeric(x[2]), 3), sep = "=")
  }
))
shapley_2_rpart <- shapley_rpart_tr$plot() +
  theme_minimal() +
  ggtitle(
    paste(
      "Actual value: ", newest_numbers_train$value[251],
      "\nActual prediction: ", round(shapley_rpart_tr$y.hat.interest, 2),
      "\nAverage prediction: ", round(shapley_rpart_tr$y.hat.average, 2),
      sep = ""
    ),
    subtitle = paste("Municipality:", newest_numbers_train$kommune_name[251], "(Train)")
  )
shapley_2_rpart +
  shapley_1_rpart +
  plot_annotation(
    title = "Explaining predictions using Shapley values",
    subtitle = "Regression tree"
  )
