# This code generates the explanations
source("setup.R")

# Utility function
find_closest <- function(pt, data) {
  pt <- as.matrix(pt)
  data <- as.matrix(data)
  dst <- rep(0, nrow(data))
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      dst[i] <- dst[i] + (data[i,j]-pt[j])^2
    }
    dst[i] <- sqrt(dst[i])
  }
  return(which.min(dst))
}  

# For 5D with two noise variables
load("data/sp5_data_tr.rda")
load("data/sp5_data_ts.rda")
load("data/sp5_data_true.rda")
load("data/sp5_rf.rda")
sp5_rf$importance
sp5_global <- sp5_rf$importance/max(sp5_rf$importance)

# choose a point to explain, need to have coordinate axes
p <- ggplot(sp5_data_true, aes(x = x2, y = x4, colour = fitted)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1)
p

pt <- tibble(x1=0, x2=-0.5, x3=0, x4=-0.33, x5=0)
pt <- tibble(x1=0, x2=0, x3=0, x4=-0.48, x5=0)
pt <- tibble(x1=0, x2=0.6, x3=0, x4=0, x5=0)
pt <- tibble(x1=0, x2=0.8, x3=0, x4=0.75, x5=0)
p + 
  geom_point(data=as.data.frame(pt), aes(x=x2, y=x4), inherit.aes = FALSE)

# kumquat still requires a point from the training set
idx <- find_closest(pt, sp5_data_tr[,1:5])
pt <- sp5_data_tr[idx,]

# anchors using kultarr
sp5_func <- carrier::crate(function(data) {
  return(randomForest:::predict.randomForest(!!sp5_rf, data))
})
sp5_anchors <- make_anchors(
  dataset = sp5_data_tr,
  cols = c("x1", "x2", "x3", "x4", "x5"),
  instance = pt,
  model_func = sp5_func,
  class_col = "class",
  n_bins = 3,
  seed = 913,
  perturb_distance = 0.01,
  perturb_step = 0.002,
  verbose = FALSE
)
sp5_anchor_bounds <- sp5_anchors |>
  pluck("final_anchor") 
sp5_perturb_bounds <- sp5_anchors |>
  pluck("perturb_bounds") 

sp5_anchors_interp <- tibble(
  x1 = 1-(sp3_anchor_bounds$x1[2] - sp3_anchor_bounds$x1[1])/(sp3_perturb_bounds$x1[2] - sp3_perturb_bounds$x1[1]),
  x2 = 1-(sp3_anchor_bounds$x2[2] - sp3_anchor_bounds$x2[1])/(sp3_perturb_bounds$x2[2] - sp3_perturb_bounds$x2[1]),
  x3 = 1-(sp3_anchor_bounds$x3[2] - sp3_anchor_bounds$x3[1])/(sp3_perturb_bounds$x3[2] - sp3_perturb_bounds$x3[1]),
  x4 = 1-(sp3_anchor_bounds$x4[2] - sp3_anchor_bounds$x4[1])/(sp3_perturb_bounds$x4[2] - sp3_perturb_bounds$x4[1]),
  x5 = 1-(sp3_anchor_bounds$x5[2] - sp3_anchor_bounds$x5[1])/(sp3_perturb_bounds$x5[2] - sp3_perturb_bounds$x5[1])
)
sp5_anchors_interp_smry <- sp5_anchors_interp/max(sp5_anchors_interp)

# kumquat (lime)
rfmodel_bundled <- bundle::bundle(sp3_rf)
sp3_kumquat <- kumquat(
  rfmodel_bundled,
  sp3_data_tr,
  idx,
  predictor_vars = names(sp3_data_tr)[1:3],
  class_names = unique(sp3_data_tr$class)
)
sp3_kumquat_smry <- sp3_kumquat[[1]]$local_model$importances/max(abs(sp3_kumquat[[1]]$local_model$importances))

# shap
sp3_shap <- kernelshap(
  sp3_rf,
  sp3_data_tr,
  pred_fun = \(x, ...) {
    stats::predict(x, type = "prob", ...)[, 1]
  },
  feature_names = names(sp3_data_tr)[1:3],
  verbose = FALSE,
  seed = 818
)
sp3_shap_smry <- sp3_shap$S[idx,]/max(abs(sp3_shap$S[idx,]))

# counterfactuals
predictor_rf <- iml::Predictor$new(
  sp3_rf,
  data = sp3_data_tr,
  type = "prob"
)
cf_gen <- counterfactuals::NICEClassif$new(
  predictor_rf
)
sp3_cf <- cf_gen$find_counterfactuals(
  x_interest = pt,
  desired_class = ifelse(pt$class == "Above", "Below", "Above"),
  desired_prob = c(0.99, 1)
)
idx_cf <- find_closest(sp3_cf$data, sp3_data_tr[,1:3])
sp3_cf_interp <- tibble(x1 = pt$x1-sp3_cf$data$x1, 
                        x2 = pt$x2-sp3_cf$data$x2,
                        x3 = pt$x3-sp3_cf$data$x3,)
sp3_cf_interp_smry <- sp3_cf_interp/max(abs(sp3_cf_interp))

p + 
  geom_point(data=as.data.frame(pt), aes(x=x2, y=x3), 
             inherit.aes = FALSE) +
  geom_point(data=sp3_cf$data, aes(x=x2, y=x3), 
             inherit.aes = FALSE, shape = 3)

sp3_explainers <- tibble(
  method = c("global", "shap", "anchors", "lime", "counterfactuals"),
  x1 = c(sp3_global[1], 
        sp3_shap_smry[1],
        sp3_anchors_interp_smry$x1,
        sp3_kumquat_smry[1],
        sp3_cf_interp_smry$x1),
  x2 = c(sp3_global[2], 
         sp3_shap_smry[2],
         sp3_anchors_interp_smry$x2,
         sp3_kumquat_smry[2],
         sp3_cf_interp_smry$x2),
  x3 = c(sp3_global[3], 
         sp3_shap_smry[3],
         sp3_anchors_interp_smry$x3,
         sp3_kumquat_smry[3],
         sp3_cf_interp_smry$x3)
)

sp3_explainers 


