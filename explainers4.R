# This code generates the explanations
source("setup.R")

# For 4D with two noise variables
load("data/sp4_data_tr.rda")
load("data/sp4_data_ts.rda")
load("data/sp4_data_true.rda")
load("data/sp4_rf.rda")
sp4_rf$importance
sp4_global <- sp4_rf$importance/max(sp4_rf$importance)
sp4_global <- data.frame(x1=sp4_global[1], 
                         x2=sp4_global[2],
                         x3=sp4_global[3],
                         x4=sp4_global[4])

# choose a point to explain, need to have coordinate axes
p <- ggplot(sp4_data_true, aes(x = x2, y = x4, colour = fitted)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1)
p

pt <- tibble(x1=c(0, 0),
             x2=c(0, -0.4), 
             x3=c(0, 0),
             x4=c(0, -0.36))
p + 
  geom_point(data=as.data.frame(pt), aes(x=x2, y=x4), inherit.aes = FALSE)

# kumquat still requires a point from the training set
#idx <- find_closest(pt, sp4_data_tr[,1:4])
idx <- find_closest(pt[,c(2,4)], sp4_data_tr[,c(2,4)])
pt <- sp4_data_tr[idx,]

# anchors using kultarr
sp4_func <- carrier::crate(function(data) {
  return(randomForest:::predict.randomForest(!!sp4_rf, data))
})
sp4_anchors <- make_anchors(
  dataset = sp4_data_tr,
  cols = c("x1", "x2", "x3", "x4"),
  instance = pt,
  model_func = sp4_func,
  class_col = "class",
  n_bins = 4,
  seed = 913,
  perturb_distance = 0.1,
  perturb_step = 0.01,
  verbose = FALSE
)
sp4_anchor_bounds <- sp4_anchors |>
  pluck("final_anchor") 
sp4_perturb_bounds <- sp4_anchors |>
  pluck("perturb_bounds") 

sp4_anchors_interp <- tibble(
  x1 = 1-(sp3_anchor_bounds$x1[2] - sp3_anchor_bounds$x1[1])/(sp3_perturb_bounds$x1[2] - sp3_perturb_bounds$x1[1]),
  x2 = 1-(sp3_anchor_bounds$x2[2] - sp3_anchor_bounds$x2[1])/(sp3_perturb_bounds$x2[2] - sp3_perturb_bounds$x2[1]),
  x3 = 1-(sp3_anchor_bounds$x3[2] - sp3_anchor_bounds$x3[1])/(sp3_perturb_bounds$x3[2] - sp3_perturb_bounds$x3[1]),
  x4 = 1-(sp3_anchor_bounds$x4[2] - sp3_anchor_bounds$x4[1])/(sp3_perturb_bounds$x4[2] - sp3_perturb_bounds$x4[1])
)
sp4_anchors_interp_smry <- sp4_anchors_interp/max(sp4_anchors_interp)
if (sign(sp4_anchors_interp_smry[which.max(abs(sp4_anchors_interp_smry))]) < 1)  
  sp4_anchors_interp_smry <- sp4_anchors_interp_smry * (-1)

# kumquat (lime)
rfmodel_bundled <- bundle::bundle(sp4_rf)
sp4_kumquat <- kumquat(
  rfmodel_bundled,
  sp4_data_tr,
  idx,
  predictor_vars = names(sp4_data_tr)[1:4],
  class_names = unique(sp4_data_tr$class)
)
sp4_kumquat_smry <- sp4_kumquat[[1]]$local_model$importances/max(abs(sp4_kumquat[[1]]$local_model$importances))
if (sign(sp4_kumquat_smry[which.max(abs(sp4_kumquat_smry))]) < 1)  
  sp4_kumquat_smry <- sp4_kumquat_smry * (-1)

# shap
sp4_shap <- kernelshap(
  sp4_rf,
  sp4_data_tr,
  pred_fun = \(x, ...) {
    stats::predict(x, type = "prob", ...)[, 1]
  },
  feature_names = names(sp4_data_tr)[1:4],
  verbose = FALSE,
  seed = 818
)
sp4_shap_smry <- sp4_shap$S[idx,]/max(abs(sp4_shap$S[idx,]))
if (sign(sp4_shap_smry[which.max(abs(sp4_shap_smry))]) < 1)  
  sp4_shap_smry <- sp4_shap_smry * (-1)

# counterfactuals
predictor_rf <- iml::Predictor$new(
  sp4_rf,
  data = sp4_data_tr,
  type = "prob"
)
cf_gen <- counterfactuals::NICEClassif$new(
  predictor_rf
)
sp4_cf <- cf_gen$find_counterfactuals(
  x_interest = pt,
  desired_class = ifelse(pt$class == "Above", "Below", "Above"),
  desired_prob = c(0.99, 1)
)
idx_cf <- find_closest(sp4_cf$data, sp4_data_tr[,1:4])
sp4_cf_interp <- tibble(x1 = pt$x1-sp4_cf$data$x1, 
                        x2 = pt$x2-sp4_cf$data$x2,
                        x3 = pt$x3-sp4_cf$data$x3,
                        x4 = pt$x4-sp4_cf$data$x4)
sp4_cf_interp_smry <- sp4_cf_interp/max(abs(sp4_cf_interp))
if (sign(sp4_cf_interp_smry[which.max(abs(sp4_cf_interp_smry))]) < 1)  
  sp4_cf_interp_smry <- sp4_cf_interp_smry * (-1)

p + 
  geom_point(data=as.data.frame(pt), aes(x=x2, y=x4), 
             inherit.aes = FALSE) +
  geom_point(data=sp4_cf$data, aes(x=x2, y=x4), 
             inherit.aes = FALSE, shape = 3)

sp4_explainers <- tibble(
  method = c("global", "shap", "anchors", "lime", "counterfactuals"),
  x1 = c(sp4_global[1], 
        sp4_shap_smry[1],
        sp4_anchors_interp_smry$x1,
        sp4_kumquat_smry[1],
        sp4_cf_interp_smry$x1),
  x2 = c(sp4_global[2], 
         sp4_shap_smry[2],
         sp4_anchors_interp_smry$x2,
         sp4_kumquat_smry[2],
         sp4_cf_interp_smry$x2),
  x3 = c(sp4_global[3], 
         sp4_shap_smry[3],
         sp4_anchors_interp_smry$x3,
         sp4_kumquat_smry[3],
         sp4_cf_interp_smry$x3),
  x4 = c(sp4_global[4], 
         sp4_shap_smry[4],
         sp4_anchors_interp_smry$x4,
         sp4_kumquat_smry[4],
         sp4_cf_interp_smry$x4)
)

sp3_explainers 


