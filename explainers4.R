# This code generates the explanations
source("setup.R")

# For 4D with two noise variables
load("data/sp4_data_tr.rda")
load("data/sp4_data_ts.rda")
load("data/sp4_data_true.rda")
load("data/sp4_rf.rda")
sp4_rf$importance
sp4_global <- sp4_rf$importance/max(sp4_rf$importance)
sp4_global <- tibble(x1=sp4_global[1], 
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
# use a point from the training set
idx <- NULL
for (i in 1:nrow(pt)) {
  idx <- c(idx, find_closest(pt[i,], sp4_data_tr[,1:4]))
}
pt <- sp4_data_tr[idx,]

# kumquat (lime)
rfmodel_bundled <- bundle::bundle(sp4_rf)
sp4_kumquat <- kumquat(
  rfmodel_bundled,
  sp4_data_tr,
  sp4_data_tr[idx,],
  predictor_vars = names(sp4_data_tr)[1:4],
  class_names = unique(sp4_data_tr$class)
)
sp4_kumquat_interp <- bind_rows(
  sp4_kumquat[[1]]$local_model$importances/max(abs(sp4_kumquat[[1]]$local_model$importances)),
  sp4_kumquat[[2]]$local_model$importances/max(abs(sp4_kumquat[[2]]$local_model$importances))
)
for (i in 1:nrow(sp4_kumquat_interp)) {
  if (sign(sp4_kumquat_interp[i, which.max(abs(sp4_kumquat_interp[i,]))]) < 1)  
    sp4_kumquat_interp[i,] <- sp4_kumquat_interp[i,] * (-1)
}
# Code above not working, compute manually
sp4_kumquat_interp <- NULL
for (i in 1:nrow(pt)) {
  perm_d <- tibble(x1 = runif(1000, pt$x1[i]-0.2, pt$x1[i]+0.2),
                   x2 = runif(1000, pt$x2[i]-0.2, pt$x2[i]+0.2),
                   x3 = runif(1000, pt$x3[i]-0.2, pt$x3[i]+0.2),
                   x4 = runif(1000, pt$x4[i]-0.2, pt$x4[i]+0.2)) 
  perm_d$fitted <- as.numeric(predict(sp4_rf, perm_d))
  coefs <- lm(fitted~., data=perm_d)$coefficients[-1]
  coefs <- coefs/max(abs(coefs))
  if (sign(coefs[which.max(abs(coefs))]) < 1) 
    coefs <- coefs*(-1)
  sp4_kumquat_interp <- rbind(sp4_kumquat_interp, coefs)
}
sp4_kumquat_interp <- as_tibble(sp4_kumquat_interp)

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
sp4_shap_interp <- tibble(x1=rep(0, nrow(pt)), 
                          x2=rep(0, nrow(pt)),
                          x3=rep(0, nrow(pt)),
                          x4=rep(0, nrow(pt)))
for (i in 1:length(idx)) {
  x <- sp4_shap$S[idx[i],]/max(abs(sp4_shap$S[idx[i],]))
  sp4_shap_interp[i,1] <- x[1]
  sp4_shap_interp[i,2] <- x[2]
  sp4_shap_interp[i,3] <- x[3]
  sp4_shap_interp[i,4] <- x[4]
}
for (i in 1:nrow(sp4_shap_interp)) {
  if (sign(sp4_shap_interp[i, which.max(abs(sp4_shap_interp[i,]))]) < 1)  
    sp4_shap_interp[i,] <- sp4_shap_interp[i,] * (-1)
}

# counterfactuals
predictor_rf <- iml::Predictor$new(
  sp4_rf,
  data = sp4_data_tr,
  type = "prob"
)
cf_gen <- counterfactuals::NICEClassif$new(
  predictor_rf
)
sp_cf <- tibble(x1 = rep(0, nrow(pt)), 
                x2 = rep(0, nrow(pt)),
                x3 = rep(0, nrow(pt)),
                x4 = rep(0, nrow(pt)),
                class = c("Above", "Above"))
sp_cf_idx <- rep(0, nrow(pt))
for (i in 1:nrow(pt)) {
  cf <- cf_gen$find_counterfactuals(
    x_interest = pt[i,],
    desired_class = ifelse(pt$class[i] == "Above", "Below", "Above"),
    desired_prob = c(0.99, 1)
  )
  
  # Match counterfactual rows back to training data,
  # because row index is not provided!
  sp_cf_idx[i] <- find_closest(cf$data, sp4_data_tr[,1:4])
  sp_cf[i,] <- sp4_data_tr[sp_cf_idx[i],]
}
sp4_cf_interp <- tibble(x1=rep(0, nrow(pt)), 
                        x2=rep(0, nrow(pt)),
                        x3=rep(0, nrow(pt)),
                        x4=rep(0, nrow(pt)))
for (i in 1:nrow(pt)) {
  sp4_cf_interp$x1[i] <- pt$x1[i]-sp_cf$x1[i] 
  sp4_cf_interp$x2[i] <- pt$x2[i]-sp_cf$x2[i]
  sp4_cf_interp$x3[i] <- pt$x3[i]-sp_cf$x3[i] 
  sp4_cf_interp$x4[i] <- pt$x4[i]-sp_cf$x4[i]
  sp4_cf_interp[i,] <- max(abs(sp4_cf_interp[i,]))/sp4_cf_interp[i,]
  # reverse
  sp4_cf_interp[i,] <- sp4_cf_interp[i,]/max(abs(sp4_cf_interp[i,]))
  if (sign(sp4_cf_interp[i, which.max(abs(sp4_cf_interp[i,]))]) < 1)  
    sp4_cf_interp[i,] <- sp4_cf_interp[i,] * (-1)
}

sp_pt_cf <- bind_rows(pt, sp_cf) 
sp_pt_cf$type <- rep(c("pt", "cf"), c(2,2))
sp_pt_cf$id <- rep(c(1,2), 2)
sp_pt_cf_wide <- sp_pt_cf |>
  pivot_wider(names_from = type, values_from = c(x1,x2,x3,x4), id_cols=id)

p + 
  geom_point(data=as.data.frame(pt), aes(x=x2, y=x4), 
             inherit.aes = FALSE) +
  geom_point(data=sp4_cf$data, aes(x=x2, y=x4), 
             inherit.aes = FALSE, shape = 3)

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
  seed = 135,
  perturb_distance = 0.2,
  perturb_step = 0.005,
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

# Only use LIME and SHAP
sp4_explainers <- bind_rows(
  sp4_global,
  sp4_kumquat_interp,
  sp4_shap_interp
)
sp4_explainers$method <- c("global", 
                          rep("lime", nrow(pt)),
                          rep("shap", nrow(pt)))
sp4_explainers$method <- factor(sp4_explainers$method,
                               levels = c("global", "lime", "shap"))
sp4_explainers$id <- c(0, rep(c(1:nrow(pt)), 2))
sp4_explainers <- sp4_explainers |>
  select(id, method, x1, x2, x3, x4)
sp4_explainers <- sp4_explainers |> arrange(id, method)

sp4_explainers 
save(sp4_explainers, file="data/explainers4.rda")
pt$idx <- idx
save(pt, file="data/sp4_pt.rda")

# Build local explanations space
sp4_kumquat_interp <- NULL
for (i in 1:nrow(pt)) {
  perm_d <- tibble(x1 = runif(1000, pt$x1[i]-0.2, pt$x1[i]+0.2),
                   x2 = runif(1000, pt$x2[i]-0.2, pt$x2[i]+0.2),
                   x3 = runif(1000, pt$x3[i]-0.2, pt$x3[i]+0.2),
                   x4 = runif(1000, pt$x4[i]-0.2, pt$x4[i]+0.2)) 
  perm_d$fitted <- as.numeric(predict(sp4_rf, perm_d))
  coefs <- lm(fitted~., data=perm_d)$coefficients[-1]
  coefs <- coefs/max(abs(coefs))
  if (sign(coefs[which.max(abs(coefs))]) < 1) 
    coefs <- coefs*(-1)
  sp4_kumquat_interp <- rbind(sp4_kumquat_interp, coefs)
}
sp4_kumquat_interp <- as_tibble(sp4_kumquat_interp)

set.seed(711)
sp4_d1 <- tibble(x1 = c(pt$x1[1], runif(1000, pt$x1[1]-0.2, pt$x1[1]+0.2)),
                 x2 = c(pt$x2[1], runif(1000, pt$x2[1]-0.2, pt$x2[1]+0.2)),
                 x3 = c(pt$x3[1], runif(1000, pt$x3[1]-0.2, pt$x3[1]+0.2)),
                 x4 = c(pt$x4[1], runif(1000, pt$x4[1]-0.2, pt$x4[1]+0.2))) 
sp4_d1$fitted <- c("A1", as.character(predict(sp4_rf, sp4_d1[-1,])))
sp4_d1$labels <- c("1", rep("o", 1000))
sp4_d2 <- tibble(x1 = c(pt$x1[2], runif(1000, pt$x1[2]-0.2, pt$x1[2]+0.2)),
                 x2 = c(pt$x2[2], runif(1000, pt$x2[2]-0.2, pt$x2[2]+0.2)),
                 x3 = c(pt$x3[2], runif(1000, pt$x3[2]-0.2, pt$x3[2]+0.2)),
                 x4 = c(pt$x4[2], runif(1000, pt$x4[2]-0.2, pt$x4[2]+0.2))) 
sp4_d2$fitted <- c("A2", as.character(predict(sp4_rf, sp4_d2[-1,])))
sp4_d2$labels <- c("2", rep("o", 1000))
save(sp4_d1, file="data/sp4_d1.rda")
save(sp4_d2, file="data/sp4_d2.rda")

