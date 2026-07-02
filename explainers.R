# This code generates the explanations
source("setup.R")

load("data/sp_data_tr.rda")
load("data/sp_data_ts.rda")
load("data/sp_data_true.rda")
load("data/sp_rf.rda")
sp_rf$importance
sp_global <- sp_rf$importance/max(sp_rf$importance)
sp_global <- data.frame(x=sp_global[1], y=sp_global[2])

# choose a point to explain, need to have coordinate axes
p <- ggplot(sp_data_true, aes(x = x, y = y, colour = fitted)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1)
p

pt <- tibble(x=c(0, -0.4), y=c(0, -0.36))
p + 
  geom_point(data=as.data.frame(pt), aes(x=x, y=y), inherit.aes = FALSE)

# use a point from the training set
idx <- NULL
for (i in 1:nrow(pt)) {
  idx <- c(idx, find_closest(pt[i,], sp_data_tr[,1:2]))
}
pt <- sp_data_tr[idx,]
p + 
  geom_point(data=as.data.frame(pt), aes(x=x, y=y), inherit.aes = FALSE)


# kumquat (lime)
rfmodel_bundled <- bundle::bundle(sp_rf)
sp_kumquat <- kumquat(
  rfmodel_bundled,
  sp_data_tr,
  sp_data_tr[idx,],
  class_names = unique(sp_data_tr$class)
)
sp_kumquat_interp <- bind_rows(
  sp_kumquat[[1]]$local_model$importances/max(abs(sp_kumquat[[1]]$local_model$importances)),
  sp_kumquat[[2]]$local_model$importances/max(abs(sp_kumquat[[2]]$local_model$importances))
)
for (i in 1:nrow(sp_kumquat_interp)) {
  if (sign(sp_kumquat_interp[i, which.max(abs(sp_kumquat_interp[i,]))]) < 1)  
    sp_kumquat_interp[i,] <- sp_kumquat_interp[i,] * (-1)
}

# shap
sp_shap <- kernelshap(
  sp_rf,
  sp_data_tr,
  pred_fun = \(x, ...) {
    stats::predict(x, type = "prob", ...)[, 1]
  },
  feature_names = c("x", "y"),
  verbose = FALSE,
  seed = 818
)
sp_shap_interp <- tibble(x=rep(0, nrow(pt)), 
                           y=rep(0, nrow(pt)))
for (i in 1:length(idx)) {
  x <- sp_shap$S[idx[i],]/max(abs(sp_shap$S[idx[i],]))
  sp_shap_interp[i,1] <- x[1]
  sp_shap_interp[i,2] <- x[2]
}
for (i in 1:nrow(sp_shap_interp)) {
  if (sign(sp_shap_interp[i, which.max(abs(sp_shap_interp[i,]))]) < 1)  
    sp_shap_interp[i,] <- sp_shap_interp[i,] * (-1)
}

# counterfactuals
predictor_rf <- iml::Predictor$new(
  sp_rf,
  data = sp_data_tr,
  type = "prob"
)
cf_gen <- counterfactuals::NICEClassif$new(
  predictor_rf
)
sp_cf <- tibble(x = rep(0, nrow(pt)), 
                y = rep(0, nrow(pt)),
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
  sp_cf_idx[i] <- find_closest(cf$data, sp_data_tr[,1:2])
  sp_cf[i,] <- sp_data_tr[sp_cf_idx[i],]
}
sp_cf_interp <- tibble(x=rep(0, nrow(pt)), 
                       y=rep(0, nrow(pt)))
for (i in 1:nrow(pt)) {
  sp_cf_interp$x[i] <- pt$x[i]-sp_cf$x[i] 
  sp_cf_interp$y[i] <- pt$y[i]-sp_cf$y[i]
  sp_cf_interp[i,] <- max(abs(sp_cf_interp[i,]))/sp_cf_interp[i,]
  # reverse
  sp_cf_interp[i,] <- sp_cf_interp[i,]/max(abs(sp_cf_interp[i,]))
  if (sign(sp_cf_interp[i, which.max(abs(sp_cf_interp[i,]))]) < 1)  
    sp_cf_interp[i,] <- sp_cf_interp[i,] * (-1)
}

sp_pt_cf <- bind_rows(pt, sp_cf) 
sp_pt_cf$type <- rep(c("pt", "cf"), c(2,2))
sp_pt_cf$id <- rep(c(1,2), 2)
sp_pt_cf_wide <- sp_pt_cf |>
  pivot_wider(names_from = type, values_from = c(x,y), id_cols=id)
p + 
  geom_point(data=sp_pt_cf, aes(x=x, y=y, shape=type), 
             inherit.aes = FALSE) +
  geom_segment(data=sp_pt_cf_wide, 
               aes(x=x_pt, y=y_pt, xend=x_cf, yend=y_cf), 
               inherit.aes = FALSE) +
  scale_shape_manual(values = c(4, 16))
# How to interpret counterfactuals, it is 
# again that a smaller value is more important
# because you only need to go a small way to 
# find an observation of the opposite class.

# anchors using kultarr
sp_func <- carrier::crate(function(data) {
  return(randomForest:::predict.randomForest(!!sp_rf, data))
})
sp_anchors <- make_anchors(
  dataset = sp_data_tr,
  cols = c("x", "y"),
  instance = pt,
  model_func = sp_func,
  class_col = "class",
  verbose = TRUE
)
sp_anchor_bounds <- sp_anchors |>
  pluck("final_anchor") 
sp_perturb_bounds <- sp_anchors |>
  pluck("perturb_bounds") 

# Smallest side is the most important, 
# so divide by the anchor and then scale to 
# have largest as 1
sp_anchor_interp <- tibble(x=rep(0, nrow(pt)), 
                           y=rep(0, nrow(pt)))
for (i in 1:nrow(pt)) {
  anc <- filter(sp_anchor_bounds, id == i)
  pert <- filter(sp_perturb_bounds, id == i)
  sp_anchor_interp$x[i] <- (pert$x[2] - pert$x[1])/
    (anc$x[2] - anc$x[1])
  sp_anchor_interp$y[i] <- (pert$y[2] - pert$y[1])/
    (anc$y[2] - anc$y[1]) 
  sp_anchor_interp[i,] <- sp_anchor_interp[i,]/max(sp_anchor_interp[i,])
}
#if (sign(sp_anchors_interp_smry[which.max(abs(sp_anchors_interp_smry))]) < 1)  
#  sp_anchors_interp_smry <- sp_anchors_interp_smry * (-1)
# Need to handle direction at some point

sp_explainers <- bind_rows(
  sp_global,
  sp_kumquat_interp,
  sp_shap_interp,
  sp_cf_interp,
  sp_anchor_interp
)
sp_explainers$method <- c("global", 
                          rep("lime", nrow(pt)),
                          rep("shap", nrow(pt)), 
                          rep("counterfactuals", nrow(pt)), 
                          rep("anchors", nrow(pt)))
sp_explainers$method <- factor(sp_explainers$method,
    levels = c("global", "lime", "shap", "counterfactuals", "anchors"))
sp_explainers$id <- c(0, rep(c(1:nrow(pt)), 4))
sp_explainers <- sp_explainers |>
  select(id, method, x, y)
sp_explainers <- sp_explainers |> arrange(id, method)

save(sp_explainers, file="data/explainers1.rda")
save(sp_pt_cf, file="data/sp_pt_cf.rda")
save(sp_pt_cf_wide, file="data/sp_pt_cf_wide.rda")


