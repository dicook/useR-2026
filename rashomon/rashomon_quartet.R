# Read data
train <- read.table("rashomon/rq_train.csv", sep=";", header=TRUE)
test  <- read.table("rashomon/rq_test.csv",  sep=";", header=TRUE)

# Train models
set.seed(1568)
library("DALEX")

library("partykit")
model_dt <- ctree(y~., data=train,
                  control=ctree_control(maxdepth=3, minsplit=250))
exp_dt <- DALEX::explain(model_dt, data=test[,-1], y=test[,1],
                         verbose=F, label="decision tree")
mp_dt <- model_performance(exp_dt)

model_lm <- lm(y~., data=train)
exp_lm <- DALEX::explain(model_lm, data=test[,-1], y=test[,1],
                         verbose=F, label="linear regression")
mp_lm <- model_performance(exp_lm)

library("randomForest")
model_rf <- randomForest(y~., data=train, ntree=100)
exp_rf <- DALEX::explain(model_rf, data=test[,-1], y=test[,1],
                         verbose=F, label="random forest")
mp_rf <- model_performance(exp_rf)

library("neuralnet")
model_nn <- neuralnet(y~., data=train, hidden=c(8, 4), threshold=0.05)
exp_nn <- DALEX::explain(model_nn, data=test[,-1], y=test[,1],
                         verbose=F, label="neural network")
mp_nn <- model_performance(exp_nn)

# Calculate performance
mp_all <- list(lm=mp_lm, dt=mp_dt, nn=mp_nn, rf=mp_rf)
R2   <- sapply(mp_all, function(x) x$measures$r2)
round(R2, 4)
#     lm     dt     nn     rf
# 0.7290 0.7287 0.7290 0.7287

rmse <- sapply(mp_all, function(x) x$measures$rmse)
round(rmse, 4)
#     lm     dt     nn     rf
# 0.3535 0.3537 0.3535 0.3537

# Visualize models
plot(model_dt)
summary(model_lm)
model_rf
plot(model_nn)

# Visualize variable importance
imp_dt <- model_parts(exp_dt, N=NULL, B=1)
imp_lm <- model_parts(exp_lm, N=NULL, B=1)
imp_rf <- model_parts(exp_rf, N=NULL, B=1)
imp_nn <- model_parts(exp_nn, N=NULL, B=1)

plot(imp_dt, imp_nn, imp_rf, imp_lm)

# Visualize partial dependence profiles
pd_dt <- model_profile(exp_dt, N=NULL)
pd_rf <- model_profile(exp_rf, N=NULL)
pd_lm <- model_profile(exp_lm, N=NULL)
pd_nn <- model_profile(exp_nn, N=NULL)
save(pd_dt, file="rashomon/pd_dt.rda")
save(pd_rf, file="rashomon/pd_rf.rda")
save(pd_lm, file="rashomon/pd_lm.rda")
save(pd_nn, file="rashomon/pd_nn.rda")

plot(pd_dt, pd_nn, pd_rf, pd_lm)

# Visualize partial dependence profiles
yhat_dt <- model_prediction(exp_dt, train)
yhat_rf <- model_prediction(exp_rf, train)
yhat_lm <- model_prediction(exp_lm, train)
yhat_nn <- model_prediction(exp_nn, train)

# Plot data distribution
library("GGally")
both <- rbind(data.frame(train, label="train"),
              data.frame(test, label="test"))
ggpairs(both, aes(color=label),
        lower = list(continuous = wrap("points", alpha=0.2, size=1),
                     combo = wrap("facethist", bins=25)),
        diag = list(continuous = wrap("densityDiag", alpha=0.5, bw="SJ"),
                    discrete = "barDiag"),
        upper = list(continuous = wrap("cor", stars=FALSE)))

# Tour
library(tourr)
library(detourr)
animate_xy(train[,-1]) # strong collinearity
animate_xy(train, dependence_tour(c(2,1,1,1)))
animate_xy(train)

predictors_detour <- detour(train,
      tour_aes(projection = c(x1:x3))) |>
  tour_path(grand_tour(2), 
            start = basis_random(3,2), 
            max_bases = 5) |>
  show_scatter(alpha = 0.7,
               axes = TRUE,
               center = TRUE,
               size = 1,
               scale_factor = 0.15,
               width = "1000px",
               height = "800px")
saveRDS(predictors_detour, file="detour/predictors_detour.rds")

train_all_detour <- detour(train,
                            tour_aes(projection = c(y, x1:x3))) |>
  tour_path(grand_tour(2), 
            start = basis_random(4,2), 
            max_bases = 10) |>
  show_scatter(alpha = 0.7,
               axes = TRUE,
               center = TRUE,
               size = 1,
               scale_factor = 0.2,
               width = "1000px",
               height = "800px")
saveRDS(train_all_detour, file="detour/train_all_detour.rds")

# Check training and test
all <- rbind(cbind(train, cl=rep("train", nrow(train))),
             cbind(test, cl=rep("test", nrow(test))))
animate_xy(all[,1:4], col=all$cl)

# Look at models
models <- rbind(
  cbind(train[,-1], yhat=mp_lm$residuals[,1], m=rep("lm", nrow(train))),
  cbind(train[,-1], yhat=mp_dt$residuals[,1], m=rep("dt", nrow(train))),
  cbind(train[,-1], yhat=mp_rf$residuals[,1], m=rep("rf", nrow(train))),
  cbind(train[,-1], yhat=mp_nn$residuals[,1], m=rep("nn", nrow(train))))
models$m <- factor(models$m)
animate_xy(models[,1:4], col=models$m)
animate_xy(models[,1:4], guided_tour(lda_pp(models$m)), col=models$m)
animate_xy(models[models$m %in% c("lm", "dt"),1:4], col=models$m[models$m %in% c("lm", "dt")])
animate_xy(models[models$m %in% c("lm", "dt"),1:4], guided_tour(lda_pp(models$m[models$m %in% c("lm", "dt")])), col=models$m[models$m %in% c("lm", "dt")])

# Predict on a grid of X
grid <- expand.grid(x1=seq(-4, 4, 0.5),
                    x2=seq(-4, 4, 0.5),
                    x3=seq(-4, 4, 0.5))
models_grid <- data.frame(x1=grid$x1,
                          x2=grid$x2,
                          x3=grid$x3,
                          yhat=predict(model_lm, grid),
                          m="lm")

models_grid <- rbind(models_grid,
                     data.frame(x1=grid$x1,
                                x2=grid$x2,
                                x3=grid$x3,
                                yhat=predict(model_dt, grid),
                                m="dt"))
models$m <- factor(models$m)

animate_xy(models_grid[,1:4], col=models_grid$m)
animate_xy(models_grid[,1:4], guided_tour(lda_pp(models_grid$m)), col=models_grid$m)

library(detourr)
tpath <- save_history(train, dependence_tour(c(1, 2, 2, 2)), max = 4)
tmp <- tpath[,1,1]
tpath[,1,1] <- tpath[,2,1]
tpath[,2,1] <- tmp

tpath[1,1,2] <- 1
tmp <- c(0, 0, 1, 0)
tpath[,2,2] <- tpath[,1,2]
tpath[,1,2] <- tmp

tpath[1,1,3] <- 1
tmp <- c(0, 0, 0, 1)
tpath[,2,3] <- tpath[,1,3]
tpath[,1,3] <- tmp

tpath[1,1,4] <- 1
tmp <- c(0, 1, 0, 0)
tpath[,2,4] <- tpath[,1,4]
tpath[,1,4] <- tmp

library(htmltools)

# Compare observed and fitted
train_lm <- rbind(
  cbind(y=train[,1], train[,-1], m=rep("obs", nrow(train))),
  cbind(y=yhat_lm, train[,-1], m=rep("lm", nrow(train)))
)

train_lm_detour <- detour(train_lm,
                          tour_aes(projection = c(y, x1:x3), 
                                   colour = m)) |>
  tour_path(planned_tour(tpath)) |>
  show_scatter(alpha = 0.7,
               axes = TRUE,
               center = TRUE,
               size = 1,
               scale_factor = 0.2,
               width = "1000px",
               height = "800px")
saveRDS(train_lm_detour, file="detour/train_lm_detour.rds")

train_dt <- rbind(
  cbind(y=train[,1], train[,-1], m=rep("obs", nrow(train))),
  cbind(y=yhat_dt, train[,-1], m=rep("lm", nrow(train)))
)

train_dt_detour <- detour(train_dt,
                          tour_aes(projection = c(y, x1:x3), 
                                   colour = m)) |>
  tour_path(planned_tour(tpath)) |>
  show_scatter(alpha = 0.7,
               axes = TRUE,
               center = TRUE,
               size = 1,
               scale_factor = 0.2,
               width = "1000px",
               height = "800px")
saveRDS(train_dt_detour, file="detour/train_dt_detour.rds")

train_rf <- rbind(
  cbind(y=train[,1], train[,-1], m=rep("obs", nrow(train))),
  cbind(y=yhat_rf, train[,-1], m=rep("lm", nrow(train)))
)

train_rf_detour <- detour(train_rf,
                          tour_aes(projection = c(y, x1:x3), 
                                   colour = m)) |>
  tour_path(planned_tour(tpath)) |>
  show_scatter(alpha = 0.7,
               axes = TRUE,
               center = TRUE,
               size = 1,
               scale_factor = 0.2,
               width = "1000px",
               height = "800px")
saveRDS(train_rf_detour, file="detour/train_rf_detour.rds")

train_nn <- rbind(
  cbind(y=train[,1], train[,-1], m=rep("obs", nrow(train))),
  cbind(y=yhat_nn, train[,-1], m=rep("lm", nrow(train)))
)

train_nn_detour <- detour(train_nn,
                          tour_aes(projection = c(y, x1:x3), 
                                   colour = m)) |>
  tour_path(planned_tour(tpath)) |>
  show_scatter(alpha = 0.7,
               axes = TRUE,
               center = TRUE,
               size = 1,
               scale_factor = 0.2,
               width = "1000px",
               height = "800px")
saveRDS(train_nn_detour, file="detour/train_nn_detour.rds")

train_detour <- detour(train,
       tour_aes(projection = c(y, x1:x3))) |>
  tour_path(planned_tour(tpath)) |>
  show_scatter(alpha = 0.7,
               axes = TRUE,
               center = TRUE,
               size = 1,
               scale_factor = 0.2,
               width = "1000px",
               height = "800px")

#save_html(train_detour, file="detour/train_detour.html")
saveRDS(train_detour, file="detour/train_detour.rds")

# Fitted values
models_fitted <- rbind(
  cbind(yhat=yhat_lm, train[,-1], m=rep("lm", nrow(train))),
  cbind(yhat=yhat_dt, train[,-1], m=rep("dt", nrow(train))),
  cbind(yhat=yhat_rf, train[,-1], m=rep("rf", nrow(train))),
  cbind(yhat=yhat_nn, train[,-1], m=rep("nn", nrow(train))))
models_fitted$m <- factor(models_fitted$m)

clrs <- divergingx_hcl(7, "Zissou 1")
model_colors <- c(
  "dt" = clrs[1],
  "lm"   = clrs[3],
  "nn"   = clrs[5],
  "rf"   = clrs[7]
)

detour(models_fitted,
       tour_aes(projection = c(yhat, x1:x3), colour = m, labels = m)) |>
  tour_path(planned_tour(tpath)) |>
  show_scatter(alpha = 0.7,
               axes = TRUE,
               center = FALSE,
               scale_factor = 0.3,
               width = "1000px",
               height = "800px")

model_detour1 <- detour(models_fitted[models_fitted$m %in% c("lm", "dt"),],
                        tour_aes(projection = c(yhat, x1:x3), colour = m, labels = m)) |>
  tour_path(planned_tour(tpath)) |>
  show_scatter(alpha = 0.7,
               axes = TRUE,
               center = FALSE,
               size = 1,
               scale_factor = 0.2,
               width = "1000px",
               height = "800px")
#save_html(model_detour1, file="detour/model_detour1.html")
saveRDS(model_detour1, file="detour/model_detour1.rds")

model_detour2 <- detour(models_fitted[models_fitted$m %in% c("lm", "rf"),],
       tour_aes(projection = c(yhat, x1:x3), colour = m, labels = m)) |>
  tour_path(planned_tour(tpath)) |>
  show_scatter(alpha = 0.7,
               axes = TRUE,
               center = FALSE,
               size = 1, 
               scale_factor = 0.2,
               width = "1000px",
               height = "800px")
#save_html(model_detour2, file="detour/model_detour2.html")
saveRDS(model_detour2, file="detour/model_detour2.rds")

model_detour3 <- detour(models_fitted[models_fitted$m %in% c("lm", "nn"),],
                        tour_aes(projection = c(yhat, x1:x3), colour = m, labels = m)) |>
  tour_path(planned_tour(tpath)) |>
  show_scatter(alpha = 0.7,
               axes = TRUE,
               center = TRUE,
               scale_factor = 0.2,
               size = 1,
               width = "1000px",
               height = "800px")
#save_html(model_detour3, file="detour/model_detour3.html")
saveRDS(model_detour3, file="detour/model_detour3.rds")

