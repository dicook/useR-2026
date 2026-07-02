# Generate the data
library(sillysplines)
library(dplyr)
library(ggplot2)
library(randomForest)
library(colorspace)
library(jsonlite)
# sillysplines() # Generates boundary

# This data is 2D with both variables important
n_tr <- 1000
n_ts <- 500
sp_data_tr <- create_data(coord_filepath="data/coords5.json", 
                n_samples = n_tr, seed=730, gap=0.05) |>
  mutate(class = factor(class))
sp_data_ts <- create_data(coord_filepath="data/coords5.json", 
                n_samples = n_ts, seed=237, gap=0.05) |>
  mutate(class = factor(class))
boundary <- fromJSON("data/coords5.json")
sp_data_true <- expand.grid(x = seq(-1, 1, 0.02),
                            y = seq(-1, 1, 0.02)) 
sp_data_true$class = classify_boundary(sp_data_true, boundary)$preds 
sp_data_true <- sp_data_true |>
  mutate(class = factor(class))

# Check data
ggplot(sp_data_true, aes(x = x, y = y, colour = class)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1)
ggplot(sp_data_tr, aes(x = x, y = y, colour = class)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1)
ggplot(sp_data_ts, aes(x = x, y = y, colour = class)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1)

# Fit model
set.seed(358)
sp_rf <- randomForest(class~., data = sp_data_tr, ntree=300)

# Predict full grid of values using model
sp_data_true <- sp_data_true |>
  mutate(fitted = predict(sp_rf, sp_data_true))

# Plot all
ggplot(sp_data_true, aes(x = x, y = y, colour = fitted)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1)

save(sp_rf, file = "data/sp_rf.rda")
save(sp_data_tr, file = "data/sp_data_tr.rda")
save(sp_data_ts, file = "data/sp_data_ts.rda")
save(sp_data_true, file = "data/sp_data_true.rda")

# Here we add one variable which is pure noise
sp3_data_tr <- tibble(x1 = runif(n_tr, -1, 1),
                      x2 = sp_data_tr$x,
                      x3 = sp_data_tr$y,
                      class = sp_data_tr$class)
sp3_data_ts <- tibble(x1 = runif(n_ts, -1, 1),
                      x2 = sp_data_ts$x,
                      x3 = sp_data_ts$y,
                      class = sp_data_ts$class)
sp3_data_true <- tibble(x1 = runif(nrow(sp_data_true), -1, 1),
                        x2 = sp_data_true$x,
                        x3 = sp_data_true$y,
                        class = sp_data_true$class)

ggscatmat(sp3_data_true, columns = 1:3, 
          color = "class", alpha=0.1) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1,
        axis.text = element_blank(),
        axis.title = element_blank())

sp3_rf <- randomForest(class~., data = sp3_data_tr, ntree=150)

# Predict full grid of values using model
sp3_data_true <- sp3_data_true |>
  mutate(fitted = predict(sp3_rf, sp3_data_true))

ggscatmat(sp3_data_true, columns = 1:3, 
          color = "fitted", alpha=0.1) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1,
        axis.text = element_blank(),
        axis.title = element_blank())

save(sp3_rf, file = "data/sp3_rf.rda")
save(sp3_data_tr, file = "data/sp3_data_tr.rda")
save(sp3_data_ts, file = "data/sp3_data_ts.rda")
save(sp3_data_true, file = "data/sp3_data_true.rda")

# Here we add two noise variables 
n_obs_tr <- nrow(sp_data_tr)
n_obs_ts <- nrow(sp_data_ts)
sp4_data_tr <- tibble(x1 = runif(n_obs_tr, -1, 1),
                      x2 = sp_data_tr$x,
                      x3 = runif(n_obs_tr, -1, 1),
                      x4 = sp_data_tr$y,
                      class = sp_data_tr$class)
sp4_data_ts <- tibble(x1 = runif(n_obs_ts, -1, 1),
                      x2 = sp_data_ts$x,
                      x3 = runif(n_obs_ts, -1, 1),
                      x4 = sp_data_ts$y,
                      class = sp_data_ts$class)
sp4_data_true <- tibble(x1 = runif(nrow(sp_data_true), -1, 1),
                        x2 = sp_data_true$x,
                        x3 = runif(nrow(sp_data_true), -1, 1),
                        x4 = sp_data_true$y,
                        class = sp_data_true$class)

ggscatmat(sp4_data_true, columns = 1:4, 
          color = "class", alpha=0.1) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1,
        axis.text = element_blank(),
        axis.title = element_blank())

sp4_rf <- randomForest(class~., data = sp4_data_tr, ntree=150)

# Predict full grid of values using model
sp4_data_true <- sp4_data_true |>
  mutate(fitted = predict(sp4_rf, sp4_data_true))

ggscatmat(sp4_data_true, columns = 1:4, 
          color = "fitted", alpha=0.1) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1,
        axis.text = element_blank(),
        axis.title = element_blank())

save(sp4_rf, file = "data/sp4_rf.rda")
save(sp4_data_tr, file = "data/sp4_data_tr.rda")
save(sp4_data_ts, file = "data/sp4_data_ts.rda")
save(sp4_data_true, file = "data/sp4_data_true.rda")

# Here we add three noise variables 
sp5_data_tr <- tibble(x1 = runif(n_tr, -1, 1),
                      x2 = sp_data_tr$x,
                      x3 = runif(n_tr, -1, 1),
                      x4 = sp_data_tr$y,
                      x5 = runif(n_tr, -1, 1),
                      class = sp_data_tr$class)
sp5_data_ts <- tibble(x1 = runif(n_ts, -1, 1),
                      x2 = sp_data_ts$x,
                      x3 = runif(n_ts, -1, 1),
                      x4 = sp_data_ts$y,
                      x5 = runif(n_ts, -1, 1),
                      class = sp_data_ts$class)
sp5_data_true <- tibble(x1 = runif(nrow(sp_data_true), -1, 1),
                        x2 = sp_data_true$x,
                        x3 = runif(nrow(sp_data_true), -1, 1),
                        x4 = sp_data_true$y,
                        x5 = runif(nrow(sp_data_true), -1, 1),
                        class = sp_data_true$class)

ggscatmat(sp5_data_true, columns = 1:5, 
          color = "class", alpha=0.1) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1,
        axis.text = element_blank(),
        axis.title = element_blank())

sp5_rf <- randomForest(class~., data = sp5_data_tr, ntree=150)

# Predict full grid of values using model
sp5_data_true <- sp5_data_true |>
  mutate(fitted = predict(sp5_rf, sp5_data_true))

ggscatmat(sp5_data_true, columns = 1:5, 
          color = "fitted", alpha=0.1) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio = 1,
        axis.text = element_blank(),
        axis.title = element_blank())

save(sp5_rf, file = "data/sp5_rf.rda")
save(sp5_data_tr, file = "data/sp5_data_tr.rda")
save(sp5_data_ts, file = "data/sp5_data_ts.rda")
save(sp5_data_true, file = "data/sp5_data_true.rda")

