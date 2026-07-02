# Looking at the model with tours
source("setup.R")
load("data/sp4_data_true.rda")
load("data/sp4_rf.rda")

# Training data
animate_xy(sp4_data_tr[,1:4], 
           guided_tour(lda_pp(sp4_data_tr$class)),
           col=sp4_data_tr$class,
           axes = "bottomleft")

# Make this with same path as used later
set.seed(1026)
tpath4 <- save_history(sp4_data_true[,1:4],
                       guided_tour(lda_pp(sp4_data_true$fitted)))
sp4_tr_detour <- detour(sp4_data_tr,
       tour_aes(projection = x1:x4, colour = class)) |>
  tour_path(planned_tour(tpath4), 
            fps = 100) |>
  show_scatter(alpha = 0.7, 
               axes = TRUE,
               scale_factor = 0.5, 
               palette = ochre_clrs)
saveRDS(sp4_tr_detour, file="detour/sp4_tr_detour.rds")

set.seed(707)
prj <- animate_xy(sp4_data_tr[,1:4], 
                  guided_tour(lda_pp(sp4_data_tr$class)),
                  col=sp4_data_tr$class,
                  axes = "bottomleft")
best_prj <- prj$basis[length(prj$basis)][[1]]

sp4_tr_detour_x2 <- detour(sp4_data_tr,
       tour_aes(projection = x1:x4, colour = class)) |>
  tour_path(radial_tour(best_prj, 2), fps = 100) |>
  show_scatter(alpha = 0.7, 
               axes = TRUE,
               palette = ochre_clrs)
sp4_tr_detour_x4 <- detour(sp4_data_tr,
                           tour_aes(projection = x1:x4, colour = class)) |>
  tour_path(radial_tour(best_prj, 4), fps = 100) |>
  show_scatter(alpha = 0.7, 
               axes = TRUE,
               palette = ochre_clrs)

# Too hard with training data, work with fitted data grid
sp4_true_detour <- detour(sp4_data_true,
                        tour_aes(projection = x1:x4, 
                                 colour = fitted)) |>
  tour_path(planned_tour(tpath4), 
            fps = 100) |>
  show_scatter(alpha = 0.7, 
               axes = TRUE,
               scale_factor = 0.4,
               palette = ochre_clrs)
saveRDS(sp4_true_detour, file="detour/sp4_true_detour.rds")

sp4_true_detour_slice <- detour(sp4_data_true, 
       tour_aes(projection = x1:x4, 
                colour = fitted)) |>
  tour_path(planned_tour(tpath4),  
            fps = 100) |>
  show_slice(alpha = 0.7, 
               axes = TRUE,
               scale_factor = 0.4,
               slice_relative_volume = 0.05,
               palette = ochre_clrs)
saveRDS(sp4_true_detour_slice, file="detour/sp4_true_detour_slice.rds")

best_prj <- matrix(
  tpath4[,,length(tpath4)],
  nrow = 4,
  ncol = 2
)

sp4_true_detour_x1 <- detour(sp4_data_true,
                           tour_aes(projection = x1:x4, colour = fitted)) |>
  tour_path(radial_tour(best_prj, 1), fps = 100, max_bases = 3) |>
  show_slice(alpha = 0.7, 
             axes = TRUE,
             scale_factor = 0.4,
             slice_relative_volume = 0.05,
             palette = ochre_clrs)
saveRDS(sp4_true_detour_x1, file="detour/sp4_true_detour_x1.rds")

sp4_true_detour_x2 <- detour(sp4_data_true,
                             tour_aes(projection = x1:x4, colour = fitted)) |>
  tour_path(radial_tour(best_prj, 2), fps = 100, max_bases = 3) |>
  show_slice(alpha = 0.7, 
             axes = TRUE,
             scale_factor = 0.4,
             slice_relative_volume = 0.05,
             palette = ochre_clrs)
saveRDS(sp4_true_detour_x2, file="detour/sp4_true_detour_x2.rds")


# Graphics
animate_xy(sp4_data_true[,1:3], 
           col=sp4_data_true$fitted,
           axes = "bottomleft")
set.seed(707)
prj <- animate_xy(sp4_data_true[,1:3], 
                  guided_tour(lda_pp(sp4_data_true$fitted)), 
                  col=sp4_data_true$fitted,
                  axes = "bottomleft")
best_prj <- prj$basis[length(prj$basis)][[1]]

animate_xy(sp4_data_true[,1:3], 
           radial_tour(best_prj, 1), 
           col=sp4_data_true$fitted,
           axes = "bottomleft", fps = 100)

detour(sp4_data_true,
       tour_aes(projection = x1:x3, colour = fitted)) |>
  tour_path(radial_tour(best_prj, 1), fps = 100) |>
  show_scatter(alpha = 0.7, axes = TRUE)

# Compute points near the boundary
sp4_data_true <- sp4_data_true |>
  mutate(adv = 
           abs(predict(sp4_rf, sp4_data_true, type="prob")[,1] -
               predict(sp4_rf, sp4_data_true, type="prob")[,2]))

sp4_data_true_bnd <- sp4_data_true |> dplyr::filter(adv < 0.4) 

detour(sp4_data_true_bnd,
       tour_aes(projection = x1:x3, colour = fitted)) |>
  tour_path(grand_tour(), fps = 100) |>
  show_scatter(alpha = 0.7, axes = TRUE)

detour(sp4_data_true_bnd,
       tour_aes(projection = x1:x3, colour = fitted)) |>
  tour_path(little_tour(), fps = 100) |>
  show_scatter(alpha = 0.7, axes = TRUE)

detour(sp4_data_true_bnd,
       tour_aes(projection = x1:x3, colour = fitted)) |>
  tour_path(radial_tour(best_prj, 1), fps = 100) |>
  show_scatter(alpha = 0.7, axes = TRUE)
