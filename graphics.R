# Looking at the model with tours
source("setup.R")
load("data/sp4_data_true.rda")
load("data/sp4_rf.rda")

# Training data
animate_xy(sp4_data_tr[,1:4], 
           guided_tour(lda_pp(sp4_data_tr$class)),
           col=sp4_data_tr$class,
           axes = "bottomleft")

set.seed(209)
sp4_tr_detour_gt <- detour(sp4_data_tr,
                        tour_aes(projection = x1:x4, colour = class)) |>
  tour_path(grand_tour(2), start = basis_random(4,2),
            fps = 100) |>
  show_scatter(alpha = 0.7, 
               axes = TRUE,
               scale_factor = 0.5, 
               palette = ochre_clrs)
saveRDS(sp4_tr_detour_gt, file="detour/sp4_tr_detour_gt.rds")

# Make this with same path as used later
set.seed(1026)
tpath4 <- save_history(sp4_data_true[,1:4],
                       guided_tour(lda_pp(sp4_data_true$fitted)))
save(tpath4, file="data/tpath4.rda")
sp4_tr_detour <- detour(sp4_data_tr,
       tour_aes(projection = x1:x4, colour = class)) |>
  tour_path(planned_tour(tpath4), 
            fps = 100) |>
  show_scatter(alpha = 0.7, 
               axes = TRUE,
               scale_factor = 0.5, 
               palette = ochre_clrs)
saveRDS(sp4_tr_detour, file="detour/sp4_tr_detour.rds")

load("data/sp4_pt.rda")
sp4_data_tr_pts <- sp4_data_tr
sp4_data_tr_pts$class <- as.character(sp4_data_tr_pts$class)
sp4_data_tr_pts$class[pt$idx[1]] <- rep("A1", 2)
sp4_data_tr_pts$class[pt$idx[2]] <- rep("A2", 2)
sp4_data_tr_pts$labels <- "o"
sp4_data_tr_pts$labels[pt$idx[1]] <- "1"
sp4_data_tr_pts$labels[pt$idx[2]] <- "2"
sp4_tr_detour_pts <- detour(sp4_data_tr_pts,
                        tour_aes(projection = x1:x4, 
                                 colour = class,
                                 labels = labels)) |>
  tour_path(planned_tour(tpath4), 
            fps = 100) |>
  show_scatter(alpha = 0.7,
               size = 1.5,
               axes = TRUE,
               scale_factor = 0.5, 
               palette = c("#FF793B", "#33FF11", ochre_clrs))
saveRDS(sp4_tr_detour_pts, file="detour/sp4_tr_detour_pts.rds")

# set.seed(707)
# prj <- animate_xy(sp4_data_tr[,1:4], 
#                   guided_tour(lda_pp(sp4_data_tr$class)),
#                   col=sp4_data_tr$class,
#                   axes = "bottomleft")
# best_prj <- prj$basis[length(prj$basis)][[1]]
# 
# sp4_tr_detour_x2 <- detour(sp4_data_tr,
#        tour_aes(projection = x1:x4, colour = class)) |>
#   tour_path(radial_tour(best_prj, 2), fps = 100) |>
#   show_scatter(alpha = 0.7, 
#                axes = TRUE,
#                palette = ochre_clrs)
# sp4_tr_detour_x4 <- detour(sp4_data_tr,
#                            tour_aes(projection = x1:x4, colour = class)) |>
#   tour_path(radial_tour(best_prj, 4), fps = 100) |>
#   show_scatter(alpha = 0.7, 
#                axes = TRUE,
#                palette = ochre_clrs)

# Work with fitted data grid
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
                           tour_aes(projection = x1:x4, 
                                    colour = fitted)) |>
  tour_path(radial_tour(best_prj, 2), fps = 100, max_bases = 3) |>
  show_slice(alpha = 0.7, 
             axes = TRUE,
             scale_factor = 0.4,
             slice_relative_volume = 0.05,
             palette = ochre_clrs)
saveRDS(sp4_true_detour_x1, file="detour/sp4_true_detour_x1.rds")

sp4_true_detour_x2 <- detour(sp4_data_true,
                             tour_aes(projection = x1:x4, 
                                      colour = fitted)) |>
  tour_path(radial_tour(best_prj, 2), fps = 100, max_bases = 3) |>
  show_slice(alpha = 0.7, 
             axes = TRUE,
             scale_factor = 0.4,
             slice_relative_volume = 0.05,
             palette = ochre_clrs)
saveRDS(sp4_true_detour_x2, file="detour/sp4_true_detour_x2.rds")

# Local views
load("data/sp4_d1.rda")
load("data/sp4_d2.rda")
sp4_d1_detour_x2 <- detour(sp4_d1,
                             tour_aes(projection = x1:x4, 
                                      colour = fitted,
                                      labels = labels)) |>
  tour_path(radial_tour(best_prj, 2), fps = 100, max_bases = 3) |>
  show_scatter(alpha = 0.7, 
             axes = TRUE,
             scale_factor = 2,
             palette = c("#FF793B", ochre_clrs))
saveRDS(sp4_d1_detour_x2, file="detour/sp4_d1_detour_x2.rds")

sp4_d1_detour_x4 <- detour(sp4_d1,
                           tour_aes(projection = x1:x4, 
                                    colour = fitted,
                                    labels = labels)) |>
  tour_path(radial_tour(best_prj, 4), fps = 100, max_bases = 3) |>
  show_scatter(alpha = 0.7, 
             axes = TRUE,
             scale_factor = 2,
             palette = c("#FF793B", ochre_clrs))
saveRDS(sp4_d1_detour_x4, file="detour/sp4_d1_detour_x4.rds")

sp4_d2_detour_x2 <- detour(sp4_d2,
                           tour_aes(projection = x1:x4, 
                                    colour = fitted,
                                    labels = labels)) |>
  tour_path(radial_tour(best_prj, 2), fps = 100, max_bases = 3) |>
  show_scatter(alpha = 0.7, 
               axes = TRUE,
               scale_factor = 2,
               palette = c("#33FF11", ochre_clrs))
saveRDS(sp4_d2_detour_x2, file="detour/sp4_d2_detour_x2.rds")

sp4_d2_detour_x4 <- detour(sp4_d2,
                           tour_aes(projection = x1:x4, 
                                    colour = fitted,
                                    labels = labels)) |>
  tour_path(radial_tour(best_prj, 4), fps = 100, max_bases = 3) |>
  show_scatter(alpha = 0.7, 
               axes = TRUE,
               scale_factor = 2,
               palette = c("#33FF11", ochre_clrs))
saveRDS(sp4_d2_detour_x4, file="detour/sp4_d2_detour_x4.rds")


# Extra stuff
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
