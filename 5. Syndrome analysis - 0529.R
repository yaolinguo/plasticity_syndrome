library(missForest)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(cluster)
library(factoextra)
library(fmsb)
library(ggplot2)
library(ggplotify)
library(cowplot)
library(maps)
library(scales)
library(tibble)
library(dendextend)
library(MASS)

# ----------------------------
# 1. Define trait variables
# ----------------------------
setwd("/Users/yaolin/Desktop/My papers/My manuscripts/2025 - Guo - Plasticity syndrome/Submission - Journal of Ecology - 0328 - R2/Data & Code")

trait_cols <- c(
  "Leaf_thickness", 
  "Leaf_length", 
  "Leaf_width", 
  "Leaf_area", 
  "SLA",
  "Leaf_saturated_fresh_weight", 
  "Leaf_dry_weight",
  "LDMC",
  "Aboveground_biomass",
  "Belowground_biomass", 
  "Shoot_height",
  "Plant_number", 
  "Shoot_diameter",
  "Leaf_C",
  "Leaf_N", 
  "Leaf_CN",
  "Root_C",
  "Root_N", 
  "Root_CN", 
  "SPAD"
)

trait_labels <- c(
  "Leaf_thickness"              = "Leaf thickness",
  "Leaf_length"                 = "Leaf length",
  "Leaf_width"                  = "Leaf width",
  "Leaf_area"                   = "Leaf area",
  "SLA"                         = "SLA",
  "Leaf_saturated_fresh_weight" = "Leaf saturated mass",
  "Leaf_dry_weight"             = "Leaf dry mass",
  "LDMC"                        = "LDMC",
  "Aboveground_biomass"         = "Aboveground biomass",
  "Belowground_biomass"         = "Belowground biomass",
  "Shoot_height"                = "Shoot height",
  "Plant_number"                = "Shoot number",
  "Shoot_diameter"              = "Shoot diameter",
  "Leaf_C"                      = "Leaf C",
  "Leaf_N"                      = "Leaf N",
  "Leaf_CN"                     = "Leaf C:N",
  "Root_C"                      = "Root C",
  "Root_N"                      = "Root N",
  "Root_CN"                     = "Root C:N",
  "SPAD"                        = "Chlorophyll"
)

quiet_num <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(
    readr::parse_number(as.character(x), na = c("", "NA", "P"))
  )
}

mean_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

range01 <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if ((rng[2] - rng[1]) == 0) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

calc_pi <- function(sh, qd) {
  out <- abs(sh - qd) / pmax(sh, qd)
  out[!is.finite(out)] <- NA_real_
  out
}

# ----------------------------
# 2. Read pre-computed PI from Data cleaning pipeline
# ----------------------------
data_PI_imputed <- read.csv("Data_imp_PI.csv", check.names = FALSE)
data_PI_imputed <- as.data.frame(data_PI_imputed)

# ----------------------------
# 3. Cluster samples into PI syndromes
# ----------------------------
pi_cols <- paste0(trait_cols, "_PI")

cluster_df <- data_PI_imputed %>%
  dplyr::select(Sample_ID, all_of(pi_cols)) %>%
  mutate(across(all_of(pi_cols), range01)) %>%
  filter(if_all(all_of(pi_cols), ~ !is.na(.))) %>%
  as.data.frame(stringsAsFactors = FALSE)

rownames(cluster_df) <- cluster_df$Sample_ID
cluster_data <- cluster_df[, pi_cols, drop = FALSE]

dist_matrix <- dist(cluster_data, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")

# ----------------------------
# 4. Evaluate candidate cluster numbers with the gap statistic
# ----------------------------
gap_stat <- clusGap(
  as.matrix(cluster_data),
  FUN    = hcut,
  nstart = 25,
  K.max  = 10,
  B      = 500
)

gap_df <- data.frame(
  cluster = 1:nrow(gap_stat$Tab),
  gap     = gap_stat$Tab[, "gap"],
  SE      = gap_stat$Tab[, "SE.sim"]
)

p_gap <- ggplot(gap_df, aes(x = cluster, y = gap)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = gap - SE, ymax = gap + SE),
    width = 0.2,
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = 3,
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  labs(
    x = "Number of clusters (k)",
    y = "Gap statistic"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line    = element_line(color = "black", linewidth = 0.5),
    axis.ticks   = element_line(color = "black"),
    axis.text    = element_text(color = "black"),
    axis.title   = element_text(face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

print(p_gap)

ggsave(
  "Figure_PI_gap_statistic.pdf",
  p_gap,
  height = 90,
  width  = 120,
  units  = "mm",
  limitsize = FALSE
)

# ----------------------------
# 5. Cut the dendrogram and assign syndrome labels
# ----------------------------
k_optimal <- 3
hc_clusters <- cutree(hc, k = k_optimal)
table(hc_clusters)

color_border <- c("#E41A1C", "#377EB8", "#4DAF4A")[1:k_optimal]
color_fill   <- c("#E41A1C30", "#377EB830", "#4DAF4A30")[1:k_optimal]

dend <- as.dendrogram(hc)
dend_colored <- color_branches(dend, k = k_optimal, col = color_border)
dend_colored <- set(dend_colored, "labels_cex", 0.45)
dend_colored <- set(dend_colored, "branches_lwd", 1.0)

p_dend <- ggplotify::as.ggplot(~{
  par(mar = c(6, 4, 3, 2))
  plot(
    dend_colored,
    main = paste0("Hierarchical clustering with k = ", k_optimal),
    ylab = "Height"
  )
  rect.hclust(hc, k = k_optimal, border = color_border)
})

print(p_dend)

ggsave(
  "Figure_PI_dendrogram.pdf",
  p_dend,
  height = 120,
  width  = 260,
  units  = "mm",
  limitsize = FALSE
)

cluster_assign <- data.frame(
  Sample_ID = names(hc_clusters),
  cluster   = factor(unname(hc_clusters), levels = 1:k_optimal),
  stringsAsFactors = FALSE
)

syndrome_names <- c(
  "1" = "Syndrome 1",
  "2" = "Syndrome 2",
  "3" = "Syndrome 3"
)

cluster_assign <- cluster_assign %>%
  mutate(
    syndrome = factor(
      syndrome_names[as.character(cluster)],
      levels = syndrome_names
    )
  )

cluster_data_final <- cluster_df %>%
  left_join(cluster_assign, by = "Sample_ID")

# ----------------------------
# 6. Figure 4a
# ----------------------------
# Average PI profiles within each syndrome for the radar chart
data_radar_raw <- cluster_data_final %>%
  group_by(cluster, syndrome) %>%
  summarise(across(all_of(pi_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(cluster)

rownames(data_radar_raw) <- as.character(data_radar_raw$syndrome)
data_radar_raw <- data_radar_raw[, pi_cols, drop = FALSE]

desired_order <- paste0(
  c(
    "Leaf_thickness", "Leaf_length", "Leaf_width", "Leaf_area", "SLA",
    "Leaf_saturated_fresh_weight", "Leaf_dry_weight", "LDMC",
    "Aboveground_biomass", "Belowground_biomass", "Shoot_height",
    "Plant_number", "Shoot_diameter",
    "Leaf_C", "Leaf_N", "Leaf_CN",
    "Root_C", "Root_N", "Root_CN", "SPAD"
  ),
  "_PI"
)

data_radar_raw <- data_radar_raw[, desired_order, drop = FALSE]
colnames(data_radar_raw) <- unname(trait_labels[str_remove(desired_order, "_PI")])

syndrome_levels <- rownames(data_radar_raw)
trait_levels    <- colnames(data_radar_raw)
n_traits        <- length(trait_levels)

angle_df <- tibble(
  trait = trait_levels,
  id    = 1:n_traits,
  angle = pi / 2 + 2 * pi * (0:(n_traits - 1)) / n_traits
)

radar_long <- data_radar_raw %>%
  rownames_to_column("syndrome") %>%
  pivot_longer(
    cols = -syndrome,
    names_to = "trait",
    values_to = "value"
  ) %>%
  mutate(
    syndrome = factor(syndrome, levels = syndrome_levels),
    trait    = factor(trait, levels = trait_levels)
  ) %>%
  left_join(angle_df, by = "trait") %>%
  mutate(
    x = value * cos(angle),
    y = value * sin(angle)
  ) %>%
  arrange(syndrome, id)

radar_poly <- radar_long %>%
  group_by(syndrome) %>%
  arrange(id, .by_group = TRUE) %>%
  dplyr::slice(c(seq_len(dplyr::n()), 1)) %>%
  ungroup()

circle_breaks <- c(0.25, 0.50, 0.75, 1.00)
theta_seq <- seq(0, 2 * pi, length.out = 500)

grid_circles <- expand_grid(
  radius = circle_breaks,
  theta  = theta_seq
) %>%
  mutate(
    x = radius * cos(theta),
    y = radius * sin(theta)
  )

spokes_df <- angle_df %>%
  mutate(
    x    = 0,
    y    = 0,
    xend = cos(angle),
    yend = sin(angle)
  )

label_radius <- 1.22
trait_lab_df <- angle_df %>%
  mutate(
    x = label_radius * cos(angle),
    y = label_radius * sin(angle),
    hjust = case_when(
      cos(angle) >  0.15 ~ 0,
      cos(angle) < -0.15 ~ 1,
      TRUE               ~ 0.5
    ),
    vjust = case_when(
      sin(angle) >  0.85 ~ 0,
      sin(angle) < -0.85 ~ 1,
      TRUE               ~ 0.5
    )
  )

axis_lab_df <- tibble(
  x     = 0.03,
  y     = c(0.03, 0.25, 0.50, 0.75, 1.00),
  label = c("0", "0.25", "0.50", "0.75", "1.00")
)

p_a_raw <- ggplot() +
  geom_path(
    data = grid_circles,
    aes(x = x, y = y, group = radius),
    color = "grey80",
    linewidth = 0.5
  ) +
  geom_segment(
    data = spokes_df,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "grey80",
    linewidth = 0.5
  ) +
  geom_polygon(
    data = radar_poly,
    aes(x = x, y = y, group = syndrome, fill = syndrome),
    alpha = 0.22,
    color = NA
  ) +
  geom_path(
    data = radar_poly,
    aes(x = x, y = y, group = syndrome, color = syndrome),
    linewidth = 1.2
  ) +
  geom_point(
    data = radar_long,
    aes(x = x, y = y, color = syndrome),
    size = 2.2
  ) +
  geom_text(
    data = trait_lab_df,
    aes(x = x, y = y, label = trait, hjust = hjust, vjust = vjust),
    size = 4
  ) +
  geom_text(
    data = axis_lab_df,
    aes(x = x, y = y, label = label),
    color = "blue",
    size = 5
  ) +
  scale_color_manual(
    values = setNames(color_border, syndrome_levels),
    name   = NULL
  ) +
  scale_fill_manual(
    values = setNames(color_fill, syndrome_levels),
    guide  = "none"
  ) +
  coord_equal(
    xlim = c(-1.35, 1.35),
    ylim = c(-1.28, 1.30),
    clip = "off"
  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.text      = element_text(size = 11),
    plot.margin      = margin(8, 18, 0, 18)
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        linewidth = 1.2,
        size = 2.5,
        alpha = 1
      )
    )
  )

p_a <- plot_grid(
  NULL, p_a_raw, NULL,
  nrow = 1,
  rel_widths = c(0.03, 0.94, 0.03)
)

# ----------------------------
# 7. Figure 4b
# ----------------------------
data_cluster_map <- data_PI_imputed %>%
  mutate(Sample_ID = as.character(Sample_ID)) %>%
  left_join(cluster_assign, by = "Sample_ID")

world <- map_data("world")

p_b_raw <- ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill  = "grey90",
    color = "grey75"
  ) +
  geom_point(
    data = data_cluster_map %>% filter(!is.na(syndrome), !is.na(Latitude), !is.na(Longitude)),
    aes(x = Longitude, y = Latitude, color = syndrome),
    size  = 3,
    shape = 16,
    alpha = 0.8
  ) +
  scale_color_manual(
    values = setNames(color_border, syndrome_names[1:k_optimal]),
    name   = "Syndrome"
  ) +
  xlab("Longitude (°)") +
  ylab("Latitude (°)") +
  scale_x_continuous(breaks = c(-100, 0, 100)) +
  coord_quickmap() +
  theme_classic(base_size = 14) +
  theme(
    axis.text         = element_text(size = 12, color = "black"),
    axis.title        = element_text(size = 13, color = "black"),
    legend.title      = element_text(size = 12),
    legend.text       = element_text(size = 11),
    panel.border      = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major  = element_line(colour = "grey80", linetype = "dashed", linewidth = 0.3),
    panel.grid.minor  = element_line(colour = "grey88", linetype = "dotted", linewidth = 0.2),
    legend.position   = c(0.13, 0.22),
    legend.background = element_blank(),
    plot.margin       = margin(5, 10, 5, 10)
  )

p_b <- plot_grid(
  NULL, p_b_raw, NULL,
  nrow = 1,
  rel_widths = c(0.03, 0.94, 0.03)
)

# ----------------------------
# 8. Combine Figure 4 panels a and b
# ----------------------------
final_fig_ab <- plot_grid(
  p_a, p_b,
  ncol = 1,
  rel_heights = c(1.0, 1.1),
  labels = c("a", "b"),
  label_fontface = "bold",
  label_size = 18,
  label_x = 0.01,
  label_y = 0.99
)

print(final_fig_ab)

ggsave(
  "Figure_PI_syndromes_ab.pdf",
  final_fig_ab,
  height = 210,
  width  = 250,
  units  = "mm",
  limitsize = FALSE
)

# ----------------------------
# 9. Prepare complete PI data for LDA
# ----------------------------
lda_data <- cluster_data_final %>%
  dplyr::select(Sample_ID, syndrome, all_of(pi_cols)) %>%
  dplyr::filter(!is.na(syndrome)) %>%
  tidyr::drop_na(all_of(pi_cols)) %>%
  dplyr::mutate(syndrome = factor(syndrome))

# ----------------------------
# 10. Fit LDA
# ----------------------------
lda_fit <- MASS::lda(
  x = as.matrix(lda_data[, pi_cols]),
  grouping = lda_data$syndrome
)

lda_pred <- predict(lda_fit)

lda_scores <- bind_cols(
  lda_data %>% dplyr::select(Sample_ID, syndrome),
  as.data.frame(lda_pred$x)
)

var_explained <- lda_fit$svd^2 / sum(lda_fit$svd^2)

# ----------------------------
# 11. Extract and rank PI trait loadings
# ----------------------------
lda_loadings <- as.data.frame(lda_fit$scaling) %>%
  tibble::rownames_to_column("trait") %>%
  dplyr::mutate(
    trait_label = trait %>%
      stringr::str_remove("_PI$") %>%
      stringr::str_replace_all("_", " "),
    abs_LD1 = abs(LD1),
    abs_LD2 = if ("LD2" %in% names(.)) abs(LD2) else NA_real_
  ) %>%
  dplyr::mutate(total_abs = abs_LD1 + dplyr::coalesce(abs_LD2, 0)) %>%
  dplyr::arrange(dplyr::desc(total_abs))

top_traits <- lda_loadings %>%
  dplyr::slice_head(n = 10)

# ----------------------------
# 12. Figure S: LDA ordination
# ----------------------------
p_lda_a <- ggplot(lda_scores, aes(LD1, LD2, color = syndrome)) +
  stat_ellipse(
    aes(fill = syndrome),
    geom = "polygon",
    alpha = 0.15,
    color = NA,
    level = 0.95
  ) +
  geom_point(size = 2.6, alpha = 0.85) +
  scale_color_manual(
    values = setNames(color_border, levels(lda_scores$syndrome)),
    name = "Syndrome"
  ) +
  scale_fill_manual(
    values = setNames(color_fill, levels(lda_scores$syndrome)),
    name = "Syndrome"
  ) +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 5),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_y_continuous(
    breaks = function(x) pretty(x, n = 5),
    labels = scales::label_number(accuracy = 0.1)
  ) +
  labs(
    x = paste0("LD1 (", scales::percent(var_explained[1], accuracy = 0.1), ")"),
    y = paste0("LD2 (", scales::percent(var_explained[2], accuracy = 0.1), ")")
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    aspect.ratio = 1,
    plot.margin = margin(6, 6, 6, 6)
  )

# ----------------------------
# 13. Figure S6b: top absolute loadings on LD1 and LD2
# ----------------------------
loadings_long <- top_traits %>%
  dplyr::select(trait_label, abs_LD1, abs_LD2) %>%
  tidyr::pivot_longer(
    cols = c(abs_LD1, abs_LD2),
    names_to = "axis",
    values_to = "abs_loading"
  ) %>%
  dplyr::mutate(
    axis = dplyr::recode(axis, abs_LD1 = "LD1", abs_LD2 = "LD2"),
    trait_label = factor(trait_label, levels = rev(unique(top_traits$trait_label)))
  )

p_lda_b <- ggplot(loadings_long, aes(abs_loading, trait_label, fill = axis)) +
  geom_col(position = "dodge", width = 0.75) +
  scale_fill_manual(values = c("LD1" = "grey35", "LD2" = "grey75")) +
  scale_x_continuous(
    breaks = function(x) pretty(x, n = 4),
    labels = scales::label_number(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = "Absolute loading", y = NULL, fill = "Axis") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    aspect.ratio = 3 / 2,
    plot.margin = margin(6, 6, 6, 6)
  )

# ----------------------------
# 14. Extract legends, align main panels, and save outputs
# ----------------------------
legend_a <- cowplot::get_legend(
  p_lda_a +
    theme(
      legend.position = "bottom",
      legend.box.margin = margin(0, 0, 0, 0)
    )
)

legend_b <- cowplot::get_legend(
  p_lda_b +
    theme(
      legend.position = "bottom",
      legend.box.margin = margin(0, 0, 0, 0)
    )
)

p_lda_a_main <- p_lda_a + theme(legend.position = "none")
p_lda_b_main <- p_lda_b + theme(legend.position = "none")

aligned_plots <- cowplot::align_plots(
  p_lda_a_main,
  p_lda_b_main,
  align = "h",
  axis = "tb"
)

fig_s6_main <- cowplot::plot_grid(
  aligned_plots[[1]], aligned_plots[[2]],
  ncol = 2,
  rel_widths = c(1, 1),
  labels = c("a", "b"),
  label_fontface = "bold",
  label_size = 16,
  align = "h",
  axis = "tb"
)

legend_row <- cowplot::plot_grid(
  legend_a, legend_b,
  ncol = 2,
  rel_widths = c(1, 1)
)

fig_s6 <- cowplot::plot_grid(
  fig_s6_main, legend_row,
  ncol = 1,
  rel_heights = c(1, 0.16)
)

print(fig_s6)

ggsave(
  "Figure_LDA_syndromes.pdf",
  fig_s6,
  width = 250,
  height = 170,
  units = "mm"
)

write.csv(
  lda_loadings,
  "Table_LDA_loadings.csv",
  row.names = FALSE
)