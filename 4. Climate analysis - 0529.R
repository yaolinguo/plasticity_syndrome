library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(cowplot)
library(pheatmap)
library(ggplotify)
library(ranger)
library(openxlsx)
library(scales)
library(grid)

# ----------------------------
# Data preparing
# ----------------------------
setwd("/Users/yaolin/Desktop/My papers/My manuscripts/2025 - Guo - Plasticity syndrome/Submission - Journal of Ecology - 0328 - R2/Data & Code")
data <- read.csv("Data_imp_PI.csv", check.names = FALSE)
data <- as.data.frame(data)

if (!file.exists("Climate.csv")) {
  
  cat("Climate.csv not found. Generating it from WorldClim now...\n\n")
  
  required_climate_pkgs <- c("geodata", "terra")
  missing_climate_pkgs  <- setdiff(
    required_climate_pkgs,
    rownames(installed.packages())
  )
  if (length(missing_climate_pkgs) > 0) {
    stop(
      "Install missing packages first: ",
      paste0(
        "install.packages(c('",
        paste(missing_climate_pkgs, collapse = "', '"),
        "'))"
      )
    )
  }
  library(geodata)
  library(terra)
  
  coords <- data %>%
    dplyr::distinct(Sample_ID, Longitude, Latitude) %>%
    dplyr::filter(!is.na(Longitude), !is.na(Latitude))
  
  cat("Unique populations with coordinates:", nrow(coords), "\n")
  
  worldclim_cache <- "worldclim_cache"
  dir.create(worldclim_cache, showWarnings = FALSE, recursive = TRUE)
  
  cat("Loading WorldClim bioclim layers (res = 10 minutes)...\n")
  cat("(First run downloads ~340 MB; later runs reuse the cache.)\n")
  
  wc <- geodata::worldclim_global(
    var  = "bio",
    res  = 10,
    path = worldclim_cache
  )
  
  names(wc) <- gsub("^wc2\\.1_[0-9.]+m_", "", names(wc))
  
  cat("Extracting bioclim values for", nrow(coords), "populations...\n")
  clim_values <- terra::extract(
    wc,
    as.matrix(coords[, c("Longitude", "Latitude")])
  )
  if ("ID" %in% names(clim_values)) clim_values$ID <- NULL
  
  climate_full <- dplyr::bind_cols(
    coords %>% dplyr::select(Sample_ID),
    clim_values
  )
  
  write.csv(climate_full, "Climate_all_bioclim.csv", row.names = FALSE)
  climate_out <- climate_full[, c("Sample_ID", "bio_1", "bio_4", "bio_12", "bio_15")]
  write.csv(climate_out, "Climate.csv", row.names = FALSE)
  
  cat("Saved Climate.csv (", nrow(climate_out), " rows).\n", sep = "")
  cat("Saved Climate_all_bioclim.csv (", nrow(climate_full),
      " rows, all 19 bio vars).\n", sep = "")
  
  rm(coords, wc, clim_values, climate_full, climate_out,
     worldclim_cache, required_climate_pkgs, missing_climate_pkgs)
  
} else {
  cat("Climate.csv already exists -- skipping WorldClim preparation.\n")
}

climate <- read.csv("Climate.csv", check.names = FALSE)
climate <- as.data.frame(climate)
climate$Sample_ID <- as.character(climate$Sample_ID)

data$Sample_ID <- as.character(data$Sample_ID)
data <- data %>%
  dplyr::left_join(climate, by = "Sample_ID")

if ("Continent" %in% names(data) && !"Region" %in% names(data)) {
  data <- data %>% dplyr::rename(Region = Continent)
}

required_basic_cols <- c("Sample_ID", "Latitude", "Longitude")
missing_basic_cols <- setdiff(required_basic_cols, names(data))
if (length(missing_basic_cols) > 0) {
  stop("Missing required columns in Data_imp_PI.csv: ",
       paste(missing_basic_cols, collapse = ", "))
}

# ----------------------------
# 2. Trait columns and labels
# ----------------------------
trait_vars <- c(
  "Leaf_thickness_PI",
  "Leaf_length_PI",
  "Leaf_width_PI",
  "Leaf_area_PI",
  "SLA_PI",
  "Leaf_saturated_fresh_weight_PI",
  "Leaf_dry_weight_PI",
  "LDMC_PI",
  "Aboveground_biomass_PI",
  "Belowground_biomass_PI",
  "Shoot_height_PI",
  "Plant_number_PI",
  "Shoot_diameter_PI",
  "Leaf_C_PI",
  "Leaf_N_PI",
  "Leaf_CN_PI",
  "Root_C_PI",
  "Root_N_PI",
  "Root_CN_PI",
  "SPAD_PI"
)

trait_labels <- c(
  "Leaf_thickness_PI"              = "Leaf thickness PI",
  "Leaf_length_PI"                 = "Leaf length PI",
  "Leaf_width_PI"                  = "Leaf width PI",
  "Leaf_area_PI"                   = "Leaf area PI",
  "SLA_PI"                         = "SLA PI",
  "Leaf_saturated_fresh_weight_PI" = "Leaf saturated mass PI",
  "Leaf_dry_weight_PI"             = "Leaf dry mass PI",
  "LDMC_PI"                        = "LDMC PI",
  "Aboveground_biomass_PI"         = "Aboveground biomass PI",
  "Belowground_biomass_PI"         = "Belowground biomass PI",
  "Shoot_height_PI"                = "Shoot height PI",
  "Plant_number_PI"                = "Shoot number PI",
  "Shoot_diameter_PI"              = "Shoot diameter PI",
  "Leaf_C_PI"                      = "Leaf C PI",
  "Leaf_N_PI"                      = "Leaf N PI",
  "Leaf_CN_PI"                     = "Leaf C:N PI",
  "Root_C_PI"                      = "Root C PI",
  "Root_N_PI"                      = "Root N PI",
  "Root_CN_PI"                     = "Root C:N PI",
  "SPAD_PI"                        = "Chlorophyll PI"
)

my_region_colors <- c(
  "America" = "#55B7E6",
  "Asia"    = "#193E8F",
  "Europe"  = "#E53528",
  "Oceania" = "#F09739",
  "All"     = "grey40"
)

# ----------------------------
# 3. Select 4 climate variables a priori
# ----------------------------
selected_climate <- c(
  "bio_1",
  "bio_4",
  "bio_12",
  "bio_15"
)

climate_labels <- c(
  "bio_1"  = "BIO1 Annual mean temperature",
  "bio_4"  = "BIO4 Temperature seasonality",
  "bio_12" = "BIO12 Annual precipitation",
  "bio_15" = "BIO15 Precipitation seasonality"
)

climate_short_labels <- c(
  "bio_1"  = "BIO1",
  "bio_4"  = "BIO4",
  "bio_12" = "BIO12",
  "bio_15" = "BIO15"
)

climate_group_map <- c(
  "bio_1"  = "Mean temperature",
  "bio_4"  = "Temp variability",
  "bio_12" = "Mean precipitation",
  "bio_15" = "Precip variability"
)

climate_group_colors <- c(
  "Mean temperature"   = "#F8766D",
  "Temp variability"   = "#7CAE00",
  "Mean precipitation" = "#00BFC4",
  "Precip variability" = "#C77CFF",
  "Other"              = "grey70"
)

# ----------------------------
# 4. Settings
# ----------------------------
rf_repeats <- 100
rf_num_trees <- 1000
rf_seed_base <- 20260411
rf_mtry <- max(2, floor(sqrt(length(selected_climate))))
rf_min_node_size <- 5
min_complete_n <- 20
q_threshold <- 0.10
n_top_predictors_for_regression <- 1

# ----------------------------
# 5. Prepare analysis dataset
# ----------------------------
missing_traits <- setdiff(trait_vars, names(data))
missing_climate <- setdiff(selected_climate, names(data))

if (length(missing_traits) > 0) {
  stop("Missing PI trait columns: ", paste(missing_traits, collapse = ", "))
}
if (length(missing_climate) > 0) {
  stop("Missing selected climate columns: ", paste(missing_climate, collapse = ", "))
}

data$Sample_ID <- as.character(data$Sample_ID)

if ("Region" %in% names(data)) {
  data$Region <- factor(data$Region, levels = c("America", "Asia", "Europe", "Oceania"))
} else {
  data$Region <- factor("All")
}

analysis_data <- data %>%
  dplyr::select(
    any_of(c("Sample_ID", "Latitude", "Longitude", "Region", "Group", "Lineage")),
    all_of(trait_vars),
    all_of(selected_climate)
  ) %>%
  mutate(
    across(all_of(c(trait_vars, selected_climate)), ~ suppressWarnings(as.numeric(.x)))
  )

cat("Analysis dataset rows:", nrow(analysis_data), "\n")
cat("Selected climate variables:", paste(selected_climate, collapse = ", "), "\n")

# ----------------------------
# 6. Helper functions
# ----------------------------
format_p <- function(p) {
  ifelse(
    is.na(p),
    NA_character_,
    ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  )
}

make_star_q <- function(q) {
  dplyr::case_when(
    is.na(q)  ~ "",
    q < 0.001 ~ "***",
    q < 0.01  ~ "**",
    q < 0.10  ~ "*",
    TRUE      ~ ""
  )
}

safe_cor_test <- function(x, y) {
  ok <- complete.cases(x, y)
  x_ok <- x[ok]
  y_ok <- y[ok]
  
  if (length(x_ok) < 3 || stats::sd(x_ok) == 0 || stats::sd(y_ok) == 0) {
    return(list(estimate = NA_real_, p.value = NA_real_, n = length(x_ok)))
  }
  
  out <- suppressWarnings(cor.test(x_ok, y_ok, method = "pearson"))
  
  list(
    estimate = unname(out$estimate),
    p.value  = out$p.value,
    n        = length(x_ok)
  )
}

run_repeated_rf <- function(df, response, predictors,
                            n_repeats = 100,
                            num_trees = 1000,
                            mtry = 2,
                            min_node_size = 5,
                            seed_base = 1) {
  dat <- df %>%
    dplyr::select(all_of(c(response, predictors))) %>%
    dplyr::filter(complete.cases(.))
  
  if (nrow(dat) < min_complete_n || stats::sd(dat[[response]]) == 0) {
    trait_summary <- tibble(
      Trait = response,
      Trait_label = unname(trait_labels[response]),
      n = nrow(dat),
      mean_oob_r2 = NA_real_,
      sd_oob_r2 = NA_real_,
      median_oob_r2 = NA_real_,
      prop_positive_oob_r2 = NA_real_
    )
    
    importance_summary <- tibble(
      Trait = response,
      Trait_label = unname(trait_labels[response]),
      Climate = predictors,
      Climate_label = unname(climate_labels[predictors]),
      mean_importance = NA_real_,
      sd_importance = NA_real_,
      median_importance = NA_real_
    )
    
    return(list(
      trait_summary = trait_summary,
      importance_summary = importance_summary
    ))
  }
  
  var_y <- stats::var(dat[[response]], na.rm = TRUE)
  oob_r2_vec <- rep(NA_real_, n_repeats)
  importance_mat <- matrix(NA_real_, nrow = n_repeats, ncol = length(predictors))
  colnames(importance_mat) <- predictors
  
  for (i in seq_len(n_repeats)) {
    current_seed <- seed_base + i
    set.seed(current_seed)
    
    rf_fit <- ranger(
      formula = reformulate(predictors, response = response),
      data = dat,
      num.trees = num_trees,
      mtry = mtry,
      min.node.size = min_node_size,
      importance = "permutation",
      seed = current_seed
    )
    
    oob_mse <- rf_fit$prediction.error
    oob_r2 <- ifelse(is.na(var_y) || var_y == 0, NA_real_, 1 - (oob_mse / var_y))
    oob_r2_vec[i] <- oob_r2
    
    this_imp <- rf_fit$variable.importance
    importance_mat[i, names(this_imp)] <- this_imp
  }
  
  trait_summary <- tibble(
    Trait = response,
    Trait_label = unname(trait_labels[response]),
    n = nrow(dat),
    mean_oob_r2 = mean(oob_r2_vec, na.rm = TRUE),
    sd_oob_r2 = stats::sd(oob_r2_vec, na.rm = TRUE),
    median_oob_r2 = stats::median(oob_r2_vec, na.rm = TRUE),
    prop_positive_oob_r2 = mean(oob_r2_vec > 0, na.rm = TRUE)
  )
  
  importance_summary <- tibble(
    Trait = response,
    Trait_label = unname(trait_labels[response]),
    Climate = predictors,
    Climate_label = unname(climate_labels[predictors]),
    mean_importance = colMeans(importance_mat, na.rm = TRUE),
    sd_importance = apply(importance_mat, 2, stats::sd, na.rm = TRUE),
    median_importance = apply(importance_mat, 2, stats::median, na.rm = TRUE)
  )
  
  return(list(
    trait_summary = trait_summary,
    importance_summary = importance_summary
  ))
}

safe_lm <- function(df, response, predictor) {
  dat <- df %>%
    dplyr::select(any_of(c(response, predictor, "Region"))) %>%
    dplyr::filter(complete.cases(.))
  
  if (nrow(dat) < min_complete_n ||
      stats::sd(dat[[response]]) == 0 ||
      stats::sd(dat[[predictor]]) == 0) {
    return(
      tibble(
        Trait = response,
        Trait_label = unname(trait_labels[response]),
        Climate = predictor,
        Climate_label = unname(climate_labels[predictor]),
        n = nrow(dat),
        beta_raw = NA_real_,
        beta_std = NA_real_,
        se = NA_real_,
        t = NA_real_,
        p = NA_real_,
        p_label = NA_character_,
        r2 = NA_real_
      )
    )
  }
  
  mod_raw <- lm(reformulate(predictor, response = response), data = dat)
  sm_raw <- summary(mod_raw)
  co_raw <- sm_raw$coefficients[predictor, ]
  
  dat_std <- dat %>%
    mutate(
      y_std = as.numeric(scale(.data[[response]])),
      x_std = as.numeric(scale(.data[[predictor]]))
    )
  mod_std <- lm(y_std ~ x_std, data = dat_std)
  beta_std <- coef(mod_std)["x_std"]
  
  tibble(
    Trait = response,
    Trait_label = unname(trait_labels[response]),
    Climate = predictor,
    Climate_label = unname(climate_labels[predictor]),
    n = nrow(dat),
    beta_raw = unname(co_raw["Estimate"]),
    beta_std = unname(beta_std),
    se = unname(co_raw["Std. Error"]),
    t = unname(co_raw["t value"]),
    p = unname(co_raw["Pr(>|t|)"]),
    p_label = format_p(unname(co_raw["Pr(>|t|)"])),
    r2 = sm_raw$r.squared
  )
}

make_reg_parse_label <- function(r2_val, p_val) {
  r2_txt <- sprintf("%.3f", r2_val)
  
  if (is.na(p_val)) {
    return(sprintf(
      "atop(italic(R)^2 == '%s', italic(p) == 'NA')",
      r2_txt
    ))
  }
  
  if (p_val < 0.001) {
    return(sprintf(
      "atop(italic(R)^2 == '%s', italic(p) < '0.001')",
      r2_txt
    ))
  }
  
  p_txt <- sprintf("%.3f", p_val)
  sprintf(
    "atop(italic(R)^2 == '%s', italic(p) == '%s')",
    r2_txt,
    p_txt
  )
}

plot_regression_panel <- function(df, response, predictor) {
  dat <- df %>%
    dplyr::select(any_of(c(response, predictor, "Region"))) %>%
    dplyr::filter(complete.cases(.))
  
  if (nrow(dat) < min_complete_n ||
      stats::sd(dat[[response]]) == 0 ||
      stats::sd(dat[[predictor]]) == 0) {
    return(
      ggplot() +
        annotate(
          "text", x = 0.5, y = 0.5,
          label = paste(unname(trait_labels[response]),
                        "\n",
                        unname(climate_labels[predictor]),
                        "\nNot enough data"),
          size = 10 / .pt
        ) +
        theme_void()
    )
  }
  
  mod <- lm(reformulate(predictor, response = response), data = dat)
  sm <- summary(mod)
  p_val <- sm$coefficients[predictor, "Pr(>|t|)"]
  r2_val <- sm$r.squared
  
  ann_label <- make_reg_parse_label(r2_val, p_val)
  
  ggplot(dat, aes(x = .data[[predictor]], y = .data[[response]])) +
    geom_point(aes(color = Region), size = 2.5, alpha = 0.75) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      color = "grey35",
      fill = "grey70",
      alpha = 0.25,
      linewidth = 1
    ) +
    scale_color_manual(values = my_region_colors, drop = FALSE) +
    theme_classic(base_size = 10) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.75),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 10),
      legend.position = "none",
      plot.margin = margin(4, 4, 4, 4),
      aspect.ratio = 0.92
    ) +
    labs(
      x = unname(climate_labels[predictor]),
      y = unname(trait_labels[response])
    ) +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = ann_label,
      parse = TRUE,
      hjust = 1.03, vjust = 1.18,
      size = 10 / .pt
    )
}

# ----------------------------
# 7. Figure a
# ----------------------------
pearson_long <- purrr::map_dfr(trait_vars, function(tr) {
  purrr::map_dfr(selected_climate, function(cl) {
    tmp <- safe_cor_test(analysis_data[[tr]], analysis_data[[cl]])
    tibble(
      Trait = tr,
      Trait_label = unname(trait_labels[tr]),
      Climate = cl,
      Climate_label = unname(climate_labels[cl]),
      n = tmp$n,
      r = tmp$estimate,
      p = tmp$p.value,
      p_label = format_p(tmp$p.value)
    )
  })
})

pearson_long_adj <- pearson_long %>%
  group_by(Trait, Trait_label) %>%
  mutate(
    p_bh_within_trait = p.adjust(p, method = "BH"),
    sig_bh_within_trait = p_bh_within_trait < q_threshold,
    q_label = format_p(p_bh_within_trait),
    star = make_star_q(p_bh_within_trait)
  ) %>%
  ungroup()

write.csv(
  pearson_long_adj,
  "Table_Pearson_PI_climate.csv",
  row.names = FALSE
)

pearson_cor_mat <- pearson_long_adj %>%
  dplyr::select(Trait_label, Climate, r) %>%
  tidyr::pivot_wider(names_from = Climate, values_from = r) %>%
  as.data.frame()

rownames(pearson_cor_mat) <- pearson_cor_mat$Trait_label
pearson_cor_mat$Trait_label <- NULL
pearson_cor_mat <- as.matrix(pearson_cor_mat)

pearson_lab_mat <- pearson_long_adj %>%
  mutate(label = ifelse(is.na(r), "", paste0(sprintf("%.2f", r), star))) %>%
  dplyr::select(Trait_label, Climate, label) %>%
  tidyr::pivot_wider(names_from = Climate, values_from = label) %>%
  as.data.frame()

rownames(pearson_lab_mat) <- pearson_lab_mat$Trait_label
pearson_lab_mat$Trait_label <- NULL
pearson_lab_mat <- as.matrix(pearson_lab_mat)

colnames(pearson_cor_mat) <- unname(climate_short_labels[colnames(pearson_cor_mat)])
colnames(pearson_lab_mat) <- unname(climate_short_labels[colnames(pearson_lab_mat)])

ph_pearson <- pheatmap(
  pearson_cor_mat,
  cluster_rows = TRUE,
  cluster_cols = FALSE,
  display_numbers = pearson_lab_mat,
  number_color = "black",
  color = colorRampPalette(c("blue", "white", "red"))(100),
  breaks = seq(-1, 1, length.out = 101),
  na_col = "grey90",
  legend = TRUE,
  main = NA,
  fontsize = 10,
  fontsize_number = 10,
  angle_col = 0,
  silent = TRUE
)

p_pearson <- ggplotify::as.ggplot(ph_pearson$gtable)

# ----------------------------
# 8. Define focal traits from trait-wise BH-FDR
# ----------------------------
focal_traits <- pearson_long_adj %>%
  group_by(Trait, Trait_label) %>%
  summarise(
    n_sig_bh_trait = sum(sig_bh_within_trait, na.rm = TRUE),
    min_q_bh_trait = min(p_bh_within_trait, na.rm = TRUE),
    max_abs_r = max(abs(r), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    min_q_bh_trait = ifelse(is.infinite(min_q_bh_trait), NA_real_, min_q_bh_trait)
  ) %>%
  filter(n_sig_bh_trait >= 1) %>%
  arrange(desc(n_sig_bh_trait), min_q_bh_trait, desc(max_abs_r))

if (nrow(focal_traits) == 0) {
  stop("No focal traits selected: none had trait-wise BH-FDR q < 0.10.")
}

write.csv(
  focal_traits,
  "Table_focal_traits.csv",
  row.names = FALSE
)

# ----------------------------
# 9. Run RF only for focal traits
# ----------------------------
rf_results <- purrr::map(focal_traits$Trait, function(tr) {
  run_repeated_rf(
    df = analysis_data,
    response = tr,
    predictors = selected_climate,
    n_repeats = rf_repeats,
    num_trees = rf_num_trees,
    mtry = rf_mtry,
    min_node_size = rf_min_node_size,
    seed_base = rf_seed_base
  )
})

rf_trait_performance <- purrr::map_dfr(rf_results, "trait_summary") %>%
  left_join(focal_traits, by = c("Trait", "Trait_label")) %>%
  arrange(desc(n_sig_bh_trait), min_q_bh_trait, desc(mean_oob_r2))

rf_importance_long <- purrr::map_dfr(rf_results, "importance_summary") %>%
  left_join(focal_traits, by = c("Trait", "Trait_label")) %>%
  group_by(Trait) %>%
  arrange(desc(mean_importance), .by_group = TRUE) %>%
  mutate(rank_within_trait = row_number()) %>%
  ungroup()

write.csv(
  rf_trait_performance,
  "Table_RF_trait_performance.csv",
  row.names = FALSE
)

write.csv(
  rf_importance_long,
  "Table_RF_importance.csv",
  row.names = FALSE
)

# ----------------------------
# 10. Figure b
# ----------------------------
rf_relative_importance <- rf_importance_long %>%
  mutate(
    importance_pos = pmax(mean_importance, 0)
  ) %>%
  group_by(Trait, Trait_label) %>%
  mutate(
    importance_sum = sum(importance_pos, na.rm = TRUE),
    relative_influence = ifelse(
      is.na(importance_sum) | importance_sum <= 0,
      NA_real_,
      100 * importance_pos / importance_sum
    )
  ) %>%
  ungroup() %>%
  mutate(
    Climate_group = unname(climate_group_map[Climate]),
    Climate_group = ifelse(is.na(Climate_group), "Other", Climate_group),
    Climate_short = unname(climate_short_labels[Climate]),
    Trait_label = factor(Trait_label, levels = focal_traits$Trait_label)
  ) %>%
  group_by(Trait, Trait_label) %>%
  arrange(desc(relative_influence), .by_group = TRUE) %>%
  mutate(
    Climate_ordered = paste0(Climate_short, "___", Trait_label)
  ) %>%
  ungroup()

rf_relative_importance$Climate_ordered <- factor(
  rf_relative_importance$Climate_ordered,
  levels = unique(rf_relative_importance$Climate_ordered)
)

write.csv(
  rf_relative_importance,
  "Table_RF_relative_importance.csv",
  row.names = FALSE
)

p_rf_relative <- ggplot(
  rf_relative_importance,
  aes(x = Climate_ordered, y = relative_influence)
) +
  geom_col(
    width = 0.82,
    color = "black",
    fill = NA,
    linewidth = 0.4
  ) +
  facet_wrap(~Trait_label, ncol = 2, scales = "free") +
  scale_x_discrete(labels = function(x) sub("___.*$", "", x)) +
  theme_classic(base_size = 10) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none",
    panel.spacing = unit(0.8, "lines"),
    aspect.ratio = 0.92
  ) +
  labs(
    x = NULL,
    y = "Relative influence (%)"
  )

# ----------------------------
# 11. Select top predictor from Figure b
# ----------------------------
top_predictors_tbl <- rf_relative_importance %>%
  group_by(Trait, Trait_label) %>%
  arrange(desc(relative_influence), .by_group = TRUE) %>%
  slice_head(n = n_top_predictors_for_regression) %>%
  ungroup() %>%
  # Reorder traits to match Panel B's facet order (focal_traits)
  arrange(match(Trait, focal_traits$Trait))

write.csv(
  top_predictors_tbl,
  "Table_RF_top_predictors.csv",
  row.names = FALSE
)

# ----------------------------
# 12. Figure c
# ----------------------------
regression_results <- purrr::pmap_dfr(
  list(top_predictors_tbl$Trait, top_predictors_tbl$Climate),
  function(tr, cl) safe_lm(analysis_data, response = tr, predictor = cl)
)

write.csv(
  regression_results,
  "Table_regression_top_predictors.csv",
  row.names = FALSE
)

plot_specs <- regression_results %>%
  filter(!is.na(Trait), !is.na(Climate)) %>%
  dplyr::select(Trait, Climate)

if (nrow(plot_specs) == 0) {
  stop("No valid linear regression panels could be created.")
}

plot_list_reg <- purrr::map2(
  plot_specs$Trait,
  plot_specs$Climate,
  ~ plot_regression_panel(analysis_data, response = .x, predictor = .y)
)

n_panels_reg <- length(plot_list_reg)
ncol_reg <- 3
nrow_reg <- ceiling(n_panels_reg / ncol_reg)

p_regression <- plot_grid(
  plotlist = plot_list_reg,
  ncol = ncol_reg,
  align = "hv"
)

# ----------------------------
# 13. Combine a + b + c into one figure
# ----------------------------
panel_a <- ggdraw() +
  draw_plot(p_pearson, x = 0, y = 0, width = 1, height = 1) +
  draw_label(
    "a",
    x = 0.01, y = 0.99,
    hjust = 0, vjust = 1,
    fontface = "bold",
    size = 10
  )

panel_b <- ggdraw() +
  draw_plot(p_rf_relative, x = 0, y = 0, width = 1, height = 1) +
  draw_label(
    "b",
    x = 0.01, y = 0.99,
    hjust = 0, vjust = 1,
    fontface = "bold",
    size = 10
  )

panel_c <- ggdraw() +
  draw_plot(p_regression, x = 0.08, y = 0.03, width = 0.84, height = 0.94) +
  draw_label(
    "c",
    x = 0.01, y = 0.99,
    hjust = 0, vjust = 1,
    fontface = "bold",
    size = 10
  )

top_row <- plot_grid(
  panel_a,
  panel_b,
  ncol = 2,
  rel_widths = c(0.98, 1.02),
  align = "h"
)

final_combined_figure <- plot_grid(
  top_row,
  panel_c,
  ncol = 1,
  rel_heights = c(1.06, 0.84)
)

print(final_combined_figure)

ggsave(
  "Figure 3.pdf",
  final_combined_figure,
  width = 280,
  height = 330,
  units = "mm",
  limitsize = FALSE
)

# ----------------------------
# 14. Export results to Excel
# ----------------------------
wb <- createWorkbook()

addWorksheet(wb, "selected_climate")
writeData(
  wb,
  "selected_climate",
  data.frame(
    Climate = selected_climate,
    Climate_label = unname(climate_labels[selected_climate])
  )
)

addWorksheet(wb, "pearson_long_traitBH")
writeData(wb, "pearson_long_traitBH", pearson_long_adj)

addWorksheet(wb, "focal_traits")
writeData(wb, "focal_traits", focal_traits)

addWorksheet(wb, "rf_trait_performance")
writeData(wb, "rf_trait_performance", rf_trait_performance)

addWorksheet(wb, "rf_importance_long")
writeData(wb, "rf_importance_long", rf_importance_long)

addWorksheet(wb, "rf_relative_importance")
writeData(wb, "rf_relative_importance", rf_relative_importance)

addWorksheet(wb, "top_predictors")
writeData(wb, "top_predictors", top_predictors_tbl)

addWorksheet(wb, "linear_regression")
writeData(wb, "linear_regression", regression_results)

saveWorkbook(
  wb,
  "PI_climate_results.xlsx",
  overwrite = TRUE
)