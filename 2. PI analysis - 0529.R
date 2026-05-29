library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(cowplot)
library(purrr)
library(tidyr)
library(writexl)
library(openxlsx)
library(scales)
library(agricolae)
library(tibble)

setwd("/Users/yaolin/Desktop/My papers/My manuscripts/2025 - Guo - Plasticity syndrome/Submission - Journal of Ecology - 0328 - R2/Data & Code")

# ----------------------------
# 1. Read and prepare the input data
# ----------------------------
data <- read.csv("Data_imp_PI.csv", check.names = FALSE)
data <- as.data.frame(data)

if ("Continent" %in% names(data) && !"Region" %in% names(data)) {
  data <- data %>% dplyr::rename(Region = Continent)
}

data <- data %>%
  dplyr::mutate(
    Region  = factor(Region, levels = c("America", "Asia", "Europe", "Oceania")),
    Lineage = factor(Lineage)
  )

# ----------------------------
# 2. Define trait variables, labels, and plotting colors
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

trait_labels_table <- c(
  "Leaf_thickness_PI"              = "Leaf thickness",
  "Leaf_length_PI"                 = "Leaf length",
  "Leaf_width_PI"                  = "Leaf width",
  "Leaf_area_PI"                   = "Leaf area",
  "SLA_PI"                         = "SLA",
  "Leaf_saturated_fresh_weight_PI" = "Leaf saturated mass",
  "Leaf_dry_weight_PI"             = "Leaf dry mass",
  "LDMC_PI"                        = "LDMC",
  "Aboveground_biomass_PI"         = "Aboveground biomass",
  "Belowground_biomass_PI"         = "Belowground biomass",
  "Shoot_height_PI"                = "Shoot height",
  "Plant_number_PI"                = "Shoot number",
  "Shoot_diameter_PI"              = "Shoot diameter",
  "Leaf_C_PI"                      = "Leaf C",
  "Leaf_N_PI"                      = "Leaf N",
  "Leaf_CN_PI"                     = "Leaf C:N",
  "Root_C_PI"                      = "Root C",
  "Root_N_PI"                      = "Root N",
  "Root_CN_PI"                     = "Root C:N",
  "SPAD_PI"                        = "Chlorophyll"
)

# Region colors used consistently across figures
my_colors <- c(
  "America" = "#55B7E6",
  "Asia"    = "#193E8F",
  "Europe"  = "#E53528",
  "Oceania" = "#F09739"
)

# ----------------------------
# 3. Helper functions
# ----------------------------
safe_qlogis <- function(x, eps = 1e-4) {
  out <- ifelse(
    is.na(x),
    NA_real_,
    qlogis(pmin(pmax(x, eps), 1 - eps))
  )
  out
}

format_p <- function(p) {
  ifelse(is.na(p), NA_character_,
         ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

fmt_num <- function(x) {
  ifelse(is.na(x), NA_character_, sprintf("%.3f", x))
}

extract_term <- function(anv_df, term_name) {
  if (!term_name %in% rownames(anv_df)) {
    return(c(chisq = NA_real_, df = NA_real_, p = NA_real_))
  }
  c(
    chisq = anv_df[term_name, "Chisq"],
    df    = anv_df[term_name, "Df"],
    p     = anv_df[term_name, "Pr(>Chisq)"]
  )
}

make_stats_label <- function(r2, p) {
  r2_txt <- sprintf("%.3f", r2)
  
  if (is.na(p)) {
    return(
      paste0("paste(italic(R)^2, ' = ", r2_txt, ", ', italic(p), ' = NA')")
    )
  }
  
  if (p < 0.001) {
    return(
      paste0("paste(italic(R)^2, ' = ", r2_txt, ", ', italic(p), ' < 0.001')")
    )
  }
  
  paste0(
    "paste(italic(R)^2, ' = ", r2_txt, ", ', italic(p), ' = ", sprintf('%.3f', p), "')"
  )
}

# Write a formatted LMM table
write_LMM_table_xlsx <- function(table_raw, title_text, file_path,
                                 sheet_name = "Table") {
  
  wb <- createWorkbook()
  addWorksheet(wb, sheet_name)
  
  writeData(wb, sheet_name, title_text, startRow = 1, startCol = 1, colNames = FALSE)
  mergeCells(wb, sheet_name, cols = 1:10, rows = 1)
  
  writeData(wb, sheet_name, "Independent\nvariables", startRow = 3, startCol = 1, colNames = FALSE)
  writeData(wb, sheet_name, "Latitude",          startRow = 3, startCol = 2, colNames = FALSE)
  writeData(wb, sheet_name, "Region",            startRow = 3, startCol = 5, colNames = FALSE)
  writeData(wb, sheet_name, "Latitude × Region", startRow = 3, startCol = 8, colNames = FALSE)
  
  mergeCells(wb, sheet_name, cols = 1,    rows = 3:4)
  mergeCells(wb, sheet_name, cols = 2:4,  rows = 3)
  mergeCells(wb, sheet_name, cols = 5:7,  rows = 3)
  mergeCells(wb, sheet_name, cols = 8:10, rows = 3)
  
  sub_header <- c("χ²", "df", "P", "χ²", "df", "P", "χ²", "df", "P")
  writeData(wb, sheet_name, sub_header, startRow = 4, startCol = 2, colNames = FALSE)
  
  table_body <- table_raw %>%
    dplyr::select(
      Independent_variables,
      Latitude_chisq, Latitude_df, Latitude_p,
      Region_chisq, Region_df, Region_p,
      Interaction_chisq, Interaction_df, Interaction_p
    )
  writeData(wb, sheet_name, table_body, startRow = 5, startCol = 1, colNames = FALSE)
  
  title_style <- createStyle(
    textDecoration = "bold", fontSize = 11,
    halign = "left", valign = "center", wrapText = TRUE
  )
  header_style <- createStyle(
    textDecoration = "bold", halign = "center", valign = "center",
    border = "TopBottom", fontSize = 11, wrapText = TRUE
  )
  subheader_style <- createStyle(
    textDecoration = "bold", halign = "center", valign = "center",
    border = "Bottom", fontSize = 11
  )
  body_left_style   <- createStyle(halign = "left",   valign = "center", fontSize = 11)
  body_center_style <- createStyle(halign = "center", valign = "center", fontSize = 11)
  sig_style         <- createStyle(textDecoration = "bold",
                                   halign = "center", valign = "center", fontSize = 11)
  
  addStyle(wb, sheet_name, title_style,     rows = 1, cols = 1,    gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, header_style,    rows = 3, cols = 1:10, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, subheader_style, rows = 4, cols = 2:10, gridExpand = TRUE, stack = TRUE)
  
  addStyle(wb, sheet_name, body_left_style,
           rows = 5:(4 + nrow(table_raw)), cols = 1,
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, body_center_style,
           rows = 5:(4 + nrow(table_raw)), cols = 2:10,
           gridExpand = TRUE, stack = TRUE)
  
  for (i in seq_len(nrow(table_raw))) {
    excel_row <- i + 4
    if (!is.na(table_raw$Latitude_p_num[i]) && table_raw$Latitude_p_num[i] < 0.05) {
      addStyle(wb, sheet_name, sig_style, rows = excel_row, cols = 2:4,
               gridExpand = TRUE, stack = TRUE)
    }
    if (!is.na(table_raw$Region_p_num[i]) && table_raw$Region_p_num[i] < 0.05) {
      addStyle(wb, sheet_name, sig_style, rows = excel_row, cols = 5:7,
               gridExpand = TRUE, stack = TRUE)
    }
    if (!is.na(table_raw$Interaction_p_num[i]) && table_raw$Interaction_p_num[i] < 0.05) {
      addStyle(wb, sheet_name, sig_style, rows = excel_row, cols = 8:10,
               gridExpand = TRUE, stack = TRUE)
    }
  }
  
  setColWidths(wb, sheet_name, cols = 1,    widths = 24)
  setColWidths(wb, sheet_name, cols = 2:10, widths = 10)
  setRowHeights(wb, sheet_name, rows = 1,   heights = 35)
  setRowHeights(wb, sheet_name, rows = 3:4, heights = 22)
  
  freezePane(wb, sheet_name, firstActiveRow = 5, firstActiveCol = 2)
  saveWorkbook(wb, file_path, overwrite = TRUE)
}

# ----------------------------
# 4. Table 1
# ----------------------------
fit_mixed_model_table <- function(var, df = data) {
  dat <- df %>%
    dplyr::select(Sample_ID, Latitude_ab, Region, Lineage, all_of(var)) %>%
    dplyr::filter(
      !is.na(.data[[var]]),
      !is.na(Latitude_ab),
      !is.na(Region),
      !is.na(Lineage)
    ) %>%
    dplyr::mutate(y = safe_qlogis(.data[[var]])) %>%
    dplyr::filter(!is.na(y)) %>%
    droplevels()
  
  mod <- lmer(y ~ Latitude_ab * Region + (1 | Lineage), data = dat, REML = FALSE)
  anv <- car::Anova(mod, type = 2, test.statistic = "Chisq")
  anv <- as.data.frame(anv)
  
  lat <- extract_term(anv, "Latitude_ab")
  reg <- extract_term(anv, "Region")
  int <- extract_term(anv, "Latitude_ab:Region")
  
  tibble::tibble(
    Independent_variables = trait_labels_table[[var]],
    
    Latitude_chisq = fmt_num(lat["chisq"]),
    Latitude_df    = fmt_num(lat["df"]),
    Latitude_p     = format_p(lat["p"]),
    
    Region_chisq   = fmt_num(reg["chisq"]),
    Region_df      = fmt_num(reg["df"]),
    Region_p       = format_p(reg["p"]),
    
    Interaction_chisq = fmt_num(int["chisq"]),
    Interaction_df    = fmt_num(int["df"]),
    Interaction_p     = format_p(int["p"]),
    
    Latitude_p_num    = as.numeric(lat["p"]),
    Region_p_num      = as.numeric(reg["p"]),
    Interaction_p_num = as.numeric(int["p"])
  )
}

# Run the mixed model for all PIs
table_PI_raw <- purrr::map_dfr(trait_vars, fit_mixed_model_table)

Table_PI <- table_PI_raw %>%
  dplyr::select(
    Independent_variables,
    Latitude_chisq, Latitude_df, Latitude_p,
    Region_chisq, Region_df, Region_p,
    Interaction_chisq, Interaction_df, Interaction_p
  )

print(Table_PI)

# ----------------------------
# 5. Export Table PI LMM
# ----------------------------
write_LMM_table_xlsx(
  table_raw  = table_PI_raw,
  title_text = paste0(
    "Table PI LMM. Effects of latitude, region and their interactions on the plant ",
    "functional trait plasticity indices of Phragmites australis. ",
    "df, degrees of freedom; χ², chi-squared test statistic. ",
    "The significant effects are shown in bold."
  ),
  file_path  = "Table_PI_LMM.xlsx",
  sheet_name = "PI LMM"
)

# ----------------------------
# 6. Simple latitude regressions
# ----------------------------
region_levels <- c("Overall", "America", "Asia", "Europe", "Oceania")
region_display <- c(
  "Overall" = "Overall",
  "America" = "North America",
  "Asia"    = "Asia",
  "Europe"  = "Europe",
  "Oceania" = "Oceania"
)

fit_latitude_model <- function(var, region_name = "Overall") {
  dat <- data %>%
    dplyr::select(Latitude_ab, Region, all_of(var)) %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(Latitude_ab))
  
  if (region_name != "Overall") {
    dat <- dat %>% dplyr::filter(Region == region_name)
  }
  
  dat <- dat %>%
    dplyr::mutate(y = safe_qlogis(.data[[var]])) %>%
    dplyr::filter(!is.na(y)) %>%
    droplevels()
  
  if (nrow(dat) < 4) {
    return(
      tibble(
        Variable  = trait_labels_table[[var]],
        Continent = unname(region_display[region_name]),
        beta      = NA_character_,
        SE        = NA_character_,
        t         = NA_character_,
        p         = NA_character_,
        R2        = NA_character_,
        df        = NA_character_,
        p_num     = NA_real_
      )
    )
  }
  
  mod <- lm(y ~ Latitude_ab, data = dat)
  sm  <- summary(mod)
  co  <- sm$coefficients["Latitude_ab", ]
  
  tibble(
    Variable  = trait_labels_table[[var]],
    Continent = unname(region_display[region_name]),
    beta      = sprintf("%.3f", unname(co["Estimate"])),
    SE        = sprintf("%.3f", unname(co["Std. Error"])),
    t         = sprintf("%.3f", unname(co["t value"])),
    p         = format_p(unname(co["Pr(>|t|)"])),
    R2        = sprintf("%.3f", sm$r.squared),
    df        = as.character(as.integer(sm$df[2])),
    p_num     = as.numeric(co["Pr(>|t|)"])
  )
}

Table_latitude_PI_raw <- purrr::map_dfr(
  trait_vars,
  function(v) purrr::map_dfr(region_levels, ~ fit_latitude_model(v, .x))
)

Table_latitude_PI <- Table_latitude_PI_raw %>%
  dplyr::select(Variable, Continent, beta, SE, t, p, R2, df)

print(Table_latitude_PI)

# ----------------------------
# 7. Export formatted latitude regression table
# ----------------------------
wb_lat <- createWorkbook()
addWorksheet(wb_lat, "PI latitude")

title_text_lat <- paste0(
  "Table PI Latitude. Simple linear regression models for the relationships between latitude and Phragmites australis plasticity (PI). ",
  "β = slope estimator; SE = standard error; df = degrees of freedom; R² = goodness of fit of the model. ",
  "Significant results (p < 0.05) are shown in bold."
)

writeData(wb_lat, "PI latitude", title_text_lat, startRow = 1, startCol = 1, colNames = FALSE)
mergeCells(wb_lat, "PI latitude", cols = 1:8, rows = 1)

writeData(wb_lat, "PI latitude", "Variables", startRow = 3, startCol = 1, colNames = FALSE)
writeData(wb_lat, "PI latitude", "Continent", startRow = 3, startCol = 2, colNames = FALSE)
writeData(wb_lat, "PI latitude", "P. australis plasticity", startRow = 3, startCol = 3, colNames = FALSE)

mergeCells(wb_lat, "PI latitude", cols = 1,   rows = 3:4)
mergeCells(wb_lat, "PI latitude", cols = 2,   rows = 3:4)
mergeCells(wb_lat, "PI latitude", cols = 3:8, rows = 3)

sub_header_lat <- c("β", "SE", "t", "p", "R²", "df")
writeData(wb_lat, "PI latitude", sub_header_lat, startRow = 4, startCol = 3, colNames = FALSE)

title_style_lat <- createStyle(
  textDecoration = "bold",
  fontSize = 11,
  halign = "left",
  valign = "center",
  wrapText = TRUE
)

header_style_lat <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "TopBottom",
  fontSize = 11,
  wrapText = TRUE
)

subheader_style_lat <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom",
  fontSize = 11
)

body_var_style <- createStyle(
  halign = "left",
  valign = "center",
  fontSize = 11
)

body_continent_style <- createStyle(
  halign = "left",
  valign = "center",
  fontSize = 11
)

body_num_style <- createStyle(
  halign = "center",
  valign = "center",
  fontSize = 11
)

sig_var_style <- createStyle(
  textDecoration = "bold",
  halign = "left",
  valign = "center",
  fontSize = 11
)

sig_continent_style <- createStyle(
  textDecoration = "bold",
  halign = "left",
  valign = "center",
  fontSize = 11
)

sig_num_style <- createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  fontSize = 11
)

addStyle(wb_lat, "PI latitude", title_style_lat, rows = 1, cols = 1, gridExpand = TRUE, stack = TRUE)
addStyle(wb_lat, "PI latitude", header_style_lat, rows = 3, cols = 1:8, gridExpand = TRUE, stack = TRUE)
addStyle(wb_lat, "PI latitude", subheader_style_lat, rows = 4, cols = 3:8, gridExpand = TRUE, stack = TRUE)

start_row_lat <- 5
n_block <- length(region_levels)

for (i in seq_along(trait_vars)) {
  var_i <- trait_vars[i]
  trait_name_i <- trait_labels_table[[var_i]]
  
  block <- Table_latitude_PI_raw %>%
    dplyr::filter(Variable == trait_name_i) %>%
    dplyr::select(Continent, beta, SE, t, p, R2, df, p_num)
  
  row_start <- start_row_lat + (i - 1) * n_block
  row_end   <- row_start + n_block - 1
  
  writeData(wb_lat, "PI latitude", trait_name_i, startRow = row_start, startCol = 1, colNames = FALSE)
  mergeCells(wb_lat, "PI latitude", cols = 1, rows = row_start:row_end)
  
  writeData(
    wb_lat, "PI latitude",
    block %>% dplyr::select(Continent, beta, SE, t, p, R2, df),
    startRow = row_start, startCol = 2, colNames = FALSE
  )
  
  addStyle(wb_lat, "PI latitude", body_var_style, rows = row_start, cols = 1, stack = TRUE)
  addStyle(wb_lat, "PI latitude", body_continent_style, rows = row_start:row_end, cols = 2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb_lat, "PI latitude", body_num_style, rows = row_start:row_end, cols = 3:8, gridExpand = TRUE, stack = TRUE)
  
  if (any(!is.na(block$p_num) & block$p_num < 0.05)) {
    addStyle(wb_lat, "PI latitude", sig_var_style, rows = row_start, cols = 1, stack = TRUE)
  }
  
  sig_rows <- which(!is.na(block$p_num) & block$p_num < 0.05)
  
  if (length(sig_rows) > 0) {
    for (sr in sig_rows) {
      excel_row <- row_start + sr - 1
      addStyle(wb_lat, "PI latitude", sig_continent_style, rows = excel_row, cols = 2, stack = TRUE)
      addStyle(wb_lat, "PI latitude", sig_num_style, rows = excel_row, cols = 3:8, gridExpand = TRUE, stack = TRUE)
    }
  }
}

setColWidths(wb_lat, "PI latitude", cols = 1, widths = 22)
setColWidths(wb_lat, "PI latitude", cols = 2, widths = 18)
setColWidths(wb_lat, "PI latitude", cols = 3:8, widths = 10)
setRowHeights(wb_lat, "PI latitude", rows = 1, heights = 38)
setRowHeights(wb_lat, "PI latitude", rows = 3:4, heights = 22)
freezePane(wb_lat, "PI latitude", firstActiveRow = 5, firstActiveCol = 3)
saveWorkbook(wb_lat, "Table_PI_latitude.xlsx", overwrite = TRUE)

# ----------------------------
# 8. Figure 2 merged
# ----------------------------
plot_region_bar_panel <- function(var, y_upper) {
  dat_plot <- data %>%
    dplyr::select(Region, all_of(var)) %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(Region)) %>%
    dplyr::group_by(Region) %>%
    dplyr::summarise(
      mean_PI = mean(.data[[var]]),
      sd_PI   = sd(.data[[var]]),
      n       = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se    = sd_PI / sqrt(n),
      lower = pmax(0, mean_PI - se),
      upper = mean_PI + se
    )
  
  ggplot(dat_plot, aes(x = Region, y = mean_PI, fill = Region, color = Region)) +
    geom_col(width = 0.5, alpha = 0.75, color = NA) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      width = 0.15,
      linewidth = 1,
      alpha = 0.75
    ) +
    scale_fill_manual(values = my_colors) +
    scale_color_manual(values = my_colors) +
    scale_x_discrete(labels = c(
      "America" = "AM",
      "Asia"    = "AS",
      "Europe"  = "EU",
      "Oceania" = "OC"
    )) +
    theme_minimal() +
    theme(
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.background   = element_blank(),
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
      axis.line          = element_blank(),
      axis.ticks         = element_line(color = "black", linewidth = 0.5),
      axis.title         = element_text(size = 20),
      axis.text.y        = element_text(size = 20, color = "black"),
      axis.text.x        = element_text(size = 20, color = "black"),
      legend.position    = "none",
      plot.margin        = margin(t = 16, r = 0, b = 6, l = 10)
    ) +
    scale_y_continuous(
      limits = c(0, y_upper),
      labels = scales::number_format(accuracy = 0.01),
      expand = c(0, 0)
    ) +
    coord_cartesian(clip = "off") +
    labs(
      x = NULL,
      y = trait_labels[[var]]
    )
}

plot_lat_scatter_panel <- function(var, y_upper, show_x_label = FALSE) {
  dat <- data %>%
    dplyr::select(Latitude_ab, Region, all_of(var)) %>%
    dplyr::filter(!is.na(.data[[var]]), !is.na(Latitude_ab), !is.na(Region)) %>%
    droplevels()
  
  lmm_info <- table_PI_raw %>%
    dplyr::filter(Independent_variables == trait_labels_table[[var]])
  
  if (nrow(lmm_info) == 0) {
    lat_p <- NA_real_
    int_p <- NA_real_
  } else {
    lat_p <- lmm_info$Latitude_p_num[1]
    int_p <- lmm_info$Interaction_p_num[1]
  }
  
  draw_fit <- (!is.na(lat_p) && lat_p < 0.05) ||
    (!is.na(int_p) && int_p < 0.05)

  dat_logit <- dat %>%
    dplyr::mutate(y_logit = safe_qlogis(.data[[var]])) %>%
    dplyr::filter(!is.na(y_logit))
  mod_overall <- lm(y_logit ~ Latitude_ab, data = dat_logit)
  sm_overall  <- summary(mod_overall)
  overall_slope <- sm_overall$coefficients["Latitude_ab", "Estimate"]
  ann_label <- sprintf("paste(italic(beta), ' = %.3f')", overall_slope)
  
  x_limits <- c(
    floor(min(dat$Latitude_ab, na.rm = TRUE) / 5) * 5,
    ceiling(max(dat$Latitude_ab, na.rm = TRUE) / 5) * 5
  )
  x_range <- diff(x_limits)
  x_stats <- x_limits[2] - 0.03 * x_range
  y_stats <- y_upper - 0.04 * y_upper
  
  p <- ggplot(dat, aes(x = Latitude_ab, y = .data[[var]])) +
    geom_point(aes(color = Region), size = 3, shape = 16, alpha = 0.65) +
    scale_color_manual(values = my_colors) +
    theme_minimal() +
    theme(
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.background   = element_blank(),
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
      axis.line.x        = element_blank(),
      axis.line.y        = element_blank(),
      axis.ticks.x       = element_line(color = "black", linewidth = 0.5),
      axis.ticks.y       = element_blank(),
      axis.text.x        = element_text(size = 20, color = "black"),
      axis.text.y        = element_blank(),
      axis.title.x       = element_text(size = 20),
      axis.title.y       = element_blank(),
      legend.position    = "none",
      plot.margin        = margin(t = 16, r = 8, b = 6, l = 0)
    ) +
    xlab(if (show_x_label) "Absolute latitude (°)" else NULL) +
    scale_x_continuous(
      breaks = seq(x_limits[1], x_limits[2], by = 10),
      labels = scales::number_format(accuracy = 1)
    ) +
    scale_y_continuous(
      limits = c(0, y_upper),
      expand = c(0, 0)
    ) +
    coord_cartesian(
      xlim = x_limits,
      ylim = c(0, y_upper),
      clip = "off"
    )
  
  if (draw_fit) {
    p <- p +
      geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = TRUE,
        level = 0.95,
        color = "grey45",
        fill = "grey70",
        alpha = 0.25,
        linewidth = 1.1
      ) +
      annotate(
        "text",
        x = x_stats, y = y_stats,
        label = ann_label,
        parse = TRUE,
        hjust = 1, vjust = 1,
        size = 6
      )
  }
  
  return(p)
}

plot_combined_panel <- function(var, panel_letter, show_x_label = FALSE) {
  dat_full <- data %>% dplyr::filter(!is.na(.data[[var]]))
  ymax_raw <- max(dat_full[[var]], na.rm = TRUE)
  y_upper  <- min(1, ymax_raw + max(0.06, ymax_raw * 0.12))
  
  p_bar     <- plot_region_bar_panel(var, y_upper)
  p_scatter <- plot_lat_scatter_panel(var, y_upper, show_x_label = show_x_label)
  
  p_combined <- plot_grid(
    p_bar, p_scatter,
    ncol       = 2,
    rel_widths = c(0.50, 0.50),
    align      = "h",
    axis       = "tb"
  )
  
  ggdraw() +
    draw_plot(p_combined, 0, 0, 1, 1) +
    draw_label(
      panel_letter,
      x = 0.02, y = 0.985,
      hjust = 0, vjust = 1,
      fontface = "bold",
      size = 26
    )
}

# Build all 20 combined panels
ncol_fig2 <- 4
nrow_fig2 <- ceiling(length(trait_vars) / ncol_fig2)
panel_letters <- letters[seq_along(trait_vars)]

plot_list_fig2 <- purrr::map(
  seq_along(trait_vars),
  function(i) {
    row_id <- ceiling(i / ncol_fig2)
    show_x <- row_id == nrow_fig2
    
    plot_combined_panel(
      var = trait_vars[i],
      panel_letter = panel_letters[i],
      show_x_label = show_x
    )
  }
)

final_plot_fig2 <- plot_grid(
  plotlist = plot_list_fig2,
  ncol = ncol_fig2,
  align = "hv"
) +
  theme(plot.margin = margin(15, 15, 15, 15))

print(final_plot_fig2)

ggsave(
  "Figure_2_combined.pdf",
  final_plot_fig2,
  height = 490,
  width  = 600,
  units  = "mm",
  limitsize = FALSE
)

# ----------------------------
# 9. Sensitivity analyses
# ----------------------------
data_raw_PI <- read.csv("Data_raw_PI.csv", check.names = FALSE) %>%
  as.data.frame()

if ("Continent" %in% names(data_raw_PI) && !"Region" %in% names(data_raw_PI)) {
  data_raw_PI <- data_raw_PI %>% dplyr::rename(Region = Continent)
}

data_raw_PI <- data_raw_PI %>%
  dplyr::mutate(
    Region  = factor(Region, levels = c("America", "Asia", "Europe", "Oceania")),
    Lineage = factor(Lineage)
  )

cat("Imputed-PI dataset n =", nrow(data), "\n")
cat("Raw-PI dataset n     =", nrow(data_raw_PI), "\n")

table_PI_LMM_sens_raw <- purrr::map_dfr(
  trait_vars,
  ~ fit_mixed_model_table(.x, df = data_raw_PI)
)

print(
  table_PI_LMM_sens_raw %>%
    dplyr::select(-Latitude_p_num, -Region_p_num, -Interaction_p_num)
)

write_LMM_table_xlsx(
  table_raw  = table_PI_LMM_sens_raw,
  title_text = paste0(
    "Table PI LMM Sensitivity (Raw). Effects of latitude, region and their interactions ",
    "on Phragmites australis plasticity, refit using the raw (non-imputed) PI dataset. ",
    "df, degrees of freedom; χ², chi-squared test statistic. ",
    "The significant effects are shown in bold."
  ),
  file_path  = "Table_PI_LMM_sensitivity_raw.xlsx",
  sheet_name = "Sensitivity raw"
)

# ----------------------------
# 10. Exploratory Fisher's LSD post hoc tests
# ----------------------------
lsd_traits <- c(
  "Leaf_width_PI",
  "LDMC_PI",
  "Aboveground_biomass_PI",
  "Shoot_diameter_PI",
  "SPAD_PI"
)

missing_lsd_traits <- setdiff(lsd_traits, names(data))
if (length(missing_lsd_traits) > 0) {
  stop(
    paste0(
      "These LSD traits are missing from data: ",
      paste(missing_lsd_traits, collapse = ", ")
    )
  )
}

fmt_p_lsd <- function(p) {
  ifelse(
    is.na(p),
    NA_character_,
    ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  )
}

run_lsd_one_trait <- function(var) {
  
  trait_name <- trait_labels_table[[var]]
  
  dat <- data %>%
    dplyr::select(Region, all_of(var)) %>%
    dplyr::filter(
      !is.na(.data[[var]]),
      !is.na(Region)
    ) %>%
    dplyr::mutate(
      Region = droplevels(factor(Region)),
      PI_raw = .data[[var]],
      PI_logit = safe_qlogis(.data[[var]])
    ) %>%
    dplyr::filter(!is.na(PI_logit)) %>%
    droplevels()
  
  if (nrow(dat) < 4 || dplyr::n_distinct(dat$Region) < 2) {
    return(
      tibble(
        Trait = trait_name,
        Response_variable = var,
        Row_type = "insufficient_data",
        Region = NA_character_,
        Contrast = NA_character_,
        n = nrow(dat),
        Raw_PI_mean = NA_real_,
        Raw_PI_SE = NA_real_,
        Logit_PI_mean = NA_real_,
        Difference_logit = NA_real_,
        SE_difference = NA_real_,
        df_error = NA_real_,
        t_value = NA_real_,
        p_value = NA_real_,
        p_formatted = NA_character_,
        LSD_letters = NA_character_,
        Overall_F = NA_real_,
        Overall_p = NA_real_,
        Overall_p_formatted = NA_character_
      )
    )
  }
  
  mod <- lm(PI_logit ~ Region, data = dat)
  anv <- anova(mod)
  
  overall_F <- anv$`F value`[1]
  overall_p <- anv$`Pr(>F)`[1]
  df_error <- anv$Df[2]
  mse <- anv$`Mean Sq`[2]
  
  lsd_group <- agricolae::LSD.test(
    mod,
    "Region",
    p.adj = "none",
    group = TRUE,
    console = FALSE
  )
  
  groups_raw <- lsd_group$groups %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Region")
  
  letter_col <- if ("groups" %in% names(groups_raw)) {
    "groups"
  } else {
    tail(names(groups_raw), 1)
  }
  
  letters_df <- groups_raw %>%
    dplyr::transmute(
      Region = as.character(Region),
      LSD_letters = .data[[letter_col]]
    )
  
  region_means <- dat %>%
    dplyr::group_by(Region) %>%
    dplyr::summarise(
      n = dplyr::n(),
      Raw_PI_mean = mean(PI_raw, na.rm = TRUE),
      Raw_PI_SE = sd(PI_raw, na.rm = TRUE) / sqrt(n),
      Logit_PI_mean = mean(PI_logit, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Region = as.character(Region)
    ) %>%
    dplyr::left_join(letters_df, by = "Region") %>%
    dplyr::mutate(
      Trait = trait_name,
      Response_variable = var,
      Row_type = "Region mean",
      Contrast = NA_character_,
      Difference_logit = NA_real_,
      SE_difference = NA_real_,
      df_error = df_error,
      t_value = NA_real_,
      p_value = NA_real_,
      p_formatted = NA_character_,
      Overall_F = overall_F,
      Overall_p = overall_p,
      Overall_p_formatted = fmt_p_lsd(overall_p)
    ) %>%
    dplyr::select(
      Trait,
      Response_variable,
      Row_type,
      Region,
      Contrast,
      n,
      Raw_PI_mean,
      Raw_PI_SE,
      Logit_PI_mean,
      Difference_logit,
      SE_difference,
      df_error,
      t_value,
      p_value,
      p_formatted,
      LSD_letters,
      Overall_F,
      Overall_p,
      Overall_p_formatted
    )
  
  # Pairwise Fisher LSD comparisons
  region_levels_present <- levels(dat$Region)
  pair_list <- combn(region_levels_present, 2, simplify = FALSE)
  
  pairwise_df <- dplyr::bind_rows(
    lapply(pair_list, function(pair) {
      
      r1 <- pair[1]
      r2 <- pair[2]
      
      m1 <- region_means$Logit_PI_mean[region_means$Region == r1]
      m2 <- region_means$Logit_PI_mean[region_means$Region == r2]
      n1 <- region_means$n[region_means$Region == r1]
      n2 <- region_means$n[region_means$Region == r2]
      
      diff_logit <- m1 - m2
      se_diff <- sqrt(mse * (1 / n1 + 1 / n2))
      t_val <- diff_logit / se_diff
      p_val <- 2 * pt(-abs(t_val), df = df_error)
      
      tibble(
        Trait = trait_name,
        Response_variable = var,
        Row_type = "Pairwise LSD contrast",
        Region = NA_character_,
        Contrast = paste0(r1, " - ", r2),
        n = NA_integer_,
        Raw_PI_mean = NA_real_,
        Raw_PI_SE = NA_real_,
        Logit_PI_mean = NA_real_,
        Difference_logit = diff_logit,
        SE_difference = se_diff,
        df_error = df_error,
        t_value = t_val,
        p_value = p_val,
        p_formatted = fmt_p_lsd(p_val),
        LSD_letters = NA_character_,
        Overall_F = overall_F,
        Overall_p = overall_p,
        Overall_p_formatted = fmt_p_lsd(overall_p)
      )
    })
  )
  
  dplyr::bind_rows(region_means, pairwise_df)
}

PI_LSD_selected_traits_table <- dplyr::bind_rows(
  lapply(lsd_traits, run_lsd_one_trait)
)

print(PI_LSD_selected_traits_table, n = Inf)

writexl::write_xlsx(
  list(PI_LSD = PI_LSD_selected_traits_table),
  "Table_PI_LSD.xlsx"
)