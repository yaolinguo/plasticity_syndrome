library(tidyverse)
library(readxl)
library(missForest)
library(maps)

setwd("/Users/yaolin/Desktop/My papers/My manuscripts/2025 - Guo - Plasticity syndrome/Submission - Journal of Ecology - 0328 - R2/Data & Code")

# ----------------------------
# 1. Read raw data
# ----------------------------
data_raw <- read_excel("Raw data - 0525.xlsx", sheet = 1)
data_raw <- as.data.frame(data_raw)
original_cols <- names(data_raw)

# ----------------------------
# 2. Define trait columns
# ----------------------------
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

# PI = |Shanghai - Qingdao| / max(Shanghai, Qingdao)
calc_pi <- function(sh, qd) {
  denom <- pmax(sh, qd)
  out <- ifelse(
    is.na(sh) | is.na(qd) | is.na(denom) | denom <= 0,
    NA_real_,
    abs(sh - qd) / denom
  )
  out[!is.finite(out)] <- NA_real_
  out
}

# ----------------------------
# 3. Imputation block
# ----------------------------
data_raw <- data_raw %>% 
  mutate(row_id = row_number())

id_cols     <- intersect(c("row_id", "Sample_ID"), names(data_raw))
factor_cols <- intersect(c("Garden_ID", "Continent", "Group", "Lineage"), names(data_raw))
num_cols    <- setdiff(names(data_raw), c(id_cols, factor_cols))

data_raw <- data_raw %>%
  mutate(across(all_of(num_cols), quiet_num)) %>%
  mutate(across(all_of(factor_cols), as.factor)) %>%
  mutate(
    trait_row_all_missing = if_all(all_of(trait_cols), is.na)
  )

data_to_impute <- data_raw %>%
  filter(!trait_row_all_missing)

data_all_missing <- data_raw %>%
  filter(trait_row_all_missing)

df_prep <- data_to_impute %>%
  dplyr::select(-all_of(id_cols), -trait_row_all_missing)

# Run missForest
set.seed(123)
mf_out <- missForest(
  df_prep,
  ntree   = 2500,
  maxiter = 1000,
  verbose = TRUE
)

print(mf_out$OOBerror)

data_imputed_nonempty <- bind_cols(
  data_to_impute %>% dplyr::select(row_id, Sample_ID, trait_row_all_missing),
  mf_out$ximp
)

raw_data_imputed <- bind_rows(
  data_imputed_nonempty,
  data_all_missing
) %>%
  arrange(row_id)

# ----------------------------
# 4. Build Data_raw and Data_imp
# ----------------------------
Data_raw <- data_raw %>%
  dplyr::select(all_of(original_cols))

Data_imp <- raw_data_imputed %>%
  dplyr::select(all_of(original_cols))

write_csv(Data_raw, "Data_raw.csv")
write_csv(Data_imp, "Data_imp.csv")

# ----------------------------
# 5. Function build_PI
# ----------------------------
build_PI <- function(pot_df, trait_cols, meta_cols) {
  
  df_mean <- pot_df %>%
    group_by(Sample_ID, Garden_ID) %>%
    summarise(
      across(all_of(trait_cols), mean_or_na),
      across(all_of(meta_cols),  dplyr::first),
      .groups = "drop"
    )
  
  df_mean <- df_mean %>%
    filter(!if_all(all_of(trait_cols), is.na))

  paired_ID <- df_mean %>%
    group_by(Sample_ID) %>%
    summarise(n_garden = dplyr::n_distinct(Garden_ID), .groups = "drop") %>%
    filter(n_garden == 2) %>%
    pull(Sample_ID)
  
  df_mean <- df_mean %>%
    filter(Sample_ID %in% paired_ID)
  
  df_meta <- df_mean %>%
    group_by(Sample_ID) %>%
    summarise(
      across(all_of(meta_cols), dplyr::first),
      .groups = "drop"
    )

  df_wide <- df_mean %>%
    dplyr::select(Sample_ID, Garden_ID, all_of(trait_cols)) %>%
    pivot_wider(
      names_from  = Garden_ID,
      values_from = all_of(trait_cols),
      names_glue  = "{.value}.{Garden_ID}"
    )

  PI_tbl <- purrr::map_dfc(trait_cols, function(tr) {
    sh_col <- paste0(tr, ".Shanghai")
    qd_col <- paste0(tr, ".Qingdao")
    tibble(
      !!paste0(tr, "_PI") := calc_pi(df_wide[[sh_col]], df_wide[[qd_col]])
    )
  })
  
  bind_cols(
    df_wide %>% dplyr::select(Sample_ID),
    PI_tbl
  ) %>%
    left_join(df_meta, by = "Sample_ID")
}

# ----------------------------
# 6. Compute PI data
# ----------------------------
meta_cols <- intersect(
  c("Latitude", "Latitude_ab", "Longitude", "Continent", "Group", "Lineage"),
  names(Data_raw)
)

Data_raw_PI <- build_PI(Data_raw, trait_cols, meta_cols)
Data_imp_PI <- build_PI(Data_imp, trait_cols, meta_cols)

# ----------------------------
# 7. Save PI outputs
# ----------------------------
write_csv(Data_raw_PI, "Data_raw_PI.csv")
write_csv(Data_imp_PI, "Data_imp_PI.csv")

# ----------------------------
# 8. Mapping
# ----------------------------
data1 <- Data_imp_PI %>%
  filter(!is.na(Latitude), !is.na(Longitude))

world <- map_data("world")

p <- ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "grey90",
    color = "grey75"
  ) +
  geom_point(
    data = data1,
    aes(x = Longitude, y = Latitude, color = Continent),
    size = 3,
    shape = 16,
    alpha = 0.7
  ) +
  scale_color_manual(
    values = c(
      "America" = "#55B7E6",
      "Asia"    = "#193E8F",
      "Europe"  = "#E53528",
      "Oceania" = "#F09739"
    )
  ) +
  xlab("Longitude (°)") +
  ylab("Latitude (°)") +
  scale_x_continuous(breaks = c(-100, 0, 100)) +
  coord_quickmap() +
  theme(
    axis.text         = element_text(size = 14),
    axis.title.x      = element_text(size = 14),
    axis.title.y      = element_text(size = 14),
    legend.title      = element_text(size = 14),
    legend.text       = element_text(size = 14),
    panel.border      = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background  = element_blank(),
    panel.grid.major  = element_line(colour = "grey70", linetype = "dashed", linewidth = 0.3),
    panel.grid.minor  = element_line(colour = "grey85", linetype = "dotted", linewidth = 0.2),
    legend.position   = "none"
  )

print(p)

ggsave(
  filename = "./Figure 1.pdf",
  plot     = p,
  height   = 125,
  width    = 250,
  units    = "mm"
)