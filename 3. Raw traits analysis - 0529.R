packages <- c(
  "dplyr", "stringr", "readr", "forcats", "tibble",
  "lme4", "car", "emmeans",
  "ggplot2", "cowplot", "ggExtra", "scales"
)

invisible(lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Package not installed: ", pkg))
  }
  library(pkg, character.only = TRUE)
}))

# ----------------------------
# 1. Read raw data
# ----------------------------
setwd("/Users/yaolin/Desktop/My papers/My manuscripts/2025 - Guo - Plasticity syndrome/Submission - Journal of Ecology - 0328 - R2/Data & Code")
data <- read.csv("Data_imp.csv", check.names = FALSE) %>%
  as.data.frame()

# ----------------------------
# 2. Trait name map
# ----------------------------
trait_map <- c(
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

# ----------------------------
# 3. Check required columns
# ----------------------------
required_cols <- c(
  "Sample_ID", "Garden_ID", "Continent", "Lineage",
  "Latitude_ab", names(trait_map)
)

missing_cols <- setdiff(required_cols, names(data))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The following required columns are missing from the dataset:\n",
      paste(missing_cols, collapse = ", ")
    )
  )
}

# ----------------------------
# 4. Clean character variables
# ----------------------------
data <- data %>%
  mutate(
    across(
      where(is.character),
      ~ str_squish(as.character(.x))
    )
  ) %>%
  mutate(
    across(
      where(is.character),
      ~ na_if(.x, "")
    )
  ) %>%
  mutate(
    across(
      where(is.character),
      ~ ifelse(
        .x %in% c("NA", "N/A", "na", "n/a", "NaN", "NULL", "null", "."),
        NA,
        .x
      )
    )
  )

# ----------------------------
# 5. Clean numeric variables
# ----------------------------
numeric_cols <- intersect(c("Latitude_ab", names(trait_map)), names(data))

data <- data %>%
  mutate(
    across(
      all_of(numeric_cols),
      ~ {
        x <- as.character(.x)
        x <- str_squish(x)
        x <- ifelse(
          x %in% c("", "NA", "N/A", "na", "n/a", "NaN", "NULL", "null", "."),
          NA,
          x
        )
        x <- parse_number(x)
        as.numeric(x)
      }
    )
  )

# ----------------------------
# 6. Set factor variables
# ----------------------------
data <- data %>%
  mutate(
    Sample_ID   = fct_drop(factor(Sample_ID)),
    Garden_ID   = fct_drop(factor(Garden_ID)),
    Continent   = fct_drop(factor(Continent)),
    Lineage     = fct_drop(factor(Lineage)),
    Latitude_ab = as.numeric(Latitude_ab)
  )

# ----------------------------
# 7. Make sure Garden_ID levels are ordered
# ----------------------------
expected_gardens <- c("Qingdao", "Shanghai")
observed_gardens <- sort(unique(as.character(data$Garden_ID)))
print(observed_gardens)

if (!all(expected_gardens %in% observed_gardens)) {
  stop(
    paste0(
      "Garden_ID must contain both 'Qingdao' and 'Shanghai'. Observed levels are: ",
      paste(observed_gardens, collapse = ", "),
      "\nPlease edit expected_gardens or recode Garden_ID."
    )
  )
}

data <- data %>%
  mutate(
    Garden_ID = factor(Garden_ID, levels = expected_gardens)
  )

# ----------------------------
# 8. Basic data checks
# ----------------------------
print(str(data))
print(sapply(data[, c("Sample_ID", "Garden_ID", "Continent", "Lineage", "Latitude_ab")], class))
print(colSums(is.na(data[, required_cols])))

sample_pair_check <- data %>%
  dplyr::filter(!is.na(Sample_ID), !is.na(Garden_ID)) %>%
  distinct(Sample_ID, Garden_ID) %>%
  count(Sample_ID, name = "n_gardens") %>%
  count(n_gardens, name = "n_sample_ids")
print(sample_pair_check)

sample_too_many_gardens <- data %>%
  dplyr::filter(!is.na(Sample_ID), !is.na(Garden_ID)) %>%
  distinct(Sample_ID, Garden_ID) %>%
  count(Sample_ID, name = "n_gardens") %>%
  dplyr::filter(n_gardens > 2)
print(sample_too_many_gardens)

sample_lineage_check <- data %>%
  dplyr::filter(!is.na(Sample_ID), !is.na(Lineage)) %>%
  distinct(Sample_ID, Lineage) %>%
  count(Sample_ID, name = "n_lineages") %>%
  dplyr::filter(n_lineages > 1)
print(sample_lineage_check)

sample_continent_check <- data %>%
  dplyr::filter(!is.na(Sample_ID), !is.na(Continent)) %>%
  distinct(Sample_ID, Continent) %>%
  count(Sample_ID, name = "n_continents") %>%
  dplyr::filter(n_continents > 1)
print(sample_continent_check)

sample_lineage_continent_check <- data %>%
  dplyr::filter(!is.na(Sample_ID), !is.na(Lineage), !is.na(Continent)) %>%
  mutate(Lineage_Continent = interaction(Lineage, Continent, drop = TRUE, sep = ":")) %>%
  distinct(Sample_ID, Lineage_Continent) %>%
  count(Sample_ID, name = "n_lineage_continent_groups") %>%
  dplyr::filter(n_lineage_continent_groups > 1)
print(sample_lineage_continent_check)

sample_latitude_check <- data %>%
  dplyr::filter(!is.na(Sample_ID), !is.na(Latitude_ab)) %>%
  distinct(Sample_ID, Latitude_ab) %>%
  count(Sample_ID, name = "n_latitudes") %>%
  dplyr::filter(n_latitudes > 1)
print(sample_latitude_check)

# ----------------------------
# 9. Optional log transformation settings
# ----------------------------
log_traits <- character(0)

# ----------------------------
# 10. Formatting functions
# ----------------------------
fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

fmt_num <- function(x, digits = 3) {
  if (is.na(x)) return(NA_character_)
  sprintf(paste0("%.", digits, "f"), x)
}

# ----------------------------
# 11. Function to fit one LMM and extract
# ----------------------------
run_one_trait <- function(resp, label, dat, log_traits = character(0)) {
  
  required_model_cols <- c(
    resp, "Latitude_ab", "Garden_ID",
    "Sample_ID", "Lineage", "Continent"
  )
  
  dat_mod <- dat %>%
    dplyr::select(all_of(required_model_cols)) %>%
    dplyr::filter(
      !is.na(.data[[resp]]),
      !is.na(Latitude_ab),
      !is.na(Garden_ID),
      !is.na(Sample_ID),
      !is.na(Lineage),
      !is.na(Continent)
    ) %>%
    mutate(
      Sample_ID = fct_drop(factor(Sample_ID)),
      Garden_ID = factor(Garden_ID, levels = expected_gardens),
      Garden_ID = fct_drop(Garden_ID),
      Lineage = fct_drop(factor(Lineage)),
      Continent = fct_drop(factor(Continent))
    )
  
  n_obs <- nrow(dat_mod)
  n_sample_id <- n_distinct(dat_mod$Sample_ID)
  n_lineage <- n_distinct(dat_mod$Lineage)
  n_continent <- n_distinct(dat_mod$Continent)
  n_lineage_continent <- n_distinct(
    interaction(dat_mod$Lineage, dat_mod$Continent, drop = TRUE, sep = ":")
  )
  mean_latitude <- mean(dat_mod$Latitude_ab, na.rm = TRUE)
  
  empty_anova <- function(status, singular_value = NA, warning_msg = NA_character_,
                          sample_var = NA_real_, lineage_continent_var = NA_real_,
                          residual_var = NA_real_) {
    tibble(
      Trait = label,
      Response_variable = resp,
      Log_transformed = resp %in% log_traits,
      n_obs = n_obs,
      n_sample_id = n_sample_id,
      n_lineage = n_lineage,
      n_continent = n_continent,
      n_lineage_continent = n_lineage_continent,
      fit_status = status,
      singular = singular_value,
      Sample_ID_var = sample_var,
      Lineage_Continent_var = lineage_continent_var,
      Residual_var = residual_var,
      model_warning = warning_msg,
      
      Latitude_chisq = NA_real_,
      Latitude_df    = NA_real_,
      Latitude_p     = NA_real_,
      Garden_chisq   = NA_real_,
      Garden_df      = NA_real_,
      Garden_p       = NA_real_,
      Int_chisq      = NA_real_,
      Int_df         = NA_real_,
      Int_p          = NA_real_
    )
  }
  
  empty_contrast <- function(status, singular_value = NA, warning_msg = NA_character_) {
    tibble(
      Trait = label,
      Response_variable = resp,
      Log_transformed = resp %in% log_traits,
      n_obs = n_obs,
      n_sample_id = n_sample_id,
      n_lineage = n_lineage,
      n_continent = n_continent,
      n_lineage_continent = n_lineage_continent,
      mean_Latitude_ab = mean_latitude,
      fit_status = status,
      singular = singular_value,
      model_warning = warning_msg,
      contrast = "Shanghai - Qingdao",
      estimate = NA_real_,
      SE = NA_real_,
      df = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      ratio_if_log_transformed = NA_real_
    )
  }
  
  if (
    n_obs < 5 ||
    n_distinct(dat_mod$Garden_ID) < 2 ||
    n_sample_id < 2 ||
    n_lineage_continent < 2
  ) {
    return(
      list(
        anova = empty_anova("insufficient_data"),
        contrast = empty_contrast("insufficient_data")
      )
    )
  }
  
  # Optional log transformation
  use_log <- resp %in% log_traits
  
  if (use_log) {
    if (any(dat_mod[[resp]] <= 0, na.rm = TRUE)) {
      return(
        list(
          anova = empty_anova("log_transform_failed_nonpositive_values"),
          contrast = empty_contrast("log_transform_failed_nonpositive_values")
        )
      )
    }
    dat_mod$response_y <- log(dat_mod[[resp]])
  } else {
    dat_mod$response_y <- dat_mod[[resp]]
  }
  
  form <- response_y ~ Latitude_ab * Garden_ID +
    (1 | Sample_ID) +
    (1 | Lineage:Continent)
  
  warn_msg <- character(0)
  err_msg <- NA_character_
  
  mod <- tryCatch(
    withCallingHandlers(
      lmer(
        form,
        data = dat_mod,
        control = lmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 100000)
        )
      ),
      warning = function(w) {
        warn_msg <<- c(warn_msg, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      err_msg <<- conditionMessage(e)
      NULL
    }
  )
  
  warning_text <- ifelse(
    length(warn_msg) == 0,
    NA_character_,
    paste(unique(warn_msg), collapse = " | ")
  )
  
  if (is.null(mod)) {
    return(
      list(
        anova = empty_anova(paste0("model_failed: ", err_msg), warning_msg = warning_text),
        contrast = empty_contrast(paste0("model_failed: ", err_msg), warning_msg = warning_text)
      )
    )
  }
  
  singular_status <- isSingular(mod, tol = 1e-4)
  
  # Variance components
  vc <- as.data.frame(VarCorr(mod))
  
  get_vc <- function(grp_name) {
    out <- vc %>%
      dplyr::filter(grp == grp_name) %>%
      pull(vcov)
    
    if (length(out) == 0) return(NA_real_)
    out[1]
  }
  
  sample_var <- get_vc("Sample_ID")
  lineage_continent_var <- get_vc("Lineage:Continent")
  residual_var <- get_vc("Residual")
  
  aov_tab <- tryCatch(
    car::Anova(mod, type = 2),
    error = function(e) NULL
  )
  
  if (is.null(aov_tab)) {
    anova_out <- empty_anova(
      status = "anova_failed",
      singular_value = singular_status,
      warning_msg = warning_text,
      sample_var = sample_var,
      lineage_continent_var = lineage_continent_var,
      residual_var = residual_var
    )
  } else {
    aov_df <- as.data.frame(aov_tab)
    aov_df$term <- rownames(aov_df)
    
    extract_term <- function(term_name) {
      row <- aov_df[aov_df$term == term_name, ]
      
      if (nrow(row) == 0) {
        return(list(chisq = NA_real_, df = NA_real_, p = NA_real_))
      }
      
      list(
        chisq = row$Chisq[1],
        df = row$Df[1],
        p = row$`Pr(>Chisq)`[1]
      )
    }
    
    lat <- extract_term("Latitude_ab")
    gar <- extract_term("Garden_ID")
    
    int_row <- aov_df[
      aov_df$term %in% c("Latitude_ab:Garden_ID", "Garden_ID:Latitude_ab"),
    ]
    
    if (nrow(int_row) == 0) {
      int <- list(chisq = NA_real_, df = NA_real_, p = NA_real_)
    } else {
      int <- list(
        chisq = int_row$Chisq[1],
        df = int_row$Df[1],
        p = int_row$`Pr(>Chisq)`[1]
      )
    }
    
    anova_out <- tibble(
      Trait = label,
      Response_variable = resp,
      Log_transformed = use_log,
      n_obs = n_obs,
      n_sample_id = n_sample_id,
      n_lineage = n_lineage,
      n_continent = n_continent,
      n_lineage_continent = n_lineage_continent,
      fit_status = "ok",
      singular = singular_status,
      Sample_ID_var = sample_var,
      Lineage_Continent_var = lineage_continent_var,
      Residual_var = residual_var,
      model_warning = warning_text,
      
      Latitude_chisq = lat$chisq,
      Latitude_df    = lat$df,
      Latitude_p     = lat$p,
      
      Garden_chisq   = gar$chisq,
      Garden_df      = gar$df,
      Garden_p       = gar$p,
      
      Int_chisq      = int$chisq,
      Int_df         = int$df,
      Int_p          = int$p
    )
  }
  
  emm <- tryCatch(
    emmeans(
      mod,
      ~ Garden_ID,
      at = list(Latitude_ab = mean_latitude),
      lmer.df = "asymptotic"
    ),
    error = function(e) NULL
  )
  
  if (is.null(emm)) {
    contrast_out <- empty_contrast(
      status = "emmeans_failed",
      singular_value = singular_status,
      warning_msg = warning_text
    )
  } else {
    contr <- tryCatch(
      contrast(
        emm,
        method = list("Shanghai - Qingdao" = c(-1, 1)),
        adjust = "none"
      ),
      error = function(e) NULL
    )
    
    if (is.null(contr)) {
      contrast_out <- empty_contrast(
        status = "contrast_failed",
        singular_value = singular_status,
        warning_msg = warning_text
      )
    } else {
      contr_df <- as.data.frame(contr)
      stat_col <- intersect(c("z.ratio", "t.ratio"), names(contr_df))[1]
      
      df_value <- if ("df" %in% names(contr_df)) contr_df$df[1] else NA_real_
      stat_value <- if (!is.na(stat_col)) contr_df[[stat_col]][1] else NA_real_
      
      contrast_out <- tibble(
        Trait = label,
        Response_variable = resp,
        Log_transformed = use_log,
        n_obs = n_obs,
        n_sample_id = n_sample_id,
        n_lineage = n_lineage,
        n_continent = n_continent,
        n_lineage_continent = n_lineage_continent,
        mean_Latitude_ab = mean_latitude,
        fit_status = "ok",
        singular = singular_status,
        model_warning = warning_text,
        contrast = contr_df$contrast[1],
        estimate = contr_df$estimate[1],
        SE = contr_df$SE[1],
        df = df_value,
        statistic = stat_value,
        p_value = contr_df$p.value[1],
        ratio_if_log_transformed = ifelse(use_log, exp(contr_df$estimate[1]), NA_real_)
      )
    }
  }
  
  list(
    anova = anova_out,
    contrast = contrast_out
  )
}

# ----------------------------
# 12. Run all trait models
# ----------------------------
model_outputs <- lapply(names(trait_map), function(tr) {
  run_one_trait(
    resp = tr,
    label = trait_map[[tr]],
    dat = data,
    log_traits = log_traits
  )
})

table_s2_raw <- bind_rows(lapply(model_outputs, function(x) x$anova))
garden_contrasts_raw <- bind_rows(lapply(model_outputs, function(x) x$contrast))

print(table_s2_raw, n = Inf)
print(garden_contrasts_raw, n = Inf)

# ----------------------------
# 13. Model diagnostics
# ----------------------------
model_diagnostics <- table_s2_raw %>%
  dplyr::select(
    Trait,
    Response_variable,
    Log_transformed,
    n_obs,
    n_sample_id,
    n_lineage,
    n_continent,
    n_lineage_continent,
    fit_status,
    singular,
    Sample_ID_var,
    Lineage_Continent_var,
    Residual_var,
    model_warning
  )

print(model_diagnostics, n = Inf)
print(model_diagnostics %>% dplyr::filter(singular == TRUE), n = Inf)

# ----------------------------
# 14. Summary counts for Results text
# ----------------------------
sig_summary <- table_s2_raw %>%
  summarise(
    n_traits = n(),
    Latitude_significant = sum(Latitude_p < 0.05, na.rm = TRUE),
    Garden_significant = sum(Garden_p < 0.05, na.rm = TRUE),
    Interaction_significant = sum(Int_p < 0.05, na.rm = TRUE)
  )

print(sig_summary)

# ----------------------------
# 15. Format Table S2 display table
# ----------------------------
table_s2_show <- table_s2_raw %>%
  transmute(
    `Independent variables` = Trait,
    
    `Latitude χ²` = ifelse(is.na(Latitude_chisq), NA, sprintf("%.3f", Latitude_chisq)),
    `df`          = ifelse(is.na(Latitude_df), NA, as.character(Latitude_df)),
    `p`           = sapply(Latitude_p, fmt_p),
    
    `Garden χ²`   = ifelse(is.na(Garden_chisq), NA, sprintf("%.3f", Garden_chisq)),
    `df `         = ifelse(is.na(Garden_df), NA, as.character(Garden_df)),
    `p `          = sapply(Garden_p, fmt_p),
    
    `Latitude × Garden χ²` = ifelse(is.na(Int_chisq), NA, sprintf("%.3f", Int_chisq)),
    `df  `                 = ifelse(is.na(Int_df), NA, as.character(Int_df)),
    `p  `                  = sapply(Int_p, fmt_p)
  )

print(table_s2_show, n = Inf)

# ----------------------------
# 16. Format Garden contrast table
# ----------------------------
garden_contrasts <- garden_contrasts_raw %>%
  mutate(
    p_BH = p.adjust(p_value, method = "BH"),
    significance_BH = case_when(
      is.na(p_BH) ~ "",
      p_BH < 0.001 ~ "***",
      p_BH < 0.01 ~ "**",
      p_BH < 0.05 ~ "*",
      TRUE ~ ""
    ),
    direction = case_when(
      is.na(estimate) ~ NA_character_,
      estimate > 0 ~ "Shanghai > Qingdao",
      estimate < 0 ~ "Qingdao > Shanghai",
      estimate == 0 ~ "No difference"
    )
  )

garden_contrasts_show <- garden_contrasts %>%
  transmute(
    Trait = Trait,
    Contrast = contrast,
    Direction = direction,
    `Estimate` = sapply(estimate, fmt_num),
    `SE` = sapply(SE, fmt_num),
    `df` = ifelse(is.na(df), NA, ifelse(is.infinite(df), "Inf", sprintf("%.1f", df))),
    `z/t` = sapply(statistic, fmt_num),
    `p` = sapply(p_value, fmt_p),
    `BH-adjusted p` = sapply(p_BH, fmt_p),
    `BH sig.` = significance_BH,
    `Mean absolute latitude` = sapply(mean_Latitude_ab, fmt_num),
    `Singular fit` = singular
  )

print(garden_contrasts_show, n = Inf)


# ----------------------------
# 17. Sensitivity LMM on raw (non-imputed) data
# ----------------------------
data_raw <- read.csv("Data_raw.csv", check.names = FALSE) %>%
  as.data.frame()

data_raw <- data_raw %>%
  mutate(across(where(is.character), ~ str_squish(as.character(.x)))) %>%
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  mutate(across(
    where(is.character),
    ~ ifelse(.x %in% c("NA", "N/A", "na", "n/a", "NaN", "NULL", "null", "."),
             NA, .x)
  )) %>%
  mutate(across(
    all_of(intersect(c("Latitude_ab", names(trait_map)), names(.))),
    ~ {
      x <- str_squish(as.character(.x))
      x <- ifelse(x %in% c("", "NA", "N/A", "na", "n/a", "NaN", "NULL", "null", "."),
                  NA, x)
      as.numeric(parse_number(x))
    }
  )) %>%
  mutate(
    Sample_ID   = fct_drop(factor(Sample_ID)),
    Garden_ID   = factor(Garden_ID, levels = expected_gardens),
    Continent   = fct_drop(factor(Continent)),
    Lineage     = fct_drop(factor(Lineage)),
    Latitude_ab = as.numeric(Latitude_ab)
  )

model_outputs_sens <- lapply(names(trait_map), function(tr) {
  run_one_trait(
    resp = tr,
    label = trait_map[[tr]],
    dat = data_raw,
    log_traits = log_traits
  )
})

table_raw_LMM_sens_full <- bind_rows(lapply(model_outputs_sens, function(x) x$anova))

table_raw_LMM_sens_show <- table_raw_LMM_sens_full %>%
  transmute(
    `Independent variables` = Trait,
    
    `Latitude χ²` = ifelse(is.na(Latitude_chisq), NA, sprintf("%.3f", Latitude_chisq)),
    `df`          = ifelse(is.na(Latitude_df), NA, as.character(Latitude_df)),
    `p`           = sapply(Latitude_p, fmt_p),
    
    `Garden χ²`   = ifelse(is.na(Garden_chisq), NA, sprintf("%.3f", Garden_chisq)),
    `df `         = ifelse(is.na(Garden_df), NA, as.character(Garden_df)),
    `p `          = sapply(Garden_p, fmt_p),
    
    `Latitude × Garden χ²` = ifelse(is.na(Int_chisq), NA, sprintf("%.3f", Int_chisq)),
    `df  `                 = ifelse(is.na(Int_df), NA, as.character(Int_df)),
    `p  `                  = sapply(Int_p, fmt_p)
  )

print(table_raw_LMM_sens_show, n = Inf)


# ----------------------------
# 18. Latitude regression per garden
# ----------------------------

latitude_per_garden <- bind_rows(lapply(names(trait_map), function(tr) {
  bind_rows(lapply(c("Shanghai", "Qingdao"), function(g) {
    
    d <- data %>%
      dplyr::filter(
        Garden_ID == g,
        !is.na(.data[[tr]]),
        !is.na(Latitude_ab)
      )
    
    if (nrow(d) < 4) {
      return(tibble(
        Trait  = trait_map[[tr]],
        Garden = g,
        n      = nrow(d),
        beta   = NA_character_,
        SE     = NA_character_,
        t      = NA_character_,
        p      = NA_character_,
        R2     = NA_character_,
        df     = NA_character_
      ))
    }
    
    m  <- lm(reformulate("Latitude_ab", response = tr), data = d)
    sm <- summary(m)
    co <- sm$coefficients["Latitude_ab", ]
    
    tibble(
      Trait  = trait_map[[tr]],
      Garden = g,
      n      = nrow(d),
      beta   = sprintf("%.3f", co["Estimate"]),
      SE     = sprintf("%.3f", co["Std. Error"]),
      t      = sprintf("%.3f", co["t value"]),
      p      = fmt_p(co["Pr(>|t|)"]),
      R2     = sprintf("%.3f", sm$r.squared),
      df     = as.character(sm$df[2])
    )
  }))
}))

print(latitude_per_garden, n = Inf)

# ----------------------------
# 19. Export CSV files
# ----------------------------
write.csv(table_s2_show,            "Table_raw_LMM.csv",             row.names = FALSE)
write.csv(table_raw_LMM_sens_show,  "Table_raw_LMM_sensitivity.csv", row.names = FALSE)
write.csv(garden_contrasts_show,    "Table_raw_garden_contrast.csv", row.names = FALSE)
write.csv(latitude_per_garden,      "Table_raw_latitude.csv",        row.names = FALSE)

# ----------------------------
# 20. Figure S2
# ----------------------------
fmt_p_ann <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  sprintf("= %.3f", p)
}

make_lat_panel_S2 <- function(trait_col, panel_letter, show_x_label = FALSE) {
  
  y_label <- trait_map[[trait_col]]
  
  d <- data %>%
    dplyr::filter(!is.na(.data[[trait_col]]), !is.na(Latitude_ab))
  
  d_sh <- d %>% dplyr::filter(Garden_ID == "Shanghai")
  d_qd <- d %>% dplyr::filter(Garden_ID == "Qingdao")
  
  m_sh <- lm(reformulate("Latitude_ab", response = trait_col), data = d_sh)
  m_qd <- lm(reformulate("Latitude_ab", response = trait_col), data = d_qd)
  
  sm_sh <- summary(m_sh)
  sm_qd <- summary(m_qd)
  
  sh_r2 <- sm_sh$r.squared
  qd_r2 <- sm_qd$r.squared
  sh_p  <- sm_sh$coefficients["Latitude_ab", "Pr(>|t|)"]
  qd_p  <- sm_qd$coefficients["Latitude_ab", "Pr(>|t|)"]
  
  sh_label <- sprintf(
    "paste(\"SH: \", italic(R)^2, \" = %.3f, \", italic(p), \" %s\")",
    sh_r2, fmt_p_ann(sh_p)
  )
  qd_label <- sprintf(
    "paste(\"QD: \", italic(R)^2, \" = %.3f, \", italic(p), \" %s\")",
    qd_r2, fmt_p_ann(qd_p)
  )
  
  sh_lty <- if (!is.na(sh_p) && sh_p < 0.05) "solid" else "dashed"
  qd_lty <- if (!is.na(qd_p) && qd_p < 0.05) "solid" else "dashed"
  
  p <- ggplot(d, aes(x = Latitude_ab, y = .data[[trait_col]])) +
    geom_point(aes(color = Garden_ID),
               size = 2.5, shape = 16, stroke = 0.25, alpha = 0.65) +
    geom_smooth(data = d_sh, method = "lm", formula = y ~ x,
                color = "#C24E44", fill = "#C24E44",
                se = TRUE, alpha = 0.2, linetype = sh_lty, linewidth = 0.9) +
    geom_smooth(data = d_qd, method = "lm", formula = y ~ x,
                color = "#387EB8", fill = "#387EB8",
                se = TRUE, alpha = 0.2, linetype = qd_lty, linewidth = 0.9) +
    scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                  "Shanghai" = "#C24E44")) +
    scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                                 "Shanghai" = "#C24E44"),
                      guide = "none") +
    theme_minimal() +
    theme(
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.background   = element_blank(),
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
      axis.line.x        = element_line(color = "black", linewidth = 0.5),
      axis.line.y        = element_line(color = "black", linewidth = 0.5),
      axis.ticks         = element_line(color = "black", linewidth = 0.5),
      axis.text          = element_text(size = 12, color = "black"),
      axis.title         = element_text(size = 13),
      legend.position    = "none"
    ) +
    xlab(if (show_x_label) "Absolute latitude (°)" else NULL) +
    ylab(y_label) +
    scale_x_continuous(limits = c(20, 60),
                       labels = scales::number_format(accuracy = 1)) +
    annotate("text", x = -Inf, y = Inf,
             hjust = -0.05, vjust = 1.5,
             label = sh_label, parse = TRUE,
             size = 4.5, color = "#C24E44") +
    annotate("text", x = -Inf, y = Inf,
             hjust = -0.05, vjust = 3.2,
             label = qd_label, parse = TRUE,
             size = 4.5, color = "#387EB8")

  p_marg <- ggMarginal(
    p,
    type        = "density",
    margins     = "y",
    groupColour = TRUE,
    groupFill   = TRUE,
    alpha       = 0.2
  )
  
  p_final <- cowplot::ggdraw() +
    cowplot::draw_plot(p_marg, 0, 0, 1, 1) +
    cowplot::draw_label(panel_letter, x = 0.04, y = 0.985,
                        hjust = 0, vjust = 1,
                        fontface = "bold", size = 18)
  
  return(p_final)
}

# Build all 20 panels
ncol_S2 <- 4
nrow_S2 <- ceiling(length(trait_map) / ncol_S2)
panel_letters <- letters[seq_along(trait_map)]

plot_list_S2 <- lapply(seq_along(trait_map), function(i) {
  trait_col <- names(trait_map)[i]
  row_id    <- ceiling(i / ncol_S2)
  show_x    <- (row_id == nrow_S2)
  make_lat_panel_S2(trait_col,
                    panel_letter = panel_letters[i],
                    show_x_label = show_x)
})

final_plot_S2 <- plot_grid(
  plotlist = plot_list_S2,
  ncol     = ncol_S2,
  align    = "hv"
) +
  theme(plot.margin = margin(20, 20, 20, 20))

final_plot_S2
ggsave("Figure S2.pdf", final_plot_S2,
       height = 375, width = 390, units = "mm")

# ----------------------------
# 21. Figure S3
# ----------------------------
sig_star <- function(p) {
  if (is.na(p)) return("ns")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  return("ns")
}

make_garden_bar_S3 <- function(trait_col, panel_letter, show_x_label = FALSE) {
  
  y_label    <- trait_map[[trait_col]]
  trait_lab  <- trait_map[[trait_col]]

  garden_p <- table_s2_raw %>%
    dplyr::filter(Trait == trait_lab) %>%
    dplyr::pull(Garden_p)
  garden_p <- if (length(garden_p) > 0) garden_p[1] else NA_real_
  sig_lab  <- sig_star(garden_p)
  
  d <- data %>%
    dplyr::filter(!is.na(.data[[trait_col]]))
  
  dat_plot <- d %>%
    dplyr::group_by(Garden_ID) %>%
    dplyr::summarise(
      coef  = mean(.data[[trait_col]], na.rm = TRUE),
      sdVal = sd(.data[[trait_col]], na.rm = TRUE),
      n     = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      se    = sdVal / sqrt(n),
      lower = pmax(0, coef - se),
      upper = coef + se
    )
  
  y_max_bar <- max(dat_plot$upper, na.rm = TRUE)
  y_buffer  <- 0.20 * y_max_bar
  ann_y     <- y_max_bar + 0.10 * y_max_bar
  ylim_top  <- y_max_bar + y_buffer
  
  p <- ggplot(dat_plot, aes(x = Garden_ID, y = coef,
                            fill = Garden_ID, color = Garden_ID)) +
    geom_col(width = 0.4, alpha = 0.75, color = NA) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.1, linewidth = 1, alpha = 0.75) +
    scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                                 "Shanghai" = "#C24E44")) +
    scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                  "Shanghai" = "#C24E44")) +
    theme_minimal() +
    theme(
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.background   = element_blank(),
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
      axis.line.x        = element_line(color = "black", linewidth = 0.5),
      axis.line.y        = element_line(color = "black", linewidth = 0.5),
      axis.ticks         = element_line(color = "black", linewidth = 0.5),
      axis.text          = element_text(size = 12, color = "black"),
      axis.title         = element_text(size = 13),
      legend.position    = "none"
    ) +
    labs(x = if (show_x_label) "Common garden" else NULL,
         y = y_label) +
    coord_cartesian(ylim = c(0, ylim_top), clip = "off") +
    annotate("text", x = 1.5, y = ann_y,
             label = sig_lab,
             size = 8,
             fontface = if (sig_lab == "ns") "plain" else "bold")
  
  # Add panel letter
  p_final <- cowplot::ggdraw() +
    cowplot::draw_plot(p, 0, 0, 1, 1) +
    cowplot::draw_label(panel_letter, x = 0.04, y = 0.985,
                        hjust = 0, vjust = 1,
                        fontface = "bold", size = 18)
  
  return(p_final)
}

ncol_S3 <- 4
nrow_S3 <- ceiling(length(trait_map) / ncol_S3)

plot_list_S3 <- lapply(seq_along(trait_map), function(i) {
  trait_col <- names(trait_map)[i]
  row_id    <- ceiling(i / ncol_S3)
  show_x    <- (row_id == nrow_S3)
  make_garden_bar_S3(trait_col,
                     panel_letter = panel_letters[i],
                     show_x_label = show_x)
})

final_plot_S3 <- plot_grid(
  plotlist = plot_list_S3,
  ncol     = ncol_S3,
  align    = "hv"
) +
  theme(plot.margin = margin(20, 20, 20, 20))

final_plot_S3
ggsave("Figure S3.pdf", final_plot_S3,
       height = 375, width = 390, units = "mm",
       limitsize = FALSE)