library(cluster)
library(dendextend)
library(MASS)
library(dplyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readxl)
library(tidyverse)

setwd("/Users/yaolin/Desktop/My papers/Manuscripts/2025 - Guo - Plasticity syndrome/Submission")
data_imp <- read_excel("data_imp.xlsx")
data_imp <- as.data.frame(data_imp)
data_imp

id_col      <- "Sample_ID"
factor_cols <- c("Garden_ID", "Continent")
trait_cols <- c("Leaf_thickness","Leaf_length","Leaf_width","Leaf_area","SLA",
                "Leaf_saturated_fresh_weight","Leaf_dry_weight","LDMC",
                "Aboveground_biomass","Belowground_biomass","Shoot_height",
                "Plant_number","Shoot_diameter",
                "Leaf_C","Leaf_N","Leaf_CN",
                "Root_C","Root_N","Root_CN","SPAD",
                "Group")

num_cols <- setdiff(names(data_imp), c(id_col, factor_cols))
quiet_num <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(
    parse_number(as.character(x), na = c("", "NA", "P"))
  )
}

data_clean <- data_imp %>% 
  mutate(across(all_of(num_cols), quiet_num)) %>%   
  mutate(across(all_of(factor_cols), as.factor))

df_mean <- data_clean %>%
  group_by(Sample_ID, Garden_ID) %>%           
  summarise(across(all_of(trait_cols), \(x) mean(x, na.rm = TRUE)),
            Latitude  = first(Latitude),
            Longitude = first(Longitude),
            Continent = first(Continent),
            Latitude_ab = first(Latitude_ab),
            .groups   = "drop")

paired_ID <- df_mean %>% 
  count(Sample_ID) %>%     
  filter(n == 2) %>%             
  pull(Sample_ID)

df_mean <- df_mean %>% filter(Sample_ID %in% paired_ID)
df_wide <- df_mean %>%                          
  pivot_wider(names_from  = Garden_ID,
              values_from = all_of(trait_cols),
              names_glue  = "{.value}.{Garden_ID}")

safe_log_ratio <- function(sh, qd) {
  out <- log(sh / qd)
  out[!is.finite(out)] <- NA_real_
  out
}

df_lnRR <- df_wide %>% mutate(across(
  ends_with(".Shanghai"),
  ~ safe_log_ratio(
    .x,
    get(str_replace(cur_column(), "\\.Shanghai$", ".Qingdao"))),
  .names = "{str_remove(.col, fixed('.Shanghai'))}_lnRR")) %>% 
  dplyr::select(Sample_ID, Latitude, Latitude_ab, Longitude, Continent, ends_with("_lnRR"))

write_csv(df_lnRR, "data_original_lnRR.csv")
print(df_lnRR)

df_lnRR <- df_lnRR %>% rename(Region = Continent)
data <- df_lnRR
data

# Global map
data1 <- subset(data, !is.na(Latitude))
world <- map_data("world")
p <- ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group), 
               fill = "grey90", 
               color = "grey75") +
  geom_point(data = data1, 
             aes(x = Longitude, y = Latitude, color = Region), 
             size = 3, 
             shape = 16,
             alpha = 0.7) +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  xlab("Longitude (°)") + 
  ylab("Latitude (°)") +
  theme(
    axis.text       = element_text(size = 14),
    axis.title.x    = element_text(size = 14),
    axis.title.y    = element_text(size = 14),
    legend.title    = element_text(size = 14),
    legend.text     = element_text(size = 14),
    panel.border    = element_rect(color="black", fill=NA, size=1),
    panel.background= element_blank(),
    panel.grid.major   = element_line(colour = "grey70", linetype = "dashed", linewidth = 0.3),
    panel.grid.minor   = element_line(colour = "grey85", linetype = "dotted", linewidth = 0.2),
    legend.position = "none") +
  scale_x_continuous(breaks = c(-100, 0, 100)) +
  coord_quickmap()
print(p)
ggsave('./Figure 1111111.pdf', p, height = 125, width = 250, units = "mm")



# Q1 - GLMM - Table 1 
data <- read.csv("data_original_lnRR.csv")
model1 <- lmer(Leaf_thickness_lnRR               ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model1)
model2 <- lmer(Leaf_length_lnRR                  ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model2)
model3 <- lmer(Leaf_width_lnRR	                 ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model3)
model4 <- lmer(Leaf_area_lnRR                    ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model4)
model5 <- lmer(SLA_lnRR	                         ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model5)
model6 <- lmer(Leaf_saturated_fresh_weight_lnRR  ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model6)
model7 <- lmer(Leaf_dry_weight_lnRR              ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model7)
model8 <- lmer(LDMC_lnRR                     	   ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model8)
model9 <- lmer(Aboveground_biomass_lnRR          ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model9)
model10 <- lmer(Belowground_biomass_lnRR         ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model10)
model11 <- lmer(Shoot_height_lnRR                ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model11)
model12 <- lmer(Plant_number_lnRR	               ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model12)
model13 <- lmer(Shoot_diameter_lnRR              ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model13)
model14 <- lmer(Leaf_C_lnRR                      ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model14)
model15 <- lmer(Leaf_N_lnRR	                     ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model15)
model16 <- lmer(Leaf_CN_lnRR                     ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model16)
model17 <- lmer(Root_C_lnRR                      ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model17)
model18 <- lmer(Root_N_lnRR	                     ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model18)
model19 <- lmer(Root_CN_lnRR                     ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model19)
model20 <- lmer(SPAD_lnRR                        ~ Latitude_ab * Region   + (1 | Group), data = data, na.action = na.omit)
Anova(model20)


# Figure 2 - Latitudinal gradient
# Leaf_thickness_lnRR
data1 <- subset(data, !is.na(Leaf_thickness_lnRR))
model <- lm(Leaf_thickness_lnRR ~ Latitude_ab, data = subset(data1))
summary(model)
model <- lm(Leaf_thickness_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Leaf_thickness_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Leaf_thickness_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Leaf_thickness_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Leaf_thickness_lnRR))
p1 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_thickness_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf thickness ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.7, 0.6), labels = scales::number_format(accuracy = 0.1))
p1 <- ggMarginal(p1,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p1)
ggsave("Figure 2 - Leaf_thickness_lnRR.pdf", p1, height = 75, width = 95, units = "mm")



# Leaf_length_lnRR
data1 <- subset(data, !is.na(Leaf_length_lnRR))
model <- lm(Leaf_length_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Leaf_length_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Leaf_length_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Leaf_length_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Leaf_length_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Leaf_length_lnRR))
p2 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_length_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "dashed") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf length ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.9, 0.9), labels = scales::number_format(accuracy = 0.1))
p2 <- ggMarginal(p2,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p2)
ggsave("Figure 2 - Leaf_length_lnRR.pdf", p2, height = 75, width = 90, units = "mm")



# Leaf_width_lnRR
data1 <- subset(data, !is.na(Leaf_width_lnRR))
model <- lm(Leaf_width_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Leaf_width_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Leaf_width_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Leaf_width_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Leaf_width_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Leaf_width_lnRR))
p3 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_width_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "dashed") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf width ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.8, 0.6), labels = scales::number_format(accuracy = 0.1))
p3 <- ggMarginal(p3,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p3)
ggsave("Figure 2 - Leaf_width_lnRR.pdf", p3, height = 75, width = 95, units = "mm")



# Leaf_area_lnRR
data1 <- subset(data, !is.na(Leaf_area_lnRR))
model <- lm(Leaf_area_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Leaf_area_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Leaf_area_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Leaf_area_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Leaf_area_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

p4 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_area_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "dashed") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf area ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-1.5, 1.4), labels = scales::number_format(accuracy = 0.1))
p4 <- ggMarginal(p4,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p4)
ggsave("Figure 2 - Leaf_area_lnRR.pdf", p4, height = 75, width = 95, units = "mm")



# SLA_lnRR
data1 <- subset(data, !is.na(SLA_lnRR))
model <- lm(SLA_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(SLA_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(SLA_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(SLA_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(SLA_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(SLA_lnRR))
p5 <- ggplot(data1, aes(x = Latitude_ab, y = SLA_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "dashed") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("SLA ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.4, 1.1), labels = scales::number_format(accuracy = 0.1))
p5 <- ggMarginal(p5,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p5)
ggsave("Figure 2 - SLA_lnRR.pdf", p5, height = 75, width = 95, units = "mm")



# Leaf_saturated_fresh_weight_lnRR
data1 <- subset(data, !is.na(Leaf_saturated_fresh_weight_lnRR))
model <- lm(Leaf_saturated_fresh_weight_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Leaf_saturated_fresh_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Leaf_saturated_fresh_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Leaf_saturated_fresh_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Leaf_saturated_fresh_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Leaf_saturated_fresh_weight_lnRR))
p6 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_saturated_fresh_weight_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "dashed") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf saturated weight ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-2, 1.6), labels = scales::number_format(accuracy = 0.1))
p6 <- ggMarginal(p6,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p6)
ggsave("Figure 2 - Leaf_saturated_fresh_weight_lnRR.pdf", p6, height = 75, width = 95, units = "mm")



# Leaf_dry_weight_lnRR
data1 <- subset(data, !is.na(Leaf_dry_weight_lnRR))
model <- lm(Leaf_dry_weight_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Leaf_dry_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Leaf_dry_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Leaf_dry_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Leaf_dry_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Leaf_dry_weight_lnRR))
p7 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_dry_weight_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf dry weight ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-2.7, 1.5), labels = scales::number_format(accuracy = 0.1))
p7 <- ggMarginal(p7,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p7)
ggsave("Figure 2 - Leaf_dry_weight_lnRR.pdf", p7, height = 75, width = 95, units = "mm")



# LDMC_lnRR
data1 <- subset(data, !is.na(LDMC_lnRR))
model <- lm(LDMC_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(LDMC_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(LDMC_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(LDMC_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(LDMC_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

p8 <- ggplot(data1, aes(x = Latitude_ab, y = LDMC_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("LDMC ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-2.5, 0.75), labels = scales::number_format(accuracy = 0.1))
p8 <- ggMarginal(p8,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p8)
ggsave("Figure 2 - LDMC_lnRR.pdf", p8, height = 75, width = 95, units = "mm")



# Aboveground_biomass_lnRR
data1 <- subset(data, !is.na(Aboveground_biomass_lnRR))
model <- lm(Aboveground_biomass_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Aboveground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Aboveground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Aboveground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Aboveground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Aboveground_biomass_lnRR))
p9 <- ggplot(data1, aes(x = Latitude_ab, y = Aboveground_biomass_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "dashed") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Aboveground biomass ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-1.9, 1.1), labels = scales::number_format(accuracy = 0.1))
p9 <- ggMarginal(p9,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p9)
ggsave("Figure 2 - Aboveground_biomass_lnRR.pdf", p9, height = 75, width = 95, units = "mm")



# Belowground_biomass_lnRR
data1 <- subset(data, !is.na(Belowground_biomass_lnRR))
model <- lm(Belowground_biomass_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Belowground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Belowground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Belowground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Belowground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Belowground_biomass_lnRR))
p10 <- ggplot(data1, aes(x = Latitude_ab, y = Belowground_biomass_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "dashed") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Belowground biomass ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-2, 1.3), labels = scales::number_format(accuracy = 0.1))
p10 <- ggMarginal(p10,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p10)
ggsave("Figure 2 - Belowground_biomass_lnRR.pdf", p10, height = 75, width = 95, units = "mm")



# Shoot_height_lnRR
data1 <- subset(data, !is.na(Shoot_height_lnRR))
model <- lm(Shoot_height_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Shoot_height_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Shoot_height_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Shoot_height_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Shoot_height_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Shoot_height_lnRR))
p11 <- ggplot(data1, aes(x = Latitude_ab, y = Shoot_height_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Shoot height ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.6, 0.6), labels = scales::number_format(accuracy = 0.1))
p11 <- ggMarginal(p11,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p11)
ggsave("Figure 2 - Shoot_height_lnRR.pdf", p11, height = 75, width = 95, units = "mm")



# Plant_number_lnRR
data1 <- subset(data, !is.na(Plant_number_lnRR))
model <- lm(Plant_number_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Plant_number_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Plant_number_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Plant_number_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Plant_number_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Plant_number_lnRR))
p12 <- ggplot(data1, aes(x = Latitude_ab, y = Plant_number_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "dashed") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Plant number ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-1.8, 1.3), labels = scales::number_format(accuracy = 0.1))
p12 <- ggMarginal(p12,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p12)
ggsave("Figure 2 - Plant_number_lnRR.pdf", p12, height = 75, width = 95, units = "mm")



# Shoot_diameter_lnRR
data1 <- subset(data, !is.na(Shoot_diameter_lnRR))
model <- lm(Shoot_diameter_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Shoot_diameter_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Shoot_diameter_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Shoot_diameter_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Shoot_diameter_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Shoot_diameter_lnRR))
p13 <- ggplot(data1, aes(x = Latitude_ab, y = Shoot_diameter_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "dashed") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Shoot diameter ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-1.3, 1.1), labels = scales::number_format(accuracy = 0.1))
p13 <- ggMarginal(p13,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p13)
ggsave("Figure 2 - Shoot_diameter_lnRR.pdf", p13, height = 75, width = 95, units = "mm")



# Leaf_C_lnRR
data1 <- subset(data, !is.na(Leaf_C_lnRR))
model <- lm(Leaf_C_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Leaf_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Leaf_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Leaf_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Leaf_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Leaf_C_lnRR))
p14 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_C_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf C ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.1, 0.035), labels = scales::number_format(accuracy = 0.1))
p14 <- ggMarginal(p14,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p14)
ggsave("Figure 2 - Leaf_C_lnRR.pdf", p14, height = 75, width = 95, units = "mm")



# Leaf_N_lnRR
data1 <- subset(data, !is.na(Leaf_N_lnRR))
model <- lm(Leaf_N_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Leaf_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Leaf_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Leaf_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Leaf_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Leaf_N_lnRR))
p15 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_N_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf N ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.9, 0.4), labels = scales::number_format(accuracy = 0.1))
p15 <- ggMarginal(p15,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p15)
ggsave("Figure 2 - Leaf_N_lnRR.pdf", p15, height = 75, width = 95, units = "mm")



# Leaf_CN_lnRR
data1 <- subset(data, !is.na(Leaf_CN_lnRR))
model <- lm(Leaf_CN_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Leaf_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Leaf_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Leaf_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Leaf_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Leaf_CN_lnRR))
p16 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_CN_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf C:N ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.5, 0.8), labels = scales::number_format(accuracy = 0.1))
p16 <- ggMarginal(p16,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p16)
ggsave("Figure 2 - Leaf_CN_lnRR.pdf", p16, height = 75, width = 95, units = "mm")



# Root_C_lnRR
data1 <- subset(data, !is.na(Root_C_lnRR))
model <- lm(Root_C_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Root_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Root_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Root_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Root_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Root_C_lnRR))
p17 <- ggplot(data1, aes(x = Latitude_ab, y = Root_C_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Root C ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.11, 0.045), labels = scales::number_format(accuracy = 0.1))
p17 <- ggMarginal(p17,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p17)
ggsave("Figure 2 - Root_C_lnRR.pdf", p17, height = 75, width = 95, units = "mm")



# Root_N_lnRR
data1 <- subset(data, !is.na(Root_N_lnRR))
model <- lm(Root_N_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Root_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Root_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Root_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Root_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Root_N_lnRR))
p18 <- ggplot(data1, aes(x = Latitude_ab, y = Root_N_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "dashed") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Root N ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.9, 0.7), labels = scales::number_format(accuracy = 0.1))
p18 <- ggMarginal(p18,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p18)
ggsave("Figure 2 - Root_N_lnRR.pdf", p18, height = 75, width = 95, units = "mm")



# Root_CN_lnRR
data1 <- subset(data, !is.na(Root_CN_lnRR))
model <- lm(Root_CN_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(Root_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(Root_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(Root_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(Root_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(Root_CN_lnRR))
p19 <- ggplot(data1, aes(x = Latitude_ab, y = Root_CN_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "dashed") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Root C:N ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.75, 0.85), labels = scales::number_format(accuracy = 0.1))
p19 <- ggMarginal(p19,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p19)
ggsave("Figure 2 - Root_CN_lnRR.pdf", p19, height = 75, width = 90, units = "mm")



# SPAD_lnRR
data1 <- subset(data, !is.na(SPAD_lnRR))
model <- lm(SPAD_lnRR ~ Latitude_ab, data = data1)
summary(model)
model <- lm(SPAD_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model)
model <- lm(SPAD_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model)
model <- lm(SPAD_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model)
model <- lm(SPAD_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model)

data1 <- subset(data, !is.na(SPAD_lnRR))
p20 <- ggplot(data1, aes(x = Latitude_ab, y = SPAD_lnRR)) +
  geom_point(aes(color = Region), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1),
              color   = "grey50",
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  scale_color_manual(values = c("America" = "#55B7E6",
                                "Asia"    = "#193E8F",
                                "Europe"  = "#E53528",
                                "Oceania" = "#F09739")) +
  scale_fill_manual(values = c("America" = "#55B7E6",
                               "Asia"    = "#193E8F",
                               "Europe"  = "#E53528",
                               "Oceania" = "#F09739"), 
                    guide = "none") +
  geom_hline(yintercept = 0, 
             color = "steelblue",
             linetype = "dashed",
             linewidth = 0.5) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("SPAD ln"~italic(RR)~"") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(limits = c(-0.75, 0.3), labels = scales::number_format(accuracy = 0.1))
p20 <- ggMarginal(p20,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p20)
ggsave("Figure 2 - SPAD_lnRR.pdf", p20, height = 75, width = 90, units = "mm")

final_plot <- plot_grid(p1, p2, p3, p4, 
                        p5, p6, p7, p8,
                        p9, p10, p11, p12,
                        p13, p14, p15, p16,
                        p17, p18, p19, p20,
                        ncol = 4,
                        align = "hv")
final_plot
ggsave("Figure 2.pdf", final_plot, height = 375, width = 390, units = "mm")










# Figure 3
setwd("/Users/yaolin/Desktop/My papers/Manuscripts/2025 - Guo - Phragmites common garden/Submission")
data <- read_excel("RawDataCommonGarden_New.xlsx")
data <- as.data.frame(data)
my_colors <- c("America"  = "#55B7E6",
               "Asia"     = "#193E8F",
               "Europe"   = "#E53528",
               "Oceania"  = "#F09739")
library(emmeans)
library(multcomp)
library(multcompView)  
library(car)
library(agricolae)
library(cowplot)



# 1. Leaf_thickness_lnRR
data1 <- subset(data, !is.na(Leaf_thickness_lnRR))
data1 <- data1[, c("Leaf_thickness_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Leaf_thickness_lnRR)
leveneTest(Leaf_thickness_lnRR ~ Region, data = data1)
fit <- aov(Leaf_thickness_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Leaf_thickness_lnRR))
data1 <- data1[, c("Leaf_thickness_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Leaf_thickness_lnRR, na.rm = TRUE),
            sdVal   = sd(Leaf_thickness_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p1 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values  = my_colors) +
  scale_color_manual(values = my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.45, 0.15), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Leaf thickness ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p1
ggsave("Figure 3 - Leaf_thickness_lnRR.pdf", p1, height=75, width=90, units="mm")



# 2. Leaf_length_lnRR
data1 <- subset(data, !is.na(Leaf_length_lnRR))
data1 <- data1[, c("Leaf_length_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Leaf_length_lnRR)
leveneTest(Leaf_length_lnRR ~ Region, data = data1)
fit <- aov(Leaf_length_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Leaf_length_lnRR))
data1 <- data1[, c("Leaf_length_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Leaf_length_lnRR, na.rm = TRUE),
            sdVal   = sd(Leaf_length_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p2 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values  = my_colors) +
  scale_color_manual(values = my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.65, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Leaf length ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p2
ggsave("Figure 3 - Leaf_length_lnRR.pdf", p2, height=75, width=90, units="mm")



# 3. Leaf_width_lnRR
data1 <- subset(data, !is.na(Leaf_width_lnRR))
data1 <- data1[, c("Leaf_width_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Leaf_width_lnRR)
leveneTest(Leaf_width_lnRR ~ Region, data = data1)
fit <- aov(Leaf_width_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Leaf_width_lnRR))
data1 <- data1[, c("Leaf_width_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Leaf_width_lnRR, na.rm = TRUE),
            sdVal   = sd(Leaf_width_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p3 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values  = my_colors) +
  scale_color_manual(values = my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.8, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Leaf width ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p3
ggsave("Figure 3 - Leaf_width_lnRR.pdf", p3, height=75, width=90, units="mm")



# 4. Leaf_area_lnRR
data1 <- subset(data, !is.na(Leaf_area_lnRR))
data1 <- data1[, c("Leaf_area_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Leaf_area_lnRR)
leveneTest(Leaf_area_lnRR ~ Region, data = data1)
fit <- aov(Leaf_area_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Leaf_area_lnRR))
data1 <- data1[, c("Leaf_area_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Leaf_area_lnRR, na.rm = TRUE),
            sdVal   = sd(Leaf_area_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p4 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values  = my_colors) +
  scale_color_manual(values = my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-1.15, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Leaf area ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p4
ggsave("Figure 3 - Leaf_area_lnRR.pdf", p4, height=75, width=90, units="mm")



# 5. SLA_lnRR
data1 <- subset(data, !is.na(SLA_lnRR))
data1 <- data1[, c("SLA_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$SLA_lnRR)
leveneTest(SLA_lnRR ~ Region, data = data1)
fit <- aov(SLA_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(SLA_lnRR))
data1 <- data1[, c("SLA_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(SLA_lnRR, na.rm = TRUE),
            sdVal   = sd(SLA_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p5 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values  = my_colors) +
  scale_color_manual(values = my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.1, 0.9), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "SLA ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p5
ggsave("Figure 3 - SLA_lnRR.pdf", p5, height=75, width=90, units="mm")



# 6. Leaf_saturated_fresh_weight_lnRR
data1 <- subset(data, !is.na(Leaf_saturated_fresh_weight_lnRR))
data1 <- data1[, c("Leaf_saturated_fresh_weight_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Leaf_saturated_fresh_weight_lnRR)
leveneTest(Leaf_saturated_fresh_weight_lnRR ~ Region, data = data1)
fit <- aov(Leaf_saturated_fresh_weight_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Leaf_saturated_fresh_weight_lnRR))
data1 <- data1[, c("Leaf_saturated_fresh_weight_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Leaf_saturated_fresh_weight_lnRR, na.rm = TRUE),
            sdVal   = sd(Leaf_saturated_fresh_weight_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p6 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-1.55, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Leaf saturated weight ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p6
ggsave("Figure 3 - Leaf_saturated_fresh_weight_lnRR.pdf", p6, height=75, width=90, units="mm")



# 7. Leaf_dry_weight_lnRR
data1 <- subset(data, !is.na(Leaf_dry_weight_lnRR))
data1 <- data1[, c("Leaf_dry_weight_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Leaf_dry_weight_lnRR)
leveneTest(Leaf_dry_weight_lnRR ~ Region, data = data1)
fit <- aov(Leaf_dry_weight_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Leaf_dry_weight_lnRR))
data1 <- data1[, c("Leaf_dry_weight_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Leaf_dry_weight_lnRR, na.rm = TRUE),
            sdVal   = sd(Leaf_dry_weight_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p7 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values  = my_colors) +
  scale_color_manual(values = my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-2.2, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Leaf dry weight ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p7
ggsave("Figure 3 - Leaf_dry_weight_lnRR.pdf", p7, height=75, width=90, units="mm")



# 8. LDMC_lnRR
data1 <- subset(data, !is.na(LDMC_lnRR))
data1 <- data1[, c("LDMC_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$LDMC_lnRR)
leveneTest(LDMC_lnRR ~ Region, data = data1)
kruskal.test(LDMC_lnRR ~ Region, data = data1)
kw <- kruskal(data1$LDMC_lnRR, data1$Region, group = TRUE, p.adj = "none")
print(kw$groups)

data1 <- subset(data, !is.na(LDMC_lnRR))
data1 <- data1[, c("LDMC_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(LDMC_lnRR, na.rm = TRUE),
            sdVal   = sd(LDMC_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p8 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-1.1, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "LDMC ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p8
ggsave("Figure 3 - LDMC_lnRR.pdf", p8, height=75, width=90, units="mm")



# 9. Aboveground_biomass_lnRR
data1 <- subset(data, !is.na(Aboveground_biomass_lnRR))
data1 <- data1[, c("Aboveground_biomass_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Aboveground_biomass_lnRR)
leveneTest(Aboveground_biomass_lnRR ~ Region, data = data1)
fit <- aov(Aboveground_biomass_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Aboveground_biomass_lnRR))
data1 <- data1[, c("Aboveground_biomass_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Aboveground_biomass_lnRR, na.rm = TRUE),
            sdVal   = sd(Aboveground_biomass_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p9 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-1.3, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Aboveground biomass ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p9
ggsave("Figure 3 - Aboveground_biomass_lnRR.pdf", p9, height=75, width=90, units="mm")



# 10. Belowground_biomass_lnRR
data1 <- subset(data, !is.na(Belowground_biomass_lnRR))
data1 <- data1[, c("Belowground_biomass_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Belowground_biomass_lnRR)
leveneTest(Belowground_biomass_lnRR ~ Region, data = data1)
fit <- aov(Belowground_biomass_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Belowground_biomass_lnRR))
data1 <- data1[, c("Belowground_biomass_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Belowground_biomass_lnRR, na.rm = TRUE),
            sdVal   = sd(Belowground_biomass_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p10 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-1.1, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Belowground biomass ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p10
ggsave("Figure 3 - Belowground_biomass_lnRR.pdf", p10, height=75, width=95, units="mm")



# 11. Shoot_height_lnRR
data1 <- subset(data, !is.na(Shoot_height_lnRR))
data1 <- data1[, c("Shoot_height_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Shoot_height_lnRR)
leveneTest(Shoot_height_lnRR ~ Region, data = data1)
fit <- aov(Shoot_height_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Shoot_height_lnRR))
data1 <- data1[, c("Shoot_height_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Shoot_height_lnRR, na.rm = TRUE),
            sdVal   = sd(Shoot_height_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p11 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.3, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Shoot height ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p11
ggsave("Figure 3 - Shoot_height_lnRR.pdf", p11, height=75, width=90, units="mm")



# 12. Plant_number_lnRR
data1 <- subset(data, !is.na(Plant_number_lnRR))
data1 <- data1[, c("Plant_number_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Plant_number_lnRR)
leveneTest(Plant_number_lnRR ~ Region, data = data1)
fit <- aov(Plant_number_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Plant_number_lnRR))
data1 <- data1[, c("Plant_number_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Plant_number_lnRR, na.rm = TRUE),
            sdVal   = sd(Plant_number_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p12 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.9, 0.2), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Plant number ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p12
ggsave("Figure 3 - Plant_number_lnRR.pdf", p12, height=75, width=90, units="mm")



# 13. Shoot_diameter_lnRR
data1 <- subset(data, !is.na(Shoot_diameter_lnRR))
data1 <- data1[, c("Shoot_diameter_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Shoot_diameter_lnRR)
leveneTest(Shoot_diameter_lnRR ~ Region, data = data1)
fit <- aov(Shoot_diameter_lnRR~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Shoot_diameter_lnRR))
data1 <- data1[, c("Shoot_diameter_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Shoot_diameter_lnRR, na.rm = TRUE),
            sdVal   = sd(Shoot_diameter_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p13 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.4, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Shoot diameter ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p13
ggsave("Figure 3 - Shoot_diameter_lnRR.pdf", p13, height=75, width=90, units="mm")



# 14. Leaf_C_lnRR
data1 <- subset(data, !is.na(Leaf_C_lnRR))
data1 <- data1[, c("Leaf_C_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Leaf_C_lnRR)
leveneTest(Leaf_C_lnRR ~ Region, data = data1)
fit <- aov(Leaf_C_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Leaf_C_lnRR))
data1 <- data1[, c("Leaf_C_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Leaf_C_lnRR, na.rm = TRUE),
            sdVal   = sd(Leaf_C_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p14 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.1, 0.01), labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Region", y = "Leaf C ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p14
ggsave("Figure 3 - Leaf_C_lnRR.pdf", p14, height=75, width=90, units="mm")



# 15. Leaf_N_lnRR
data1 <- subset(data, !is.na(Leaf_N_lnRR))
data1 <- data1[, c("Leaf_N_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Leaf_N_lnRR)
leveneTest(Leaf_N_lnRR ~ Region, data = data1)
kruskal.test(Leaf_N_lnRR ~ Region, data = data1)
kw <- kruskal(data1$Leaf_N_lnRR, data1$Region, group = TRUE, p.adj = "none")
print(kw$groups)

fit <- aov(Leaf_N_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Leaf_N_lnRR))
data1 <- data1[, c("Leaf_N_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Leaf_N_lnRR, na.rm = TRUE),
            sdVal   = sd(Leaf_N_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p15 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.7, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Leaf N ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p15
ggsave("Figure 3 - Leaf_N_lnRR.pdf", p15, height=75, width=95, units="mm")



# 16. Leaf_CN_lnRR
data1 <- subset(data, !is.na(Leaf_CN_lnRR))
data1 <- data1[, c("Leaf_CN_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Leaf_CN_lnRR)
leveneTest(Leaf_CN_lnRR ~ Region, data = data1)
kruskal.test(Leaf_CN_lnRR ~ Region, data = data1)
kw <- kruskal(data1$Leaf_CN_lnRR, data1$Region, group = TRUE, p.adj = "none")
print(kw$groups)

fit <- aov(Leaf_CN_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Leaf_CN_lnRR))
data1 <- data1[, c("Leaf_CN_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Leaf_CN_lnRR, na.rm = TRUE),
            sdVal   = sd(Leaf_CN_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p16 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.1, 0.6), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Leaf C:N ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p16
ggsave("Figure 3 - Leaf_CN_lnRR.pdf", p16, height=75, width=90, units="mm")



# 17. Root_C_lnRR
data1 <- subset(data, !is.na(Root_C_lnRR))
data1 <- data1[, c("Root_C_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Root_C_lnRR)
leveneTest(Root_C_lnRR ~ Region, data = data1)
fit <- aov(Root_C_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Root_C_lnRR))
data1 <- data1[, c("Root_C_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Root_C_lnRR, na.rm = TRUE),
            sdVal   = sd(Root_C_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p17 <- ggplot(dat_plot, aes(x    = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.1, 0.01), labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Region", y = "Root C ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p17
ggsave("Figure 3 - Root_C_lnRR.pdf", p17, height=75, width=90, units="mm")



# 18. Root_N_lnRR
data1 <- subset(data, !is.na(Root_N_lnRR))
data1 <- data1[, c("Root_N_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Root_N_lnRR)
leveneTest(Root_N_lnRR ~ Region, data = data1)
fit <- aov(Root_N_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Root_N_lnRR))
data1 <- data1[, c("Root_N_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Root_N_lnRR, na.rm = TRUE),
            sdVal   = sd(Root_N_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p18 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.55, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Root N ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p18
ggsave("Figure 3 - Root_N_lnRR.pdf", p18, height=75, width=90, units="mm")



# 19. Root_CN_lnRR
data1 <- subset(data, !is.na(Root_CN_lnRR))
data1 <- data1[, c("Root_CN_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$Root_CN_lnRR)
leveneTest(Root_CN_lnRR ~ Region, data = data1)
fit <- aov(Root_CN_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(Root_CN_lnRR))
data1 <- data1[, c("Root_CN_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(Root_CN_lnRR, na.rm = TRUE),
            sdVal   = sd(Root_CN_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p19 <- ggplot(dat_plot, aes(x    = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values=my_colors) +
  scale_color_manual(values=my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.1, 0.4), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Root C:N ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p19
ggsave("Figure 3 - Root_CN_lnRR.pdf", p19, height=75, width=95, units="mm")



# 20. SPAD_lnRR
data1 <- subset(data, !is.na(SPAD_lnRR))
data1 <- data1[, c("SPAD_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)
shapiro.test(data1$SPAD_lnRR)
leveneTest(SPAD_lnRR ~ Region, data = data1)
fit <- aov(SPAD_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

data1 <- subset(data, !is.na(SPAD_lnRR))
data1 <- data1[, c("SPAD_lnRR", "Region")]
dat_plot <- data1 %>%
  group_by(Region) %>%
  summarise(coef    = mean(SPAD_lnRR, na.rm = TRUE),
            sdVal   = sd(SPAD_lnRR, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)
p20 <- ggplot(dat_plot, aes(x     = Region, 
                           y     = coef,
                           fill  = Region, 
                           color = Region)) +
  geom_col(width = 0.5, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15,
                size  = 1,
                alpha = 0.75) +
  scale_fill_manual(values  = my_colors) +
  scale_color_manual(values = my_colors) +
  theme_minimal() +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size = 14, hjust = 0.5),
        axis.title.x       = element_text(size = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size = 14),
        axis.text          = element_text(size = 14),
        legend.position    = "none") +
  scale_y_continuous(limits = c(-0.55, 0.1), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Region", y = "Leaf SPAD ln"~italic(RR)~"") +
  geom_hline(yintercept = 0, 
             color      = "grey30",
             linetype   = "dashed",
             linewidth  = 0.5)
p20
ggsave("Figure 3 - SPAD_lnRR.pdf", p20, height=75, width=90, units="mm")

final_plot <- plot_grid(p1, p2, p3, p4, 
                        p5, p6, p7, p8,
                        p9, p10, p11, p12,
                        p13, p14, p15, p16,
                        p17, p18, p19, p20,
                        ncol = 4,
                        align = "hv")
final_plot 
ggsave("Figure 3 - new.pdf", final_plot, height = 375, width = 390, units = "mm")






Leaf_thickness_lnRR	
Leaf_length_lnRR
Leaf_width_lnRR
Leaf_area_lnRR
SLA_lnRR
Leaf_saturated_fresh_weight_lnRR
Leaf_dry_weight_lnRR
LDMC_lnRR
Aboveground_biomass_lnRR
Belowground_biomass_lnRR
Shoot_height_lnRR
Plant_number_lnRR
Shoot_diameter_lnRR
Leaf_C_lnRR
Leaf_N_lnRR
Leaf_CN_lnRR
Root_C_lnRR
Root_N_lnRR
Root_CN_lnRR
SPAD_lnRR			
