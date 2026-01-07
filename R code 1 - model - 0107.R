library(cluster)
library(dendextend)
library(MASS)
library(dplyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readxl)
library(tidyverse)
library(car)
library(lme4)
library(ggExtra)

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
ggsave('./Figure 1.pdf', p, height = 125, width = 250, units = "mm")

######################
# Q1 - GLMM - Table S1
######################

setwd("/Users/yaolin/Desktop/My papers/Manuscripts/2025 - Guo - Plasticity syndrome/Submission")
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

fit <- aov(Leaf_thickness_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

fit <- aov(Leaf_dry_weight_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

fit <- aov(LDMC_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

fit <- aov(Aboveground_biomass_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

fit <- aov(Belowground_biomass_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

fit <- aov(Shoot_height_lnRR ~ Region, data = data1)
summary(fit)
lsd <- LSD.test(fit, "Region", p.adj = "none")
print(lsd)

# Figure 2 - Latitudinal gradient
# Leaf_thickness_lnRR
data <- read.csv("data_original_lnRR.csv")
data1 <- subset(data, !is.na(Leaf_thickness_lnRR))

model1 <- lm(Leaf_thickness_lnRR ~ Latitude_ab, data = subset(data1))
summary(model1)
model2 <- lm(Leaf_thickness_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model2)
model3 <- lm(Leaf_thickness_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model3)
model4 <- lm(Leaf_thickness_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model4)
model5 <- lm(Leaf_thickness_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model5)

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



# Leaf_length_lnRR
data1 <- subset(data, !is.na(Leaf_length_lnRR))

model6 <- lm(Leaf_length_lnRR ~ Latitude_ab, data = data1)
summary(model6)
model7 <- lm(Leaf_length_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model7)
model8 <- lm(Leaf_length_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model8)
model9 <- lm(Leaf_length_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model9)
model10 <- lm(Leaf_length_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model10)

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

# Leaf_width_lnRR
data1 <- subset(data, !is.na(Leaf_width_lnRR))

model11 <- lm(Leaf_width_lnRR ~ Latitude_ab, data = data1)
summary(model11)
model12 <- lm(Leaf_width_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model12)
model13 <- lm(Leaf_width_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model13)
model14 <- lm(Leaf_width_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model14)
model15 <- lm(Leaf_width_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model15)

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

# Leaf_area_lnRR
data1 <- subset(data, !is.na(Leaf_area_lnRR))

model16 <- lm(Leaf_area_lnRR ~ Latitude_ab, data = data1)
summary(model16)
model17 <- lm(Leaf_area_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model17)
model18 <- lm(Leaf_area_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model18)
model19 <- lm(Leaf_area_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model19)
model20 <- lm(Leaf_area_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model20)

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

# SLA_lnRR
data1 <- subset(data, !is.na(SLA_lnRR))

model21 <- lm(SLA_lnRR ~ Latitude_ab, data = data1)
summary(model21)
model22 <- lm(SLA_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model22)
model23 <- lm(SLA_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model23)
model24 <- lm(SLA_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model24)
model25 <- lm(SLA_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model25)

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

# Leaf_saturated_fresh_weight_lnRR
data1 <- subset(data, !is.na(Leaf_saturated_fresh_weight_lnRR))

model26 <- lm(Leaf_saturated_fresh_weight_lnRR ~ Latitude_ab, data = data1)
summary(model26)
model27 <- lm(Leaf_saturated_fresh_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model27)
model28 <- lm(Leaf_saturated_fresh_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model28)
model29 <- lm(Leaf_saturated_fresh_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model29)
model30 <- lm(Leaf_saturated_fresh_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model30)

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

# Leaf_dry_weight_lnRR
data1 <- subset(data, !is.na(Leaf_dry_weight_lnRR))

model31 <- lm(Leaf_dry_weight_lnRR ~ Latitude_ab, data = data1)
summary(model31)
model32 <- lm(Leaf_dry_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model32)
model33 <- lm(Leaf_dry_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model33)
model34 <- lm(Leaf_dry_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model34)
model35 <- lm(Leaf_dry_weight_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model35)

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

# LDMC_lnRR
data1 <- subset(data, !is.na(LDMC_lnRR))

model36 <- lm(LDMC_lnRR ~ Latitude_ab, data = data1)
summary(model36)
model37 <- lm(LDMC_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model37)
model38 <- lm(LDMC_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model38)
model39 <- lm(LDMC_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model39)
model40 <- lm(LDMC_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model40)

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

# Aboveground_biomass_lnRR
data1 <- subset(data, !is.na(Aboveground_biomass_lnRR))

model41 <- lm(Aboveground_biomass_lnRR ~ Latitude_ab, data = data1)
summary(model41)
model42 <- lm(Aboveground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model42)
model43 <- lm(Aboveground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model43)
model44 <- lm(Aboveground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model44)
model45 <- lm(Aboveground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model45)

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



# Belowground_biomass_lnRR
data1 <- subset(data, !is.na(Belowground_biomass_lnRR))

model46 <- lm(Belowground_biomass_lnRR ~ Latitude_ab, data = data1)
summary(model46)
model47 <- lm(Belowground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model47)
model48 <- lm(Belowground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model48)
model49 <- lm(Belowground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model49)
model50 <- lm(Belowground_biomass_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model50)

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

# Shoot_height_lnRR
data1 <- subset(data, !is.na(Shoot_height_lnRR))

model51 <- lm(Shoot_height_lnRR ~ Latitude_ab, data = data1)
summary(model51)
model52 <- lm(Shoot_height_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model52)
model53 <- lm(Shoot_height_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model53)
model54 <- lm(Shoot_height_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model54)
model55 <- lm(Shoot_height_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model55)

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

# Plant_number_lnRR
data1 <- subset(data, !is.na(Plant_number_lnRR))

model56 <- lm(Plant_number_lnRR ~ Latitude_ab, data = data1)
summary(model56)
model57 <- lm(Plant_number_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model57)
model58 <- lm(Plant_number_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model58)
model59 <- lm(Plant_number_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model59)
model60 <- lm(Plant_number_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model60)

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

# Shoot_diameter_lnRR
data1 <- subset(data, !is.na(Shoot_diameter_lnRR))

model61 <- lm(Shoot_diameter_lnRR ~ Latitude_ab, data = data1)
summary(model61)
model62 <- lm(Shoot_diameter_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model62)
model63 <- lm(Shoot_diameter_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model63)
model64 <- lm(Shoot_diameter_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model64)
model65 <- lm(Shoot_diameter_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model65)

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
  scale_y_continuous(limits = c(-1, 0.3), labels = scales::number_format(accuracy = 0.1))
p13 <- ggMarginal(p13,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.3)
print(p13)

# Leaf_C_lnRR
data1 <- subset(data, !is.na(Leaf_C_lnRR))

model66 <- lm(Leaf_C_lnRR ~ Latitude_ab, data = data1)
summary(model66)
model67 <- lm(Leaf_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model67)
model68 <- lm(Leaf_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model68)
model69 <- lm(Leaf_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model69)
model70 <- lm(Leaf_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model70)

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



# Leaf_N_lnRR
data1 <- subset(data, !is.na(Leaf_N_lnRR))

model71 <- lm(Leaf_N_lnRR ~ Latitude_ab, data = data1)
summary(model71)
model72 <- lm(Leaf_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model72)
model73 <- lm(Leaf_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model73)
model74 <- lm(Leaf_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model74)
model75 <- lm(Leaf_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model75)

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

# Leaf_CN_lnRR
data1 <- subset(data, !is.na(Leaf_CN_lnRR))

model76 <- lm(Leaf_CN_lnRR ~ Latitude_ab, data = data1)
summary(model76)
model77 <- lm(Leaf_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model77)
model78 <- lm(Leaf_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model78)
model79 <- lm(Leaf_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model79)
model80 <- lm(Leaf_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model80)

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

# Root_C_lnRR
data1 <- subset(data, !is.na(Root_C_lnRR))

model81 <- lm(Root_C_lnRR ~ Latitude_ab, data = data1)
summary(model81)
model82 <- lm(Root_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model82)
model83 <- lm(Root_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model83)
model84 <- lm(Root_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model85)
model85 <- lm(Root_C_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model85)

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

# Root_N_lnRR
data1 <- subset(data, !is.na(Root_N_lnRR))

model86 <- lm(Root_N_lnRR ~ Latitude_ab, data = data1)
summary(model86)
model87 <- lm(Root_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model87)
model88 <- lm(Root_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model88)
model89 <- lm(Root_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model89)
model90 <- lm(Root_N_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model90)

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

# Root_CN_lnRR
data1 <- subset(data, !is.na(Root_CN_lnRR))

model91 <- lm(Root_CN_lnRR ~ Latitude_ab, data = data1)
summary(model91)
model92 <- lm(Root_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model92)
model93 <- lm(Root_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model93)
model94 <- lm(Root_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model94)
model95 <- lm(Root_CN_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model95)

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

# SPAD_lnRR
data1 <- subset(data, !is.na(SPAD_lnRR))

model96 <- lm(SPAD_lnRR ~ Latitude_ab, data = data1)
summary(model96)
model97 <- lm(SPAD_lnRR ~ Latitude_ab, data = subset(data1, Region == "America"))
summary(model97)
model98 <- lm(SPAD_lnRR ~ Latitude_ab, data = subset(data1, Region == "Asia"))
summary(model98)
model99 <- lm(SPAD_lnRR ~ Latitude_ab, data = subset(data1, Region == "Europe"))
summary(model99)
model100 <- lm(SPAD_lnRR ~ Latitude_ab, data = subset(data1, Region == "Oceania"))
summary(model100)

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

final_plot <- plot_grid(p1, p2, p3, p4, 
                        p5, p6, p7, p8,
                        p9, p10, p11, p12,
                        p13, p14, p15, p16,
                        p17, p18, p19, p20,
                        ncol = 4,
                        align = "hv")
final_plot
ggsave("Figure 2.pdf", final_plot, height = 375, width = 390, units = "mm")

# extract for model1 - model100
library(dplyr)
library(broom)
library(writexl)

mod_names <- paste0("model", 1:100)
mod_list  <- lapply(mod_names, get, envir = .GlobalEnv)

get_row <- function(mod) {
  s      <- summary(mod)
  coefs  <- s$coefficients
  if (!"Latitude_ab" %in% rownames(coefs)) {
    return(tibble(
      beta = NA_real_,
      SE   = NA_real_,
      t    = NA_real_,
      p    = NA_real_,
      R2   = NA_real_,
      df   = NA_integer_
    ))
  }
  co <- coefs["Latitude_ab", ]
  tibble(
    beta = unname(co["Estimate"]),
    SE   = unname(co["Std. Error"]),
    t    = unname(co["t value"]),
    p    = unname(co["Pr(>|t|)"]),
    R2   = s$r.squared,
    df   = s$df[2]
  )
}
res <- Map(
  f  = function(mn, m) {
    get_row(m) %>% mutate(model = mn, .before = 1)
  },
  mn = mod_names,
  m  = mod_list
) %>%
  bind_rows()

vars <- c("Leaf thickness",
          "Leaf length",
          "Leaf width",
          "Leaf area",
          "SLA",
          "Leaf saturated fresh weight",
          "Leaf dry weight",
          "LDMC",
          "Aboveground biomass",
          "Belowground biomass",
          "Shoot height",
          "Plant number",
          "Shoot diameter",
          "Leaf C",
          "Leaf N",
          "Leaf C:N",
          "Root C",
          "Root N",
          "Root C:N",
          "SPAD")

# model1–5:   Leaf thickness × (Overall, NA, Asia, Europe, Oceania)
# model6–10:  Leaf length   × (Overall, NA, Asia, Europe, Oceania)
conts <- c("Overall", "North America", "Asia", "Europe", "Oceania")

meta <- expand.grid(Continent = conts,
                    Variable  = vars,
                    KEEP.OUT.ATTRS = FALSE,
                    stringsAsFactors = FALSE)

meta$model <- mod_names
final_tbl <- meta %>%
  left_join(res, by = "model") %>%
  transmute(Variable,
            Continent,
            beta = round(beta, 3),
            SE   = round(SE,   3),
            t    = round(t,    3),
            p    = round(p,    3),
            R2   = round(R2,   3),
            df   = as.integer(df))

print(head(final_tbl))
write_xlsx(final_tbl, "Table_S_results.xlsx")










# Code for figure 3
library(emmeans)
library(multcomp)
library(multcompView)  
library(car)
library(agricolae)
library(cowplot)
library(agricolae)

data <- read.csv("data_original_lnRR.csv")
data <- as.data.frame(data)
my_colors <- c("America"  = "#55B7E6",
               "Asia"     = "#193E8F",
               "Europe"   = "#E53528",
               "Oceania"  = "#F09739")

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

# 13. Shoot_diameter_lnRR
data1 <- subset(data, !is.na(Shoot_diameter_lnRR))
data1 <- data1[, c("Shoot_diameter_lnRR", "Region")]
data1$Region <- as.factor(data1$Region)

shapiro.test(data1$Shoot_diameter_lnRR)
leveneTest(Shoot_diameter_lnRR ~ Region, data = data1)

kruskal.test(Shoot_diameter_lnRR ~ Region, data = data1)

kw <- kruskal(data1$Shoot_diameter_lnRR, data1$Region, group = TRUE, p.adj = "none")
print(kw$groups)

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
