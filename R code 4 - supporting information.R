library(cluster)
library(dendextend)
library(MASS)
library(dplyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readxl)
library(ggExtra)
library(car)
library(lme4)

setwd("/Users/yaolin/Desktop/My papers/Manuscripts/2025 - Guo - Phragmites common garden/Submission")
data <- read_excel("data_imp.xlsx")
data <- as.data.frame(data)

model <- lmer(Leaf_thickness              ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Leaf_length                 ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Leaf_width	                ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Leaf_area                   ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(SLA	                        ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Leaf_saturated_fresh_weight ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Leaf_dry_weight             ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(LDMC                      	~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Aboveground_biomass         ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Belowground_biomass         ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Shoot_height                ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Plant_number	              ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Shoot_diameter              ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Leaf_C                      ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Leaf_N	                    ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Leaf_CN                     ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Root_C                      ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Root_N	                    ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Root_CN                     ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)
model <- lmer(SPAD                        ~ Latitude_ab * Garden_ID   + (1 | Continent), data = data, na.action = na.omit)
Anova(model)

# Leaf_thickness
data1 <- subset(data, !is.na(Leaf_thickness))
model <- lm(Leaf_thickness ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Leaf_thickness ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p1 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_thickness)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf thickness (mm)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p1 <- ggMarginal(p1,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p1)



# Leaf_length
data1 <- subset(data, !is.na(Leaf_length))
model <- lm(Leaf_length ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Leaf_length ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p2 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_length)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf length (cm)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p2 <- ggMarginal(p2,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p2)



# Leaf_width
data1 <- subset(data, !is.na(Leaf_width))
model <- lm(Leaf_width ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Leaf_width ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p3 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_width)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf width (cm)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p3 <- ggMarginal(p3,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p3)



# Leaf_area
data1 <- subset(data, !is.na(Leaf_area))
model <- lm(Leaf_area ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Leaf_area ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p4 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_area)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf area (cm2)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
p4 <- ggMarginal(p4,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p4)



# SLA
data1 <- subset(data, !is.na(SLA))
model <- lm(SLA ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(SLA ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p5 <- ggplot(data1, aes(x = Latitude_ab, y = SLA)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("SLA (cm2/g)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
p5 <- ggMarginal(p5,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p5)



# Leaf_saturated_fresh_weight
data1 <- subset(data, !is.na(Leaf_saturated_fresh_weight))
model <- lm(Leaf_saturated_fresh_weight ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Leaf_saturated_fresh_weight ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p6 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_saturated_fresh_weight)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf saturated weight (g)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p6 <- ggMarginal(p6,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p6)



# Leaf_dry_weight
data1 <- subset(data, !is.na(Leaf_dry_weight))
model <- lm(Leaf_dry_weight ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Leaf_dry_weight ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p7 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_dry_weight)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf dry weight (g)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p7 <- ggMarginal(p7,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p7)



# LDMC
data1 <- subset(data, !is.na(LDMC))
model <- lm(LDMC ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(LDMC ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p8 <- ggplot(data1, aes(x = Latitude_ab, y = LDMC)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "dashed") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("LDMC (%)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p8 <- ggMarginal(p8,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p8)



# Aboveground_biomass
data1 <- subset(data, !is.na(Aboveground_biomass))
model <- lm(Aboveground_biomass ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Aboveground_biomass ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p9 <- ggplot(data1, aes(x = Latitude_ab, y = Aboveground_biomass)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Aboveground biomass (g)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
p9 <- ggMarginal(p9,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p9)



# Belowground_biomass
data1 <- subset(data, !is.na(Belowground_biomass))
model <- lm(Belowground_biomass ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Belowground_biomass ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p10 <- ggplot(data1, aes(x = Latitude_ab, y = Belowground_biomass)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "dashed") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Belowground biomass (g)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
p10 <- ggMarginal(p10,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p10)



# Shoot_height
data1 <- subset(data, !is.na(Shoot_height))
model <- lm(Shoot_height ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Shoot_height ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p11 <- ggplot(data1, aes(x = Latitude_ab, y = Shoot_height)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "dashed") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Shoot height (cm)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
p11 <- ggMarginal(p11,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p11)



# Plant_number
data1 <- subset(data, !is.na(Plant_number))
model <- lm(Plant_number ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Plant_number ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p12 <- ggplot(data1, aes(x = Latitude_ab, y = Plant_number)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Plant number (indiv./m2)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
p12 <- ggMarginal(p12,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p12)



# Shoot_diameter
data1 <- subset(data, !is.na(Shoot_diameter))
model <- lm(Shoot_diameter ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Shoot_diameter ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p13 <- ggplot(data1, aes(x = Latitude_ab, y = Shoot_diameter)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Shoot diameter (mm)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p13 <- ggMarginal(p13,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p13)



# Leaf_C
data1 <- subset(data, !is.na(Leaf_C))
data$Leaf_C <- as.numeric(as.character(data$Leaf_C))
model <- lm(Leaf_C ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Leaf_C ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p14 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_C)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "dashed") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf C (mg/g)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p14 <- ggMarginal(p14,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p14)



# Leaf_N
data1 <- subset(data, !is.na(Leaf_N))
data1$Leaf_N <- as.numeric(as.character(data1$Leaf_N))
model <- lm(Leaf_N ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Leaf_N ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p15 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_N)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "dashed") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf N (mg/g)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p15 <- ggMarginal(p15,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p15)



# Leaf_CN
data1 <- subset(data, !is.na(Leaf_CN))
data1$Leaf_N <- as.numeric(as.character(data1$Leaf_CN))
model <- lm(Leaf_CN ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Leaf_CN ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p16 <- ggplot(data1, aes(x = Latitude_ab, y = Leaf_CN)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "dashed") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Leaf C:N") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
p16 <- ggMarginal(p16,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p16)



# Root_C
data1 <- subset(data, !is.na(Root_C))
data1$Root_C <- as.numeric(as.character(data1$Root_C))
model <- lm(Root_C ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Root_C ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p17 <- ggplot(data1, aes(x = Latitude_ab, y = Root_C)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "dashed") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "dashed") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Root C (mg/g)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))
p17 <- ggMarginal(p17,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p17)



# Root_N
data1 <- subset(data, !is.na(Root_N))
data1$Root_N <- as.numeric(as.character(data1$Root_N))
model <- lm(Root_N ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Root_N ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p18 <- ggplot(data1, aes(x = Latitude_ab, y = Root_N)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "dashed") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Root N (mg/g)") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p18 <- ggMarginal(p18,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p18)


# Root_CN
data1 <- subset(data, !is.na(Root_CN))
data1$Root_CN <- as.numeric(as.character(data1$Root_CN))
model <- lm(Root_CN ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(Root_CN ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p19 <- ggplot(data1, aes(x = Latitude_ab, y = Root_CN)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "dashed") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Root C:N") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p19 <- ggMarginal(p19,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
print(p19)



# SPAD
data1 <- subset(data, !is.na(SPAD))
model <- lm(SPAD ~ Latitude_ab, data = subset(data1, Garden_ID == "Shanghai"))
summary(model)
model <- lm(SPAD ~ Latitude_ab, data = subset(data1, Garden_ID == "Qingdao"))
summary(model)

p20 <- ggplot(data1, aes(x = Latitude_ab, y = SPAD)) +
  geom_point(aes(color = Garden_ID), size = 3.5, shape = 16, stroke = 0.25, alpha = 0.65) +
  geom_smooth(data = subset(data1, Garden_ID == "Shanghai"),
              method  = "lm",
              formula = y ~ x,
              color   = "#C24E44",
              fill    = "#C24E44",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "solid") +
  geom_smooth(data = subset(data1, Garden_ID == "Qingdao"),
              method  = "lm",
              formula = y ~ x,
              color   = "#387EB8",
              fill    = "#387EB8",
              se      = TRUE,
              alpha   = 0.2,
              linetype = "dashed") +
  scale_color_manual(values = c("Qingdao"  = "#387EB8",
                                "Shanghai" = "#C24E44")) +
  scale_fill_manual(values = c("Qingdao"  = "#387EB8",
                               "Shanghai" = "#C24E44"),
                    guide = "none") +
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
        legend.position    = "none") +
  xlab("Absolute latitude (° N)") +
  ylab("Chlorophyll") +
  scale_x_continuous(limits = c(20, 60), labels = scales::number_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
p20 <- ggMarginal(p20,
                 type        = "density",
                 margins     = "y",
                 groupColour = TRUE,
                 groupFill   = TRUE,
                 alpha       = 0.2)
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
ggsave("Figure S1.pdf", final_plot, height = 375, width = 390, units = "mm")
##############################













#
my_colors <- c("Qingdao"  = "#387EB8",
               "Shanghai" = "#C24E44")

# 1. Leaf_thickness_lnRR
data1 <- subset(data, !is.na(Leaf_thickness))
data1 <- data1[, c("Leaf_thickness", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Leaf_thickness)
result <- t.test(Leaf_thickness ~ Garden_ID, data = data, var.equal = FALSE)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Leaf_thickness, na.rm = TRUE),
            sdVal   = sd(Leaf_thickness, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p1 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 0.25), labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Common garden", y = "Leaf thickness (mm)")
p1



# 2. Leaf_length_lnRR
data1 <- subset(data, !is.na(Leaf_length))
data1 <- data1[, c("Leaf_length", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Leaf_length)
result <- t.test(Leaf_length ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Leaf_length ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Leaf_length, na.rm = TRUE),
            sdVal   = sd(Leaf_length, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p2 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 30), labels = scales::number_format(accuracy = 1)) +
  labs(x = "Common garden", y = "Leaf_length (cm)")
p2



# 3. Leaf_width
data1 <- subset(data, !is.na(Leaf_width))
data1 <- data1[, c("Leaf_width", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Leaf_width)
result <- t.test(Leaf_width ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Leaf_width ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Leaf_width, na.rm = TRUE),
            sdVal   = sd(Leaf_width, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p3 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 1.9), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Common garden", y = "Leaf width (cm)")
p3



# 4. Leaf_area
data1 <- subset(data, !is.na(Leaf_area))
data1 <- data1[, c("Leaf_area", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Leaf_area)
result <- t.test(Leaf_area ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Leaf_area ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Leaf_area, na.rm = TRUE),
            sdVal   = sd(Leaf_area, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p4 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 30), labels = scales::number_format(accuracy = 1)) +
  labs(x = "Common garden", y = "Leaf area (cm2)")
p4



# 5. SLA
data1 <- subset(data, !is.na(SLA))
data1 <- data1[, c("SLA", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$SLA)
result <- t.test(SLA ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(SLA ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(SLA, na.rm = TRUE),
            sdVal   = sd(SLA, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p5 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 250), labels = scales::number_format(accuracy = 1)) +
  labs(x = "Common garden", y = "SLA (cm2/g)")
p5



# 6. Leaf_saturated_fresh_weight
data1 <- subset(data, !is.na(Leaf_saturated_fresh_weight))
data1 <- data1[, c("Leaf_saturated_fresh_weight", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Leaf_saturated_fresh_weight)
result <- t.test(Leaf_saturated_fresh_weight ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Leaf_saturated_fresh_weight ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Leaf_saturated_fresh_weight, na.rm = TRUE),
            sdVal   = sd(Leaf_saturated_fresh_weight, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p6 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 0.6), labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Common garden", y = "Leaf saturated weight (g)")
p6



# 7. Leaf_dry_weight
data1 <- subset(data, !is.na(Leaf_dry_weight))
data1 <- data1[, c("Leaf_dry_weight", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Leaf_dry_weight)
result <- t.test(Leaf_dry_weight ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Leaf_dry_weight ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Leaf_dry_weight, na.rm = TRUE),
            sdVal   = sd(Leaf_dry_weight, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p7 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 0.25), labels = scales::number_format(accuracy = 0.01)) +
  labs(x = "Common garden", y = "Leaf dry weight (g)")
p7



# 8. LDMC
data1 <- subset(data, !is.na(LDMC))
data1 <- data1[, c("LDMC", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$LDMC)
result <- t.test(LDMC ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(LDMC ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(LDMC, na.rm = TRUE),
            sdVal   = sd(LDMC, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p8 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 50), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Common garden", y = "LDMC (%)")
p8



# 9. Aboveground_biomass
data1 <- subset(data, !is.na(Aboveground_biomass))
data1 <- data1[, c("Aboveground_biomass", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Aboveground_biomass)
result <- t.test(Aboveground_biomass ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Aboveground_biomass ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Aboveground_biomass, na.rm = TRUE),
            sdVal   = sd(Aboveground_biomass, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p9 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 150), labels = scales::number_format(accuracy = 1)) +
  labs(x = "Common garden", y = "Aboveground biomass (g)")
p9



# 10. Belowground_biomass
data1 <- subset(data, !is.na(Belowground_biomass))
data1 <- data1[, c("Belowground_biomass", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Belowground_biomass)
result <- t.test(Belowground_biomass ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Belowground_biomass ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Belowground_biomass, na.rm = TRUE),
            sdVal   = sd(Belowground_biomass, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p10 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 220), labels = scales::number_format(accuracy = 1)) +
  labs(x = "Common garden", y = "Belowground biomass (g)")
p10



# 11. Shoot_height
data1 <- subset(data, !is.na(Shoot_height))
data1 <- data1[, c("Shoot_height", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Shoot_height)
result <- t.test(Shoot_height ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Shoot_height ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Shoot_height, na.rm = TRUE),
            sdVal   = sd(Shoot_height, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p11 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 160), labels = scales::number_format(accuracy = 1)) +
  labs(x = "Common garden", y = "Shoot height (cm)")
p11



# 12. Plant_number
data1 <- subset(data, !is.na(Plant_number))
data1 <- data1[, c("Plant_number", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Plant_number)
result <- t.test(Plant_number ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Plant_number ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Plant_number, na.rm = TRUE),
            sdVal   = sd(Plant_number, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p12 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 120), labels = scales::number_format(accuracy = 1)) +
  labs(x = "Common garden", y = "Plant number (indiv./m2)")
p12



# 13. Shoot_diameter
data1 <- subset(data, !is.na(Shoot_diameter))
data1 <- data1[, c("Shoot_diameter", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Shoot_diameter)
result <- t.test(Shoot_diameter ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Shoot_diameter ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Shoot_diameter, na.rm = TRUE),
            sdVal   = sd(Shoot_diameter, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p13 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 6.5), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Common garden", y = "Shoot diameter (mm)")
p13



# 14. Leaf_C
data1 <- subset(data, !is.na(Leaf_C))
data1$Leaf_C <- as.numeric(as.character(data1$Leaf_C))
data1 <- data1[, c("Leaf_C", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Leaf_C)
result <- t.test(Leaf_C ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Leaf_C ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Leaf_C, na.rm = TRUE),
            sdVal   = sd(Leaf_C, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p14 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 60), labels = scales::number_format(accuracy = 1)) +
  labs(x = "Common garden", y = "Leaf C (mg/g)")
p14



# 15. Leaf_N
data1 <- subset(data, !is.na(Leaf_N))
data1$Leaf_N <- as.numeric(as.character(data1$Leaf_N))
data1 <- data1[, c("Leaf_N", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Leaf_N)
result <- t.test(Leaf_N ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Leaf_N ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Leaf_N, na.rm = TRUE),
            sdVal   = sd(Leaf_N, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p15 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 4.5), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Common garden", y = "Leaf N (mg/g)")
p15



# 16. Leaf_CN
data1 <- subset(data, !is.na(Leaf_CN))
data1 <- data1[, c("Leaf_CN", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Leaf_CN)
result <- t.test(Leaf_CN ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Leaf_CN ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Leaf_CN, na.rm = TRUE),
            sdVal   = sd(Leaf_CN, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p16 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 30), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Common garden", y = "Leaf C:N")
p16



# 17. Root_C
data1 <- subset(data, !is.na(Root_C))
data1 <- data1[, c("Root_C", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Root_C)
result <- t.test(Root_C ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Root_C ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Root_C, na.rm = TRUE),
            sdVal   = sd(Root_C, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p17 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 60), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Common garden", y = "Root C (mg/g)")
p17



# 18. Root_N
data1 <- subset(data, !is.na(Root_N))
data1 <- data1[, c("Root_N", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Root_N)
result <- t.test(Root_N ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Root_N ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Root_N, na.rm = TRUE),
            sdVal   = sd(Root_N, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p18 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 1.5), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Common garden", y = "Root N (mg/g)")
p18



# 19. Root_CN
data1 <- subset(data, !is.na(Root_CN))
data1 <- data1[, c("Root_CN", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$Root_CN)
result <- t.test(Root_CN ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(Root_CN ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(Root_CN, na.rm = TRUE),
            sdVal   = sd(Root_CN, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p19 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 80), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Common garden", y = "Root_CN")
p19



# 20. SPAD
data1 <- subset(data, !is.na(SPAD))
data1 <- data1[, c("SPAD", "Garden_ID")]
data1$Garden_ID <- as.factor(data1$Garden_ID)
shapiro.test(data1$SPAD)
result <- t.test(SPAD ~ Garden_ID, data = data, var.equal = FALSE)
print(result)
result <- wilcox.test(SPAD ~ Garden_ID, data = data1)
print(result)

dat_plot <- data1 %>%
  group_by(Garden_ID) %>%
  summarise(coef    = mean(SPAD, na.rm = TRUE),
            sdVal   = sd(SPAD, na.rm = TRUE),
            n       = n(),
            .groups = "drop") %>%
  mutate(se    = sdVal / sqrt(n),
         lower = coef - se,
         upper = coef + se)

p20 <- ggplot(dat_plot, aes(x     = Garden_ID, 
                           y     = coef,
                           fill  = Garden_ID, 
                           color = Garden_ID)) +
  geom_col(width = 0.4, alpha = 0.75, color = NA) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.1,
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
  scale_y_continuous(limits = c(0, 50), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = "Common garden", y = "Chlorophyll")
p20


final_plot <- plot_grid(p1, p2, p3, p4, 
                        p5, p6, p7, p8,
                        p9, p10, p11, p12,
                        p13, p14, p15, p16,
                        p17, p18, p19, p20,
                        ncol = 4,
                        align = "hv")
final_plot
ggsave("Figure S3.pdf", final_plot, height = 375, width = 390, units = "mm", limitsize = FALSE)





#####
library(Hmisc)
setwd("/Users/yaolin/Desktop/My papers/Manuscripts/2025 - Guo - Phragmites common garden/Submission")
data <- read_excel("data_imp.xlsx")
data <- as.data.frame(data)
traits_data <- data[, c("Leaf_thickness", "Leaf_length", "Leaf_width", "Leaf_area",
                        "SLA", "LDMC", "Aboveground_biomass", "Belowground_biomass",
                        "Shoot_height", "Plant_number", "Shoot_diameter", "Leaf_C",
                        "Leaf_N", "Leaf_CN", "Root_C", "Root_N", "Root_CN", "SPAD")]
cor_result <- rcorr(as.matrix(traits_data), type = "pearson")
cor_mat <- cor_result$r
p_mat <- cor_result$P
library(corrplot)
corrplot(cor_mat, method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE,
         number.cex = 0.6, 
         tl.cex = 0.7, 
         cl.cex = 0.7)  


####
setwd("/Users/yaolin/Desktop/My papers/Manuscripts/2025 - Guo - Phragmites common garden/Submission")
data <- read.csv("data_imp_filled_lnRR.csv")
data <- as.data.frame(data)
traits_data <- data[, c("Leaf_thickness_lnRR", "Leaf_length_lnRR", "Leaf_width_lnRR", "Leaf_area_lnRR",
                        "SLA_lnRR", "LDMC_lnRR", "Aboveground_biomass_lnRR", "Belowground_biomass_lnRR",
                        "Shoot_height_lnRR", "Plant_number_lnRR", "Shoot_diameter_lnRR", "Leaf_C_lnRR",
                        "Leaf_N_lnRR", "Leaf_CN_lnRR", "Root_C_lnRR", "Root_N_lnRR", "Root_CN_lnRR", "SPAD_lnRR")]
cor_result <- rcorr(as.matrix(traits_data), type = "pearson")

cor_mat <- cor_result$r
p_mat <- cor_result$P
library(corrplot)

corrplot(cor_mat, method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE,
         number.cex = 0.6,
         tl.cex = 0.7,
         cl.cex = 0.7)



