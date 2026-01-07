# Worldclim - climate data

library(raster)
library(sp)
library(geodata)
library(terra)
library(ggplotify)

data <- read.csv("data_original_lnRR.csv")
bio_10m <- worldclim_global(var="bio", res=10, path=".")
data <- data[!is.na(data$Longitude) & !is.na(data$Latitude), ]
points_sv <- vect(data, geom = c("Longitude","Latitude"), crs = "EPSG:4326")
bio_values <- extract(bio_10m, points_sv)
final_data <- cbind(data, bio_values[, -1])
data_clim <- as.data.frame(final_data)

colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_1"] <- "bio_1"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_2"] <- "bio_2"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_3"] <- "bio_3"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_4"] <- "bio_4"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_5"] <- "bio_5"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_6"] <- "bio_6"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_7"] <- "bio_7"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_8"] <- "bio_8"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_9"] <- "bio_9"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_10"] <- "bio_10"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_11"] <- "bio_11"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_12"] <- "bio_12"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_13"] <- "bio_13"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_14"] <- "bio_14"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_15"] <- "bio_15"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_16"] <- "bio_16"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_17"] <- "bio_17"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_18"] <- "bio_18"
colnames(data_clim)[colnames(data_clim) == "wc2.1_10m_bio_19"] <- "bio_19"
names(data_clim)

vars1 <- paste0("bio_", 1:11)    # bio1 ~ bio11
vars2 <- paste0("bio_", 12:19)   # bio12 ~ bio19
pca_1_11 <- prcomp(data_clim[, vars1], center = TRUE, scale. = TRUE)
pca_12_19 <- prcomp(data_clim[, vars2], center = TRUE, scale. = TRUE)  
data_clim$PC1_1_11   <- pca_1_11$x[, "PC1"]
data_clim$PC1_12_19  <- pca_12_19$x[, "PC1"]
head(data_clim)
colnames(data_clim)[colnames(data_clim) == "PC1_1_11"] <- "PC1_temperature"
colnames(data_clim)[colnames(data_clim) == "PC1_12_19"] <- "PC1_precipitation"
names(data_clim)
data <- data_clim

data <- data[,c("Sample_ID","Leaf_thickness_lnRR",
                "Leaf_length_lnRR", 
                "Leaf_width_lnRR", 
                "Leaf_area_lnRR",
                "SLA_lnRR", 
                "Leaf_saturated_fresh_weight_lnRR", 
                "Leaf_dry_weight_lnRR", 
                "LDMC_lnRR",
                "Aboveground_biomass_lnRR", 
                "Belowground_biomass_lnRR", 
                "Shoot_height_lnRR",
                "Plant_number_lnRR", 
                "Shoot_diameter_lnRR", 
                "Leaf_C_lnRR", 
                "Leaf_N_lnRR", 
                "Leaf_CN_lnRR",
                "Root_C_lnRR", 
                "Root_N_lnRR", 
                "Root_CN_lnRR", 
                "SPAD_lnRR",
                "Group",
                "Region",
                "bio_1",
                "bio_2",
                "bio_3",
                "bio_4",
                "bio_5",
                "bio_6",
                "bio_7",
                "bio_8",
                "bio_9",
                "bio_10",
                "bio_11",
                "bio_12",
                "bio_13",
                "bio_14",
                "bio_15",
                "bio_16",
                "bio_17",
                "bio_18",
                "bio_19",
                "PC1_temperature",
                "PC1_precipitation")]
write.csv(data, "data_clean.csv", row.names = FALSE)

# Relationship between lnRR and climate
library(vegan)
library(pheatmap)

trait_cols <- c("Leaf_thickness_lnRR",
                "Leaf_length_lnRR", 
                "Leaf_width_lnRR", 
                "Leaf_area_lnRR",
                "SLA_lnRR", 
                "Leaf_saturated_fresh_weight_lnRR", 
                "Leaf_dry_weight_lnRR", 
                "LDMC_lnRR",
                "Aboveground_biomass_lnRR", 
                "Belowground_biomass_lnRR", 
                "Shoot_height_lnRR",
                "Plant_number_lnRR", 
                "Shoot_diameter_lnRR", 
                "Leaf_C_lnRR", 
                "Leaf_N_lnRR", 
                "Leaf_CN_lnRR",
                "Root_C_lnRR", 
                "Root_N_lnRR", 
                "Root_CN_lnRR", 
                "SPAD_lnRR")
  
env_cols <- paste0("bio_", 1:19)

n_trait <- length(trait_cols)
n_env   <- length(env_cols)
cor_mat <- matrix(NA, nrow=n_trait, ncol=n_env,
                  dimnames = list(trait_cols, env_cols))
p_mat   <- matrix(NA, nrow=n_trait, ncol=n_env,
                  dimnames = list(trait_cols, env_cols))

# cor、p-value
for(i in seq_along(trait_cols)) {
  for(j in seq_along(env_cols)) {
    x <- data_clim[[ trait_cols[i] ]]
    y <- data_clim[[ env_cols[j] ]]
    test_res <- cor.test(x, y, use="pairwise.complete.obs", method="pearson")
    cor_mat[i,j] <- test_res$estimate
    p_mat[i,j]   <- test_res$p.value
  }
}

star_mat <- matrix("",
                   nrow = n_trait, ncol = n_env,
                   dimnames = list(trait_cols, env_cols))

for(i in 1:n_trait) {
  for(j in 1:n_env) {
    pval <- p_mat[i,j]
    if(!is.na(pval)) {
      if(pval < 0.001) {
        star_mat[i,j] <- "***"
      } else if(pval < 0.01) {
        star_mat[i,j] <- "**"
      } else if(pval < 0.05) {
        star_mat[i,j] <- "*"
      } else {
        star_mat[i,j] <- ""
      }
    }
  }
}

label_mat <- matrix("", nrow=n_trait, ncol=n_env,
                    dimnames = list(trait_cols, env_cols))

for(i in 1:n_trait) {
  for(j in 1:n_env) {
    r_str <- formatC(cor_mat[i,j], digits=2, format="f")
    label_mat[i,j] <- paste0(r_str, star_mat[i,j])
  }
}

ph <- pheatmap(cor_mat,          
               cluster_rows = TRUE,
               cluster_cols = TRUE,
               display_numbers = label_mat,
               number_color = "black",
               color = colorRampPalette(c("blue","white","red"))(100), 
               main = "Correlation (Pearson) with Significance",
               fontsize = 9)
ph_gg <- as.ggplot(ph$gtable)

ggsave("Figure.pdf", ph_gg, height = 130, width = 270, units = "mm")
