library(naniar)
library(missForest)
library(readr) 
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(fmsb)
library(dplyr)
library(corrplot)
library(factoextra)
library(fmsb)
library(RColorBrewer)
library(terra)
library(tidyverse)
library(tidyverse)
library(MASS)
library(caret)
library(ranger)
library(doParallel)
library(parallel)
library(doParallel)
library(caret)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(patchwork)
library(terra)
library(tidyterra)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(cluster)
library(dendextend)
library(MASS)
library(dplyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(readxl)
library(sf)
library(broom)
library(ggplot2)
library(dplyr)
library(maps)
library(geodata)
library(terra)
library(dendextend)
library(tidyverse)
library(readxl)

setwd("/Users/yaolin/Desktop/My papers/Manuscripts/2025 - Guo - Phragmites common garden/Submission")
data_imp <- read_excel("data_imp.xlsx")
data_imp <- as.data.frame(data_imp)
data_imp

id_col      <- "Sample_ID"    
factor_cols <- c("Garden_ID", "Continent")
num_cols    <- setdiff(names(data_imp), c(id_col, factor_cols))

quiet_num <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(
    parse_number(as.character(x), na = c("", "NA", "P"))
  )
}

df_prep <- data_imp %>% 
  mutate(across(all_of(num_cols), quiet_num)) %>% 
  mutate(across(all_of(factor_cols), as.factor)) %>%
  dplyr::select(-all_of(id_col))       
set.seed(123) 

mf_out <- missForest(df_prep,
                     ntree   = 2500,
                     maxiter = 1000,
                     verbose = TRUE)
data_imp_filled <- bind_cols(data_imp[id_col], mf_out$ximp)
write_csv(data_imp_filled, "data_imp_filled.csv")
print(mf_out$OOBerror)
print(data_imp_filled)

##########
trait_cols <- c("Leaf_thickness","Leaf_length","Leaf_width","Leaf_area","SLA",
                "Leaf_saturated_fresh_weight","Leaf_dry_weight","LDMC",
                "Aboveground_biomass","Belowground_biomass","Shoot_height",
                "Plant_number","Shoot_diameter",
                "Leaf_C","Leaf_N","Leaf_CN",
                "Root_C","Root_N","Root_CN","SPAD")
df_mean <- data_imp_filled %>%                 
  group_by(Sample_ID, Garden_ID) %>%           
  summarise(across(all_of(trait_cols), \(x) mean(x, na.rm = TRUE)),
            Latitude  = first(Latitude),
            Longitude = first(Longitude),
            Continent = first(Continent),
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
  dplyr::select(Sample_ID, Latitude, Longitude, Continent, ends_with("_lnRR"))
write_csv(df_lnRR, "data_imp_filled_lnRR.csv")
data_imp_lnRR <- df_lnRR

# Figure 5
lnrr_cols <- names(data_imp_lnRR)[str_detect(names(data_imp_lnRR), "lnRR")]
cluster_data <- data_imp_lnRR[, lnrr_cols]
range01 <- function(x) {(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}
cluster_data <- as.data.frame(lapply(cluster_data, range01))
cor_matrix <- cor(cluster_data, use = "pairwise.complete.obs", method = "pearson")
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.cex = 0.8,
         number.cex = 0.7)

cluster_data$Sample_ID <- data_imp_lnRR$Sample_ID
cluster_data <- na.omit(cluster_data)
rownames(cluster_data) <- cluster_data$Sample_ID
cluster_data$Sample_ID <- NULL
dist_matrix <- dist(cluster_data, method = "euclidean")  
hc <- hclust(dist_matrix, method = "ward.D2")
plot(hc, cex = 0.6, hang = -1, main = "Dendrogram of lnRR traits (Ward + Euclidean)")
gap_stat <- clusGap(cluster_data, FUN = hcut, nstart = 25, K.max = 10, B = 500)
fviz_gap_stat(gap_stat) + ggplot2::theme_minimal()

# Figure for Scree plot for factor selection
gap_df <- data.frame(cluster = 1:nrow(gap_stat$Tab),
                     gap     = gap_stat$Tab[, "gap"],
                     SE      = gap_stat$Tab[, "SE.sim"])

p <- ggplot(gap_df, aes(x = cluster, y = gap)) +
  geom_line(color = "#2c7fb8", linewidth = 1) +
  geom_point(color = "#2c7fb8", size = 3) +
  geom_errorbar(aes(ymin = gap - SE, ymax = gap + SE), 
                width = 0.2, linewidth = 0.8, color = "#2c7fb8") +
  geom_vline(xintercept = which.max(gap_df$gap), linetype = "dashed", color = "steelblue", linewidth = 1) +
  scale_x_continuous(breaks = 1:10, labels = 1:10) +
  labs(title = "Gap Statistic: Optimal number of clusters",
       x = "Number of clusters (k)",
       y = "Gap statistic") +
  theme_classic(base_size = 14) +
  theme(axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))
p
ggsave("Figure S.pdf", p, height = 100, width = 120, units = "mm", limitsize = FALSE)

k_optimal <- 4
hc_clusters <- cutree(hc, k = k_optimal)
table(hc_clusters)

cluster_data_final <- cluster_data %>%  mutate(cluster = factor(hc_clusters))
dend <- as.dendrogram(hc)
dend_colored <- color_branches(dend, k = k_optimal)
dend_colored <- set(dend_colored, "labels_cex", 0.7)
plot(dend_colored, main = "Hierarchical Clustering with k = 4", ylab = "Height")
rect.hclust(hc, k = k_optimal, border = 2:5)

hc_clusters <- cutree(hc, k = 4)
names(hc_clusters) <- rownames(cluster_data)
hc_clusters 
cluster_df <- data.frame(Sample_ID = names(hc_clusters), Cluster = hc_clusters)
cluster_df <- cluster_df[order(cluster_df$Cluster), ]
print(cluster_df)

num_vars <- setdiff(names(cluster_data_final), "cluster")
data_radar_raw <- cluster_data_final %>%
  group_by(cluster) %>%
  summarise(across(all_of(num_vars), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()
rownames(data_radar_raw) <- data_radar_raw[["cluster"]]
data_radar_raw <- data_radar_raw[, !names(data_radar_raw) %in% "cluster"]

max_min <- as.data.frame(
  apply(data_radar_raw, 2, function(x) c(max(x, na.rm = TRUE), min(x, na.rm = TRUE)))
)
colnames(max_min) <- colnames(data_radar_raw)
rownames(max_min) <- c("Max", "Min")

radar_data <- rbind(max_min, data_radar_raw)
num_clusters <- nrow(radar_data) - 2   # eg: 4
color_border <- c("red","blue","green","purple")[1:num_clusters]
color_fill   <- c("#FF000030","#0000FF30","#00FF0030")[1:num_clusters]
desired_order <- c("Leaf_thickness_lnRR",
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
names(radar_data)
radar_data <- radar_data[, desired_order]
radarchart(radar_data,
           axistype = 1,            
           seg = 4,                   
           caxislabels = c(-1, -0.5, 0, 0.5, 1),
           pcol = color_border,    
           pfcol = color_fill,      
           plwd = 2,               
           plty = 1,                 
           cglcol = "grey80",      
           cglty  = 1,
           cglwd  = 0.8,
           vlcex  = 0.8,             
           title  = "Radar Chart with 0 as Middle Ring")

legend("topright",
       legend = rownames(radar_data)[3:nrow(radar_data)],
       col = color_border,
       lty = 1,
       lwd = 2,
       cex = 0.8,
       bty = "n")

cluster_assign <- tibble(Sample_ID = rownames(cluster_data), cluster    = factor(hc_clusters)) 
data_cluster_map <- data_imp_lnRR %>%
  mutate(Sample_ID = as.character(Sample_ID)) %>% 
  left_join(cluster_assign, by = "Sample_ID")
table(is.na(data_cluster_map$cluster))

# Cluster map
data1 <- subset(data_cluster_map, !is.na(cluster))
data1 <- subset(data_cluster_map, !is.na(Latitude))
world <- map_data("world")
p <- ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group), 
               fill  = "grey90", 
               color = "grey75") +
  geom_point(data = data1, 
             aes(x = Longitude, y = Latitude, color = cluster),
             size = 3, 
             shape = 16,
             alpha = 0.7) +
  scale_color_manual(values = c("1" = "#FF0000",
                                "2" = "#0000FF",
                                "3" = "#00FF00",
                                "4" = "#984EA3")) +
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

ggsave('./Figure 5b - map.pdf', p, height = 125, width = 250, units = "mm")

write.csv(data_cluster_map,
          file = "data_cluster_map.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")


### Figure S8 LDA
lda_data <- cluster_data_final
lda_data$cluster <- factor(lda_data$cluster)

head(lda_data)
lda_model <- lda(cluster ~ ., data = lda_data)
print(lda_model)

lda_pred <- predict(lda_model)
lda_scores <- as.data.frame(lda_pred$x)
lda_scores$cluster <- lda_data$cluster
head(lda_scores)

p_lda <- ggplot(lda_scores, aes(x = LD1, y = LD2, color = cluster, fill = cluster)) +
  geom_point(alpha = 0.8, size = 3, shape = 16, stroke = 0.6) +
  stat_ellipse(level = 0.95, linewidth = 0.8, linetype = "dashed", alpha = 0.3) +
  scale_color_manual(values = c("1" = "#FF0000", "2" = "#0000FF", 
                                "3" = "#00FF00", "4" = "#984EA3")) +
  scale_fill_manual(values = c("1" = "#FF000050", "2" = "#0000FF50", 
                               "3" = "#00FF0050", "4" = "#984EA350")) +
  theme_classic(base_size = 14) +
  theme(axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 13),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) +
  labs(title = "Linear Discriminant Analysis (LDA)",
       x = "LD1",
       y = "LD2",
       color = "Cluster",
       fill = "Cluster")
print(p_lda)
ggsave("Figure_LDA.pdf", p_lda, height = 100, width = 135, units = "mm", limitsize = FALSE)



library(geodata)
bio_10m <- worldclim_global(var = "bio", res = 10, path = ".")
data_cluster_map <- data_cluster_map %>% filter(!is.na(Longitude) & !is.na(Latitude))
pts <- vect(data_cluster_map,
            geom = c("Longitude", "Latitude"),
            crs  = "EPSG:4326")
bio_vals <- terra::extract(bio_10m, pts)
bio_vals <- bio_vals[,-1]
names(bio_vals) <- str_replace(names(bio_vals), "wc2.1_10m_bio_", "bio_")
data_cluster_map <- data_cluster_map %>% mutate(row_id = row_number())
bio_vals <- bio_vals %>% mutate(row_id = data_cluster_map$row_id)
data_cluster_map <- data_cluster_map %>% 
  dplyr::left_join(bio_vals, by = "row_id") %>% 
  dplyr::select(-row_id)   

write.csv(data_cluster_map,
          file = "data_cluster_map.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)
traits <- c("Leaf_thickness_lnRR", 
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

clims  <- paste0("bio_", 1:19)
data_cluster_map <- subset(data_cluster_map, !is.na(cluster))
data_cluster_map$cluster <- as.factor(data_cluster_map$cluster)      
x <- data_cluster_map[, traits]
nzv <- nearZeroVar(x)      
if(length(nzv) > 0) x <- x[, -nzv]
combo <- findLinearCombos(x)
if(length(combo$remove) > 0) x <- x[, -combo$remove]
traits_clean <- colnames(x)
X_scaled <- scale(data_cluster_map[, traits_clean])
dat_lda <- cbind(cluster = data_cluster_map$cluster,
                 as.data.frame(X_scaled))
lda_fit <- lda(cluster ~ ., data = dat_lda)
lda_scores <- as.data.frame(predict(lda_fit)$x)   # LD1, LD2
data_ld    <- cbind(data_cluster_map[, clims], lda_scores, cluster = data_cluster_map$cluster)
names(data_cluster_map)[1:100] 
set.seed(123)
ctrl <- trainControl(method = "cv",
                     number = 10,
                     allowParallel = TRUE)
grid <- expand.grid(mtry          = seq(3, 15, 4),
                    splitrule     = "variance",
                    min.node.size = c(5, 10, 20))
rf_LD1 <- train(LD1 ~ .,
                data        = data_ld[, c(clims, "LD1")],
                method      = "ranger",
                tuneGrid    = grid,
                trControl   = ctrl,
                importance  = "impurity",
                preProcess  = c("center", "scale"))
rf_LD2 <- train(LD2 ~ .,
                data        = data_ld[, c(clims, "LD2")],
                method      = "ranger",
                tuneGrid    = grid,
                trControl   = ctrl,
                importance  = "impurity",
                preProcess  = c("center", "scale"))
print(rf_LD1$results %>% arrange(RMSE) %>% head(1))
print(rf_LD2$results %>% arrange(RMSE) %>% head(1))

PA_distribution <- read.csv("PA_distribution.csv")
PA_distribution <- as.data.frame(PA_distribution)
PA_distribution <- PA_distribution %>% filter(!is.na(Longitude) & !is.na(Latitude))
pts <- vect(PA_distribution,
            geom = c("Longitude", "Latitude"),
            crs  = "EPSG:4326")
bio_vals <- terra::extract(bio_10m, pts)[,-1]
names(bio_vals) <- str_replace(names(bio_vals), "wc2.1_10m_bio_", "bio_")
PA_clim <- bind_cols(PA_distribution, bio_vals)
PA_distribution <- PA_distribution %>% mutate(row_id = row_number())
bio_vals <- bio_vals %>% mutate(row_id = PA_distribution$row_id)
data1 <- PA_distribution %>%
  dplyr::left_join(bio_vals, by = "row_id") %>%
  dplyr::select(-row_id)
clims <- paste0("bio_", 1:19)
data1 <- data1 %>% mutate(row_id = row_number())
data1_complete <- data1 %>%  filter(if_all(all_of(clims), ~ !is.na(.)))
LD1_pred <- predict(rf_LD1, newdata = data1_complete %>% dplyr::select(all_of(clims)))
LD2_pred <- predict(rf_LD2, newdata = data1_complete %>% dplyr::select(all_of(clims)))
pred_tbl <- data1_complete %>% 
  dplyr::select(row_id) %>% 
  mutate(LD1 = LD1_pred,
         LD2 = LD2_pred)
data1_ld <- data1 %>% 
  dplyr::left_join(pred_tbl, by = "row_id") %>% 
  dplyr::select(-row_id)
centroids <- aggregate(lda_scores, by = list(Group = data_cluster_map$cluster), FUN = mean)
x        <- as.matrix(data1_ld[, c("LD1", "LD2")])          # n × 2  numeric
cent_mat <- as.matrix(centroids[, c("LD1", "LD2")])         # 3 × 2  numeric
Sigma    <- cov(lda_scores[, c("LD1", "LD2")])              # 2 × 2  numeric
dmat <- matrix(NA_real_, nrow = nrow(x), ncol = nrow(cent_mat))
for (j in 1:nrow(cent_mat)) {
  dmat[, j] <- mahalanobis(x, center = cent_mat[j, ], cov = Sigma)
}
colnames(dmat) <- centroids$cluster
grp_pred <- centroids$Group[max.col(-dmat)]
dens <- exp(-0.5 * dmat)
prob <- dens / rowSums(dens)
colnames(prob) <- paste0("Pr_", centroids$Group)
data1_out <- data1_ld %>%
  mutate(Group_pred = grp_pred) %>%
  bind_cols(as.data.frame(prob))
write.csv(data1_out, "Phragmites_group_randomForest.csv", row.names = FALSE)

df <- read.csv("Phragmites_group_randomForest.csv") %>%
  mutate(maxPr = pmax(Pr_1, Pr_2, Pr_3, Pr_4)) %>%
  filter(maxPr >= 0.3)
stopifnot(nrow(df) > 0)
cols <- c("1" = "#FF0000",
          "2" = "#0000FF",
          "3" = "#00FF00",
          "4" = "#984EA3")

world_df <- map_data("world")   # long / lat / group / region / …

df$Group_pred <- factor(df$Group_pred, levels = c(1, 2, 3, 4))

p <- ggplot() +
  geom_polygon(data = world_df,
               aes(long, lat, group = group),
               fill  = "grey90",
               color = "grey75") +
  geom_point(data = df[df$Group_pred == 1, ],
             aes(Longitude, Latitude, color = factor(Group_pred)),
             size  = 2.5, shape = 16, alpha = 0.5) +
  geom_point(data = df[df$Group_pred == 2, ],
             aes(Longitude, Latitude, color = factor(Group_pred)),
             size  = 2.5, shape = 16, alpha = 0.5) +
  geom_point(data = df[df$Group_pred == 3, ],
             aes(Longitude, Latitude, color = factor(Group_pred)),
             size  = 2.5, shape = 16, alpha = 0.5) +
  geom_point(data = df[df$Group_pred == 4, ],
             aes(Longitude, Latitude, color = factor(Group_pred)),
             size  = 2.5, shape = 16, alpha = 0.5) +
  scale_color_manual(values = cols, name = "Group") +
  xlab("Longitude (°)") +
  ylab("Latitude (°)") +
  coord_quickmap() +
  theme(
    axis.text       = element_text(size = 14),
    axis.title.x    = element_text(size = 14),
    axis.title.y    = element_text(size = 14),
    legend.title    = element_text(size = 14),
    legend.text     = element_text(size = 14),
    panel.border    = element_rect(color = "black", fill = NA, size = 1),
    panel.background= element_blank(),
    panel.grid.major= element_line(colour = "grey70", linetype = "dashed", linewidth = 0.3),
    panel.grid.minor= element_line(colour = "grey85", linetype = "dotted", linewidth = 0.2),
    legend.position = "none") +
  scale_x_continuous(breaks = c(-100, 0, 100))
print(p)

ggsave('./Figure 4c.png', p, height = 125, width = 250, units = "mm",   dpi = 1000)


## Prediction to 2081-2100
bio_future <- cmip6_world(model = "ACCESS-ESM1-5",
                          ssp   = "585",
                          time  = "2081-2100",
                          var   = "bioc",
                          res   = 10,
                          path  = "./future_climate")

print(bio_future)
PA_distribution <- read.csv("PA_distribution.csv") %>% 
  filter(!is.na(Longitude) & !is.na(Latitude))
pts_future <- vect(PA_distribution,
                   geom = c("Longitude", "Latitude"),
                   crs  = "EPSG:4326")
bio_vals_future <- terra::extract(bio_future, pts_future)[,-1]
names(bio_vals_future) <- paste0("bio_", 1:19)
PA_distribution_future <- bind_cols(PA_distribution, bio_vals_future)
head(PA_distribution_future)

PA_distribution_future <- PA_distribution_future %>%
  mutate(row_id = row_number()) %>%
  filter(if_all(starts_with("bio_"), ~ !is.na(.)))

LD1_pred_future <- predict(rf_LD1, newdata = PA_distribution_future %>% dplyr::select(starts_with("bio_")))
LD2_pred_future <- predict(rf_LD2, newdata = PA_distribution_future %>% dplyr::select(starts_with("bio_")))
pred_tbl_future <- PA_distribution_future %>%
  dplyr::select(row_id) %>%
  mutate(LD1 = LD1_pred_future,
         LD2 = LD2_pred_future)
data_future_ld <- PA_distribution_future %>%
  left_join(pred_tbl_future, by = "row_id") %>%
  dplyr::select(-row_id)
x_future <- as.matrix(data_future_ld[, c("LD1", "LD2")])
dmat_future <- matrix(NA_real_, nrow = nrow(x_future), ncol = nrow(cent_mat))
for (j in 1:nrow(cent_mat)) {
  dmat_future[, j] <- mahalanobis(x_future, center = cent_mat[j, ], cov = Sigma)
}
colnames(dmat_future) <- centroids$Group
grp_pred_future <- centroids$Group[max.col(-dmat_future)]

dens_future <- exp(-0.5 * dmat_future)
prob_future <- dens_future / rowSums(dens_future)
colnames(prob_future) <- paste0("Pr_", centroids$Group)
data_future_out <- data_future_ld %>%
  mutate(Group_pred = grp_pred_future) %>%
  bind_cols(as.data.frame(prob_future))
write.csv(data_future_out, "Phragmites_group_future_2081_2100.csv", row.names = FALSE)

world_df <- map_data("world")
data_future_out <- data_future_out %>%
  mutate(maxPr = pmax(Pr_1, Pr_2, Pr_3, Pr_4)) %>%
  filter(maxPr >= 0.3)
cols  <- c("1" = "#FF0000",
           "2" = "#0000FF",
           "3" = "#00FF00",
           "4" = "#984EA3")
p_future <- ggplot() +
  geom_polygon(data = world_df,
               aes(long, lat, group = group),
               fill  = "grey90",
               color = "grey75") +
  geom_point(data = data_future_out[data_future_out$Group_pred == 1, ],
             aes(Longitude, Latitude, color = factor(Group_pred)),
             size  = 2.5, shape = 16, alpha = 0.5) +
  geom_point(data = data_future_out[data_future_out$Group_pred == 2, ],
             aes(Longitude, Latitude, color = factor(Group_pred)),
             size  = 2.5, shape = 16, alpha = 0.5) +
  geom_point(data = data_future_out[data_future_out$Group_pred == 3, ],
             aes(Longitude, Latitude, color = factor(Group_pred)),
             size  = 2.5, shape = 16, alpha = 0.5) +
  geom_point(data = data_future_out[data_future_out$Group_pred == 4, ],
             aes(Longitude, Latitude, color = factor(Group_pred)),
             size  = 2.5, shape = 16, alpha = 0.5) +
  scale_color_manual(values = cols, name = "Group") +
  xlab("Longitude (°)") +
  ylab("Latitude (°)") +
  coord_quickmap() +
  theme(axis.text       = element_text(size = 14),
        axis.title.x    = element_text(size = 14),
        axis.title.y    = element_text(size = 14),
        legend.title    = element_text(size = 14),
        legend.text     = element_text(size = 14),
        panel.border    = element_rect(color = "black", fill = NA, size = 1),
        panel.background= element_blank(),
        panel.grid.major= element_line(colour = "grey70", linetype = "dashed", linewidth = 0.3),
        panel.grid.minor= element_line(colour = "grey85", linetype = "dotted", linewidth = 0.2),
        legend.position = "none") +
  scale_x_continuous(breaks = c(-100, 0, 100))
print(p_future)

ggsave('./Figure 4d - future climate 2081-2100.png', p_future, height = 125, width = 250, units = "mm",   dpi = 1000)

