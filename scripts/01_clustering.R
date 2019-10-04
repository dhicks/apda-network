## The current version of amap requires R version 3.6 or greater; DJH is currently running 3.5.1
# devtools::install_version('amap', version = '0.8-16')

library(cluster)
library(tidyverse)
library(ggdendro)
library(amap)
library(cowplot)
library(dendextend)
library(viridis)

theme_set(theme_bw())

# Also needed: clusterSim. Not loading all of the package because it loads MASS and messes up select().
set.seed(123)

data_folder = '../data/'
output_path = str_c('../output/', '01_')


# Process graduates -----

# Load the table that links grad AOS code with the name of the area.
AOScorrespondence = read_csv(file = str_c(data_folder, "00_AosCorrespondenceTable.csv"), 
                             col_names = c("Code", "Name")) %>% 
  mutate_all(factor)

# Load University x Number of Grads per AOS area data
grads <- read_csv(file = str_c(data_folder, "00_newGradData_2018-12-11.csv")) %>% 
  select(-Unknown) %>% 
  mutate(UniversityID = factor(UniversityID)) %>% 
  rename(University.ID = UniversityID)

# Remove programs with fewer than 5 grads
# Then, remove AoS areas with fewer than 7 people total (%5 of the number of programs).
grads <- grads %>%
  filter(rowSums(select(., -University.ID)) > 4) %>% 
  select(University.ID, names(which(colSums(select(., -University.ID)) > 7)))

previousAoA <- colnames(grads)

grad.universities <- grads$University.ID # Save universities so we can return them after scaling

# Transform each university's keyword count into a within-university proportion
grads <- grads %>%
  dplyr::select(-University.ID) %>%
  t() %>%
  scale(center = F, scale = colSums(., na.rm = T)) %>%  # Scale each column (now each university) by proportion of grads
  t() %>%
  replace_na(0) %>%
  as_tibble() %>% 
  add_column(University.ID = grad.universities) %>%
  select(University.ID, everything()) %>%
  droplevels()

# Process keywords ----

# Load survey data as University x Keyword occurrence count
keywords <- read_csv(file = str_c(data_folder, "00_newSurveyData_2018-12-11.csv")) %>% 
  rename(University.ID = `University ID`, University.Name = `University Name`) %>% 
  mutate(University.Name = factor(University.Name), University.ID = factor(University.ID)) 

keyword.universities <- select(keywords, University.ID, University.Name) # Save before scaling

keywords <- keywords %>%
  select(-University.ID, -University.Name) %>%  # remove to scale (non-numeric)
  select(which(colSums(.) > 8)) %>%  # Filter keywords in less than 5% of the programs
  t() %>%
  scale(center = F, scale = colSums(., na.rm = T)) %>%
  t() %>%
  as_tibble %>%
  bind_cols(keyword.universities) %>% 
  select(University.ID, University.Name, everything())

# Join both data sources into one frame
# As we're going to keep only universities with data on both sources, we can do a left join.
all.data <- left_join(keywords, grads, by = c("University.ID")) 

# Determine which rows don't have complete data 
# can't use complete.cases because I don't want to eliminate universities with names I don't know
rows.with.NA <- all.data[, 3:ncol(all.data)] %>% 
  rowSums() %>% 
  is.na() 

# Compute distances ----

# Scale each variable to further emphasize relative differences
complete.data <- all.data %>% 
  filter(!(rows.with.NA)) %>% 
  mutate_if(is.numeric, scale)

# Compute a pairwise distance matrix using "correlation" distance (1 - corr(x, y))
distance.matrix <- complete.data %>% 
  select(-University.ID, -University.Name) %>% 
  Dist(method = 'corr')

# Clusters ----

# Compute the clusters using Ward's method.
# Provide own distance matrix
cluster.complete <- agnes(x = distance.matrix, diss = TRUE, method = 'ward', metric = NULL)

print(summary(cluster.complete)$ac) # Agglomerative coefficient of the method. 

# Calculate mean Silhouette for each value of k between 2 and 100 and plot it
all.silhouettes <- map_dfr(2:100, function(x){
  sil <- cluster.complete %>%
      as.hclust() %>% 
      cutree(tree = ., k = x) %>%
    silhouette(., distance.matrix) # returns the silhouette of each pair of neighbors in the cluster.
  tibble(Mean.Sil = mean(sil[,3]), S.D.Sil = sd(sil[,3])) %>%
    return()
})
all.silhouettes$k <- c(2:100)
sil.plot <- ggplot(all.silhouettes[1:14,], aes(x = k, y = Mean.Sil, label = k)) +
  geom_line() +
  geom_label(size = 5) +
  labs(x = "Number of clusters", y = "Silhouette") + 
  theme_cowplot() + 
  scale_x_continuous()
sil.plot
ggsave(str_c(output_path, "sil_plot.png"), height = 15, width = 25, unit ="cm")

# Compute Davies-Bouldin for each value of k between 2 and 100 and plot it
all.DB <- map_dfr(2:100, function(x){
  db <- cluster.complete %>% 
      as.hclust() %>% 
    cutree(tree = ., k = x) %>% 
    clusterSim::index.DB(x = dplyr::select(complete.data, -University.ID, -University.Name), cl = ., d = distance.matrix) %>% 
    .$`DB`
  db <- tibble(Davies.Bouldin = db) %>% return()
})
all.DB$k <- c(2:100)
db.plot <- ggplot(all.DB[1:14,], aes(x = k, y = Davies.Bouldin, label = k)) +
  geom_line() +
  geom_label(size = 5) +
  labs(x = "Number of clusters", y = "Davies-Bouldin Index") + 
  theme_cowplot() + 
  scale_x_continuous()
db.plot
ggsave(str_c(output_path, "db_plot.png"), height = 15, width = 25, unit ="cm")

# Plot both in a panel
plot_grid(sil.plot, db.plot, labels = c("A", "B"))
ggsave(str_c(output_path, "measures_panel.png"), height = 15, width = 25, unit ="cm")

# Plot both at the same time 
all.DB %>% 
  left_join(all.silhouettes) %>%
  filter(k < 10) %>% 
  mutate(k = factor(k)) %>% 
  mutate_if(is.numeric, log) %>% 
  ggplot(aes(x = Davies.Bouldin, y = Mean.Sil, label = k)) + 
  geom_label() + 
  theme_cowplot()

# Plotting dendrograms and assessing determinants   ----

# Allocate dendrogram
dendrogram <- as.dendrogram(cluster.complete)
university.names <- complete.data$University.Name %>%
  as.character()
university.names[is.na(university.names)] <- c("NA", "NA")
labels(dendrogram) <- university.names[cluster.complete$order] # Order university labels and add them to dendrogram

# k = 2
# Get the mean z-score for each variable in each cluster to get determinants
k.2 <- complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 2))) %>%
  group_by(cluster) %>% 
  summarise_if(is.numeric, mean)

# Get the top 10 mean z-score variables per cluster
k.2 %>% 
  gather("variable", "value", African:`AOS Value (General)`) %>% 
  group_by(cluster) %>% 
  top_n(10, value) %>% 
  arrange(cluster, desc(value))

# How many programs per cluster
complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 2))) %>% 
  group_by(cluster) %>% 
  tally()

# Plot and save dendrogram with color marking different clusters
k.2.plot <- dendrogram %>% 
  color_labels(k = 2, col = plasma(2, end = .9)) %>% 
  color_branches(k = 2, col = plasma(2, end = .9)) %>% 
  set("branches_lwd", c(.8,.8,.8)) %>% 
  as.ggdend() %>% 
  ggplot(labels = FALSE)
k.2.plot
ggsave(str_c(output_path, "k_2.png"), height = 15, width = 25, unit ="cm")

# k = 3

k.3 <- complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 3))) %>%
  mutate_if(is.numeric, scale) %>% 
  group_by(cluster) %>% 
  summarise_if(is.numeric, mean)
complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 3))) %>% 
  group_by(cluster) %>% 
  tally()
# Determinants
k.3 %>% 
  gather("variable", "value", African:`AOS Value (General)`) %>% 
  group_by(cluster) %>% 
  top_n(10, value) %>% 
  arrange(cluster, desc(value))
k.3.plot <- dendrogram %>% 
  color_labels(k = 3, col = plasma(3, end = .9)) %>% 
  color_branches(k = 3, col = plasma(3, end = .9)) %>% 
  set("branches_lwd", c(.8,.8,.8)) %>% 
  as.ggdend() %>% 
  ggplot(labels = FALSE)
k.3.plot
ggsave(str_c(output_path, "k_3.png"), height = 15, width = 25, unit ="cm")

# k = 8
k.8 <- complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 8))) %>%
  mutate_if(is.numeric, scale) %>% 
  group_by(cluster) %>% 
  summarise_if(is.numeric, mean)
complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 8))) %>% 
  group_by(cluster) %>% 
  tally()
k.8 %>% 
  gather("variable", "value", African:`AOS Value (General)`) %>% 
  group_by(cluster) %>% 
  top_n(10, value) %>% 
  arrange(cluster, desc(value))
k.8.plot <- dendrogram %>% 
  color_labels(k = 8, col = plasma(8, end = .9)) %>% 
  color_branches(k = 8, col = plasma(8, end = .9)) %>% 
  set("branches_lwd", c(.8,.8,.8)) %>% 
  as.ggdend() %>% 
  ggplot(labels = FALSE, nodes = TRUE)
k.8.plot
ggsave(str_c(output_path, "k_8.png"), height = 15, width = 25, unit ="cm")

plot_grid(k.2.plot, k.3.plot, k.8.plot, ncol = 1, labels = c("A", "B", "C"))
ggsave(str_c(output_path, "cluster_panel.png"), height = 10, width = 6, unit ="in")

# Get all universities along with their clustering in each value of k
# Note that the clusters in the paper do not have the same numbering as the clusters here;
# for readability, I numbered the clusters in the paper from left to right instead of merging order.
# Use university.and.cluster and dendrogram with labels to determine which cluster is which.
university.and.cluster <- complete.data %>% 
  mutate(k.2 = factor(cutree(dendrogram, k = 2))) %>% 
  mutate(k.3 = factor(cutree(dendrogram, k = 3))) %>% 
  mutate(k.8 = factor(cutree(dendrogram, k = 8))) %>% 
  select(University.ID, #University.Name, 
         k.2, k.3, k.8)
dendrogram %>% 
  color_labels(k = 8, col = plasma(8, end = .9)) %>% 
  color_branches(k = 8, col = plasma(8, end = .9)) %>% 
  set("branches_lwd", c(.8,.8,.8)) %>% 
  as.ggdend() %>% 
  ggplot(labels = TRUE, horiz = TRUE)
ggsave(str_c(output_path, "dendrogram_and_labels.pdf"), width = 20, height = 30)

# Write university and clustering
# write_csv(university.and.cluster, str_c(data_folder, "01_university_and_cluster.csv"))
write_rds(university.and.cluster, str_c(data_folder, '01_university_and_cluster.Rds'))

