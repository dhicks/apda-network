library(cluster)
library(tidyverse)
library(ggdendro)
library(cowplot)
library(dendextend)
library(viridis)

library(xtable)
library(knitr)
library(kableExtra)

theme_set(theme_minimal())

# Also needed: clusterSim. Not loading all of the package because it loads MASS and messes up select().
set.seed(123)

data_folder = '../data/'
output_path = str_c('../output/', '01_')
paper_path = str_c('../paper/')


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

# 283 universites with AOS area data, before filtering
nrow(grads)

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

# 170 programs with keyword data, before filtering etc.
nrow(keywords)

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

complete.data <- all.data %>% 
  filter(!(rows.with.NA)) %>% 
  filter(University.ID != "10000") 

raw.matrix <- complete.data %>% 
  column_to_rownames('University.ID') %>%
  select(-contains("University")) %>% 
  t() %>%
  cor(method = "pearson")

raw.matrix <- 1 - raw.matrix

raw.matrix <- as.dist(raw.matrix)

## assert_that(!is.null(attr(raw.matrix, 'Labels')))

# # Compute a pairwise distance matrix using "correlation" distance (1 - corr(x, y))
# distance.matrix <- complete.data %>% 
#   select(-University.ID, -University.Name) %>% 
#   Dist(method = 'corr')

distance.matrix <- raw.matrix


# Clusters ----

# Compute the clusters using Ward's method.
# Provide own distance matrix
cluster.complete <- agnes(x = distance.matrix, diss = TRUE, method = 'ward', metric = NULL)

print(summary(cluster.complete)$ac) # Agglomerative coefficient of the method. 

# Calculate mean Silhouette for each value of k between 2 and 100 and plot it
silhouette_k = function(k, clusters, distances) {
  ## Coerce clusters to hclust, cut with k clusters, and calculate silhouette index
  clusters %>% 
    as.hclust() %>% 
    cutree(k = k) %>% 
    silhouette(distances) %>% 
    as('matrix') %>% 
    as_tibble()
}

all.silhouettes = 2:10 %>% 
  set_names() %>% 
  map_dfr(silhouette_k, cluster.complete, distance.matrix, 
          .id = 'k') %>% 
  mutate(k = as.integer(k))

sil.plot = ggplot(all.silhouettes, 
                  aes(x = k, y = sil_width, 
                      label = k, 
                      group = k)) +
  stat_summary(fun = mean, 
               geom = 'line', 
               group = 1L) +
  stat_summary(fun = mean, geom = 'label', size = 5) +
  # ggbeeswarm::geom_beeswarm(alpha = .1)
  labs(x = "Number of clusters", y = "Silhouette")
# sil.plot
ggsave(str_c(output_path, "sil_plot.png"), sil.plot, 
       height = 15, width = 25, unit ="cm")
ggsave(str_c(paper_path, 'fig_sil_plot.png'), sil.plot, 
       height = 15, width = 25, unit = 'cm')

# Plotting dendrograms and assessing determinants   ----

# Allocate dendrogram
dendrogram <- as.dendrogram(cluster.complete)
university.names <- complete.data$University.Name %>%
  as.character()
university.names[is.na(university.names)] <- c("NA", "NA")
labels(dendrogram) <- university.names[cluster.complete$order] # Order university labels and add them to dendrogram

# k = 2 ----
# Get the mean z-score for each variable in each cluster to get traits of each cluster
means.k.2 <- complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 2))) %>%
  mutate_if(is.numeric, scale) %>%
  group_by(cluster) %>% 
  summarise_if(is.numeric, mean)

vars.k.2 <- complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 2))) %>%
  mutate_if(is.numeric, scale) %>%
  group_by(cluster) %>% 
  summarise_if(is.numeric, sd)

# Get the top 10 mean z-score variables per cluster

all.means.k.2 <- means.k.2 %>% 
  gather("variable", "mean", African:`AOS Value (General)`) 
top.means.k.2 <- all.means.k.2 %>% 
  group_by(cluster) %>% 
  top_n(5, mean) %>% 
  arrange(cluster, desc(mean))

# Get the bottom 10 mean z-score variables per cluster
bottom.means.k.2 <- all.means.k.2 %>% 
  group_by(cluster) %>% 
  top_n(-5, mean) %>% 
  arrange(cluster, mean)
top.vars.k.2 <- vars.k.2 %>% 
  gather("variable", "cv", African:`AOS Value (General)`) %>% 
  group_by(cluster) %>%
  left_join(all.means.k.2)

# How many programs per cluster
complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 2))) %>% 
  group_by(cluster) %>% 
  tally()

# Plot and save dendrogram with color marking different clusters
k.2.plot <- dendrogram %>% 
  color_labels(k = 2, col = plasma(4, end = .9)) %>% 
  color_branches(k = 2, col = plasma(4, end = .9)) %>% 
  set("branches_lwd", c(.8,.8,.8)) %>% 
  as.ggdend() %>% 
  ggplot(labels = FALSE)
# k.2.plot
ggsave(str_c(output_path, "k_2.png"), k.2.plot, 
       height = 15, width = 25, unit ="cm")

# k = 3 ----

means.k.3 <- complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 3))) %>%
  mutate_if(is.numeric, scale) %>%
  group_by(cluster) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate(cluster = recode(cluster, `1` = "1", `2` = "3", `3` = "2")) # Recode from left to right


vars.k.3 <- complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 3))) %>%
  mutate_if(is.numeric, scale) %>%
  group_by(cluster) %>% 
  summarise_if(is.numeric, sd) %>% 
  mutate(cluster = recode(cluster, `1` = "1", `2` = "3", `3` = "2")) # Recode from left to right

complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 3))) %>% 
  mutate(cluster = recode(cluster, `1` = "1", `2` = "3", `3` = "2")) %>% # Recode from left to right
  group_by(cluster) %>% 
  tally()

# Get the top 10 mean z-score variables per cluster

all.means.k.3 <- means.k.3 %>% 
  gather("variable", "mean", African:`AOS Value (General)`)
top.means.k.3 <- all.means.k.3 %>% 
  group_by(cluster) %>% 
  top_n(5, mean) %>% 
  arrange(cluster, desc(mean)) 

# Get the bottom 10 mean z-score variables per cluster
bottom.means.k.3 <- all.means.k.3 %>% 
  group_by(cluster) %>% 
  top_n(-5, mean) %>% 
  arrange(cluster, mean)
top.vars.k.3 <- vars.k.3 %>% 
  gather("variable", "cv", African:`AOS Value (General)`) %>% 
  group_by(cluster) %>%
  left_join(all.means.k.3)

k.3.plot <- dendrogram %>% 
  color_labels(k = 3, col = plasma(3, end = .9)) %>% 
  color_branches(k = 3, col = plasma(3, end = .9)) %>% 
  set("branches_lwd", c(.8,.8,.8)) %>% 
  as.ggdend() %>% 
  ggplot(labels = FALSE)

# k.3.plot
ggsave(str_c(output_path, "k_3.png"), k.3.plot, 
       height = 15, width = 25, unit ="cm")

# k = 8 ----

means.k.8 <- complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 8))) %>%
  mutate_if(is.numeric, scale) %>%
  group_by(cluster) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate(cluster = recode(cluster, `1` = "1", `2` = "3", `3` = "6",
                          `4` = "7", `5` = "8", `6` = "4", `7` = "2", `8` = "5")) # Recode from left to right

vars.k.8 <- complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 8))) %>%
  mutate_if(is.numeric, scale) %>%
  group_by(cluster) %>% 
  summarise_if(is.numeric, sd) %>% 
  mutate(cluster = recode(cluster, `1` = "1", `2` = "3", `3` = "6",
                          `4` = "7", `5` = "8", `6` = "4", `7` = "2", `8` = "5")) # Recode from left to right

complete.data %>% 
  mutate(cluster = factor(cutree(dendrogram, k = 8))) %>%
  mutate(cluster = recode(cluster, `1` = "1", `2` = "3", `3` = "6",
                          `4` = "7", `5` = "8", `6` = "4", `7` = "2", `8` = "5")) %>% # Recode from left to right
  group_by(cluster) %>% 
  tally()


# Get the top 10 mean z-score variables per cluster

all.means.k.8 <- means.k.8 %>% 
  gather("variable", "mean", African:`AOS Value (General)`) 
top.means.k.8 <- all.means.k.8 %>% 
  group_by(cluster) %>% 
  top_n(10, mean) %>% 
  arrange(cluster, desc(mean))

# Get the bottom 10 mean z-score variables per cluster
bottom.means.k.8 <- all.means.k.8 %>% 
  group_by(cluster) %>% 
  top_n(-10, mean) %>% 
  arrange(cluster, mean)
top.vars.k.8 <- vars.k.8 %>% 
  gather("variable", "cv", African:`AOS Value (General)`) %>% 
  left_join(all.means.k.8) %>% 
  group_by() # %>% 
# mutate(cluster =  recode(cluster, `1` = "1", `2` = "2", `3` = "7", `4` = "8", `5` = "5", `6` = "6", `7` = "3", `8` = "4"))

## Table showing the top and bottom 3 labels for each cluster in k=8
k8_labels = list(bottom = {bottom.means.k.8 %>% 
    group_by(cluster) %>% 
    top_n(-3, mean)}, 
    top = {top.means.k.8 %>% 
        group_by(cluster) %>% 
        top_n(3, mean)}) %>% 
  bind_rows(.id = 'side') %>% 
  ungroup() %>% 
  mutate(side = fct_relevel(side, 'top', 'bottom'), 
         cluster = as.character(cluster)) %>% 
  arrange(cluster, side, mean) %>% 
  select(cluster, side, variable, mean)

k8_labels_styled = k8_labels %>% 
  arrange(cluster, side, mean) %>% 
  select(-cluster) %>% 
  kable(col.names = c('Side', 'Trait', 'Z-score'),
        digits = 2,
        format = 'latex', 
        longtable = TRUE,
        booktabs = TRUE, 
        # table.envir = 'sidewaystable',
        label = 'k8.labels', 
        caption = 'Positive and negative traits (AOS and keywords) for $k=8$ clusters. Side indicates whether traits come from the top or bottom of the z-scores for each cluster.') %>% 
  kable_styling() %>% 
  pack_rows(index=c('Cluster 1' = 6, 'Cluster 2' = 6, 
                    'Cluster 3' = 6, 'Cluster 4' = 6, 
                    'Cluster 5' = 6, 'Cluster 6' = 6, 
                    'Cluster 7' = 6, 'Cluster 8' = 6))
write_file(k8_labels_styled, path = str_c(output_path, 'k8_labels.tex'))
write_file(k8_labels_styled, path = str_c(paper_path, 'tab_k8_labels.tex'))


k.8.plot <- dendrogram %>% 
  color_labels(k = 8, col = plasma(8, end = .9)) %>% 
  color_branches(k = 8, col = plasma(8, end = .9)) %>% 
  set("branches_lwd", c(.8,.8,.8)) %>% 
  as.ggdend() %>% 
  ggplot(labels = FALSE, nodes = TRUE) 
# k.8.plot
ggsave(str_c(output_path, "k_8.png"), k.8.plot, 
       height = 15, width = 25, unit ="cm")


## Combined dendograms ---
cluster_panel = plot_grid(k.2.plot, k.3.plot, k.8.plot, ncol = 1, 
                          labels = c("A", "B", "C"))
ggsave(str_c(output_path, "cluster_panel.png"), cluster_panel, 
       height = 9, width = 6, unit ="in")
ggsave(str_c(paper_path, "fig_cluster_panel.png"), cluster_panel, 
       height = 9, width = 6, unit ="in")

## Labeled dendogram ----
# Get all universities along with their clustering in each value of k
# Note that the clusters in the paper do not have the same numbering as the clusters here;
# for readability, I numbered the clusters in the paper from left to right instead of merging order.
# Use university.and.cluster and dendrogram with labels to determine which cluster is which.
university.and.cluster <- complete.data %>% 
  mutate(k.2 = factor(cutree(dendrogram, k = 2))) %>% 
  mutate(k.3 = factor(cutree(dendrogram, k = 3))) %>% 
  mutate(k.8 = factor(cutree(dendrogram, k = 8))) %>% 
  select(University.ID, 
         University.Name, 
         k.2, k.3, k.8) %>% 
  # Recode from left to right
  mutate(k.3 = recode(k.3, `1` = "1", `2` = "3", `3` = "2"),
         k.8 = recode(k.8, `1` = "1", `2` = "3", `3` = "6",
                      `4` = "7", `5` = "8", 
                      `6` = "4", `7` = "2", `8` = "5"))
labeled_dendro = dendrogram %>% 
  color_labels(k = 8, col = plasma(8, end = .9)) %>% 
  color_branches(k = 8, col = plasma(8, end = .9)) %>% 
  set("branches_lwd", c(.8,.8,.8)) %>% 
  as.ggdend() %>% 
  ggplot(labels = TRUE, horiz = TRUE)
# labeled_dendro
ggsave(str_c(output_path, "dendrogram_and_labels.pdf"), 
       labeled_dendro, width = 20, height = 30)


## Output ----
# Write university and clustering 
write_csv(university.and.cluster, str_c(data_folder, "01_university_and_cluster.csv"))
write_rds(university.and.cluster, str_c(data_folder, '01_university_and_cluster.Rds'))

## Table for appendix
cluster_table = university.and.cluster %>% 
  mutate_at(vars(starts_with('k')), as.character) %>% 
  select("Name" = University.Name, `K = 2` = k.2, `K = 3` = k.3, `K = 8` = k.8) %>% 
  arrange(`K = 2`, `K = 3`, `K = 8`, Name) %>% 
  # xtable(type = 'latex', 
  #        caption = 'Programs and clusters.  Programs are listed alphabetically within their $k=8$ cluster.', 
  #        label = 'tab:university_cluster')
  kable(format = 'latex', 
        longtable = TRUE, 
        booktabs = TRUE, 
        label = 'university.cluster', 
        caption = 'Programs and clusters.  Programs are listed alphabetically within their $k=8$ cluster.')
  
# print.xtable(cluster_table, tabular.environment = 'longtable',
#              floating = FALSE,
#              file = str_c(output_path, 'university_and_cluster.tex'))
# print.xtable(cluster_table, tabular.environment = 'longtable',
#              floating = FALSE,
#              file = str_c(paper_path, 'tab_university_and_cluster.tex'))

write_file(cluster_table, 
           path = str_c(output_path, 'university_and_cluster.tex'))
write_file(cluster_table, 
           path = str_c(paper_path, 'tab_university_and_cluster.tex'))

## Pairwise distance matrix (used in 05 for MDS)
write_rds(distance.matrix, str_c(data_folder, '01_dist_matrix.Rds'))

## Reproducibility ----
sessionInfo()
