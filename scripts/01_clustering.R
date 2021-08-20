library(cluster)
library(tidyverse)
library(tidylog)
library(readxl)
library(ggdendro)
library(cowplot)
library(dendextend)
library(viridis)

library(xtable)
library(knitr)
library(kableExtra)

library(assertthat)

source(file.path('..', 'R', 'process_dendro.R'))

theme_set(theme_minimal())

# Also needed: clusterSim. Not loading all of the package because it loads MASS and messes up select().
set.seed(123)

data_folder = '../data/'
output_path = str_c('../output/', '01_')
paper_path = str_c('../paper/')


# Process graduates -----

# Table that links grad AOS code with the name of the area.
AOScorrespondence = read_excel(path = file.path(data_folder, 
                                                '00_AOSDatax_2021-08-19.xlsx'), 
                               sheet = 'Key')

# Table that links university IDs and names
university_id_name = read_csv(file.path(data_folder, 
                                        '00_PaperDataAPDA_2021-08-18.csv')) %>% 
  select(University.ID = `grad uni`, 
         University.Name = `uni name`) %>% 
  distinct(University.ID, University.Name)

# Load University x Number of Grads per AOS area data

grads <- read_excel(path = file.path(data_folder, 
                                     '00_AOSDatax_2021-08-19.xlsx'), 
                    sheet = 'AOSData') %>% 
  select(University.ID = `Grad Uni`, 
         aos_code = AOS) %>% 
  left_join(AOScorrespondence, by = c('aos_code' = 'ID')) %>% 
  select(-aos_code) %>% 
  count(University.ID, AOS) %>% 
  mutate(AOS = str_c('AOS ', AOS), 
         University.ID = as.character(University.ID)) %>% 
  pivot_wider(names_from = AOS, 
              values_from = n, 
              values_fill = 0L)

# 196 universites with AOS area data, before filtering
nrow(grads)

# Remove programs with fewer than 5 grads
# Then, remove AoS areas with fewer than 7 people total (%5 of the number of programs).
grads <- grads %>%
  filter(rowSums(select(., -University.ID)) > 4) %>% 
  select(University.ID, 
         names(which(colSums(select(., -University.ID)) > 7)))

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
keyword_resp_count = read_excel(path = file.path(data_folder, 
                                                 '00_AOSDatax_2021-08-19.xlsx'), 
                                sheet = 'AOSData') %>% 
  rename(University.ID = `Grad Uni`) %>% 
  mutate(University.ID = as.character(University.ID)) %>% 
  count(University.ID, name = 'total_resp')

keywords <- read_excel(path = file.path(data_folder, 
                                        '00_AOSDatax_2021-08-19.xlsx'), 
                       sheet = 'Keyword') %>% 
  slice(-1) %>% 
  rename(University.ID = `Grad Uni/Keyword`) 

# 170 programs with keyword data, before filtering etc.
nrow(keywords)

keyword.universities <- unique(keywords$University.ID) # Save before scaling

keywords_to_drop = keywords %>% 
  pivot_longer(-University.ID, names_to = 'aos', values_to = 'value') %>% 
  filter(value != 0) %>% 
  count(aos) %>% 
  mutate(share = n / length(keyword.universities)) %>% 
  filter(share < .05)
keywords_to_drop

keywords.scaled = keywords %>%
  pivot_longer(-University.ID, names_to = 'aos', values_to = 'responses') %>% 
  filter(responses != 0L) %>% 
  anti_join(keywords_to_drop, by = 'aos') %>% 
  inner_join(keyword_resp_count, by = 'University.ID') %>% 
  mutate(share = responses / total_resp) %>% 
  select(University.ID, aos, share) %>% 
  pivot_wider(names_from = aos, values_from = share, values_fill = 0)

# Join both data sources into one frame
# As we're going to keep only universities with data on both sources, we can do a left join.
all.data <- inner_join(keywords.scaled, grads, by = c("University.ID")) 

# Determine which rows don't have complete data 
# can't use complete.cases because I don't want to eliminate universities with names I don't know
rows.with.NA <- all.data[, 3:ncol(all.data)] %>% 
  rowSums() %>% 
  is.na() 

## Assert that no rows have missing data
assert_that(!any(rows.with.NA))

# Compute distances ----

complete.data <- all.data %>% 
  filter(University.ID != "10000") %>% 
  left_join(university_id_name, by = 'University.ID') %>% 
  select(University.ID, University.Name, everything())

raw.matrix <- complete.data %>% 
  column_to_rownames('University.Name') %>%
  select(-contains("University")) %>% 
  t() %>%
  cor(method = "pearson")

raw.matrix <- 1 - raw.matrix

raw.matrix <- as.dist(raw.matrix)

assert_that(!is.null(attr(raw.matrix, 'Labels')))

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
sil.plot
ggsave(str_c(output_path, "sil_plot.png"), sil.plot, 
       height = 15, width = 25, unit ="cm")
ggsave(str_c(paper_path, 'fig_sil_plot.png'), sil.plot, 
       height = 15, width = 25, unit = 'cm')

# Plotting dendrograms and assessing determinants   ----

# Allocate dendrogram
dendrogram <- as.dendrogram(cluster.complete)
# university.names <- complete.data$University.Name %>%
#   as.character()
# university.names[is.na(university.names)] <- c("NA", "NA")
# labels(dendrogram) <- university.names[cluster.complete$order] # Order university labels and add them to dendrogram

# k = 2 ----
k2 = quietly(process_dendro)(complete.data, dendrogram, 2)$result

# How many programs per cluster
count(k2$cut_tree, cluster)

# Plot and save dendrogram
k2$plot
ggsave(str_c(output_path, "k_2.png"), k2$plot, 
       height = 15, width = 25, unit ="cm")

# k = 3 ----
k3 = quietly(process_dendro)(complete.data, dendrogram, 3)$result

# How many programs per cluster
count(k3$cut_tree, cluster)

# Plot and save dendrogram
k3$plot
ggsave(str_c(output_path, "k_3.png"), k3$plot, 
       height = 15, width = 25, unit ="cm")

# k = 6 ----
k6 = quietly(process_dendro)(complete.data, dendrogram, 6)$result

# How many programs per cluster
count(k6$cut_tree, cluster)

# Plot and save dendrogram
k6$plot
ggsave(str_c(output_path, "k_6.png"), k6$plot, 
       height = 15, width = 25, unit ="cm")

## Table showing the top labels for each cluster in k=6
k6_labels = filter(k6$top_bottom, side == 'top')
k6_labels

k6_labels_styled = k6_labels %>% 
  arrange(cluster, side, mean) %>% 
  select(-cluster, -side) %>% 
  kable(col.names = c('AOS/keyword', 'Z-score'),
        digits = 2,
        format = 'latex', 
        longtable = TRUE,
        booktabs = TRUE, 
        # table.envir = 'sidewaystable',
        label = 'k6.labels', 
        caption = 'Highest-scoring AOS and keywords for $k=6$ clusters.') %>%
  kable_styling() %>% 
  pack_rows(index=c('Cluster 1' = 5, 'Cluster 2' = 5, 
                    'Cluster 3' = 5, 'Cluster 4' = 5, 
                    'Cluster 5' = 5, 'Cluster 6' = 5))
write_file(k6_labels_styled, path = str_c(output_path, 'k6_labels.tex'))
write_file(k6_labels_styled, path = str_c(paper_path, 'tab_k6_labels.tex'))


## Combined dendograms ---
cluster_panel = plot_grid(k2$plot, k3$plot, k6$plot,
                          ncol = 1, 
                          labels = 'auto')
cluster_panel
ggsave(str_c(output_path, "cluster_panel.png"), cluster_panel, 
       height = 9, width = 6, unit ="in")
ggsave(str_c(paper_path, "fig_cluster_panel.png"), cluster_panel, 
       height = 9, width = 6, unit ="in")

## Labeled dendogram ----
# Get all universities along with their clustering in each value of k
# Note that the clusters in the paper do not have the same numbering as the clusters here;
# for readability, I numbered the clusters in the paper from left to right instead of merging order.
# Use university.and.cluster and dendrogram with labels to determine which cluster is which.
university.and.cluster <- university_id_name %>% 
  inner_join(k2$cut_tree, by = 'University.Name') %>% 
  left_join(k3$cut_tree, by = 'University.Name') %>% 
  left_join(k6$cut_tree, by = 'University.Name') %>% 
  rename(k.2 = cluster.x, k.3 = cluster.y, k.6 = cluster)
  
  # complete.data %>% 
  # left_join(k2$cut_tree, by = 'University.Name') %>% 
  # mutate(k.2 = factor(cutree(dendrogram, k = 2))) %>% 
  # mutate(k.3 = factor(cutree(dendrogram, k = 3))) %>% 
  # mutate(k.8 = factor(cutree(dendrogram, k = 8))) %>% 
  # select(University.ID, 
  #        University.Name, 
  #        k.2, k.3, k.8) %>% 
  # # Recode from left to right
  # mutate(k.3 = recode(k.3, `1` = "1", `2` = "3", `3` = "2"),
  #        k.8 = recode(k.8, `1` = "1", `2` = "3", `3` = "6",
  #                     `4` = "7", `5` = "8", 
  #                     `6` = "4", `7` = "2", `8` = "5"))

labeled_dendro = dendrogram %>% 
  color_labels(k = 8, col = plasma(8, end = .9)) %>% 
  color_branches(k = 8, col = plasma(8, end = .9)) %>% 
  set("branches_lwd", c(.8,.8,.8)) %>% 
  as.ggdend() %>% 
  ggplot(labels = TRUE, horiz = TRUE)
labeled_dendro
ggsave(str_c(output_path, "dendrogram_and_labels.pdf"), 
       labeled_dendro, width = 20, height = 30)


## Output ----
# Write university and clustering 
write_csv(university.and.cluster, file.path(data_folder, 
                                        "01_university_and_cluster.csv"))
write_rds(university.and.cluster, file.path(data_folder, 
                                        '01_university_and_cluster.Rds'))

## Table for appendix
cluster_table = university.and.cluster %>% 
  mutate_at(vars(starts_with('k')), as.character) %>% 
  select("Name" = University.Name, `K = 2` = k.2, `K = 3` = k.3, `K = 6` = k.6) %>% 
  arrange(`K = 2`, `K = 3`, `K = 6`, Name) %>% 
  # xtable(type = 'latex', 
  #        caption = 'Programs and clusters.  Programs are listed alphabetically within their $k=8$ cluster.', 
  #        label = 'tab:university_cluster')
  kable(format = 'latex', 
        longtable = TRUE, 
        booktabs = TRUE, 
        label = 'university.cluster', 
        caption = 'Programs and clusters.  Programs are listed alphabetically within their $k=6$ cluster.')

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
