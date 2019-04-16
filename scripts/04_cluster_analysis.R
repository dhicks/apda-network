library(tidyverse)
library(ggforce)
library(cowplot)

theme_set(theme_minimal())


#+ load_data -----
## This should be identical to the load_data block in 03; 
## which probably means 03 should save that dataset
data_folder = '../data/'
output_folder = '../plots/'

cluster_lvl = quo(cluster_lvl4)

load(str_c(data_folder, '01_parsed.Rdata'))
univ_df = read_rds(str_c(data_folder, '02_univ_net_stats.rds')) %>% 
    rename(cluster = !!cluster_lvl)


## Cluster vs. prestige ----
## But I think we plotted this in the network script? 
# univ_df %>% 
#     filter(!is.na(cluster)) %>% 
#     count(cluster, prestige) %>% 
#     ggplot(aes(cluster, n, color = prestige, fill = prestige, group = prestige)) +
#     # geom_point(size = 10, position = position_dodge(width = .5)) +
#     geom_label(aes(label = n), color = 'white', size = 5, 
#               position = position_dodge(width = .25)) +
#     scale_color_brewer(palette = 'Set1') +
#     scale_fill_brewer(palette = 'Set1')



## Cluster-cluster flow diagrams ----
cluster_flows_df = individual_df %>% 
    filter(permanent) %>% 
    left_join(select(univ_df, 
                     univ_id, cluster), 
              by = c('hiring_univ_id' = 'univ_id')) %>% 
    filter(!is.na(cluster)) %>% 
    left_join(select(univ_df, univ_id, cluster), 
              by = c('placing_univ_id' = 'univ_id'), 
              suffix = c('_hiring', '_placing')) %>% 
    filter(!is.na(cluster_placing)) %>% 
    count(cluster_hiring, cluster_placing)

## Non-normalized heatmap
ggplot(cluster_flows_df, aes(cluster_placing, cluster_hiring, fill = n)) +
    geom_tile() +
    geom_text(aes(label = n), color = 'white') +
    scale_fill_viridis_c(option = 'D', direction = -1) +
    theme(panel.grid = element_blank())

## Heatmap, normalizing within hiring clusters
row_norm = cluster_flows_df %>% 
    group_by(cluster_hiring) %>% 
    mutate(share = n / sum(n)) %>% 
    ungroup() %>% 
    ggplot(aes(cluster_placing, cluster_hiring, fill = share)) +
    geom_tile() +
    stat_function(fun = identity, linetype = 'dashed', inherit.aes = FALSE) +
    geom_text(aes(label = scales::percent_format(2)(share)), color = 'white') +
    scale_fill_viridis_c(option = 'A', direction = -1) +
    theme(panel.grid = element_blank()) +
    ggtitle('Where do clusters hire from?', 
            subtitle = 'Shares within hiring clusters (rows)')

## Normalizing within placing clusters
col_norm = cluster_flows_df %>% 
    group_by(cluster_placing) %>% 
    mutate(share = n / sum(n)) %>% 
    ungroup() %>% 
    ggplot(aes(cluster_placing, cluster_hiring, fill = share)) +
    geom_tile() +
    stat_function(fun = identity, linetype = 'dashed', inherit.aes = FALSE) +
    geom_text(aes(label = scales::percent_format(2)(share)), color = 'white') +
    scale_fill_viridis_c(option = 'A', direction = -1) +
    theme(panel.grid = element_blank()) +
    ggtitle('Where do clusters place their graduates?', 
            subtitle = 'Shares within placing clusters (columns)')

# heatmap_legend = get_legend(row_norm)

plot_grid(col_norm, row_norm)

## Except for 1 and 5, most clusters *don't* strongly place within themselves
## Except for 5 and 9, a plurality of hires come from cluster 1

## Alluvial plot
## Read:  within all clusters (except maybe #6), a plurality of PhDs are placed at other programs in the cluster
cluster_flows_df %>%
    gather_set_data(1:2) %>% 
    mutate(x = fct_relevel(x, 'cluster_placing')) %>% 
    ggplot(aes(x, id = id, split = y, value = n)) +
    geom_parallel_sets(aes(fill = cluster_hiring), axis.width = .1, 
                       show.legend = FALSE) +
    geom_parallel_sets_axes(axis.width = .1) +
    geom_parallel_sets_labels(color = 'white', angle = 0) +
    xlab('') +
    scale_y_continuous(breaks = NULL, labels = NULL) +
    scale_fill_viridis_d(option = 'C') +
    ggtitle('Flows between semantic clusters', 
            subtitle = Sys.time())

ggsave(str_c(output_folder, '05_cluster_flow.png'), 
       width = 6, height = 4, scale = 1.5)

## Normalizing within hiring clusters
## These are harder to read then they would seem
## RH bars (hiring clusters) all add up to 100%
## but LH bars are sums of percentages within the hiring clusters, which isn't obviously meaningful
# cluster_flows_df %>% 
#     group_by(cluster_hiring) %>% 
#     mutate(share = n / sum(n)) %>% 
#     ungroup() %>% 
#     gather_set_data(1:2) %>% 
#     mutate(x = fct_relevel(x, 'cluster_placing')) %>% 
#     ggplot(aes(x, id = id, split = y, value = share)) +
#     geom_parallel_sets(aes(fill = cluster_placing), axis.width = .1, 
#                        show.legend = FALSE) + 
#     geom_parallel_sets_axes(axis.width = .1) +
#     geom_parallel_sets_labels(color = 'white', angle = 0)


# cluster_flows_df %>%
#     group_by(cluster_placing) %>%
#     mutate(share = n / sum(n)) %>%
#     ungroup() %>%
#     gather_set_data(1:2) %>%
#     mutate(x = fct_relevel(x, 'cluster_placing')) %>%
#     ggplot(aes(x, id = id, split = y, value = share)) +
#     geom_parallel_sets(aes(fill = cluster_hiring), axis.width = .1,
#                        show.legend = FALSE) +
#     geom_parallel_sets_axes(axis.width = .1) +
#     geom_parallel_sets_labels(color = 'white', angle = 0)


