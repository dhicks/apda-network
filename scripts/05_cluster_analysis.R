library(tidyverse)
library(ggforce)
library(cowplot)
library(ggbeeswarm)
library(plotly)

theme_set(theme_minimal())


#+ load_data -----
data_folder = '../data/'
output_folder = '../output/05_'
paper_folder = '../paper/'


load(str_c(data_folder, '02_parsed.Rdata'))
univ_df = read_rds(str_c(data_folder, '03_univ_net_stats.rds')) %>% 
    mutate(cluster = case_when(cluster_3 == '1' ~ 'analytic', 
                               cluster_3 == '3' ~ 'continental', 
                               cluster_3 == '2' ~ 'science', 
                               TRUE ~ 'missing'), 
           cluster = fct_relevel(cluster, 
                                 'analytic', 'science', 
                                 'continental', 'missing'))

dist_matrix = read_rds(str_c(data_folder, '01_dist_matrix.Rds'))

## Cluster x gender ----
cluster_gender = univ_df %>% 
    filter(!is.na(frac_w), !(cluster == 'missing')) %>% 
    ggplot(aes(cluster, frac_w, fill = cluster)) +
    geom_beeswarm(shape = 21) +
    stat_summary(aes(color = cluster), 
                 geom = 'crossbar',
                 fun = 'median', 
                 fun.max = 'median', 
                 fun.min = 'median') +
    # geom_violin(draw_quantiles = .5, alpha = 0) +
    labs(x = 'program cluster') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'Share of graduates women') +
    scale_color_viridis_d(option = 'C',
                         # scale_color_brewer(palette = 'RdYlBu',
                         name = 'cluster', 
                         direction = 1,
                         guide = FALSE) +
    scale_fill_viridis_d(option = 'C',
                          # scale_color_brewer(palette = 'RdYlBu',
                          name = 'cluster', 
                          direction = 1,
                          guide = FALSE)
cluster_gender
ggsave(str_c(output_folder, 'cluster_gender.png'), 
       width = 6, height = 6)
ggsave(str_c(paper_folder, 'fig_cluster_gender.png'), 
       width = 6, height = 6)


## Cluster MDS visualization ----
mds = dist_matrix %>% 
    cmdscale(add = TRUE, 
             list. = TRUE) %>% 
    .$points %>% 
    as_tibble() %>% 
    mutate(univ_id = labels(dist_matrix))

# isomds = dist_matrix %>% 
#     MASS::isoMDS() %>% 
#     .$points %>% 
#     as_tibble() %>% 
#     mutate(univ_id = labels(dist_matrix))

univ_df %>% 
    right_join(mds) %>% 
    ggplot(aes(V1, V2)) +
    geom_point(aes(label = univ_name, 
                   fill = cluster, 
                   # size = prestige
    ), shape = 21) +
    geom_mark_hull(aes(label = cluster, color = cluster), 
                   expand = unit(3, 'mm'),
                   concavity = 5, 
                   con.border = 'all'
                   ) +
    scale_color_viridis_d(option = 'C',
                          # scale_color_brewer(palette = 'RdYlBu',
                          name = 'cluster', 
                          direction = 1,
                          guide = FALSE) +
    scale_fill_viridis_d(option = 'C', name = 'cluster', 
                         direction = 1, guide = FALSE) +
    coord_equal() +
    theme_map()

mds_interactive = ggplotly()
mds_interactive


ggsave(str_c(output_folder, 'mds.png'), 
       width = 6, height = 6)
ggsave(str_c(paper_folder, 'fig_mds.png'), 
       width = 6, height = 6)
htmlwidgets::saveWidget(mds_interactive, 
           str_c(output_folder, 'mds_interactive.html'))

## Cluster vs. prestige ----
## But I think we plotted this in the network script? 
univ_df %>%
    filter(cluster != 'missing') %>%
    count(cluster, prestige) %>%
    ggplot(aes(cluster, n, color = prestige, fill = prestige, group = prestige)) +
    geom_col(aes(xend = cluster, yend = 0),
             width = .1,
             position = position_dodge(width = .25)) +
    # geom_point(size = 10, position = position_dodge(width = .5)) +
    geom_label(aes(label = n), color = 'white', size = 5,
               position = position_dodge(width = .25)) +
    scale_color_brewer(palette = 'Set1') +
    scale_fill_brewer(palette = 'Set1')

cluster_prestige_count = univ_df %>% 
    filter(cluster != 'missing') %>%
    ggplot(aes(cluster, fill = prestige)) +
    geom_bar() +
    scale_fill_brewer(palette = 'Set1')
cluster_prestige_count

cluster_prestige_prop = univ_df %>% 
    filter(cluster != 'missing') %>% 
    ggplot(aes(cluster, fill = prestige)) +
    geom_bar(position = 'fill') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'share') +
    scale_fill_brewer(palette = 'Set1', guide = FALSE)
cluster_prestige_prop

plot_grid(cluster_prestige_count, cluster_prestige_prop, 
          rel_widths = c(1.3, 1),
          labels = 'AUTO')

ggsave(str_c(output_folder, 'cluster_vs_prestige.png'), 
       width = 8, height = 4)
ggsave(str_c(paper_folder, 'fig_cluster_vs_prestige.png'), 
       width = 8, height = 4)

## Cluster-cluster flow diagrams ----
cluster_flows_df = individual_df %>% 
    filter(permanent) %>% 
    left_join(select(univ_df, 
                     univ_id, cluster), 
              by = c('hiring_univ_id' = 'univ_id')) %>% 
    # filter(!is.na(cluster)) %>% 
    mutate(cluster = fct_explicit_na(cluster)) %>% 
    left_join(select(univ_df, univ_id, cluster), 
              by = c('placing_univ_id' = 'univ_id'), 
              suffix = c('_hiring', '_placing')) %>% 
    # filter(!is.na(cluster_placing)) %>% 
    mutate(cluster_placing = fct_explicit_na(cluster_placing)) %>% 
    count(cluster_hiring, cluster_placing)

## Non-normalized heatmap
ggplot(cluster_flows_df, aes(cluster_placing, cluster_hiring, fill = n)) +
    geom_tile() +
    geom_text(aes(label = n), color = 'white') +
    scale_fill_viridis_c(option = 'D', direction = -1) +
    theme(panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 20, hjust = 1))

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
    theme(panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 20, hjust = 1)) +
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
    theme(panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 20, hjust = 1)) +
    ggtitle('Where do clusters place their graduates?', 
            subtitle = 'Shares within placing clusters (columns)')

# heatmap_legend = get_legend(row_norm)

plot_grid(col_norm, row_norm)

ggsave(str_c(output_folder, 'cluster_heatmaps.png'), 
       width = 6, height = 3, scale = 2)

## Alluvial plot
alluvial_plot = cluster_flows_df %>%
    filter(cluster_hiring != 'missing',
           cluster_placing != 'missing') %>%
    gather_set_data(1:2) %>% 
    mutate(x = fct_relevel(x, 'cluster_placing')) %>% 
    ggplot(aes(x, id = id, split = y, value = n)) +
    geom_parallel_sets(aes(fill = cluster_hiring), axis.width = .3, 
                       show.legend = FALSE) +
    geom_parallel_sets_axes(axis.width = .3, 
                            fill = 'grey80') +
    geom_parallel_sets_labels(color = 'black', angle = 0) +
    xlab('') +
    scale_y_continuous(breaks = NULL, labels = NULL) +
    scale_fill_viridis_d(option = 'C', direction = 1)
alluvial_plot

ggsave(str_c(output_folder, 'cluster_flow.png'), 
       width = 4, height = 4, scale = 2)
ggsave(str_c(paper_folder, 'fig_cluster_flow.png'), 
       width = 4, height = 4, scale = 2)

## Bar charts by hiring cluster
hiring_bar = cluster_flows_df %>% 
    filter_at(vars(starts_with('cluster')), ~ . != 'missing') %>% 
    group_by(cluster_hiring) %>% 
    arrange(cluster_hiring, desc(cluster_placing)) %>% 
    mutate(share = n / sum(n), 
           label_pos = lag(cumsum(share), default = 0) + .5*share) %>% 
    ggplot(aes(cluster_hiring, y = share)) +
    geom_col(aes(fill = cluster_placing)) +
    geom_label(aes(label = scales::percent(share, accuracy = 2), 
                   y = label_pos), 
               position = 'identity') +
    scale_fill_viridis_d(option = 'C', direction = 1, 
                         name = 'Placing cluster') +
    xlab('Hiring cluster') +
    scale_y_continuous(labels = scales::percent_format())
hiring_bar

## All placemewnts by placing cluster
all_bar = cluster_flows_df %>% 
    group_by(cluster_placing) %>% 
    summarize(n = sum(n)) %>% 
    filter(cluster_placing != 'missing') %>% 
    mutate(share = n / sum(n)) %>% 
    arrange(desc(cluster_placing)) %>% 
    mutate(label_pos= lag(cumsum(share), default = 0) + .5*share) %>% 
    ggplot(aes('all permanent\nplacements', share)) +
    geom_col(aes(fill = cluster_placing)) +
    geom_label(aes(label = scales::percent(share, accuracy = 2), 
                   y = label_pos), 
               position = 'identity') +
    scale_fill_viridis_d(option = 'C', direction = 1, 
                         name = 'Placing cluster', 
                         guide = FALSE) +
    xlab('') +
    scale_y_continuous(labels = scales::percent_format())
    
plot_grid(alluvial_plot, hiring_bar, all_bar, 
          nrow = 1, rel_widths = c(1, 1, .3), 
          labels = 'AUTO')

ggsave(str_c(output_folder, 'cluster_flow_composite.png'), 
       width = 8, height = 4, scale = 2)
ggsave(str_c(paper_folder, 'fig_cluster_flow.png'), 
       width = 8, height = 4, scale = 2)


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


