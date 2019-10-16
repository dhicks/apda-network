#' ---
#' output:
#'     html_document: 
#'         self_contained: yes
#'         toc: true
#' ---
#' 
#' Findings
#' --------------------
#' - 37 PhD programs (22%) produce about 50% of permanent placements
#' - Centrality scores reveal a clear division between two groups of programs, with "high" and "low" scores.  
#' - The split between these programs is explained in part by the way PhD production and hiring are distributed across institutions.  However, bimodality is not due only to the degree distributions.  
#' - High output is only modestly correlated with centrality.  Programs like Leuven, New School, and Boston College produce lots of PhDs, but they aren't placed into the high-centrality departments. 
#' - There is no correlation between semantic clusters and topological communities. 
#' - A closed group of 56 programs can be identified:  All recent hires by schools within this group received their PhD within this group.  This "high-prestige" group corresponds exactly to the high-centrality programs.  
#' - Median permanent placement rate is 22 points higher at high-prestige programs (64% vs. 42% for low-prestige programs).  
#' - However, there is large variation in placement rate within both groups.  
#' - Counterfactual high-prestige status is strongly correlated with actual centrality ranking.  
#' - For low-prestige programs, counterfactual prestige seems to depend on the extent of the program's downstream hiring network. 

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
# library(smglr)
library(broom)
library(ggforce)
library(ggbeeswarm)

library(assertthat)
library(tictoc)

data_folder = '../data/'
plots_path = '../output/03_'
paper_folder = '../paper/'

## Load data ----
load(str_c(data_folder, '02_parsed.Rdata'))

individual_df %>% 
    filter(permanent) %>% 
    nrow()

#' There are 203 PhD programs producing graduate students in the data
univ_df %>%
    filter(total_placements > 0) %>%
    nrow()

#' 167 PhD programs with permanent placements
individual_df %>% 
    filter(permanent) %>% 
    count(placing_univ) %>% 
    nrow()

univ_df %>% 
    filter(perm_placements > 0) %>% 
    nrow()

#' **Finding: 37 PhD programs (37/203 = 18%) produce about 50% of permanent placements**
ggplot(univ_df, aes(perm_placement_rank, frac_cum_perm_placements)) + 
    geom_step() +
    scale_x_continuous(labels = scales::percent_format(), 
                       name = 'PhD Programs') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'Permanent Placements')

univ_df %>%
    filter(frac_cum_perm_placements <= .5) %>%
    arrange(perm_placement_rank) %>%
    mutate(perm_placement_rank = row_number()) %>%
    select(perm_placement_rank, univ_name, 
           perm_placements, frac_cum_perm_placements) %>%
    knitr::kable()

## Placements at PhD-producing programs
grad_programs = univ_df %>% 
    filter(total_placements > 0, country %in% c('U.S.')) %>% 
    pull(univ_id)
# gp2 = individual_df %>% 
#     count(placing_univ_id) %>% 
#     pull(placing_univ_id)

individual_df %>% 
    filter(permanent, hiring_univ_id %in% grad_programs) %>% 
    filter(placing_univ_id %in% grad_programs) %>%
    filter(position_type == 'Tenure-Track') %>%
    count(placing_univ) %>%
    arrange(desc(n)) %>% 
    rename(phd_program_placements = n) %>% 
    mutate(cum_phd_program_placements = cumsum(phd_program_placements), 
           share_phd_program_placements = cum_phd_program_placements / sum(phd_program_placements)) %>% 
    slice(1:20) %>% 
    knitr::kable(digits = 2)


#' Build network
#' --------------------
## build network ----
## NB Only permanent placements
hiring_network = individual_df %>%
    filter(permanent) %>% 
    select(placing_univ_id, hiring_univ_id, everything()) %>%
    graph_from_data_frame(directed = TRUE, 
                          vertices = univ_df) %>%
    as_tbl_graph() %>%
    ## Clean nodes (universities) that aren't in the network
    mutate(degree = centrality_degree(mode = 'total')) %>%
    filter(degree > 0)

hiring_network

assert_that(length(E(hiring_network)) == {individual_df %>% 
        filter(permanent) %>% 
        nrow()}, 
        msg = 'Number of edges in network ≠ number of perm. placements')

## 1 giant component contains almost all of the schools
components(hiring_network)$csize

#' Out- and in-centrality
to_reverse = function (graph) {
    ## Reverse edge direction
    if (!is.directed(graph))
        return(graph)
    e <- get.data.frame(graph, what="edges")
    ## swap "from" & "to"
    neworder <- 1:length(e)
    neworder[1:2] <- c(2,1)
    e <- e[neworder]
    names(e) <- names(e)[neworder]
    vertex_df = get.data.frame(graph, what = 'vertices')
    if (ncol(vertex_df) > 0) {
        return(as_tbl_graph(graph.data.frame(e, vertices = vertex_df)))
    } else {
        return(as_tbl_graph(graph.data.frame(e)))
    }
}

set.seed(42)
nzmin = function(x, na.rm = TRUE) {
    min(x[x > 0], na.rm = na.rm)
}
damping = .3
hiring_network = hiring_network %>%
    mutate(in_centrality = centrality_eigen(weights = NA, 
                                            directed = TRUE), 
           in_pr = centrality_pagerank(weights = NA, 
                                       directed = TRUE, 
                                       damping = damping, 
                                       personalized = NULL)) %>%
    morph(to_reverse) %>%
    mutate(out_centrality = centrality_eigen(weights = NA,
                                             directed = TRUE), 
           out_pr = centrality_pagerank(weights = NA, 
                                        directed = TRUE, 
                                        damping = damping, 
                                        personalized = NULL)) %>%
    unmorph() %>%
    ## Trim 0s to minimum non-zero values
    mutate(out_centrality = ifelse(out_centrality == 0,
                                   nzmin(out_centrality),
                                   out_centrality),
           in_centrality = ifelse(in_centrality == 0,
                                  nzmin(in_centrality),
                                  in_centrality))

## Check relationship btwn unweighted multiedges and weighted edges
## Strong correlation, esp by approx rank and high/low prestige
## But some movement, both numerically and ordinally
# E(hiring_network)$weight = count_multiple(hiring_network)
# hiring_network_simp = simplify(hiring_network)
# 
# set.seed(42)
# V(hiring_network_simp)$out_centrality = hiring_network_simp %>%
#     graph.reverse() %>%
#     eigen_centrality(directed = TRUE) %>%
#     .$vector
# 
# tibble(name = V(hiring_network)$univ_name,
#        multi = V(hiring_network)$out_centrality,
#        simp = V(hiring_network_simp)$out_centrality) %>%
#     ggplot(aes(log10(simp), log10(multi))) + 
#     geom_point() +
#     stat_function(fun = function (x) x)



#' Exploring centrality scores
#' --------------------
## exploring centrality scores ----

#' PageRank centrality is almost entirely determined by degree
#' So we use eigenvector centrality instead
ggplot(hiring_network, aes(degree, log10(out_pr))) +
    geom_point(aes(text = univ_name)) +
    geom_smooth(method = 'lm')

#' There are two clear groups of centrality scores, with high scores (in the range of ~10^-12 to 1) and low scores (10^-15 and smaller). 
##
## NB there seem to be (small?) differences in scores (at the low end?) across runs of the centrality algorithm
ggplot(as_tibble(hiring_network), aes(out_centrality)) + 
    # geom_density() +
    geom_histogram(binwidth = 1, fill = 'white', color = 'black') +
    geom_rug() +
    scale_x_continuous(trans = 'log10', 
                       name = 'Out centrality (log)', 
                       breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    facet_zoom(x = out_centrality > 10^-12, 
               ylim = c(0, 20),
               # ylim = c(0, .02),
               show.area = FALSE, 
               shrink = TRUE,
               zoom.size = .3,
               horizontal = FALSE) +
    theme_bw()
ggsave(str_c(plots_path, 'out_centrality.png'), 
       height = 3, width = 6)
ggsave(str_c(paper_folder, 'fig_out_density.png'), 
       height = 3, width = 6)

ggplot(as_tibble(hiring_network), aes(in_centrality)) + 
    geom_density() + geom_rug() +
    scale_x_continuous(trans = 'log10')

ggplot(as_tibble(hiring_network), 
       aes(out_centrality, in_centrality, 
           color = cluster_label, 
           text = univ_name)) +
    geom_jitter() + 
    scale_x_log10() + scale_y_log10()
plotly::ggplotly()

# ## Check stability of centrality calculation
# iterated_centrality = 1:500 %>%
#     map(~ {hiring_network %>%
#             graph.reverse() %>%
#             eigen_centrality(directed = TRUE, weights = NA) %>%
#             .$vector}) %>%
#     transpose() %>%
#     map(unlist) %>%
#     map(~ tibble(out_centrality = .)) %>%
#     bind_rows(.id = 'univ_id') %>%
#     group_by(univ_id) %>%
#     summarize(min = min(out_centrality),
#               max = max(out_centrality),
#               median = median(out_centrality))
# 
# ## Lots of variation among low-prestige
# ggplot(iterated_centrality, 
#        aes(reorder(univ_id, median), 
#            median)) + 
#     geom_pointrange(aes(ymin = min, ymax = max)) + 
#     scale_y_log10()
# 
# ## But extremely stable results among high-prestige
# iterated_centrality %>%
#     filter(min > 10^-12) %>%
#     ggplot(aes(reorder(univ_id, median), 
#                median)) + 
#     geom_pointrange(aes(ymin = min, ymax = max)) + 
#     scale_y_log10()

#' We completely rewire the network, preserving out-degree (number of new PhDs) and in-degree (number of permanent hires), but otherwise randomly matching job-seekers to positions.  We then recalculate out-centrality.  The plots below show the out-centrality distributions for each random rewiring, with the actual distribution in red.  The distributions are always bimodal, indicating that **the observed split into high- and low-centrality programs is due in part to the way PhD production and hiring are distributed across institutions**.  However, the high-centrality subset is never as small as in the actual hiring network.  This indicates that **bimodiality is not due only to the degree distributions**.  Some other factor is exacerbating the structural inequality in the hiring network.  
#' 
set.seed(78910)
map(1:100, 
    ~ sample_degseq(degree(hiring_network, mode = 'out'), 
                    degree(hiring_network, mode = 'in'))
) %>%
    map(to_reverse) %>%
    map(eigen_centrality, directed = TRUE, weights = NULL) %>%
    map(~ .$vector) %>%
    map(log10) %>%
    map(density) %>%
    map(~ tibble(x = .$x, y = .$y)) %>%
    bind_rows(.id = 'iteration') %>%
    ggplot(aes(x, y)) + 
    geom_line(aes(group = iteration), alpha = .5) + 
    stat_density(data = as_tibble(hiring_network), geom = 'line',
                 aes(log10(out_centrality), ..density..),
                 color = 'red', size = 1)


#' **Finding: High output is only modestly correlated w/ centrality.** 
#' Programs like Leuven, New School, and Boston College produce lots of PhDs, but they aren't placed into the high-centrality departments. 
ggplot(as_tibble(hiring_network), 
       aes(total_placements, log10(out_centrality))) +
    geom_point(aes(text = univ_name)) +
    geom_smooth()
plotly::ggplotly()

# univ_df %>%
#     mutate(out_centrality_log = log10(out_centrality)) %>%
#     filter(!is.na(out_centrality_log) &
#                out_centrality > 0 &
#                !is.na(total_placements)) %>%
#     select(total_placements, out_centrality_log) %>%
#     cor()
hiring_network %>%
    select(total_placements, out_centrality) %>%
    as_tibble() %>%
    filter(complete.cases(.)) %>%
    mutate(out_centrality = log10(out_centrality)) %>%
    cor()

ggplot(hiring_network, 
       aes(perm_placement_rank, log10(out_centrality))) +
    geom_point() +
    geom_smooth()

ggplot(hiring_network, 
       aes(log10(out_centrality), perm_placement_rate)) +
    geom_point(aes(text = univ_name)) +
    geom_smooth(method = 'lm')
plotly::ggplotly()

as_tibble(hiring_network) %>%
    select(total_placements, perm_placement_rate, 
           out_centrality) %>%
    filter(complete.cases(.)) %>%
    mutate(out_centrality = log10(out_centrality)) %>%
    cor()

## Add centrality scores to univ_df
univ_df = hiring_network %>%
    as_tibble() %>%
    rename(univ_id = name) %>%
    left_join(univ_df, .)

## Movement down the prestige hierarchy
individual_df %>%
    filter(permanent) %>%
    left_join(select(univ_df, univ_id, out_centrality),
              by = c('placing_univ_id' = 'univ_id')) %>%
    left_join(select(univ_df, univ_id, out_centrality),
              by = c('hiring_univ_id' = 'univ_id')) %>%
    # filter(out_centrality.y > 10^-12) %>%
    select(person_id, aos_category,
           placing = out_centrality.x,
           hiring = out_centrality.y) %>%
    gather(variable, value, placing, hiring) %>%
    mutate(variable = fct_rev(variable)) %>%
    ggplot(aes(variable, log10(value))) +
    geom_point() +
    geom_line(aes(group = person_id), 
              alpha = .25) +
    xlab('university') +
    ylab('centrality (log10)') +
    theme_minimal()
ggsave(str_c(plots_path, 'prestige_movement.png'), 
       width = 3, height = 4)
ggsave(str_c(paper_folder, 'fig_crossing.png'), 
       width = 3, height = 4)

## Diagonal line indicates where hiring program has the same centrality as the placing program.  
## Most placements are below this line, indicating that the centrality measure captures the idea that people typically are hired by programs with lower status
## The few placements above the line indicate that, even when individuals are hired by programs with higher status, the increase is relatively minor:  no more than 1 point on the log10 scale
individual_df %>% 
    filter(permanent) %>%
    left_join(select(univ_df, univ_id, out_centrality), 
              by = c('placing_univ_id' = 'univ_id')) %>%
    left_join(select(univ_df, univ_id, out_centrality),
              by = c('hiring_univ_id' = 'univ_id')) %>%
    filter(out_centrality.y > 10^-12) %>%
    select(person_id, 
           placing = out_centrality.x, 
           hiring = out_centrality.y) %>%
    ggplot(aes(log10(placing), log10(hiring))) + 
    geom_jitter() + 
    stat_function(fun = identity)



#' Community detection
#' --------------------
## community detection ----
## Extract giant component
hiring_network_gc = hiring_network %>%
    components %>% 
    .$csize %>%
    {which(. == max(.))} %>%
    {components(hiring_network)$membership == .} %>%
    which() %>%
    induced_subgraph(hiring_network, .) %>%
    as_tbl_graph()

#+ community_detection_params, cache = TRUE
walk_len = rep(2:100, 1)
## ~24 sec
tic()
comm_stats = walk_len %>%
    map(~ cluster_walktrap(hiring_network_gc, steps = .x)) %>%
    map(~ list(sizes = sizes(.x), length = length(.x))) %>%
    map_dfr(~ tibble(H = -sum(.x$sizes/sum(.x$sizes) * log2(.x$sizes/sum(.x$sizes))),
                     n_comms = .x$length)) %>%
    mutate(walk_len = walk_len,
           delta_H = log2(n_comms) - H)
toc()

ggplot(comm_stats, aes(walk_len, H)) +
    geom_point() +
    geom_smooth()
ggplot(comm_stats, aes(walk_len, n_comms)) +
    geom_point() +
    geom_smooth()
ggplot(comm_stats, aes(n_comms, H)) +
    geom_text(aes(label = walk_len))

#' Select the walk length that minimizes both delta_H (flatter community distribution) and n_comms (fewer communities) using regression residuals
walk_length = lm(H ~ n_comms, data = comm_stats) %>%
    augment(comm_stats) %>%
    # ggplot(aes(walk_len, .resid)) +
    # geom_text(aes(label = walk_len))
    arrange(desc(.resid)) %>%
    pull(walk_len) %>%
    first()

walk_length

communities = cluster_walktrap(hiring_network_gc, steps = walk_length)
V(hiring_network_gc)$community = membership(communities)

#' Analysis of community detection
## Summary of community sizes
hiring_network_gc %>% 
    as_tibble() %>% 
    count(community) %>% 
    pull(n) %>% 
    summary()

univ_df = univ_df %>%
    left_join({hiring_network_gc %>%
            as_tibble() %>% 
            select(univ_id = name, community) %>%
            mutate(community = as.character(community))})

univ_df %>% 
    filter(!is.na(community)) %>% 
    count(community) %>% 
    ggplot(aes(n)) +
    geom_bar(aes(text = n)) +
    scale_x_continuous(name = 'Community size (# programs)')
plotly::ggplotly()

cluster_vars = univ_df %>% 
    select(matches('cluster')) %>% 
    names()

univ_df %>% 
    filter(!is.na(community), !is.na('cluster_lvl4'))


#' **Finding: There is no correlation between semantic clusters and topological communities.**
univ_df %>%
    filter(!is.na(community), !is.na(cluster_label)) %>%
    select(community, cluster_label) %>%
    table() %>%
    chisq.test(simulate.p.value = TRUE)

univ_df %>%
    filter(!is.na(community), !is.na(cluster_label)) %>%
    count(community, cluster_label) %>%
    rename(cluster_n = n) %>%
    group_by(community) %>%
    mutate(community_tot = sum(cluster_n), 
           cluster_frac = cluster_n / community_tot, 
           H = sum(cluster_frac * log2(cluster_frac))) %>%
    ungroup() %>%
    ggplot(aes(fct_reorder(community, community_tot, .desc = FALSE),
               cluster_n, fill = cluster_label)) + 
    geom_col() + 
    coord_flip() +
    xlab('Topological communities') +
    ylab('# of schools in community') +
    scale_fill_brewer(palette = 'Set1', 
                      name = 'Semantic\nclusters')


#' High-prestige universities
#' --------------------
## high-prestige universities ----
## Start w/ Oxford, and work upstream
## Only need to go 11 or 12 steps to get closure
1:25 %>%
    map(~ make_ego_graph(hiring_network, order = .x, 
                         nodes = '512', mode = 'in')) %>%
    flatten() %>%
    map(~ length(V(.x))) %>%
    tibble(order = 1:length(.), 
           size = unlist(.)) %>%
    ggplot(aes(order, size)) + geom_point()

prestigious = make_ego_graph(hiring_network, order = 12, 
                             nodes = '512', 
                             mode = 'in') %>%
    .[[1]] %>%
    as_tbl_graph()

## How large is the high-prestige community?  
## 56 programs; 7% of all programs in the network; 
## 28% of programs with at least 1 placement in the dataset
length(V(prestigious))
length(V(prestigious)) / length(V(hiring_network))
length(V(prestigious)) / sum(univ_df$total_placements > 0, na.rm = TRUE)

## What fraction of hires are within high-prestige?  
## 13% of all permanent hires; 7% of all hires
length(E(prestigious)) / length(E(hiring_network))
length(E(prestigious)) / nrow(individual_df)

# layout_prestigious = layout_with_focus(prestigious, 
#                                        which(V(prestigious)$univ_name == 'University of Oxford')) %>% 
#     `colnames<-`(c('x', 'y')) %>% 
#     as_tibble()
layout_prestigious = create_layout(prestigious, 'focus', 
                                   focus = which(V(prestigious)$univ_name == 'University of Oxford'))

# png(file = '02_prestigious_net.png', 
#     width = 11, height = 11, units = 'in', res = 400)
# set.seed(24)
ggraph(layout_prestigious) + 
    geom_node_label(aes(label = univ_name, 
                        size = log10(out_centrality), 
                        fill = log10(out_centrality)),
                    color = 'white') + 
    geom_edge_fan(arrow = arrow(length = unit(.01, 'npc')), 
                  alpha = .25,
                  strength = 5) +
    scale_size_continuous(range = c(.5, 3), guide = FALSE) +
    scale_fill_viridis(name = 'out centrality (log10)') +
    coord_cartesian(xlim = c(-3.5, 5.5), clip = 'on') +
    theme_graph() +
    theme(legend.position = 'bottom', 
          plot.margin = margin(0, unit = 'cm'))
ggsave(str_c(plots_path, 'prestigious_net.png'), 
       width = 6, height = 4, dpi = 600, scale = 2)
ggsave(str_c(paper_folder, 'fig_prestigious_net.png'), 
       width = 6, height = 4, dpi = 600, scale = 2)
# dev.off()

## High-prestige = high centrality group
univ_df = univ_df %>%
    mutate(prestige = ifelse(univ_id %in% V(prestigious)$name, 
                             'high-prestige', 
                             'low-prestige'))
V(hiring_network)$prestigious = V(hiring_network)$out_centrality > 1e-12
V(hiring_network_gc)$prestigious = V(hiring_network_gc)$out_centrality > 1e-12

ggplot(univ_df, aes(prestige, log10(out_centrality))) + 
    geom_jitter()

## Output alphabetical list of high-prestige programs
univ_df %>% 
    filter(prestige == 'high-prestige') %>% 
    select(univ_name, cluster = cluster_label, 
           perm_placement_rate,
           country, 
           # out_centrality
           ) %>% 
    # arrange(desc(out_centrality)) %>% view()
    arrange(univ_name) %>% 
    mutate(perm_placement_rate = scales::percent_format(accuracy = 2)(perm_placement_rate)) %>% 
    knitr::kable(col.names = c('university', 'cluster', 
                               'placement rate',
                               'country'),
                 format = 'latex', 
                 longtable = TRUE,
                 booktabs = TRUE, 
                 # table.envir = 'sidewaystable',
                 label = 'high.prestige', 
                 caption = 'High-prestige universities/programs, in alphabetical order.  Placement rate refers to placements in permanent academic positions.') %>% 
    write_file(path = str_c(plots_path, 'high.prestige.tex'))

## Prestige status and clusters
## High-prestige are spread throughout, but #5 is mostly low-prestige
univ_df %>%
    filter(!is.na(cluster_label)) %>%
    ggplot(aes(cluster_label, color = prestige)) + 
    geom_point(stat = 'count') +
    geom_line(aes(group = prestige), stat = 'count')
## High-prestige are mostly in the largest community
univ_df %>%
    filter(!is.na(community)) %>%
    ggplot(aes(as.integer(community), fill = prestige)) + 
    geom_bar()

## What fraction of high-prestige graduates end up in high-prestige programs? 
## 24% of those w/ permanent placements
individual_df %>%
    filter(permanent) %>%
    left_join(univ_df, by = c('placing_univ_id' = 'univ_id')) %>%
    left_join(univ_df, by = c('hiring_univ_id' = 'univ_id')) %>%
    select(placing = prestige.x, hiring = prestige.y) %>%
    count(placing, hiring) %>%
    group_by(placing) %>%
    mutate(share = n / sum(n))

#' **Finding: Median permanent placement rate for high-prestige programs is 22 points higher than for low-prestige programs.** 
#' However, variation is also wide within each group;  
#' This is also not yet controlling for graduation year, area, or demographics. 
ggplot(univ_df, aes(prestige, perm_placement_rate, 
                    label = univ_name)) + 
    # geom_sina(aes(size = total_placements), 
    #           alpha = .5) +
    geom_beeswarm(aes(size = total_placements), 
                  priority = 'density',
                  cex = 2,
                  alpha = .5) +
    geom_violin(color = 'red', draw_quantiles = .5, 
                scale = 'count',
                fill = 'transparent') +
    # geom_jitter(aes(size = total_placements)) +
    scale_x_discrete(expand = expand_scale(add=c(0, 1))) +
    scale_y_continuous(labels = scales::percent_format()) +
    ylab('permanent placement rate') +
    scale_size(name = 'total placements') +
    theme_minimal()
ggsave(str_c(plots_path, 'prestige_placement.png'), 
       width = 8, height = 4)
ggsave(str_c(paper_folder, 'fig_placement.png'), 
       width = 8, height = 4)
plotly::ggplotly()

univ_df %>%
    group_by(prestige) %>%
    summarize_at(vars(perm_placement_rate), 
                 funs(median, max, min), na.rm = TRUE)


## prestige stability ----
#' Stability of prestige status
#' --------------------
#' By randomly rewiring the network, we can test the stability of the prestige categories to data errors and the short time frame of our data.  In each of 500 permutations, we randomly rewire 10% of the edges in the permanent hiring network, then calculate out-centralities on the rewired network.  Using a threshold of 10^-12 for high-prestige status, we count the fraction of rewired networks in which each program is high-prestige.  
## ~6 sec
tic()
set.seed(13579)
permutations = 1:500 %>%
    ## Rewire 10% of edges
    map(~ {hiring_network %>%
            rewire(keeping_degseq(loops = TRUE, 
                                  niter = floor(.05 * length(E(.)))))}) %>%
    ## Calculate out-centralities
    map(~ {.x %>%
            to_reverse() %>%
            eigen_centrality(directed = TRUE, weights = NA) %>%
            .$vector}) %>%
    transpose() %>%
    map(unlist) %>%
    ## Fraction where program is high-prestige
    map(~ sum(. > 10^-12) / length(.)) %>%
    map(~ tibble(frac_high_prestige = .)) %>%
    bind_rows(.id = 'univ_id')
toc()

## frac_high_prestige indicates the fraction of permutations in which the program was high-prestige
ggplot(permutations, aes(frac_high_prestige)) + 
    geom_density() + 
    geom_rug()

univ_df = left_join(univ_df, permutations)

#' **Finding:** Counterfactual high-prestige status is strongly correlated with centrality ranking. 
#' 
ggplot(univ_df, aes(log10(out_centrality), frac_high_prestige)) + 
    geom_point(aes(text = univ_name)) +
    geom_smooth(method = 'lm')
plotly::ggplotly()

ggplot(univ_df, aes(perm_placements, frac_high_prestige, color = prestige)) + 
    geom_point(aes(text = univ_name)) +
    geom_smooth(method = 'lm')
plotly::ggplotly()

#' **Finding:** For low-prestige programs, counterfactual prestige seems to depend on the extent of the program's downstream hiring network.  Compare Boston College (19 permanent placements; 40% high prestige) to Leuven (18 permanent placements; 20% high prestige). 
bc_leuven_net = make_ego_graph(hiring_network, order = 10, 
                               nodes = c('529', '38'),
                               mode = 'out') %>% 
    map(as_tbl_graph) %>%
    reduce(bind_graphs) %>% 
    mutate(perm_placements = ifelse(is.na(perm_placements), 0, perm_placements))

bc_leuven_layout = create_layout(bc_leuven_net, 'stress')

ggraph(bc_leuven_layout) + 
    geom_node_label(aes(label = univ_name, 
                        size = perm_placements, 
                        fill = perm_placements), 
                    color = 'white') + 
    geom_edge_fan(arrow = arrow(angle = 45, 
                                length = unit(.1, 'inches'), 
                                type = 'closed'), 
                  alpha = .3) +
    scale_size_continuous(range = c(1, 5),
                          # na.value = 1,
                          name = 'placements',
                          guide = FALSE) +
    scale_fill_viridis(na.value = 'black', name = 'permanent\nplacements') +
    theme_graph()

ggsave(str_c(plots_path, 'bc_leuven.png'), 
       height = 6, width = 6, dpi = 600, scale = 1.25)

#' Plotting
#' --------------------
## plotting ----
#' Coarser community structure
# large_comms = which(sizes(communities) > 20)
# V(hiring_network_gc)$community_coarse = ifelse(
#     V(hiring_network_gc)$community %in% large_comms, 
#     V(hiring_network_gc)$community, 
#     NA)

## Big giant hairy ball
## ~13 sec
# tic()
# layout_net = hiring_network %>%
#     layout_with_stress() %>%
#     `colnames<-`(c('x', 'y')) %>%
#     as_tibble()
# toc()

## Focus on Oxford
## GC only; ~7 sec
# tic()
# layout_net = create_layout(hiring_network_gc, 'focus', focus = 512)
# toc()

## Stress majorization
## ~2 sec
tic()
layout_net = create_layout(hiring_network_gc, 'stress')
toc()

layout_net %>%
    ## NB neither of these preserve the layout_tbl_graph class, which breaks things when we get to ggraph()
    # filter(total_placements > 0) %>% 
    # induced_subgraph(which(degree(., mode = 'out') > 0)) %>% 
    ggraph() +
    geom_edge_parallel(arrow = arrow(length = unit(.02, 'npc'), 
                                     angle = 15,
                                     type = 'closed'),
                       # spread = 5, 
                       alpha = .05) +
    geom_node_point(aes(#size = log10(out_centrality),
        alpha = log10(out_centrality),
        # color = as.factor(community)
        color = cluster_label
        # color = log10(out_centrality)
    ), size = 2) +
    # scale_color_brewer(palette = 'Set1', na.value = 'black') +
    scale_color_viridis_d(na.value = 'black', name = 'Semantic cluster') +
    # scale_size_discrete(range = c(2, 6)) +
    scale_alpha_continuous(range = c(.2, 1), 
                           name = 'Out centrality (log10)') +
    # scale_size_continuous(range = c(1, 3)) +
    theme_graph()

ggsave(str_c(plots_path, 'hairball.png'), 
       width = 12, height = 12)
ggsave(str_c(paper_folder, 'fig_hairball.png'), 
       width = 12, height = 12)

# hiring_network_gc %>%
#     induced_subgraph(!is.na(V(.)$cluster_lvl1)) %>%
#     ggraph() +
#     geom_node_label(aes(size = prestigious, 
#                         label = univ_name,
#                         # color = as.factor(community))) +
#                         fill = cluster_lvl4), color = 'black') +
#     geom_edge_fan(aes(linetype = aos_category, color = aos_category), 
#                   width = 1,
#                   arrow = arrow(length = unit(.01, 'npc')), 
#                   spread = 5) +
#     scale_edge_color_brewer(palette = 'Set1') +
#     scale_fill_brewer(palette = 'Set3', na.value = 'grey75') +
#     scale_size_discrete(range = c(3, 5)) +
#     theme_graph()


## Chord diagram
# hiring_network_gc %>%
#     # induced_subgraph(which(degree(., mode = 'out') > 0)) %>%
#     ggraph(layout = 'linear', sort.by = 'community_coarse', circular = TRUE) +
#     geom_edge_arc(arrow = arrow(length = unit(.01, 'npc')), alpha = .1) +
#     geom_node_point(aes(size = prestigious, 
#                         # color = as.factor(community))) +
#                         color = cluster_lvl4)) +
#     scale_color_brewer(palette = 'Set1', guide = FALSE) +
#     theme_graph()

## Separate networks for each cluster
# cluster_ids = hiring_network %>% 
#     as_tibble() %>% 
#     split(., .$cluster_lvl4) %>% 
#     map(pull, name)
# 
# cluster_ids %>% 
#     map(~induced_subgraph(hiring_network, .))

# hiring_network %>% 
#     # filter(!is.na(cluster_lvl4)) %>% 
#     ggraph(layout = 'manual', 
#        node.positions = layout_net) +
#     geom_edge_fan(alpha = .1) +
#     geom_node_point(aes(color = cluster_label), show.legend = TRUE) +
#     # facet_nodes(vars(cluster_lvl4)) +
#     scale_color_viridis_d(na.value = 'grey90') +
#     theme_graph()

## output ----
#' Save university-level data with network statistics
write_rds(univ_df, str_c(data_folder, '03_univ_net_stats.rds'))


sessionInfo()