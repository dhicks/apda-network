#' ---
#' output:
#'     html_document: 
#'         self_contained: yes
#'         toc: true
#' ---

TODO: 
- rewrite text

#' Findings
#' --------------------
#' - 25% of PhD programs produce about 50% of permanent placements
#' - Centrality scores reveal a clear division between two groups of programs, with "high" and "low" scores.  
#' - High output is only modestly correlated with centrality scores.  Programs like Notre Dame, CUNY, and UVA produce lots of PhDs, but they aren't placed at high-centrality programs.  
#' - There is no correlation between semantic clusters and topological communities. 
#' - A closed group of 61 programs can be identified:  All recent hires by schools within this group received their PhD within this group.  This "high-prestige" group corresponds exactly to the high-centrality programs.  
#' - Median permanent placement rate is 14 points higher at high-prestige programs (58% vs. 44% for low-prestige programs).  
#' - However, there is large variation in placement rate within both groups.  
#' - High-prestige status is highly stable; actual high-prestige programs are almost always (>90%) high-prestige in the rewired networks. 
#' - A number of actual low-prestige programs are often (>50%) high-prestige in the rewired networks.  
#'     - Notre Dame (86%)
#'     - New School (71%)
#'     - Penn State (65%)
#'     - Tulane (62%)
#'     - Boston College (61%)
#'     - UC Boulder (56%)
#' - Within the actual categories, there is no correlation between counterfactual prestige status and either (a) out-centrality and (b) the number of permanent placements.  

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

data_folder = '../data/'

## Load data
load(str_c(data_folder, '01_parsed.Rdata'))

#' **Finding: 25% of PhD programs produce about 50% of permanent placements**
ggplot(univ_df, aes(perm_placement_rank, frac_cum_perm_placements)) + 
    geom_step() +
    scale_x_continuous(labels = scales::percent_format(), 
                       name = 'PhD Programs') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'Permanent Placements')


#' Build network
#' --------------------
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
hiring_network = hiring_network %>%
    mutate(in_centrality = centrality_eigen(weights = NA, 
                                            directed = TRUE)) %>%
    morph(to_reverse) %>%
    mutate(out_centrality = centrality_eigen(weights = NA,
                                             directed = TRUE)) %>%
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
#' There are two clear groups of centrality scores, with high scores (in the range of 10^-12 to 1) and low scores (10^-15 and smaller). 
##
## NB there seem to be (small?) differences in scores (at the low end?) across runs of the centrality algorithm
ggplot(as_tibble(hiring_network_), aes(out_centrality)) + 
    geom_density() + geom_rug() +
    scale_x_continuous(trans = 'log10')

ggplot(as_tibble(hiring_network), aes(in_centrality)) + 
    geom_density() + geom_rug() +
    scale_x_continuous(trans = 'log10')

ggplot(as_tibble(hiring_network), 
       aes(out_centrality, in_centrality, 
           color = cluster_lvl3, 
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
#     filter(min > 10^-11) %>%
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
                 color = 'red', size = 1) +
    xlim(NA, 0)


#' **Finding: High output is only modestly correlated w/ centrality.** 
#' Programs like ND, CUNY, New School produce lots of PhDs, but they aren't placed into the high-centrality departments. 
ggplot(as_tibble(hiring_network), 
       aes(total_placements, log10(out_centrality))) +
    geom_point(aes(text = univ_name))
plotly::ggplotly()

# univ_df %>%
#     mutate(out_centrality_log = log10(out_centrality)) %>%
#     filter(!is.na(out_centrality_log) & 
#                out_centrality > 0 &
#                !is.na(total_placements)) %>%
#     select(total_placements, out_centrality_log) %>%
#     cor()
as_tibble(hiring_network) %>%
    select(total_placements, out_centrality) %>%
    filter(complete.cases(.)) %>%
    mutate(out_centrality = log10(out_centrality)) %>%
    cor()

ggplot(as_tibble(hiring_network), 
       aes(perm_placement_rank, log10(out_centrality))) +
    geom_point() +
    geom_smooth()

ggplot(as_tibble(hiring_network), 
       aes(perm_placement_rate, log10(out_centrality), 
           text = univ_name)) + 
    geom_point()
plotly::ggplotly()

as_tibble(hiring_network) %>%
    select(total_placements, perm_placement_rate, 
           out_centrality) %>%
    filter(complete.cases(.)) %>%
    mutate(out_centrality = log10(out_centrality)) %>%
    cor()

## Movement w/in high-prestige group
# individual_df %>% 
#     filter(permanent) %>%
#     left_join(select(univ_df, univ_id, out_centrality), 
#               by = c('placing_univ_id' = 'univ_id')) %>%
#     left_join(select(univ_df, univ_id, out_centrality),
#               by = c('hiring_univ_id' = 'univ_id')) %>%
#     filter(out_centrality.y > 10^-10) %>%
#     select(person_id, 
#            placing = out_centrality.x, 
#            hiring = out_centrality.y) %>%
#     gather(variable, value, -person_id) %>%
#     mutate(variable = fct_rev(variable)) %>%
#     ggplot(aes(variable, log10(value))) + 
#     geom_point() +
#     geom_line(aes(group = person_id))

## Add central scores to univ_df
univ_df = hiring_network %>%
    as_tibble() %>%
    rename(univ_id = name) %>%
    left_join(univ_df, .)

## Diagonal line indicates where hiring program has the same centrality as the placing program.  
## Most placements are below this line, indicating that the centrality measure captures the idea that people typically are hired by programs with lower status
## The few placements above the line indicate that, even when individuals are hired by programs with higher status, the increase is relatively minor:  no more than 1 point on the log10 scale
individual_df %>% 
    filter(permanent) %>%
    left_join(select(univ_df, univ_id, out_centrality), 
              by = c('placing_univ_id' = 'univ_id')) %>%
    left_join(select(univ_df, univ_id, out_centrality),
              by = c('hiring_univ_id' = 'univ_id')) %>%
    filter(out_centrality.y > 10^-10) %>%
    select(person_id, 
           placing = out_centrality.x, 
           hiring = out_centrality.y) %>%
    ggplot(aes(log10(placing), log10(hiring))) + 
    geom_point() + 
    stat_function(fun = function (x) x)



#' Community detection
#' --------------------
## steps = 26 has low values for both entropy of community sizes (high delta H) and total # communities
## but somewhere along the way this started producing >300 communities? 
## bc ~300 connected components
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
tictoc::tic()
comm_stats = walk_len %>%
    map(~ cluster_walktrap(hiring_network_gc, steps = .x)) %>%
    map(~ list(sizes = sizes(.x), length = length(.x))) %>%
    map_dfr(~ tibble(H = -sum(.x$sizes/sum(.x$sizes) * log2(.x$sizes/sum(.x$sizes))),
                     n_comms = .x$length)) %>%
    mutate(walk_len = walk_len,
           delta_H = log2(n_comms) - H)
tictoc::toc()

ggplot(comm_stats, aes(walk_len, H)) +
    geom_point() +
    geom_smooth()
ggplot(comm_stats, aes(walk_len, n_comms)) +
    geom_point() +
    geom_smooth()
ggplot(comm_stats, aes(n_comms, delta_H)) +
    geom_text(aes(label = walk_len))

#' Select the walk length that minimizes both delta_H and n_comms by looking at regression residuals
walk_length = lm(delta_H ~ n_comms, data = comm_stats) %>%
    augment(comm_stats) %>%
    arrange(.resid) %>%
    pull(walk_len) %>%
    first()

communities = cluster_walktrap(hiring_network_gc, steps = walk_length)
V(hiring_network_gc)$community = membership(communities)
univ_df = univ_df %>%
    left_join({hiring_network_gc %>%
            as_tibble() %>% 
            select(univ_id = name, community) %>%
            mutate(community = as.character(community))})

#' **Finding: There is no correlation between semantic clusters and topological communities.**
univ_df %>%
    filter(!is.na(community), !is.na(cluster_lvl3)) %>%
    select(community, cluster_lvl3) %>%
    table() %>%
    chisq.test(simulate.p.value = TRUE)

univ_df %>%
    filter(!is.na(community), !is.na(cluster_lvl3)) %>%
    count(community, cluster_lvl3) %>%
    rename(cluster_n = n) %>%
    group_by(community) %>%
    mutate(community_tot = sum(cluster_n), 
           cluster_frac = cluster_n / community_tot, 
           H = sum(cluster_frac * log2(cluster_frac))) %>%
    ungroup() %>%
    ggplot(aes(fct_reorder(community, community_tot, .desc = TRUE),
               cluster_n, fill = cluster_lvl3)) + 
    geom_col() + 
    coord_flip() +
    xlab('Topological communities') +
    ylab('# of schools in community') +
    scale_fill_brewer(palette = 'Set1', 
                      name = 'Semantic\nclusters')


#' High-prestige universities
#' --------------------
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
## 51 programs; 7% of all programs in the network; 
## 33% of programs with at least 1 placement in the dataset
length(V(prestigious))
length(V(prestigious)) / length(V(hiring_network))
length(V(prestigious)) / sum(!is.na(univ_df$total_placements))

## What fraction of hires are within high-prestige?  
## 13% of all permanent hires; 6% of all hires
length(E(prestigious)) / length(E(hiring_network))
length(E(prestigious)) / nrow(individual_df)

# png(file = '02_prestigious_net.png', 
#     width = 11, height = 11, units = 'in', res = 400)
set.seed(24)
ggraph(prestigious) + 
    geom_node_label(aes(label = univ_name, 
                        size = log10(out_centrality))) + 
    geom_edge_fan(arrow = arrow(length = unit(.01, 'npc')), 
                  alpha = .25,
                  spread = 5) +
    scale_size_continuous(range = c(.5, 3)) +
    theme_graph()
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

## Prestige status and clusters
## High-prestige basically don't appear in clusters 1 or 2
univ_df %>%
    filter(!is.na(cluster_lvl3)) %>%
    ggplot(aes(cluster_lvl3, color = prestige)) + 
    geom_point(stat = 'count') +
    geom_line(aes(group = prestige), stat = 'count')
## High-prestige are mostly confined to the 3 large communities
univ_df %>%
    filter(!is.na(community)) %>%
    ggplot(aes(as.integer(community), fill = prestige)) + 
    geom_bar()

## What fraction of high-prestige graduates end up in high-prestige programs? 
## 148 / (148 + 425) = 26% of those w/ permanent placements
individual_df %>%
    filter(permanent) %>%
    left_join(univ_df, by = c('placing_univ_id' = 'univ_id')) %>%
    left_join(univ_df, by = c('hiring_univ_id' = 'univ_id')) %>%
    select(placing = prestige.x, hiring = prestige.y) %>%
    table()

#' **Finding: Median permanent placement rate for high-prestige programs is 12 points higher than for low-prestige programs.** 
#' However, variation is also wide within each group; 
#' even among high-prestige programs, median permanent placement rate is only 58%. 
#' This is also not yet controlling for graduation year, area, or demographics. 
ggplot(univ_df, aes(prestige, perm_placement_rate, 
                    label = univ_name)) + 
    geom_boxplot(color = 'red') +
    geom_jitter() +
    scale_y_continuous(labels = scales::percent_format())
plotly::ggplotly()


#' Stability of prestige status
#' --------------------
#' By randomly rewiring the network, we can test the stability of the prestige categories to data errors and the short time frame of our data.  In each of 500 permutations, we randomly rewire 10% of the edges in the permanent hiring network, then calculate out-centralities on the rewired network.  Using a threshold of 10^-7 for high-prestige status, we count the fraction of rewired networks in which each program is high-prestige.  
## ~6 sec
tictoc::tic()
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
    map(~ sum(. > 10^-7) / length(.)) %>%
    map(~ tibble(frac_high_prestige = .)) %>%
    bind_rows(.id = 'univ_id')
tictoc::toc()

## frac_high_prestige indicates the fraction of permutations in which the program was high-prestige
ggplot(permutations, aes(frac_high_prestige)) + 
    geom_density() + 
    geom_rug()

univ_df = left_join(univ_df, permutations)

#' **Finding:** Counterfactual high-prestige status is strongly correlated with centrality ranking. 
#' 
#' **Finding:** At the same time, a number of actual non-high-prestige programs are often (>50%) high-prestige in the rewired networks.  
#' - Notre Dame (86%)
#' - New School (71%)
#' - Penn State (65%)
#' - Tulane (62%)
#' - Boston College (61%)
#' - UC Boulder (56%)
#' 
#' **Finding:** Within the actual categories, there is no correlation between counterfactual high-prestige status and either (a) out-centrality and (b) the number of permanent placements.  
ggplot(univ_df, aes(log10(out_centrality), frac_high_prestige, 
                    text = univ_name)) + 
    geom_point()
plotly::ggplotly()

ggplot(univ_df, aes(perm_placements, frac_high_prestige)) + 
    geom_point()

#' Among low-status programs, a likely explanation is the size of the program's total downstream network.  Notre Dame's is fairly large, with many potential PhDs; if rewiring leads any of these PhDs to a high-status position, both their PhD institution and Notre Dame join the high-status group.  UC Boulder's downstream network is smaller, with fewer opportunities to place into a high-status position.  

make_ego_graph(hiring_network, order = 10, 
               nodes = c(Villanova = '221', Boston_College = '38'),
               mode = 'out') %>% 
    disjoint_union() %>%
    induced_subgraph(., 
                     which(!is.na(V(.)$perm_placements))) %>%
    ggraph() + 
    geom_node_label(aes(label = univ_name, 
                        size = perm_placements)) + 
    geom_edge_fan(arrow = arrow(angle = 45, 
                                length = unit(.1, 'inches'), 
                                type = 'closed')) +
    scale_label_size(range = c(0, 1.5), name = 'placements') + 
    theme_graph()

#' Plotting
#' --------------------
#' Coarser community structure
large_comms = which(sizes(communities) > 20)
V(hiring_network_gc)$community_coarse = ifelse(
    V(hiring_network_gc)$community %in% large_comms, 
    V(hiring_network_gc)$community, 
    NA)

#' FR
hiring_network %>%
    # induced_subgraph(which(degree(., mode = 'out') > 0)) %>%
    ggraph() +
    # geom_node_label(aes(label = univ_name, 
    #                     size = log10(1 + out_centrality), 
    #                     color = as.factor(community))) +
    geom_edge_fan(arrow = arrow(length = unit(.01, 'npc')),
                  spread = 5, alpha = .1) +
    # geom_edge_density() +
    geom_node_point(aes(size = log10(out_centrality), 
                        alpha = log10(out_centrality),
                        # color = as.factor(community))
                        color = cluster_lvl3
                    )) +
    scale_color_brewer(palette = 'Set1', na.value = 'grey40') +
    # scale_size_discrete(range = c(2, 6)) +
    scale_size_continuous(range = c(.1, 5)) +
    theme_graph()

# hiring_network_gc %>%
#     induced_subgraph(!is.na(V(.)$cluster_lvl1)) %>%
#     ggraph() +
#     geom_node_label(aes(size = prestigious, 
#                         label = univ_name,
#                         # color = as.factor(community))) +
#                         fill = cluster_lvl3), color = 'black') +
#     geom_edge_fan(aes(linetype = aos_category, color = aos_category), 
#                   width = 1,
#                   arrow = arrow(length = unit(.01, 'npc')), 
#                   spread = 5) +
#     scale_edge_color_brewer(palette = 'Set1') +
#     scale_fill_brewer(palette = 'Set3', na.value = 'grey75') +
#     scale_size_discrete(range = c(3, 5)) +
#     theme_graph()


#' Chord diagram
hiring_network_gc %>%
    # induced_subgraph(which(degree(., mode = 'out') > 0)) %>%
    ggraph(layout = 'linear', sort.by = 'community_coarse', circular = TRUE) +
    geom_edge_arc(arrow = arrow(length = unit(.01, 'npc')), alpha = .1) +
    geom_node_point(aes(size = prestigious, 
                        # color = as.factor(community))) +
                        color = cluster_lvl3)) +
    scale_color_brewer(palette = 'Set1', guide = FALSE) +
    theme_graph()

## Save university-level data with network statistics
save(univ_df, file = '02_univ_net_stats.Rdata')
