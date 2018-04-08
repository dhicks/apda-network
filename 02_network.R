#' ---
#' output:
#'     html_document: 
#'         self_contained: yes
#'         toc: true
#' ---

#' Findings
#' --------------------
#' - 25% of programs account for 50% of permanent placements
#' - Centrality scores reveal a clear division between two groups of programs, with "high" and "low" scores.  
#' - High output is only modestly correlated with centrality scores.  Programs like Notre Dame, CUNY, and UVA produce lots of PhDs, but they aren't placed at high-centrality programs.  
#' - There is no correlation between semantic clusters and topological communities. 
#' - A closed group of 61 programs can be identified:  All recent hires by schools within this group received their PhD within this group.  This "elite" group corresponds exactly to the high-centrality programs.  
#' - Median permanent placement rate is 14 points higher at elite programs (58% vs. 44% for non-elite programs).  
#' - However, there is large variation in placement rate within both groups.  
#' - Elite status is highly stable; actual elite programs are almost always (>90%) elite in the rewired networks. 
#' - A number of actual non-elite programs are often (>50%) elite in the rewired networks.  
#'     - Notre Dame (86%)
#'     - New School (71%)
#'     - Penn State (65%)
#'     - Tulane (62%)
#'     - Boston College (61%)
#'     - UC Boulder (56%)
#' - Within the actual categories, there is no correlation between counterfactual elite status and either (a) out-centrality and (b) the number of permanent placements.  

library(tidyverse)
library(igraph)
library(ggraph)

## Load data
load('01_parsed.Rdata')

#' **Finding: 25% of programs account for about 50% of (permanent) placements**
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
                          vertices = univ_df)

## 1 giant component contains almost all of the schools
components(hiring_network)$csize

#' Out- and in-centrality
set.seed(42)
V(hiring_network)$in_centrality = eigen_centrality(hiring_network, 
                                                   directed = TRUE,
                                                   weights = NA)$vector
graph.reverse <- function (graph) {
    if (!is.directed(graph))
        return(graph)
    e <- get.data.frame(graph, what="edges")
    ## swap "from" & "to"
    neworder <- 1:length(e)
    neworder[1:2] <- c(2,1)
    e <- e[neworder]
    names(e) <- names(e)[neworder]
    graph.data.frame(e, vertices = get.data.frame(graph, what="vertices"))
}
V(hiring_network)$out_centrality = hiring_network %>%
    graph.reverse() %>%
    eigen_centrality(directed = TRUE, weights = NA) %>%
    .$vector

## Add centrality statistics to the university df
univ_df = univ_df %>%
    mutate(out_centrality = V(hiring_network)[univ_df$univ_id]$out_centrality, 
           in_centrality = V(hiring_network)[univ_df$univ_id]$in_centrality)

## Check relationship btwn unweighted multiedges and weighted edges
## Strong correlation, esp by approx rank and elite/non-elite
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
#' There are two clear groups of centrality scores, with high scores (in the range of 10^-5 to 1) and low scores (10^-15 and smaller). 
##
## NB there seem to be (small?) differences in scores (at the low end?) across runs of the centrality algorithm
ggplot(univ_df, aes(out_centrality)) + 
    geom_density() + geom_rug() +
    scale_x_continuous(trans = 'log10')

ggplot(univ_df, aes(in_centrality)) + 
    geom_density() + geom_rug() +
    scale_x_continuous(trans = 'log10')

ggplot(univ_df, aes(out_centrality, in_centrality, 
                    color = cluster, 
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
# ## Lots of variation among non-elites
# ggplot(iterated_centrality, 
#        aes(reorder(univ_id, median), 
#            median)) + 
#     geom_pointrange(aes(ymin = min, ymax = max)) + 
#     scale_y_log10()
# 
# ## But extremely stable results among elites
# iterated_centrality %>%
#     filter(min > 10^-11) %>%
#     ggplot(aes(reorder(univ_id, median), 
#                median)) + 
#     geom_pointrange(aes(ymin = min, ymax = max)) + 
#     scale_y_log10()


#' **Finding: High output is only modestly correlated w/ centrality.** 
#' Programs like ND, CUNY, New School produce lots of PhDs, but they aren't placed into the high-centrality departments. 
ggplot(univ_df, aes(total_placements, log10(out_centrality)
                    )) +
    geom_point(aes(text = univ_name))
plotly::ggplotly()

univ_df %>%
    mutate(out_centrality_log = log10(out_centrality)) %>%
    filter(!is.na(out_centrality_log) & 
               out_centrality > 0 &
               !is.na(total_placements)) %>%
    select(total_placements, out_centrality_log) %>%
    cor()

ggplot(univ_df, aes(perm_placement_rank, log10(out_centrality))) +
    geom_point()

ggplot(univ_df, aes(perm_placement_rate, log10(out_centrality), 
                    text = univ_name)) + 
    geom_point()
plotly::ggplotly()

## Movement w/in elites
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
## steps = 26 has low values for both entropy of cluster sizes (high delta H) and total # clusters
## but somewhere along the way this started producing >300 clusters? 
## bc ~300 connected components
## Extract giant component
hiring_network_gc = hiring_network %>%
    components %>% 
    .$csize %>%
    {which(. == max(.))} %>%
    {components(hiring_network)$membership == .} %>%
    which() %>%
    induced_subgraph(hiring_network, .)

# walk_len = rep(2:100, 1)
# # ## NB Takes a minute or so
# cluster_stats = walk_len %>%
#   map(~ cluster_walktrap(hiring_network_gc, steps = .x)) %>%
#   map(~ list(sizes = sizes(.x), length = length(.x))) %>%
#   map_dfr(~ tibble(H = -sum(.x$sizes/sum(.x$sizes) * log2(.x$sizes/sum(.x$sizes))),
#                    n_clusters = .x$length)) %>%
#   mutate(walk_len = as.factor(walk_len),
#          delta_H = log2(n_clusters) - H)
# 
# ggplot(cluster_stats, aes(walk_len, delta_H)) +
#     geom_point() +
#     coord_flip()
# ggplot(cluster_stats, aes(walk_len, n_clusters)) +
#     geom_point() +
#     coord_flip()
# ggplot(cluster_stats, aes(n_clusters, delta_H)) +
#   geom_text(aes(label = walk_len)) +
#   scale_x_continuous()


communities = cluster_walktrap(hiring_network_gc, steps = 26)
V(hiring_network_gc)$community = membership(communities)
univ_df = univ_df %>%
    left_join({hiring_network_gc %>%
            as_data_frame(what = 'vertices') %>% 
            select(univ_id = name, community) %>%
            mutate(community = as.character(community))})

#' **Finding: There is no correlation between semantic clusters and topological communities.**
univ_df %>%
    filter(!is.na(community), !is.na(cluster)) %>%
    select(community, cluster) %>%
    table() %>%
    chisq.test(simulate.p.value = TRUE)

univ_df %>%
    filter(!is.na(community), !is.na(cluster)) %>%
    count(community, cluster) %>%
    rename(cluster_n = n) %>%
    group_by(community) %>%
    mutate(community_tot = sum(cluster_n), 
           cluster_frac = cluster_n / community_tot, 
           H = sum(cluster_frac * log2(cluster_frac))) %>%
    ggplot(aes(fct_reorder(community, H, .desc = TRUE),
               cluster_n, fill = cluster)) + 
    geom_col() + 
    coord_flip() +
    xlab('Topological communities') +
    ylab('# of schools in community') +
    scale_fill_brewer(palette = 'Set1', 
                       name = 'Semantic\nclusters')


#' Core elite universities
#' --------------------
## Start w/ Oxford, and work upstream
## Only need to go 8 steps to get closure
1:25 %>%
    map(~ make_ego_graph(hiring_network, order = .x, 
                         nodes = '512', mode = 'in')) %>%
    flatten() %>%
    map(~ length(V(.x))) %>%
    tibble(order = 1:length(.), 
           size = unlist(.)) %>%
    ggplot(aes(order, size)) + geom_point()

elites = make_ego_graph(hiring_network, order = 8, 
                        nodes = '512', 
                        mode = 'in') %>%
    .[[1]]

## How large is the elite community?  
## 61 programs; 6% of all programs in the network; 
## 39% of programs with at least 1 placement in the dataset
length(V(elites))
length(V(elites)) / length(V(hiring_network))
length(V(elites)) / sum(!is.na(univ_df$total_placements))

## What fraction of hires are within elites?  
## 15% of all permanent hires; 7% of all hires
length(E(elites)) / length(E(hiring_network))
length(E(elites)) / nrow(individual_df)

# png(file = '02_elite_net.png', 
#     width = 11, height = 11, units = 'in', res = 400)
ggraph(elites) + 
    geom_node_label(aes(label = univ_name, 
                        size = log10(out_centrality))) + 
    geom_edge_fan(arrow = arrow(length = unit(.01, 'npc')), 
                  alpha = .25,
                  spread = 5) +
    scale_size_continuous(range = c(.5, 3)) +
    theme_graph()
# dev.off()

## "Elite" status = high centrality group
univ_df = univ_df %>%
    mutate(elite = univ_id %in% V(elites)$name)

ggplot(univ_df, aes(elite, log10(out_centrality))) + 
    geom_jitter()

## "Elite" status and clusters
## Elites basically don't appear in clusters 2, 3, 7
univ_df %>%
    filter(!is.na(cluster)) %>%
    ggplot(aes(cluster, color = elite)) + 
    geom_point(stat = 'count') +
    geom_line(aes(group = elite), stat = 'count')
## Elites are mostly confined to the 3 large communities
univ_df %>%
    filter(!is.na(community)) %>%
    ggplot(aes(as.integer(community), fill = elite)) + 
    geom_bar()

## What fraction of elite graduates end up in elite programs? 
## 221 / (221 + 569) = 28% of those w/ permanent placements
individual_df %>%
    filter(permanent) %>%
    left_join(univ_df, by = c('placing_univ_id' = 'univ_id')) %>%
    left_join(univ_df, by = c('hiring_univ_id' = 'univ_id')) %>%
    select(elite.x, elite.y) %>%
    table()

#' **Finding: Median permanent placement rate for elite programs is 14 points higher than for non-elite programs.** 
#' However, variation is also wide within each group; 
#' even among elite programs, median permanent placement rate is only 58%. 
#' This is also not yet controlling for graduation year, area, or demographics. 
ggplot(univ_df, aes(elite, perm_placement_rate, 
                    label = univ_name)) + 
    geom_boxplot(color = 'red') +
    geom_jitter() +
    scale_y_continuous(labels = scales::percent_format())
plotly::ggplotly()


#' Stability of elite status
#' --------------------
#' By randomly rewiring the network, we can test the stability of the elite/non-elite categories to data errors and the short time frame of our data.  In each of 500 permutations, we randomly rewire 10% of the edges in the permanent hiring network, then calculate out-centralities on the rewired network.  Using a threshold of 10^-7 for elite status, we count the fraction of rewired networks in which each program is elite.  
system.time({
    set.seed(13579)
    permutations = 1:500 %>%
        ## Rewire 10% of edges
        map(~ {hiring_network %>%
                rewire(keeping_degseq(loops = TRUE, 
                                      niter = floor(.05 * length(E(.)))))}) %>%
        ## Calculate out-centralities
        map(~ {.x %>%
                graph.reverse() %>%
                eigen_centrality(directed = TRUE, weights = NA) %>%
                .$vector}) %>%
        transpose() %>%
        map(unlist) %>%
        ## Fraction where program is elite
        map(~ sum(. > 10^-7) / length(.)) %>%
        map(~ tibble(frac_elite = .)) %>%
        bind_rows(.id = 'univ_id')
})

## frac_elite indicates the fraction of permutations in which the program was elite
ggplot(permutations, aes(frac_elite)) + 
    geom_density() + 
    geom_rug()

univ_df = left_join(univ_df, permutations)

#' **Finding:** Elite status is highly stable; actual elite programs are almost always (>90%) elite in the rewired networks. 
#' 
#' **Finding:** A number of actual non-elite programs are often (>50%) elite in the rewired networks.  
#' - Notre Dame (86%)
#' - New School (71%)
#' - Penn State (65%)
#' - Tulane (62%)
#' - Boston College (61%)
#' - UC Boulder (56%)
#' 
#' **Finding:** Within the actual categories, there is no correlation between counterfactual elite status and either (a) out-centrality and (b) the number of permanent placements.  
ggplot(univ_df, aes(log10(out_centrality), frac_elite, 
                    text = univ_name)) + 
    geom_point()
plotly::ggplotly()

ggplot(univ_df, aes(perm_placements, frac_elite)) + 
    geom_point()



#' Plotting
#' --------------------
#' Coarser community structure
large_clusters = which(sizes(communities) > 100)
V(hiring_network_gc)$community_coarse = ifelse(
    V(hiring_network_gc)$community %in% large_clusters, 
    V(hiring_network_gc)$community, 
    'other')

#' FR
hiring_network_gc %>%
    induced_subgraph(which(degree(., mode = 'out') > 10)) %>%
    ggraph() +
    # geom_node_label(aes(label = univ_name, 
    #                     size = log10(1 + out_centrality), 
    #                     color = as.factor(community))) +
    geom_edge_fan(arrow = arrow(length = unit(.01, 'npc')), 
                  spread = 5) +
    geom_node_point(aes(size = log10(1 + out_centrality), 
                        # color = as.factor(community))) +
                        color = community_coarse)) +
    scale_color_brewer(palette = 'Set1', guide = FALSE) +
    scale_size(range = c(2, 6)) +
    theme_graph()

#' Chord diagram
hiring_network_gc %>%
    induced_subgraph(which(degree(., mode = 'out') > 1)) %>%
    ggraph(layout = 'linear', sort.by = 'community_coarse', circular = TRUE) +
    geom_edge_arc(arrow = arrow(length = unit(.01, 'npc')), alpha = .1) +
    geom_node_point(aes(size = log10(1 + out_centrality), 
                        # color = as.factor(community))) +
                        color = community_coarse)) +
    scale_color_brewer(palette = 'Set1', guide = FALSE) +
    theme_graph()

## Save university-level data with network statistics
save(univ_df, file = '02_univ_net_stats.Rdata')
