#' # Findings #
#' - 25% of programs account for 50% of permanent placements
#' - Centrality scores reveal a clear division between two groups of programs, with "high" and "low" scores.  
#' - High output isn't correlated with centrality scores.  Programs like Notre Dame, CUNY, and UVA produce lots of PhDs, but they aren't placed at high-centrality programs.  
#' - A closed group of 61 programs can be identified:  All recent hires by schools within this group received their PhD within this group.  This "elite" group corresponds exactly to the high-centrality programs.  
#' - Median permanent placement rate is 14 points higher at elite programs (58% vs. 44% for non-elite programs).  
#' - However, there is large variation in placement rate within both groups.  
#' - [TODO: semantic vs. topological clustering]

library(tidyverse)
library(forcats)
library(igraph)
library(ggraph)

#' Load and clean data
#' --------------------
dataf = readxl::read_excel('queryjan92018.xlsx', 
                           na = c('', 'NULL'), 
                           col_names = c('person_id',
                                         'aos_category',
                                         'aos',
                                         'graduation_year',
                                         'placing_univ_id',
                                         'placing_univ',
                                         'placement_year',
                                         'hiring_univ_id', 
                                         'hiring_univ', 
                                         'position_type',
                                         'permanent'), 
                           col_types = 'text',
                           skip = 1)

dataf = dataf %>%
    mutate(aos_category = fct_recode(aos_category, 
                                     NULL = '1',
                                     'LEMM' = '2',
                                     'Value Theory' = '3',
                                     'History and Traditions' = '4', 
                                     'Science, Logic, and Math' = '5'),
           aos = fct_recode(aos, 
                            'Unknown 1' = '1', 
                            '19th / 20th 4' = '2', 
                            'Action 2' = '3', 
                            'Aesthetics 3' = '4', 
                            'African 4' = '5', 
                            'American (incl Latin American) 4' = '6', 
                            'Analytic (History of) 4' = '8', 
                            'Ancient 4' = '10', 
                            'Applied Ethics (incl Bio and Medical) 3' = '11', 
                            'Asian 4' = '12', 
                            'Biology (incl Environmental) 5' = '13', 
                            'Cognitive Science / Psychology / Neuroscience / Linguistics 5' = '14', 
                            'Comparative 4' = '15', 
                            'Continental (incl Phenomenology) 4' = '16', 
                            'Decision Theory 5' = '17', 
                            'Economics 5' = '18', 
                            'Education 3' = '19', 
                            'Epistemology 2' = '20', 
                            'Ethics 3' = '21', 
                            'Gender / Race / Sexuality / Disability Studies 3' = '22', 
                            'German (incl Kant) 4' = '23', 
                            'History (General) 4' = '24', 
                            'Language 2' = '25', 
                            'Law 3' = '26', 
                            'Logic 5' = '27', 
                            'Math 5' = '28', 
                            'Medieval / Renaissance 4' = '29', 
                            'Meta-Ethics 3' = '30', 
                            'Metaphilosophy (incl Experimental) 2' = '31', 
                            'Metaphysics 2' = '32', 
                            'Mind 2' = '33', 
                            'Modern 4' = '34', 
                            'Physics 5' = '35', 
                            'Religion 2' = '36', 
                            'Science (General) 5' = '37', 
                            'Social / Political 3' = '38', 
                            'Technology 5' = '39', 
                            'Value (General) 3' = '40', 
                            'Other 1' = '41'), 
           graduation_year = as.integer(graduation_year),
           placement_year = as.integer(placement_year),
           position_type = fct_recode(position_type, 
                                      'Tenure-Track' = '1',
                                      'Lecturer (Permanent)' = '2', 
                                      'Instructor (Permanent)' = '3', 
                                      'Adjunct (Permanent)' = '4', 
                                      'Other (Permanent)' = '5', 
                                      'Fellowship/Postdoc' = '11', 
                                      'Visiting' = '12', 
                                      'Lecturer (Temporary)' = '13', 
                                      'Instructor (Temporary)' = '14', 
                                      'Adjunct (Temporary)' = '15', 
                                      'Other (Temporary)' = '16', 
                                      'Non-Academic' = '20'),
           permanent = permanent == '1')

## Filter
dataf = dataf %>%
    filter(#permanent, 
           ## 0 and 10000 are placeholder university IDs
           placing_univ_id != '0', placing_univ_id != '10000',
           hiring_univ_id != '0', hiring_univ_id != '10000',
           !is.na(placing_univ_id), !is.na(placing_univ), 
           !is.na(hiring_univ_id), !is.na(hiring_univ))


#' University-level data
#' --------------------
univ_df = tibble(univ_id = c(dataf$placing_univ_id, dataf$hiring_univ_id),
                 univ_name = c(dataf$placing_univ, dataf$hiring_univ)) %>%
    ## Canonical names
    group_by(univ_id, univ_name) %>%
    summarize(n = n()) %>%
    group_by(univ_id) %>%
    summarize(univ_name = first(univ_name[which(n == max(n))])) %>% 
    ungroup() %>%
    ## (Fake) AOS clusters
    mutate(cluster_fake = as.factor(rep_len(1:4, nrow(.)))) %>%
    arrange(univ_name)
univ_df = dataf %>%
    ## Count number of (permanent) placements out of each program
    rename(univ_id = placing_univ_id) %>%
    group_by(univ_id) %>%
    summarize(total_placements = n(),
              n = sum(permanent, na.rm = TRUE)) %>%
    mutate(perm_placement_rate = n / total_placements) %>%
    ## Fractions and cumulative sums
    arrange(desc(n)) %>%
    mutate(frac_placement = n / sum(n, na.rm = TRUE),
           cum_placements = cumsum(ifelse(is.na(n), 0, n)), 
           cum_frac_placements = cum_placements / 
               sum(n, na.rm = TRUE),
           placement_rank = percent_rank(cum_placements)) %>%
    rename(n_placements = n) %>% 
    left_join(univ_df, .)

#' ## Finding ##
#' **25% of programs account for about 50% of (permanent) placements**
ggplot(univ_df, aes(placement_rank, cum_frac_placements)) + 
    geom_step() +
    scale_x_continuous(labels = scales::percent_format(), 
                       name = 'PhD Programs') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = 'Permanent Placements')


#' Build network
#' --------------------
hiring_network = dataf %>%
    filter(permanent) %>%
    select(placing_univ_id, hiring_univ_id, everything()) %>%
    graph_from_data_frame(directed = TRUE, 
                          vertices = univ_df)

## 1 giant component contains almost all of the schools
components(hiring_network)$csize

#' Out- and in-centrality
set.seed(42)
V(hiring_network)$in_centrality = eigen_centrality(hiring_network, 
                                                   directed = TRUE)$vector
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
    eigen_centrality(directed = TRUE) %>%
    .$vector

## Add centrality statistics to the university df
univ_df = univ_df %>%
    mutate(out_centrality = V(hiring_network)[univ_df$univ_id]$out_centrality, 
           in_centrality = V(hiring_network)[univ_df$univ_id]$in_centrality)


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
                    color = cluster_fake, 
                    text = univ_name)) +
    geom_jitter() + 
    scale_x_log10() + scale_y_log10()
#plotly::ggplotly()


#' ## Finding ##
#' **High output isn't correlated w/ centrality.** 
#' Programs like ND, CUNY, UVA produce lots of PhDs, but they aren't placed into the high-centrality departments. 
ggplot(univ_df, aes(n_placements, log10(out_centrality), 
                    text = univ_name)) +
    geom_point() +
    scale_x_continuous(trans = 'reverse')
#plotly::ggplotly()

ggplot(univ_df, aes(placement_rank, log10(out_centrality))) +
    geom_point()

ggplot(univ_df, aes(perm_placement_rate, log10(out_centrality), 
                    text = univ_name)) + 
    geom_point()
#plotly::ggplotly()

## While individuals can move from low to high centrality in temporary positions, this never happens with permanent positions.  However, this is expected from the way centrality is calculated.  
# dataf %>%
#     # filter(permanent) %>%
#     left_join(select(univ_df, univ_id, out_centrality), 
#           by = c('placing_univ_id' = 'univ_id')) %>%
#     left_join(select(univ_df, univ_id, out_centrality), 
#               by = c('hiring_univ_id' = 'univ_id')) %>%
#     ggplot(aes(log10(out_centrality.x), 
#                log10(out_centrality.y))) + 
#     geom_point() + 
#     xlab('Placing University Centrality') +
#     ylab('Hiring University Centrality') +
#     stat_function(fun = function (x) x, linetype = 'dashed') +
#     facet_grid(~ permanent)




#' Community detection
#' --------------------
## steps = 32 minimizes both entropy of cluster sizes and total # clusters
# walk_len = rep(2:100, 1)
# cluster_stats = walk_len %>%
#   map(~ cluster_walktrap(hiring_network, steps = .x)) %>%
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
#     scale_y_log10() + 
#     coord_flip()
# ggplot(cluster_stats, aes(n_clusters, delta_H)) +
#   geom_text(aes(label = walk_len)) +
#   scale_x_continuous()

communities = cluster_walktrap(hiring_network, steps = 32)
V(hiring_network)$community = membership(communities)
univ_df = univ_df %>%
    mutate(community = as.factor(V(hiring_network)[univ_df$univ_id]$community))

with(univ_df, table(cluster_fake, community)) %>%
    chisq.test(simulate.p.value = TRUE)

univ_df %>%
    group_by(community, cluster_fake) %>%
    summarize(cluster_n = n()) %>%
    group_by(community) %>%
    mutate(community_tot = sum(cluster_n), 
           cluster_frac = cluster_n / community_tot, 
           H = sum(cluster_frac * log2(cluster_frac))) %>%
    ggplot(aes(fct_reorder(community, H, .desc = TRUE),
               cluster_n, fill = cluster_fake)) + 
    geom_col() + 
    coord_flip()


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
length(E(elites)) / length(E(hiring_network))

ggraph(elites) + 
    geom_node_label(aes(label = univ_name, 
                        size = log10(out_centrality))) + 
    geom_edge_fan(arrow = arrow(length = unit(.01, 'npc')), 
                  alpha = .25,
                  spread = 5) +
    scale_size_continuous(range = c(.5, 3)) +
    theme_graph()

## "Elite" status = high centrality group
univ_df = univ_df %>%
    mutate(elite = univ_id %in% V(elites)$name)

ggplot(univ_df, aes(elite, log10(out_centrality))) + 
    geom_jitter()

## What fraction of elite graduates end up in elite programs? 
## 221 / (221 + 569) = 28% of those w/ permanent placements
dataf %>%
    filter(permanent) %>%
    left_join(univ_df, by = c('placing_univ_id' = 'univ_id')) %>%
    left_join(univ_df, by = c('hiring_univ_id' = 'univ_id')) %>%
    select(elite.x, elite.y) %>%
    table()

#' ## Finding ##
#' **Median permanent placement rate for elite programs is 14 points higher than for non-elite programs.** 
#' However, variation is also wide within each group; 
#' even among elite programs, median permanent placement rate is only 58%. 
ggplot(univ_df, aes(elite, perm_placement_rate, 
                    label = univ_name)) + 
    geom_boxplot(color = 'red') +
    geom_jitter() +
    scale_y_continuous(labels = scales::percent_format())
#plotly::ggplotly()


#' Plotting
#' --------------------
#' Coarser community structure
large_clusters = which(sizes(communities) > 100)
V(hiring_network)$community_coarse = ifelse(
    V(hiring_network)$community %in% large_clusters, 
    V(hiring_network)$community, 
    'other')

#' FR
hiring_network %>%
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
hiring_network %>%
    induced_subgraph(which(degree(., mode = 'out') > 1)) %>%
    ggraph(layout = 'linear', sort.by = 'community_coarse', circular = TRUE) +
    geom_edge_arc(arrow = arrow(length = unit(.01, 'npc')), alpha = .1) +
    geom_node_point(aes(size = log10(1 + out_centrality), 
                        # color = as.factor(community))) +
                        color = community_coarse)) +
    scale_color_brewer(palette = 'Set1', guide = FALSE) +
    theme_graph()

