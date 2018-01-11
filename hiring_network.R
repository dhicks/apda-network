library(tidyverse)
library(forcats)
library(igraph)
library(ggraph)

## Load and clean data
## --------------------
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
  filter(permanent, 
         ## 0 and 10000 are placeholder university IDs
         placing_univ_id != '0', placing_univ_id != '10000',
         hiring_univ_id != '0', hiring_univ_id != '10000',
         !is.na(placing_univ_id), !is.na(placing_univ), 
         !is.na(hiring_univ_id), !is.na(hiring_univ))


## University-level data
## --------------------
univ_df = tibble(univ_id = c(dataf$placing_univ_id, dataf$hiring_univ_id),
       univ_name = c(dataf$placing_univ, dataf$hiring_univ)) %>%
  ## Canonical names
  group_by(univ_id) %>%
  summarize(univ_name = min(univ_name)) %>%
  ## (Fake) AOS clusters
  mutate(cluster_fake = as.factor(rep_len(1:4, nrow(.)))) %>%
  arrange(univ_name)


## --------------------
## Build network
hiring_network = dataf %>%
    select(placing_univ_id, hiring_univ_id, everything()) %>%
    graph_from_data_frame(directed = TRUE, 
                          vertices = univ_df)

## 1 giant component contains almost all of the schools
components(hiring_network)$csize

## Out- and in-centrality
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

univ_df = univ_df %>%
  mutate(out_centrality = V(hiring_network)[univ_df$univ_id]$out_centrality, 
         in_centrality = V(hiring_network)[univ_df$univ_id]$in_centrality)


## --------------------
## Exploring centrality scores
## NB there seem to be (small?) differences in scores (at the low end?) across runs of the centrality algorithm
ggplot(univ_df, aes(out_centrality)) + 
  geom_density() + geom_rug() +
  scale_x_continuous(trans = 'log10')

ggplot(univ_df, aes(in_centrality)) + 
  geom_density() + geom_rug() +
  scale_x_continuous(trans = 'log10')

ggplot(univ_df, aes(out_centrality, in_centrality, 
                    color = cluster_fake)) +
  geom_jitter() + 
  scale_x_log10() + scale_y_log10()

## --------------------
## Community detection
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
# ggplot(cluster_stats, aes(walk_len, delta_H)) + geom_point() + coord_flip()
# ggplot(cluster_stats, aes(walk_len, n_clusters)) + geom_point() +
#   scale_y_log10() + coord_flip()
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
  

## --------------------
## Core elite universities
## Start w/ Oxford, and work upstream up to 100 steps
elites = make_ego_graph(hiring_network, order = 100, 
                        nodes = '512', 
                        mode = 'in') %>%
  .[[1]]

ggraph(elites) + 
  geom_node_label(aes(label = univ_name, size = out_centrality)) + 
  geom_edge_fan(arrow = arrow(length = unit(.01, 'npc')), 
                alpha = .25,
                spread = 5) +
  theme_graph()


## --------------------
## Plotting
## Coarser community structure
large_clusters = which(sizes(communities) > 100)
V(hiring_network)$community_coarse = ifelse(
  V(hiring_network)$community %in% large_clusters, 
  V(hiring_network)$community, 
  'other')

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

## Chord diagram
hiring_network %>%
  induced_subgraph(which(degree(., mode = 'out') > 1)) %>%
  ggraph(layout = 'linear', sort.by = 'community_coarse', circular = TRUE) +
  geom_edge_arc(arrow = arrow(length = unit(.01, 'npc')), alpha = .1) +
  geom_node_point(aes(size = log10(1 + out_centrality), 
                      # color = as.factor(community))) +
                      color = community_coarse)) +
  scale_color_brewer(palette = 'Set1', guide = FALSE) +
  theme_graph()


# graph_df = as_data_frame(graph, what = 'vertices')
# 
# graph_df %>%
#     filter(name != 'University of Oxford') %>%
#     ggplot(aes(log1p(in_centrality), log1p(out_centrality))) + 
#     geom_point(position = 'jitter')
