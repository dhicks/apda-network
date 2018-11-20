## Load and parse data
## Setup --------------------
library(tidyverse)
library(forcats)

individual_df_unfltd = readxl::read_excel('queryjan92018.xlsx', 
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

individual_df_unfltd = individual_df_unfltd %>%
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

## Filtered --------------------
individual_df = individual_df_unfltd %>%
    filter(#permanent, 
        ## 0 and 10000 are placeholder university IDs
        placing_univ_id != '0', placing_univ_id != '10000',
        hiring_univ_id != '0', hiring_univ_id != '10000',
        !is.na(placing_univ_id), !is.na(placing_univ), 
        !is.na(hiring_univ_id), !is.na(hiring_univ))

## University-level data --------------------
## Clusters
clusters_df = readxl::read_xlsx('00_testOfClustering.xlsx') %>%
    mutate_if(is.numeric, as.character)

## Build canonical names + attach clusters
univ_df = tibble(univ_id = c(individual_df$placing_univ_id,
                             individual_df$hiring_univ_id),
                 univ_name = c(individual_df$placing_univ, 
                               individual_df$hiring_univ)) %>%
    ## Canonical names
    group_by(univ_id, univ_name) %>%
    summarize(n = n()) %>%
    group_by(univ_id) %>%
    summarize(univ_name = first(univ_name[which(n == max(n))])) %>% 
    ungroup() %>%
    ## (Fake) AOS clusters
    # mutate(cluster_fake = as.factor(rep_len(1:4, nrow(.)))) %>%
    ## Real AOS clusters
    left_join(select(clusters_df, ID, cluster),
                     by = c('univ_id' = 'ID')) %>%
    arrange(univ_name)

## Calculate statistics from individual-level data
univ_df = individual_df %>%
    ## Count number of (permanent) placements out of each program
    rename(univ_id = placing_univ_id) %>%
    group_by(univ_id) %>%
    summarize(total_placements = n(),
              perm_placements = sum(permanent, na.rm = TRUE)) %>%
    mutate(perm_placement_rate = perm_placements / total_placements) %>%
    ## Fractions and cumulative sums
    arrange(desc(perm_placements)) %>%
    mutate(frac_perm_placements = perm_placements / sum(perm_placements, na.rm = TRUE),
           cum_perm_placements = cumsum(ifelse(is.na(perm_placements), 0, perm_placements)), 
           frac_cum_perm_placements = cum_perm_placements / 
               sum(perm_placements, na.rm = TRUE),
           perm_placement_rank = percent_rank(cum_perm_placements)) %>%
    left_join(univ_df, .)

save(individual_df_unfltd, individual_df, univ_df, file = '01_parsed.Rdata')
