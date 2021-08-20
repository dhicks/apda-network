## Load and parse data
## Setup --------------------
library(tidyverse)
library(assertthat)
library(tidylog)

data_folder = '../data/'

individual_df_raw = read_csv(str_c(data_folder, 
                                   '00_PaperDataAPDA_2021-08-18.csv'), 
                             na = c('', 'NULL'), 
                             col_names = c('gender', 
                                           'ethnicity', 
                                           'race',
                                           'aos_category',
                                           'aos',
                                           'graduation_year',
                                           'placing_univ_id',
                                           'placing_univ',
                                           'placement_year',
                                           'hiring_univ', 
                                           'hiring_univ_id', 
                                           'position_type'), 
                             col_types = str_dup('c', 12),
                             skip = 1)

## Missingness codes are 4 for ethnicity and 7 for race
## Almost all individuals are missing ethnicity and race
count(individual_df_raw, ethnicity)
count(individual_df_raw, race)

individual_df_unfltd = individual_df_raw %>%
    mutate(gender = fct_recode(gender, 
                               'm' = '1', 
                               'w' = '2', 
                               NULL = '3', 
                               'o' = '4', 
                               NULL = '5'),
           aos_category = fct_recode(aos_category, 
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
           placing_univ = str_replace(placing_univ, '__ÃŒÃ¶', "'"),
           hiring_univ = str_replace(hiring_univ, '__ÃŒÃ¶', "'"),
           permanent = as.integer(position_type) <= 5,
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
                                      'Non-Academic' = '20'))

## To ID erroneous universities:  
# universities = readxl::read_excel('00_HicksData.xlsx',
#                                   sheet = 'Universities') %>%
#     mutate(hiring_univ_id = as.character(`university id`))
# anti_join(individual_df_unfltd, universities) %>%
#     filter(graduation_year >= 2012,
#            graduation_year <= 2016,
#            placement_year >= 2012) %>% View


## Filtering --------------------
## No more 0s in placing_univ_id, but some NAs
count(individual_df_unfltd, placing_univ_id == '0')
## Still some 10000s = placing univ is Unknown
count(individual_df_unfltd, placing_univ_id == '10000')
# individual_df_unfltd %>% filter(placing_univ_id == '10000') %>% View

## Some 10000s in hiring_univ_id, but some NAs
count(individual_df_unfltd, hiring_univ_id == '10000')

## Only have good data for 2012-2018
## Hard to tell if 2019 is part of the downward trend
ggplot(individual_df_unfltd, aes(graduation_year)) +
    geom_bar() +
    xlim(2000, NA)

individual_df = individual_df_unfltd %>%
    ## Remove error code university IDs
    filter(placing_univ_id != '0', placing_univ_id != '10000',
           !(hiring_univ_id %in% c('0', '10000')),
           !is.na(placing_univ_id), !is.na(placing_univ),
           !is.na(hiring_univ_id), !is.na(hiring_univ)) %>% 
    ## Filter down to cohorts with reliable data
    filter(graduation_year >= 2012, graduation_year <= 2019, 
           placement_year >= 2012) %>% 
    ## Remove duplicates
    filter(!duplicated(.)) %>% 
    mutate(person_id = row_number()) %>% 
    select(person_id, everything())


## University-level data --------------------
univ_location = read_csv(str_c(data_folder, 
                               '00_university_table_2018-11-09.csv')) %>%
    rename(univ_id = id, 
           univ_name = name) %>%
    mutate(univ_id = as.character(univ_id), 
           country = case_when(country == 'England' ~ 'U.K.',
                               country == 'U.K' ~ 'U.K.', 
                               TRUE ~ country))

## Clusters
cluster_labels = tribble(
    ~ cluster_3, ~ cluster_label, 
    1, 'Analytic', 
    2, 'Continental',
    3, 'Science') %>% 
    mutate_at(vars(cluster_3), as.character)

clusters_df = read_rds(str_c(data_folder, 
                             '01_university_and_cluster.Rds')) %>% 
    mutate_if(is.factor, as.character) %>% 
    rename_at(vars(contains('k')), 
              funs(str_replace(., 'k.', 'cluster_'))) %>%
    left_join(cluster_labels)

assert_that(all(!is.na(clusters_df$cluster_label)), 
            msg = 'Missing cluster labels')

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
    ## AOS clusters
    left_join(clusters_df,
              by = c('univ_id' = 'University.ID')) %>%
    arrange(univ_name) %>%
    select(-University.Name) %>% 
    ## Location
    left_join(univ_location, by = 'univ_id', 
              suffix = c('', '_y')) %>%
    select(-matches('_y'))

univ_df %>% 
    pull(cluster_label) %>% 
    is.na() %>% 
    `!`() %>% 
    any() %>% 
    assert_that(msg = 'All cluster labels are NA')


## Placement totals, rates, and cumulative distributions
## These *do* include non-academic placements, which *do not* count as permanent
placement_df = individual_df %>%
    ## Count number of total & perm placements out of each program
    rename(univ_id = placing_univ_id) %>%
    group_by(univ_id) %>%
    summarize(total_placements = n(),
              perm_placements = sum(permanent, na.rm = TRUE)) %>%
    mutate(perm_placement_rate = perm_placements / total_placements) %>%
    ## Fractions and cumulative sums
    arrange(desc(perm_placements)) %>%
    mutate(frac_perm_placements = perm_placements / sum(perm_placements, 
                                                        na.rm = TRUE),
           cum_perm_placements = cumsum(ifelse(is.na(perm_placements), 
                                               0, 
                                               perm_placements)), 
           frac_cum_perm_placements = cum_perm_placements / 
               sum(perm_placements, na.rm = TRUE),
           perm_placement_rank = percent_rank(cum_perm_placements))

## Program-level AOS diversity
h_df = individual_df %>%
    mutate(aos_category = fct_explicit_na(aos_category)) %>% 
    count(univ_id = placing_univ_id, aos_category) %>%
    group_by(univ_id) %>%
    mutate(frac = n / sum(n)) %>%
    summarize(aos_diversity = -sum(frac * log2(frac)))

## Program-level fraction women, other
gender_df = individual_df %>%
    mutate(gender = fct_explicit_na(gender)) %>% 
    count(univ_id = placing_univ_id, gender) %>%
    spread(key = gender, value = n, fill = 0) %>%
    rename(m_count = m, w_count = w, o_count = o, 
           gender_na_count = `(Missing)`) %>%
    mutate(frac_w = w_count / (m_count + w_count + o_count + gender_na_count))

## Combine program-level dataframes
univ_df = univ_df %>%
    left_join(placement_df) %>%
    left_join(h_df) %>%
    left_join(gender_df)

## Output ----
save(individual_df_unfltd, individual_df, univ_df, 
     file = str_c(data_folder, '02_parsed.Rdata'))

## Reproducibility ----
sessionInfo()
