## Extract effects estimates, w/ nice orderings on variables for plotting
posterior_estimates = function(model, prob = .9) {
    fixed = broom.mixed:::tidy.stanreg(model, 
                                       effects = 'fixed',
                                       conf.int = TRUE,
                                       conf.level = prob, 
                                       conf.method = 'HPDinterval')
    varying = broom.mixed:::tidy.stanreg(model, 
                                         effects = 'ran_vals',
                                         conf.int = TRUE, 
                                         conf.level = prob, 
                                         conf.method = 'HPDinterval')
    
    combined = suppressWarnings(bind_rows(fixed, varying))
    
    combined = combined %>% 
        ## Program- vs individual-level variables
        mutate(entity = case_when(term == '(Intercept)' & is.na(group) ~ 'intercept', 
                                  str_detect(term, 'prestige') ~ 'program',
                                  str_detect(term, 'gender') ~ 'individual', 
                                  str_detect(term, 'country') ~ 'program', 
                                  term == 'perc_w' ~ 'program', 
                                  term == 'total_placements' ~ 'program',
                                  group == 'community' ~ 'program (community)',
                                  str_detect(group, 'cluster') ~ 'program',
                                  term == 'average_distance' ~ 'program',
                                  group == 'graduation_year' ~ 'individual',
                                  group == 'placement_year' ~ 'individual',
                                  group == 'aos_category' ~ 'individual', 
                                  term == 'aos_diversity' ~ 'program',
                                  term == 'log10(in_centrality)' ~ 'program',
                                  TRUE ~ NA_character_)) %>% 
        ## Variable groups
        mutate(group = case_when(!is.na(group) ~ as.character(group), 
                                 term == '(Intercept)' ~ 'intercept', 
                                 str_detect(term, 'prestige') ~ 'prestige', 
                                 str_detect(term, 'gender') ~ 'gender', 
                                 str_detect(term, 'country') ~ 'country', 
                                 term == 'perc_w' ~ 'continuous', 
                                 term == 'total_placements' ~ 'continuous', 
                                 term == 'average_distance' ~ 'continuous',
                                 term == 'aos_diversity' ~ 'continuous',
                                 term == 'log10(in_centrality)' ~ 'continuous',
                                 TRUE ~ NA_character_)) %>% 
        ## Nicer labels
        mutate(level = ifelse(!is.na(level), as.character(level), term), 
               level = str_replace_all(level, '_', ' '), 
               level = str_remove(level, '^prestige'), 
               level = case_when(level == 'genderw' ~ 'gender: woman', 
                                 level == 'gendero' ~ 'gender: other', 
                                 TRUE ~ level),
               level = str_remove(level, 'country'), 
               level = str_replace(level, 'perc w', 'women (%)'), 
               level = case_when(str_detect(group, 'cluster') ~ paste('cluster', level), 
                                 group == 'community' ~ paste('community', level), 
                                 TRUE ~ level)
        ) %>% 
        ## Arrange x-axis
        arrange(entity, group, estimate) %>% 
        mutate(group = fct_inorder(group), 
               level = fct_inorder(level), 
               ## Fix year ordering
               level = fct_relevel(level, rev(c('2012', '2013', '2014', 
                                                '2015', '2016', '2017', '2018', 
                                                '2019')))) %>%
        ## Backtransform estimates
        mutate_if(is.numeric, exp)
    
    assert_that(!any(is.na(combined$entity)), msg = 'NA values for entity')
    return(combined)
}
