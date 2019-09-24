#' ---
#' output:
#'     html_document: 
#'         self_contained: yes
#'         toc: true
#' ---

## Regression models of placement outcomes
#+ dependencies -----
library(tidyverse)
library(cowplot)
library(broom)
library(forcats)
library(rstanarm)
options(mc.cores = parallel::detectCores() - 2)
## bayesplot makes itself the default theme
theme_set(theme_minimal())

library(tictoc)
library(assertthat)

## Suppress messages when generating HTML file
knitr::opts_chunk$set(message = FALSE)

source('../R/predictions.R')
source('../R/posterior_estimates.R')

#+ load_data -----
data_folder = '../data/'
output_folder = '../plots/'
paper_folder = '../paper/'

# cluster_distances = read_csv(str_c(data_folder, 
#                                    '00_k9distances_2019-03-15.csv')) %>% 
#     count(cluster = cluster, average_distance = avgDist) %>% 
#     mutate(cluster = as.character(cluster))
# 
# ggplot(cluster_distances, aes(cluster, scale(average_distance))) +
#     geom_label(aes(label = n, fill = n, size = n), color = 'white')

load(str_c(data_folder, '01_parsed.Rdata'))
univ_df = read_rds(str_c(data_folder, '02_univ_net_stats.rds')) #%>% 
    # left_join(cluster_distances)

individual_df = individual_df %>%
    left_join(univ_df, by = c('placing_univ_id' = 'univ_id')) %>%
    ## Use the canonical names from univ_df
    select(-placing_univ) %>%
    ## Drop NAs
    # filter(complete.cases(.))
    filter_at(vars('permanent', 'aos_category', 
                   'graduation_year', 'prestige', 
                   'community', 'cluster_label',
                   'gender', 'frac_w', 
                   'frac_high_prestige', 'total_placements'), 
              all_vars(negate(is.na)(.))) %>%
    rename(cluster = cluster_label) %>% 
    mutate(perc_w = 100*frac_w, 
           perc_high_prestige = 100*frac_high_prestige)

## Variables to consider: aos_category; graduation_year; placement_year; prestige; out_centrality; cluster; community; placing_univ_id; gender; country; perc_w; total_placements

## Giant pairs plot/correlogram ----
## perc_high_prestige, out_centrality, and prestige are all tightly correlated
## All other pairs have low to moderate correlation
individual_df %>% 
    select(permanent, aos_category, aos_diversity, perc_high_prestige,
           graduation_year, placement_year, prestige, 
           in_centrality, out_centrality, community, 
           cluster, #average_distance,
           gender, country, perc_w, 
           total_placements) %>% 
    mutate_if(negate(is.numeric), function(x) as.integer(as.factor(x))) %>% 
    mutate_at(vars(in_centrality, out_centrality), log10) %>% 
    # GGally::ggpairs()
    cor() %>% 
    as_tibble(rownames = 'Var1') %>% 
    gather(key = 'Var2', value = 'cor', -Var1) %>% 
    ggplot(aes(Var1, Var2, fill = cor)) +
    geom_tile() +
    geom_text(aes(label = round(cor, digits = 2)), 
              color = 'white') +
    scale_fill_gradient2()

## No indication that AOS diversity has any effect
ggplot(individual_df, aes(aos_diversity, 1*permanent)) + 
    geom_point() +
    geom_smooth(method = 'loess')

## And not for fraction of PhDs awarded to women women, either
ggplot(individual_df, aes(frac_w, 1*permanent)) +
    geom_point() +
    geom_smooth(method = 'loess')


## Descriptive statistics ----
## Individual-level variables (all discrete)
desc_1_plot = individual_df %>%
    select(permanent, aos_category, 
           graduation_year, placement_year, 
           gender) %>%
    gather(key = variable, value = value) %>%
    count(variable, value) %>% 
    mutate(variable = str_replace_all(variable, '_', ' ')) %>% 
    ggplot(aes(fct_rev(value), n, group = variable)) +
    geom_col(aes(fill = variable), show.legend = FALSE) +
    scale_fill_brewer(palette = 'Set1') +
    xlab('') +
    coord_flip() +
    facet_wrap(vars(variable), scales = 'free', ncol = 3)
desc_1_plot
ggsave(str_c(output_folder, '03_descriptive_1.png'), 
       desc_1_plot, 
       height = 2*2, width = 2*3, scale = 1.5)

## Program-level categorical
desc_2_plot = individual_df %>%
    select(prestige, country, 
           community, cluster) %>%
    gather(key = variable, value = value) %>%
    count(variable, value) %>% 
    ggplot(aes(fct_rev(value), n, group = variable)) +
    geom_col(aes(fill = variable), show.legend = FALSE) +
    scale_fill_viridis_d() +
    xlab('') +
    coord_flip() +
    facet_wrap(vars(variable), scales = 'free', ncol = 3)
desc_2_plot
ggsave(str_c(output_folder, '03_descriptive_2.png'), 
       desc_2_plot, 
       height = 2*2, width = 2*2, scale = 1.5)

## Program-level continuous variables
# individual_df %>%
#     select(frac_w, total_placements, perm_placement_rate) %>%
#     gather(key = variable, value = value) %>%
#     group_by(variable) %>%
#     summarize_at(vars(value), 
#                  funs(min, max, mean, median, sd), 
#                  na.rm = TRUE)

program_cont = individual_df %>% 
    mutate(in_centrality = log10(in_centrality)) %>% 
    select(`women share` = frac_w, 
           `total placements` = total_placements, 
           `permanent placement rate` = perm_placement_rate, 
           `AOS diversity (bits)` = aos_diversity,
           `hiring centrality (log10)` = in_centrality) %>% 
    gather(key = variable, value = value)

desc_3_plot = ggplot(program_cont, aes(value)) +
    geom_density() +
    geom_rug() +
    geom_vline(data = {program_cont %>% 
            group_by(variable) %>% 
            summarize(mean = mean(value))}, 
            aes(xintercept = mean, 
                color = 'mean')) +
    geom_vline(data = {program_cont %>% 
            group_by(variable) %>% 
            summarize(median = median(value))}, 
            aes(xintercept = median, 
                color = 'median')) +
    scale_color_brewer(palette = 'Set1', 
                       name = 'summary\nstatistic') +
    facet_wrap(~ variable, scales = 'free', ncol = 3) +
    theme(legend.position = 'bottom')
desc_3_plot
ggsave(str_c(output_folder, '03_descriptive_3.png'), 
       desc_3_plot, 
       height = 2*2, width = 2*3.5, scale = 1.5)

plot_grid(desc_1_plot, 
          desc_2_plot, 
          desc_3_plot, 
          align = 'v', axis = 'lr', ncol = 1, 
          labels = 'AUTO',
          hjust = -2
          )
ggsave(str_c(output_folder, '03_descriptive.png'), 
       height = 6.5*2, width = 4*2, scale = 1.5)
ggsave(str_c(paper_folder, 'fig_descriptive.png'), 
       height = 6.5*2, width = 4*2, scale = 1.5)



## Model -----
#+ model, cache = FALSE
model_file = str_c(data_folder, '03_model.Rds')
if (!file.exists(model_file)) {
    ## ~700 seconds
    tic()
    model = individual_df %>% 
        mutate(prestige = fct_relevel(prestige, 'low-prestige'), 
               country = fct_relevel(country, 'U.S.')) %>% 
        stan_glmer(formula = permanent ~ 
                       (1|aos_category) +
                       gender + 
                       (1|graduation_year) +
                       (1|placement_year) +
                       1 +
                       aos_diversity +
                       (1|community) +
                       (1|cluster) +
                       # average_distance +
                       log10(in_centrality) +
                       total_placements +
                       perc_w +
                       country +
                       prestige,
                   family = 'binomial',
                   ## Priors
                   ## Constant and coefficients
                   prior_intercept = normal(0, .5), ## constant term + random intercepts
                   prior = normal(0, .5),
                   ## error sd
                   prior_aux = exponential(rate = 1, 
                                           autoscale = TRUE),
                   ## random effects covariance
                   prior_covariance =  decov(regularization = 1, 
                                             concentration = 1, 
                                             shape = 1, scale = 1),
                   seed = 1159518215,
                   adapt_delta = .99,
                   chains = 4, iter = 4000)
    toc()
    write_rds(model, model_file)
} else {
    model = read_rds(model_file)
}

prior_summary(model)

#+ model_output, fig.dim = c(2.5*5, 5)
## Check ESS and Rhat
## Rhats all look good.  ESS a little low for grad years + some sigmas
model %>%
    summary() %>%
    as.data.frame() %>%
    rownames_to_column('parameter') %>%
    select(parameter, n_eff, Rhat) %>%
    # knitr::kable()
    ggplot(aes(n_eff, Rhat, label = parameter)) +
    geom_point() +
    geom_vline(xintercept = 4000) +
    geom_hline(yintercept = 1.01)
if (require(plotly)) {
    plotly::ggplotly()    
}

## Variables w/ fewer than 3000 effective draws
## covariance on random intercepts; log posterior
model %>% 
    summary() %>% 
    as.data.frame() %>% 
    rownames_to_column('parameter') %>% 
    as_tibble() %>% 
    filter(n_eff < 3000) %>% 
    select(parameter, n_eff)

## Check predictions
pp_check(model, nreps = 200)
pp_check(model, nreps = 200, plotfun = 'ppc_bars')
## <https://arxiv.org/pdf/1605.01311.pdf>
pp_check(model, nreps = 200, plotfun = 'ppc_rootogram')
pp_check(model, nreps = 200, plotfun = 'ppc_rootogram', 
         style = 'hanging')


#+ Posterior estimates for coefficients ----
## 90% centered posterior intervals
estimates = posterior_estimates(model, prob = .9)

estimates %>% 
    filter(entity != 'intercept', 
           group != 'placement_year') %>% 
    ## posterior_estimates() already exponentiates estimates
    mutate_if(is.numeric, ~ . - 1) %>% 
    ggplot(aes(x = level, y = estimate, 
           ymin = lower, ymax = upper, 
           color = group)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_pointrange() + 
    scale_color_viridis_d(name = 'covariate\ngroup') +
    xlab('') + #ylab('') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = '') +
    coord_flip() +
    facet_wrap(~ entity, scales = 'free') +
    theme(legend.position = 'bottom')

ggsave(str_c(output_folder, '03_estimates.png'), 
       width = 6, height = 6, 
       scale = 1.5)
ggsave(str_c(paper_folder, 'fig_reg_estimates.png'), 
       width = 6, height = 6, 
       scale = 1.5)

estimates %>% 
    filter(entity != 'intercept', 
           group != 'placement_year') %>% 
    select(group, level, estimate, lower, upper) %>% 
    mutate_if(is.factor, as.character) %>% 
    arrange(group, level) %>% 
    knitr::kable(format = 'latex', 
                 digits = 2,
                 booktabs = TRUE, 
                 label = 'estimates', 
                 caption = 'Estimated regression coefficients.  Lower and upper columns give the left and right endpoints, respectively, of the centered 90\\% posterior intervals.') %>% 
    write_file(path = str_c(output_folder, '03_estimates.tex'))


## Marginal effects for gender and prestige ----
## <https://stackoverflow.com/questions/45037485/calculating-marginal-effects-in-binomial-logit-using-rstanarm>
marginals = function (dataf, model, variable, 
                      ref_value = 0L, 
                      alt_value = 1L) {
    variable = enquo(variable)
    
    all_0 = mutate(dataf, !!variable := ref_value)
    all_1 = mutate(dataf, !!variable := alt_value)
    
    pred_0 = posterior_linpred(model, newdata = all_0, 
                               transform = TRUE)
    pred_1 = posterior_linpred(model, newdata = all_1, 
                               transform = TRUE)
    
    marginal_effect = pred_1 - pred_0
    return(marginal_effect)
}

marginals_gender = individual_df %>% 
    ## posterior_linpred raises an error when there are any NAs, even in columns that aren't used by the model
    select(-city, -state) %>% 
    marginals(model, gender, 
              ref_value = 'm', 
              alt_value = 'w')

apply(marginals_gender, 1, mean) %>% 
    quantile(probs = c(.05, .5, .95))
#         5%        50%        95% 
# 0.06819497 0.10771227 0.14670850 


marginals_prestige = individual_df %>% 
    select(-city, -state) %>% 
    marginals(model, prestige, 'low-prestige', 'high-prestige')

apply(marginals_prestige, 1, mean) %>% 
    quantile(probs = c(.05, .5, .95))
#          5%        50%        95% 
# 0.07489617 0.11984656 0.16493118

marginals_canada = individual_df %>% 
    select(-city, -state) %>% 
    marginals(model, country, 'U.S.', 'Canada') %>% 
    apply(1, mean) %>% 
    quantile(probs = c(.05, .5, .95))


## Schools in certain communities ----
comms_of_interest = c(3, 5, 12, 37, 54, 
                         8, 27, 38, 43) %>% 
    as.character()

univ_df %>% 
    filter(community %in% comms_of_interest, 
           total_placements > 0) %>% 
    select(community, name = univ_name, 
           total_placements, perm_placement_rate) %>% 
    mutate(community = fct_relevel(community, comms_of_interest), 
           perm_placement_rate = scales::percent_format()(perm_placement_rate)) %>% 
    arrange(community, name) %>% 
    knitr::kable(format = 'latex', 
                 # digits = 2,
                 booktabs = TRUE, 
                 label = 'comms', 
                 caption = 'Universities in selected topological communities.  Only universities with at least 1 placement in the data are shown.') %>% 
    write_file(path = str_c(output_folder, '03_comms.tex'))


sessionInfo()
