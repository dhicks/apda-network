#' ---
#' output:
#'     html_document: 
#'         self_contained: yes
#'         toc: true
#' ---

## Regression models of placement outcomes
#+ dependencies -----
library(tidyverse)
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

load(str_c(data_folder, '01_parsed.Rdata'))
univ_df = read_rds(str_c(data_folder, '02_univ_net_stats.rds'))

individual_df = individual_df %>%
    left_join(univ_df, by = c('placing_univ_id' = 'univ_id')) %>%
    ## Use the canonical names from univ_df
    select(-placing_univ) %>%
    ## Drop NAs
    # filter(complete.cases(.))
    filter_at(vars('permanent', 'aos_category', 
                   'graduation_year', 'prestige', 
                   'community', 'cluster_lvl3',
                   'gender', 'frac_w', 
                   'frac_high_prestige', 'total_placements'), 
              all_vars(negate(is.na)(.))) %>%
    mutate(perc_w = 100*frac_w, 
           perc_high_prestige = 100*frac_high_prestige)

## Variables to consider: aos_category; graduation_year; placement_year; prestige; out_centrality; cluster; community; placing_univ_id; gender; country; perc_w; total_placements

## Giant pairs plot/correlogram
## perc_high_prestige, out_centrality, and prestige are all tightly correlated
## All other pairs have low to moderate correlation
individual_df %>% 
    select(permanent, aos_category, aos_diversity, perc_high_prestige,
           graduation_year, placement_year, prestige, 
           in_centrality, out_centrality, community, cluster_lvl3, 
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
individual_df %>%
    select(permanent, aos_category, 
           graduation_year, gender) %>%
    gather(key = variable, value = value) %>%
    count(variable, value)

individual_df %>%
    select(prestige, country) %>%
    gather(key = variable, value = value) %>%
    count(variable, value)

individual_df %>%
    select(frac_w, total_placements, perm_placement_rate) %>%
    gather(key = variable, value = value) %>%
    group_by(variable) %>%
    summarize_at(vars(value), funs(min, max, mean, median, sd), 
                 na.rm = TRUE)

## Model -----
#+ model, cache = TRUE
model_file = str_c(data_folder, '03_model.Rds')
if (!file.exists(model_file)) {
    ## ~400 seconds
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
                       (1|cluster_lvl3) +
                       log10(in_centrality) +
                       total_placements +
                       perc_w +
                       country +
                       prestige,
                   family = 'binomial',
                   ## Priors
                   ## Constant and coefficients
                   prior_intercept = normal(0, 1), 
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
                   chains = 2, iter = 4000)
    toc()
    write_rds(model, model_file)
} else {
    model = read_rds(model_file)
}

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
    geom_vline(xintercept = 2000) +
    geom_hline(yintercept = 1.10)
if (require(plotly)) {
    plotly::ggplotly()    
}

## Variables w/ fewer than 2000 effective draws
## Mostly communities, sigmas, log-posterior
## But nb **graduation year and value theory**
model %>% 
    summary() %>% 
    as.data.frame() %>% 
    rownames_to_column('parameter') %>% 
    as_tibble() %>% 
    filter(n_eff < 2000) %>% 
    select(parameter, n_eff)

## Check predictions
pp_check(model, nreps = 200)
pp_check(model, nreps = 200, plotfun = 'ppc_bars')
## <https://arxiv.org/pdf/1605.01311.pdf>
pp_check(model, nreps = 200, plotfun = 'ppc_rootogram')
pp_check(model, nreps = 200, plotfun = 'ppc_rootogram', style = 'hanging')

# pred = predictions(model)

# ggplot(pred, aes(.median, .residual)) +
#     geom_point(aes(color = permanent)) +
#     geom_smooth()
# ggplot(pred, aes(.median, .residual, color = permanent)) +
#     geom_linerange(aes(ymin = .residual - .lower, 
#                        ymax = .residual - .upper))
# 
# ggplot(pred, aes(permanent, .median)) +
#     geom_violin(draw_quantiles = c(.05, .5, .95))

#+ Posterior estimates for coefficients ----
estimates = posterior_estimates(model)

estimates %>% 
    filter(entity != 'intercept', 
           group != 'placement_year') %>% 
    ggplot(aes(x = level, y = estimate, 
           ymin = lower, ymax = upper, 
           color = group)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_pointrange() + 
    scale_color_viridis_d(name = 'covariate\ngroup') +
    xlab('') + ylab('') +
    coord_flip() +
    facet_wrap(~ entity, scales = 'free')

ggplot(estimates, 
       aes(x = level, y = estimate, 
           ymin = lower, ymax = upper, 
           color = entity)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_pointrange() +
    coord_flip() +
    facet_wrap(~ group, scales = 'free_y')


sessionInfo()
