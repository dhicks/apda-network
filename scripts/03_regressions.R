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
library(tictoc)
options(mc.cores = parallel::detectCores() - 2)
## bayesplot makes itself the default themetheme_set(theme_minimal())

## Suppress messages when generating HTML file
knitr::opts_chunk$set(message = FALSE)

## broom.mixed supposedly has an augment() method for rstanarm (stangreg objects); 
## but I can't find its documentation to configure its options. 
predictions = function (model, lower = .055, upper = 1 - lower) {
    predictions_mx = posterior_linpred(model)
    medians = apply(predictions_mx, 2, median)
    lowers = apply(predictions_mx, 2, quantile, probs = lower)
    uppers = apply(predictions_mx, 2, quantile, probs = upper)
    
    dataf = tibble(.median = medians, 
                   .lower = lowers, 
                   .upper = uppers)
    dataf = mutate_all(dataf, gtools::inv.logit)
    dataf$.residual = model$residuals
    dataf = cbind(dataf, model$data)
    return(dataf)
}

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
                   'graduation_year', 'prestige', 'community', 
                   'gender', 'frac_w', 
                   'frac_high_prestige', 'total_placements'), 
              all_vars(negate(is.na)(.))) %>%
    mutate(perc_w = frac_w, 
           perc_high_prestige = frac_high_prestige)

## Variables to consider: aos_category; aos; graduation_year; placement_year; placing_univ_id; cluster; out_centrality; prestige; community; demographics

## No indication that AOS diversity has any effect
ggplot(individual_df, aes(aos_diversity, 1*permanent)) + 
    geom_point() +
    geom_smooth(method = 'loess')

## And not for fraction of PhDs from program women, either
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
    select(fper_w, total_placements, perm_placement_rate) %>%
    gather(key = variable, value = value) %>%
    group_by(variable) %>%
    summarize_at(vars(value), funs(min, max, mean, median, sd), 
                 na.rm = TRUE)

## Model 1 --------------------
#+ model_1, cache = TRUE
## AOS alone
model_1 = stan_glmer(data = individual_df, 
             formula = permanent ~ 0 + (1|aos_category), 
             family = 'binomial', 
             ## Priors
             ## error sd
             prior_aux = exponential(rate = 1, 
                                     autoscale = TRUE),
             ## random effects covariance
             prior_covariance =  decov(regularization = 1, 
                                       concentration = 1, 
                                       shape = 1, scale = 1),
             chains = 2, iter = 2000)

## Posterior predictive checks
pp_check(model_1, nreps = 50)
## This comes from Gelman and Hill (2006), 97 fig 5.13b
# pp_check(model_1, plotfun = 'error_binned', nreps = 6)

pred_1 = predictions(model_1)

ggplot(pred_1, aes(graduation_year, .residual)) + 
    geom_jitter() +
    # stat_summary(color = 'red')
    geom_smooth(method = 'loess')


## Model 2 --------------------
#+ model_2, cache = TRUE
# AOS + grad year
model_2 = stan_glmer(data = individual_df, 
                     formula = permanent ~ 0 + (1|aos_category) +
                         (1|graduation_year), 
                     family = 'binomial', 
                     ## Priors
                     ## error sd
                     prior_aux = exponential(rate = 1, 
                                             autoscale = TRUE),
                     ## random effects covariance
                     prior_covariance =  decov(regularization = 1, 
                                               concentration = 1, 
                                               shape = 1, scale = 1),
                     chains = 2, iter = 2000)

pp_check(model_2, nreps = 50)

pred_2 = predictions(model_2)

ggplot(pred_2, aes(prestige, .residual)) + 
    geom_jitter() +
    stat_summary(color = 'red')
# geom_smooth(method = 'loess', color = 'red')

# model_2 %>%
#     tidy(parameters = 'varying', intervals = TRUE) %>%
#     select(-std.error) %>%
#     mutate_if(is.numeric, gtools::inv.logit) %>%
#     ggplot(aes(x = level, y = estimate, 
#                ymin = lower, ymax = upper)) +
#     geom_hline(yintercept = .5, linetype = 'dashed') +
#     geom_pointrange() + 
#     coord_flip()

## Model 3 --------------------
#+ model_3, cache = TRUE
# AOS + grad year + program prestige status
model_3 = stan_glmer(data = individual_df, 
                     formula = permanent ~ 0 + 
                         (1|aos_category) +
                         (1|graduation_year) + 
                         (1|prestige), 
                     family = 'binomial', 
                     ## Priors
                     ## error sd
                     prior_aux = exponential(rate = 1, 
                                             autoscale = TRUE),
                     ## random effects covariance
                     prior_covariance =  decov(regularization = 1, 
                                               concentration = 1, 
                                               shape = 1, scale = 1),
                     chains = 2, iter = 2000)

pp_check(model_3, nreps = 50)

pred_3 = predictions(model_3)
ggplot(pred_3, aes(graduation_year, .residual)) +
    geom_point() +
    stat_summary(color = 'red')
ggplot(pred_3, aes(community, .residual)) + 
    geom_point() +
    stat_summary(color = 'red')#

# Model 4 --------------------
#+ model_4, cache = TRUE
# AOS + grad year + program prestige status + clusters and communities
model_4 = stan_glmer(data = individual_df, 
                     formula = permanent ~ 0 + 
                         (1|aos_category) + 
                         (1|graduation_year) + 
                         (1|prestige) +
                         (1|community), 
                     family = 'binomial', 
                     ## Priors
                     ## error sd
                     prior_aux = exponential(rate = 1, 
                                             autoscale = TRUE),
                     ## random effects covariance
                     prior_covariance =  decov(regularization = 1, 
                                               concentration = 1, 
                                               shape = 1, scale = 1),
                     adapt_delta = .99,
                     chains = 2, iter = 2000)
pred_4 = predictions(model_4)

## Still no sign of effect from program diversity
ggplot(pred_4, aes(aos_diversity, .residual)) + 
    geom_point() +
    geom_smooth(method = 'lm')

## Gender effect
ggplot(pred_4, aes(as.numeric(gender), .residual)) + 
    geom_point() +
    geom_smooth(method = 'lm')

## Maybe effect for percent women, but small if at all
ggplot(pred_4, aes(perc_w, .residual)) +
    geom_point() +
    geom_smooth(method = 'lm')

## Ditto for total placements
ggplot(pred_4, aes(total_placements, .residual)) +
    geom_point() +
    geom_smooth(method = 'lm')

## Posterior predictive checks
pp_check(model_4, nreps = 50)


# Model 5 ----------
#+ model_5, cache = TRUE
## Now with gender! And country! 
## ~600 seconds
tic()
model_5 = stan_glmer(data = individual_df, 
                     formula = permanent ~ 0 +
                         (1|aos_category) +
                         (1|graduation_year) +
                         (1|prestige) +
                         (1|community) +
                         (1|gender) + 
                         (1|country) +
                         perc_w +
                         total_placements,
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
                     adapt_delta = .99,
                     chains = 2, iter = 4000)
toc()

#+ model_5_output, fig.dim = c(2.5*5, 5)
## Check ESS and Rhat
model_5 %>%
    summary() %>%
    as.data.frame() %>%
    rownames_to_column('parameter') %>%
    select(parameter, n_eff, Rhat) %>%
    knitr::kable()

pp_check(model_5, nreps = 50)

pred_5 = predictions(model_5)

ggplot(pred_5, aes(.median, .residual)) +
    geom_point(aes(color = permanent)) +
    geom_smooth()
ggplot(pred_5, aes(.median, .residual, color = permanent)) +
    geom_linerange(aes(ymin = .residual - .lower, ymax = .residual - .upper))

ggplot(pred_5, aes(permanent, .median)) +
    geom_violin()

## Posterior estimates for coefficients
model_5 %>%
    tidy(parameters = 'non-varying', intervals = TRUE) %>%
    mutate(level = term, group = 'continuous') %>%
    select(level, group, term, everything()) %>%
    bind_rows(tidy(model_5, parameters = 'varying', 
                   intervals = TRUE)) %>%
    select(-std.error) %>%
    arrange(group, level) %>%
    mutate(level = fct_inorder(level) %>% fct_rev(), 
           group = fct_inorder(group), 
           panel = case_when(
               level == '(Intercept)' ~ 'intercept',
               group == 'continuous' ~ 'continuous', 
               group == 'country' ~ 'country',
               str_detect(group, 'community') ~ 'community', 
               TRUE ~ 'categorical'
           )) %>%
    ## To probability scale
    # mutate_if(is.numeric, gtools::inv.logit) %>%
    mutate_if(is.numeric, exp) %>%
    ggplot(aes(x = level, y = estimate, 
               ymin = lower, ymax = upper, 
               color = group)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_pointrange() + 
    # scale_y_continuous(labels = scales::percent_format()) +
    # scale_y_continuous(labels = function(x) round(exp(x), digits = 2)) +
    scale_y_continuous(trans = scales::log_trans(), 
                       breaks = scales::log_breaks(),
                       labels = scales::number_format(accuracy = .1), 
                       name = 'Odds Ratio') +
    # scale_color_brewer(palette = 'Set1', name = 'covariate\ngroup') +
    scale_color_viridis_d(name = 'covariate\ngroup') +
    coord_flip() +
    facet_wrap(~ panel, scales = 'free')

## Playing around with ridgeline plots
library(ggridges)
model_5 %>%
    as_tibble() %>%
    gather(key = parameter, value = value) %>%
    filter(str_detect(parameter, 'aos_category') & 
               !str_detect(parameter, 'Sigma')) %>%
    mutate(parameter = str_remove(parameter, 'b\\[.*:'), 
           parameter = str_remove(parameter, '\\]'), 
           parameter = str_replace_all(parameter, '_', ' ')) %>%
    ggplot(aes(value, y = parameter, group = parameter)) + 
    geom_density_ridges(scale = 1.5, 
                        rel_min_height = .01)


## Posterior for diff between high- and low-prestige
model_5 %>%
    as_tibble() %>%
    select(high_prestige = `b[(Intercept) prestige:high-prestige]`, 
           low_prestige = `b[(Intercept) prestige:low-prestige]`) %>%
    mutate(diff = high_prestige - low_prestige) %>%
    mutate_all(exp) %>%
    ggplot(aes(diff)) + 
    geom_density() + 
    geom_vline(aes(xintercept = median(diff))) +
    scale_x_continuous(#trans = scales::exp_trans(), 
                       # breaks = scales::log_breaks(),
                       # labels = scales::number_format(accuracy = .1), 
                       name = 'Odds Ratio')
## And between genders
model_5 %>%
    as_tibble() %>%
    select(m = `b[(Intercept) gender:m]`, 
           w = `b[(Intercept) gender:w]`) %>%
    mutate(diff = w - m) %>%
    mutate_all(exp) %>%
    ggplot(aes(diff)) + 
    geom_density() + 
    geom_vline(aes(xintercept = median(diff))) + 
    scale_x_continuous(name = 'Odds Ratio')

#+ reproducibility
sessionInfo()
