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
## bayesplot makes itself the default theme 😒
theme_set(theme_minimal())

## Suppress messages when generating HTML file
knitr::opts_chunk$set(message = FALSE)

## There's an augment() method for rstanarm models; 
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
load('01_parsed.Rdata')
load('02_univ_net_stats.Rdata')
## Program-level AOS diversity
h_df = individual_df_unfltd %>%
    count(placing_univ_id, aos_category) %>%
    filter(!is.na(aos_category)) %>%
    group_by(placing_univ_id) %>%
    mutate(frac = n / sum(n)) %>%
    summarize(H = -sum(frac * log2(frac)))

ggplot(h_df, aes(H)) + geom_density() + geom_rug()
    
individual_df = individual_df %>%
    left_join(univ_df, by = c('placing_univ_id' = 'univ_id')) %>%
    left_join(h_df) %>%
    ## Use the canonical names from univ_df
    select(-placing_univ) %>%
    ## Drop NAs
    # filter(complete.cases(.))
    filter_at(vars('permanent', 'aos_category', 
                   'graduation_year', 'prestige', 'community'), 
              all_vars(negate(is.na)(.)))

## Variables to consider: aos_category; aos; graduation_year; placement_year; placing_univ_id; cluster; out_centrality; prestige; community; demographics

## No indication that AOS diversity has any effect
ggplot(individual_df, aes(H, 1*permanent)) + 
    geom_point() +
    geom_smooth(method = 'loess')

##+ models -----
# Model 1 --------------------
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
## AOS + grad year
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
## AOS + grad year + program prestige status
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
## AOS + grad year + program prestige status + clusters and communities
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
                     chains = 2, iter = 4000)
pred_4 = predictions(model_4)

## Still no sign of effect from program diversity
ggplot(pred_4, aes(H, .residual)) + 
    geom_point() +
    geom_smooth(method = 'loess')

## Posterior predictive checks
pp_check(model_4, nreps = 50)

## Check ESS and Rhat
model_4 %>%
    summary() %>%
    as.data.frame() %>%
    rownames_to_column('parameter') %>%
    select(parameter, n_eff, Rhat) %>%
    knitr::kable()

## Posterior estimates for coefficients
model_4 %>%
    tidy(parameters = 'varying', intervals = TRUE) %>%
    select(-std.error) %>%
    arrange(group, level) %>%
    mutate(level = fct_inorder(level) %>% fct_rev(), 
           group = fct_inorder(group), 
           is_community = str_detect(group, 'community')) %>%
    mutate_if(is.numeric, gtools::inv.logit) %>%
    ggplot(aes(x = level, y = estimate, 
               ymin = lower, ymax = upper, 
               color = group)) +
    geom_hline(yintercept = .5, linetype = 'dashed') +
    geom_pointrange() + 
    scale_y_continuous(labels = scales::percent_format()) +
    scale_color_brewer(palette = 'Set1') +
    coord_flip() +
    facet_wrap(~ is_community, scales = 'free')

## Playing around with ridgeline plots
library(ggridges)
model_4 %>%
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
model_4 %>%
    as_tibble() %>%
    select(high_prestige = `b[(Intercept) prestige:high-prestige]`, 
           low_prestige = `b[(Intercept) prestige:low-prestige]`) %>%
    mutate_all(gtools::inv.logit) %>%
    mutate(diff = high_prestige - low_prestige) %>%
    ggplot(aes(diff)) + 
    geom_density() + 
    geom_vline(aes(xintercept = median(diff))) + 
    scale_x_continuous(labels = scales::percent_format())


sessionInfo()
