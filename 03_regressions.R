#' ---
#' output:
#'     html_document: 
#'         self_contained: yes
#' ---

## Regression models of placement outcomes
library(tidyverse)
library(broom)
library(forcats)
library(rstanarm)
options(mc.cores = parallel::detectCores() - 2)
## bayesplot makes itself the default theme 😒
theme_set(theme_minimal())

## Suppress messages when generating HTML file
knitr::opts_chunk$set(message = FALSE)

load('01_parsed.Rdata')
load('02_univ_net_stats.Rdata')
individual_df = individual_df %>%
    left_join(univ_df, by = c('placing_univ_id' = 'univ_id')) %>%
    ## Use the canonical names from univ_df
    select(-placing_univ) %>%
    ## Drop NAs
    filter(complete.cases(.))

## Variables to consider: aos_category; aos; graduation_year; placement_year; placing_univ_id; cluster; out_centrality; elite; community; demographics
## Models to consider: linear; beta


## Model 1 --------------------
model_1 = stan_glmer(data = individual_df, 
        formula = permanent ~ 0 + (1|aos_category), 
        family = 'binomial', 
        chains = 2, iter = 2000)

model_1 %>%
    tidy(parameters = 'varying', intervals = TRUE) %>%
    select(-std.error) %>%
    mutate_if(is.numeric, gtools::inv.logit) %>%
    ggplot(aes(x = level, y = estimate, 
               ymin = lower, ymax = upper)) +
    geom_hline(yintercept = .5, linetype = 'dashed') +
    geom_pointrange() + 
    coord_flip()

## Model 2 --------------------
model_2 = stan_glmer(data = individual_df, 
                     formula = permanent ~ 0 + (1|aos_category) +
                         (1|graduation_year), 
                     family = 'binomial', 
                     chains = 2, iter = 2000)

model_2 %>%
    tidy(parameters = 'varying', intervals = TRUE) %>%
    select(-std.error) %>%
    mutate_if(is.numeric, gtools::inv.logit) %>%
    ggplot(aes(x = level, y = estimate, 
               ymin = lower, ymax = upper)) +
    geom_hline(yintercept = .5, linetype = 'dashed') +
    geom_pointrange() + 
    coord_flip()

## Model 3 --------------------
model_3 = stan_glmer(data = individual_df, 
                     formula = permanent ~ 0 + (1|aos_category) + 
                         (1|graduation_year) + 
                         (1|elite), 
                     family = 'binomial', 
                     chains = 2, iter = 4000)

## Check ESS and Rhat
model_3 %>%
    summary() %>%
    as.data.frame() %>%
    rownames_to_column('parameter') %>%
    select(parameter, n_eff, Rhat) %>%
    knitr::kable()

## Posterior estimates for coefficients
model_3 %>%
    tidy(parameters = 'varying', intervals = TRUE) %>%
    select(-std.error) %>%
    arrange(group, level) %>%
    mutate(level = fct_inorder(level) %>% fct_rev(), 
           group = fct_inorder(group)) %>%
    mutate_if(is.numeric, gtools::inv.logit) %>%
    ggplot(aes(x = level, y = estimate, 
               ymin = lower, ymax = upper, 
               color = group)) +
    geom_hline(yintercept = .5, linetype = 'dashed') +
    geom_pointrange() + 
    scale_y_continuous(labels = scales::percent_format()) +
    scale_color_brewer(palette = 'Set1') +
    coord_flip()

## Playing around with ridgeline plots
library(ggridges)
model_3 %>%
    as_tibble() %>%
    gather(key = parameter, value = value) %>%
    filter(str_detect(parameter, 'aos_category') & 
               !str_detect(parameter, 'Sigma')) %>%
    ggplot(aes(value, y = parameter, group = parameter)) + 
    geom_density_ridges(scale = 1.5, 
                        rel_min_height = .01)
    

## Posterior for diff between elite and non-elite
model_3 %>%
    as_tibble() %>%
    select(elite = `b[(Intercept) elite:TRUE]`, 
           non_elite = `b[(Intercept) elite:FALSE]`) %>%
    mutate_all(gtools::inv.logit) %>%
    mutate(diff = elite - non_elite) %>%
    ggplot(aes(diff)) + 
    geom_density() + 
    geom_vline(aes(xintercept = median(diff))) + 
    scale_x_continuous(labels = scales::percent_format())

## Posterior predictive checks
pp_check(model_3, nreps = 50)
## This comes from Gelman and Hill (2006), 97 fig 5.13b
pp_check(model_3, plotfun = 'error_binned', nreps = 6)

## Model 4 --------------------
# model_4 = stan_glmer(data = individual_df, 
#                      formula = permanent ~ 0 + (1|aos_category) + 
#                          (1|graduation_year) + 
#                          (1|elite) + 
#                          (1|placing_univ_id)
#                      family = 'binomial', 
#                      chains = 2, iter = 2000)
# 
# ## Check ESS and Rhat
# model_4 %>%
#     summary() %>%
#     as.data.frame() %>%
#     rownames_to_column('parameter') %>%
#     select(parameter, n_eff, Rhat)
# 
# ## Posterior estimates for coefficients
# model_4 %>%
#     tidy(parameters = 'varying', intervals = TRUE) %>%
#     mutate(level = fct_inorder(level), 
#            group = fct_inorder(group)) %>%
#     select(-std.error) %>%
#     mutate_if(is.numeric, gtools::inv.logit) %>%
#     ggplot(aes(x = level, y = estimate, 
#                ymin = lower, ymax = upper, 
#                color = group)) +
#     geom_hline(yintercept = .5, linetype = 'dashed') +
#     geom_pointrange() + 
#     scale_color_brewer(palette = 'Set1', 
#                        guide = guide_legend(reverse = TRUE)) +
#     coord_flip()
# 
# ## Posterior for diff between elite and non-elite
# model_4 %>%
#     as_tibble() %>%
#     select(elite = `b[(Intercept) elite:TRUE]`, 
#            non_elite = `b[(Intercept) elite:FALSE]`) %>%
#     mutate_all(gtools::inv.logit) %>%
#     mutate(diff = elite - non_elite) %>%
#     ggplot(aes(diff)) + geom_density() + 
#     geom_vline(aes(xintercept = median(diff)))
