#' ---
#' output:
#'     html_document: 
#'         self_contained: yes
#'         toc: true
#'         df_print: paged
#' ---

## Regression models of placement outcomes
#+ dependencies -----
library(tidyverse)
library(tidylog)
library(readxl)
library(cowplot)
library(broom)
library(forcats)
library(rstanarm)
## As of 2020-05-20, some kind of mismatch btwn parallel and Rstudio causes a "freeze" when using multiple cores
## <https://github.com/rstudio/rstudio/issues/6692>
# options(mc.cores = min(4, 
#                        parallel::detectCores() - 2))
options(mc.cores = 4)
## bayesplot makes itself the default theme
theme_set(theme_minimal())

library(tictoc)
library(assertthat)

## Suppress messages when generating HTML file
knitr::opts_chunk$set(message = FALSE)

source('../R/predictions.R')
source('../R/posterior_estimates.R')

## Use command-line argument to check whether to force resampling
library(argparse)
parser = ArgumentParser()
parser$add_argument("-f", "--force", 
                    action="store_true", 
                    default=FALSE,
                    help="Force resampling the regression model")
args = parser$parse_args()
force_resampling = args$force
## Or uncomment the next line
# force_resampling = TRUE

#+ load_data -----
data_folder = '../data/'
output_folder = '../output/04_'
paper_folder = '../paper/'

# cluster_distances = read_csv(str_c(data_folder, 
#                                    '00_k9distances_2019-03-15.csv')) %>% 
#     count(cluster = cluster, average_distance = avgDist) %>% 
#     mutate(cluster = as.character(cluster))
# 
# ggplot(cluster_distances, aes(cluster, scale(average_distance))) +
#     geom_label(aes(label = n, fill = n, size = n), color = 'white')

load(str_c(data_folder, '02_parsed.Rdata'))
## Programs with data validated in summer 2021
## 129
validated = read_excel(file.path(data_folder, '00_DataChecks2021APDA.xlsx')) |> 
    filter(`Placement Page?` == 1 | `Dissertation Records/ProQuest?` == 1)

univ_df = read_rds(str_c(data_folder, '03_univ_net_stats.rds'))

individual_df = individual_df %>%
    left_join(univ_df, by = c('placing_univ_id' = 'univ_id')) %>%
    filter(placing_univ_id %in% validated$`University ID`) %>%
    ## Use the canonical names from univ_df
    select(-placing_univ) %>%
    ## Don't include prestige scores
    select(-out_centrality) %>%
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

individual_df

## Variables to consider: aos_category; graduation_year; placement_year; prestige; cluster; community; placing_univ_id; gender; country; perc_w; total_placements

## Overall permanent placement rate
count(individual_df, permanent) %>% 
    mutate(share = n / sum(n))

## Giant pairs plot/correlogram ----
## perc_high_prestige and prestige are all tightly correlated
## All other pairs have low to moderate correlation
individual_df %>% 
    select(permanent, aos_category, aos_diversity, perc_high_prestige,
           graduation_year, placement_year, prestige, 
           in_centrality, #out_centrality, #community, 
           cluster, #average_distance,
           gender, country, perc_w, 
           total_placements) %>% 
    mutate_if(negate(is.numeric), function(x) as.integer(as.factor(x))) %>% 
    mutate_at(vars(in_centrality, #out_centrality
                   ), log10) %>% 
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
ggsave(str_c(output_folder, 'descriptive_1.png'), 
       desc_1_plot, 
       height = 2*2, width = 2*3, scale = 1.5)

## Program-level categorical
desc_2_plot = individual_df %>%
    select(prestige, country, 
           #community, 
           cluster) %>%
    gather(key = variable, value = value) %>%
    count(variable, value) %>% 
    ggplot(aes(fct_rev(value), n, group = variable)) +
    geom_col(aes(fill = variable), show.legend = FALSE) +
    scale_fill_viridis_d() +
    xlab('') +
    coord_flip() +
    facet_wrap(vars(variable), scales = 'free', ncol = 3)
desc_2_plot
ggsave(str_c(output_folder, 'descriptive_2.png'), 
       desc_2_plot, 
       height = 1*2, width = 2*2, scale = 1.5)

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
ggsave(str_c(output_folder, 'descriptive_3.png'), 
       desc_3_plot, 
       height = 2*2, width = 2*3.5, scale = 1.5)

plot_grid(desc_1_plot, 
          desc_2_plot, 
          desc_3_plot, 
          align = 'v', axis = 'lr', ncol = 1, 
          labels = 'auto',
          hjust = -7
          )
ggsave(str_c(output_folder, 'descriptive.png'), 
       height = 4*3, width = 3*3, scale = 1)
ggsave(str_c(paper_folder, 'fig_descriptive.png'), 
       height = 4*3, width = 3*3, scale = 1)



## Model -----
#+ model, cache = FALSE
model_file = str_c(data_folder, '04_model.Rds')
if (!file.exists(model_file) || force_resampling) {
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
                       # (1|community) +
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
                   prior_intercept = cauchy(0, 2/3, autoscale = TRUE), ## constant term + random intercepts
                   prior = cauchy(0, 2/3, autoscale = TRUE),
                   ## error sd
                   prior_aux = cauchy(0, 2/3, autoscale = TRUE),
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
    geom_vline(xintercept = 3000) +
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
## 90% HPD posterior intervals
estimates = posterior_estimates(model, prob = .9)

estimates

## Estimates plot
estimates %>% 
    filter(entity != 'intercept', 
           group != 'community',
           group != 'placement_year', 
           term != 'gendero') %>% 
    ## posterior_estimates() already exponentiates estimates
    mutate_if(is.numeric, ~ . - 1) %>% 
    ggplot(aes(x = level, y = estimate, 
           ymin = conf.low, ymax = conf.high, 
           color = group)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_pointrange(size = 1.5, fatten = 1.5) + 
    scale_color_viridis_d(name = 'covariate\ngroup') +
    xlab('') + #ylab('') +
    scale_y_continuous(labels = scales::percent_format(), 
                       name = '') +
    coord_flip(ylim = c(-1, 1.75)) +
    facet_wrap(~ entity, scales = 'free') +
    theme(legend.position = 'bottom')

ggsave(str_c(output_folder, 'estimates.png'), 
       width = 6, height = 4, 
       scale = 1.5)
ggsave(str_c(paper_folder, 'fig_reg_estimates.png'), 
       width = 6, height = 4, 
       scale = 1.5)

estimates %>% 
    filter(entity != 'intercept', 
           group != 'community',
           group != 'placement_year') %>% 
    select(group, level, estimate, conf.low, conf.high) %>% 
    mutate_if(is.factor, as.character) %>% 
    arrange(group, level) %>% 
    knitr::kable(format = 'latex', 
                 digits = 2,
                 booktabs = TRUE, 
                 label = 'estimates', 
                 caption = 'Estimated regression coefficients.  Lower and upper columns give the left and right endpoints, respectively, of the centered 90\\% posterior intervals.') %>% 
    write_file(path = str_c(output_folder, 'estimates.tex'))


## Marginal effects for gender and prestige ----
## <https://stackoverflow.com/questions/45037485/calculating-marginal-effects-in-binomial-logit-using-rstanarm>
marginals = function (dataf, model, variable, 
                      ref_value = 0L, 
                      alt_value = 1L) {
    variable = enquo(variable)
    
    all_0 = mutate(dataf, !!variable := ref_value)
    all_1 = mutate(dataf, !!variable := alt_value)
    
    pred_0 = posterior_epred(model, newdata = all_0)
    pred_1 = posterior_epred(model, newdata = all_1)
    
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
# 0.06387576 0.10352822 0.14303032


marginals_prestige = individual_df %>% 
    select(-city, -state) %>% 
    marginals(model, prestige, 'low-prestige', 'high-prestige')

apply(marginals_prestige, 1, mean) %>% 
    quantile(probs = c(.05, .5, .95))
#          5%        50%        95% 
#  0.07601945 0.11718961 0.15762968

marginals_canada = individual_df %>% 
    select(-city, -state) %>% 
    marginals(model, country, 'U.S.', 'Canada') %>% 
    apply(1, mean) %>% 
    quantile(probs = c(.05, .5, .95))
marginals_canada


## Schools in certain communities ----
# comms_of_interest = c(3, 5, 12, 37, 54, 
#                          8, 27, 38, 43) %>% 
#     as.character()
# 
# univ_df %>% 
#     filter(community %in% comms_of_interest, 
#            total_placements > 0) %>% 
#     select(community, name = univ_name, 
#            total_placements, perm_placement_rate) %>% 
#     mutate(community = fct_relevel(community, comms_of_interest), 
#            perm_placement_rate = scales::percent_format()(perm_placement_rate)) %>% 
#     arrange(community, name) %>% 
#     knitr::kable(format = 'latex', 
#                  # digits = 2,
#                  booktabs = TRUE, 
#                  label = 'comms', 
#                  caption = 'Universities in selected topological communities.  Only universities with at least 1 placement in the data are shown.') %>% 
#     write_file(path = str_c(output_folder, 'comms.tex'))


## Reproducibility ----
sessionInfo()
