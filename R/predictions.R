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
