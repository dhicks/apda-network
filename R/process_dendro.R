process_dendro = function(dataf, dendro, k) {
    ## Dataframe representation of cut dendrogram
    cut_tree = tibble(
        University.Name = names(cutree(dendrogram, k = k)),
        cluster = factor(cutree(dendrogram, k = k)))
    
    ## Program AOSes, keywords, with clusters
    dataf_cl = dataf %>% 
        left_join(cut_tree, by = 'University.Name') %>% 
        select(cluster, University.ID, everything()) %>% 
        pivot_longer(c(-cluster, -University.ID, 
                       -University.Name), 
                     names_to = 'variable', 
                     values_to = 'value')
    
    ## Mean of each AOS/keyword within each cluster
    means = dataf_cl %>% 
        group_by(variable) %>% 
        mutate(value.rs = scale(value)) %>% 
        group_by(cluster, variable) %>% 
        summarize(mean = mean(value)) %>% 
        ungroup()
    
    ## Top and bottom AOSs/keywords by mean within each cluster
    top_means = means %>% 
        group_by(cluster) %>% 
        top_n(5, mean) %>% 
        # arrange(cluster, desc(mean)) %>% 
        mutate(side = 'top') %>% 
        select(cluster, side, variable, mean) %>% 
        ungroup()
    bottom_means = means %>% 
        group_by(cluster) %>% 
        top_n(-5, mean) %>% 
        # arrange(cluster, desc(mean)) %>% 
        mutate(side = 'bottom') %>% 
        select(cluster, side, variable, mean) %>% 
        ungroup()
    top_bottom = bind_rows(top_means, bottom_means) %>% 
        mutate(side = fct_relevel(side, 'top', 'bottom')) %>% 
        arrange(cluster, side, mean, variable)
    # return(top_bottom)
    
    ## Dendrogram plot
    plot = dendro %>% 
        color_labels(k = k, col = plasma(k, end = .9)) %>% 
        color_branches(k = k, col = plasma(k, end = .9)) %>% 
        set('branches_lwd', .8) %>% 
        as.ggdend() %>% 
        ggplot(labels = FALSE)
    
    return_lst = lst(cut_tree, means, top_bottom, plot)
}
# foo = process_dendro(complete.data, dendrogram, 8)
# foo$cut_tree
# foo$means
# foo$top_bottom
# foo$plot
