process_dendro = function(dataf, dendro, k) {
    ## Dataframe representation of cut dendrogram
    cut = cutree(dendro, k = k, order_clusters_as_data = FALSE)
    
    cut_tree = tibble(
        University.Name = names(cut),
        cluster = factor(cut))
    # return(cut_tree)
    
    ## Program AOSes, keywords, with clusters
    dataf_cl = dataf %>% 
        dplyr::left_join(cut_tree, by = 'University.Name') %>% 
        dplyr::select(cluster, University.ID, everything()) %>% 
        tidyr::pivot_longer(c(-cluster, -University.ID, 
                       -University.Name), 
                     names_to = 'variable', 
                     values_to = 'value')
    
    ## Mean of each AOS/keyword within each cluster
    means = dataf_cl %>% 
        dplyr::group_by(variable) %>% 
        dplyr::mutate(value.rs = scale(value)) %>% 
        dplyr::group_by(cluster, variable) %>% 
        dplyr::summarize(mean = mean(value.rs)) %>% 
        dplyr::ungroup()
    
    ## Top and bottom AOSs/keywords by mean within each cluster
    top_means = means %>% 
        dplyr::group_by(cluster) %>% 
        dplyr::top_n(5, mean) %>% 
        # arrange(cluster, desc(mean)) %>%
        dplyr::mutate(side = 'top') %>% 
        dplyr::select(cluster, side, variable, mean) %>% 
        dplyr::ungroup()
    bottom_means = means %>% 
        dplyr::group_by(cluster) %>% 
        dplyr::top_n(-5, mean) %>% 
        # arrange(cluster, desc(mean)) %>%
        dplyr::mutate(side = 'bottom') %>% 
        dplyr::select(cluster, side, variable, mean) %>% 
        dplyr::ungroup()
    top_bottom = dplyr::bind_rows(top_means, bottom_means) %>% 
        dplyr::mutate(side = fct_relevel(side, 'top', 'bottom')) %>% 
        dplyr::arrange(cluster, side, desc(abs(mean)), variable)
    # return(top_bottom)
    
    ## Dendrogram plot
    plot = dendro %>% 
        color_labels(k = k, col = plasma(k, end = .9)) %>%
        color_branches(k = k, col = plasma(k, end = .9), 
                       groupLabels = TRUE) %>% 
        set('labels_cex', 100) %>%
        set('branches_lwd', .8) %>%
        as.ggdend() %>%
        ggplot(labels = FALSE)
    
    return_lst = lst(cut_tree, means, top_bottom, plot)
}
# debug(process_dendro)
# foo = process_dendro(complete.data, dendrogram, 6)
# foo$cut_tree
# foo$means
# foo$top_bottom
# foo$plot
