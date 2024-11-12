id_node <- function(nodes, node.name){
  nodes %>% 
    filter(variable == node.name) %>% 
    pull(id)
}

vizNetwork <- function(vis.nodes, vis.links){
  
  vis.nodes$shape  <- "dot"  
  vis.nodes$shadow <- TRUE # Nodes will drop shadow
  vis.nodes$title  <- vis.nodes$variable # Text on click
  vis.nodes$label  <- vis.nodes$variable # Node label
  #vis.nodes$size   <- vis.nodes$audience.size # Node size
  vis.nodes$borderWidth <- 2 # Node border width
  
  colrs <- setNames(c("gray", "salmon", "yellow", "cyan", "chartreuse", "darkorchid", "cornflowerblue", "pink", "aquamarine", "burlywood", "cadetblue", "chartreuse", "coral", "cornsilk", "darkseagreen", "deepskyblue"), 
                    c("Chem. Mix.", "Brain Metabolite", "Cortical Thickness", "Volume", "Subcortical Volume", "Neurobehavior", "Global", "Frontal Lobe", "Occipital Lobe", "Parietal Lobe", "Limbic Lobe", "Insular cortex", "Motor", "Language", "Cognition", "Vision"))
  
  # adapt to variable 
  vis.nodes$color.background <- dplyr::recode(vis.nodes$group, !!!colrs)
  vis.nodes$color.border <- "black"
    vis.nodes$color.highlight.background <- "orange"
      vis.nodes$color.highlight.border <- "darkred"
        
      edge.colrs <- setNames(c('green', 'red'), c("negative", "positive"))
      vis.links$color <- dplyr::recode(vis.links$estimate, !!!edge.colrs)    # line color  
      vis.links$width <- ifelse(vis.links$dashes, 1, 2) # line width
      vis.links$arrows <- "to" # arrows: 'from', 'to', or 'middle'
      vis.links$smooth <- TRUE    # should the edges be curved?
      vis.links$shadow <- FALSE    # edge shadow
      
      visnet <- visNetwork(vis.nodes, vis.links, width = 1080) %>% 
        visLayout(randomSeed = 56) %>% 
        visGroups(groupname = "Chem. Mix.", color = 'gray', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Brain Metabolite", color = 'salmon', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Cortical Thickness", color = 'yellow', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Volume", color = 'cyan', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Subcortical Volume", color = 'chartreuse', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Neurobehavior", color = 'darkorchid', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Global", color = 'cornflowerblue', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Frontal Lobe", color = 'pink', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Occipital Lobe", color = 'aquamarine', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Parietal Lobe", color = 'burlywood', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Limbic Lobe", color = 'cadetblue', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Insular cortex", color = 'chartreuse', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Motor", color = 'coral', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Language", color = 'cornsilk', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Cognition", color = 'darkseagreen', shape = "dot", color.border = 'black') %>% 
        visGroups(groupname = "Vision", color = 'deepskyblue', shape = "dot", color.border = 'black') %>% 
        visOptions(highlightNearest = TRUE, selectedBy = 'group') %>% visInteraction(navigationButtons = TRUE) %>% 
        visLegend(addEdges = data.frame(arrows = 'to',
                                        color = c('red', 'green'),
                                        label = c('positive', 'negative')))
      
      
      return(visnet)
}
