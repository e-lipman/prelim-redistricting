# functions for determining edges to split
get_external_edges <- function(merged, 
                               which_county, neighbors){
  external_edges <- filter(merged$edges_v,
                           county1!=county2,
                           (county1==which_county |
                              county2==which_county)) %>%
    mutate(other_county = ifelse(county1==which_county,
                                 county2, county1),
           this_vtd = ifelse(county1==which_county,
                             vtd1, vtd2)) %>%
    filter(other_county %in% neighbors)
  
  selected_edges <- external_edges %>%
    group_by(other_county) %>%
    sample_n(1) %>% 
    ungroup() %>%
    select(other_county, this_vtd) %>%
    unnest(other_county) %>%
    mutate(other_county=as.character(other_county),
           this_vtd=as.character(this_vtd))
  
  return(selected_edges)
}

expand_node <- function(tree, node, subtree_info, pop, eps, merged){
  # make district graph
  G_v <- make_graph(merged$nodes_v, merged$edges_v,
                    "vtd",node)
  if (length(V(G_v))<2){return(list(tibble()))}
  
  # get external edges
  external_edges <- get_external_edges(merged, node, 
                                       subtree_info$name)
  
  # add county pops to neighboring precinct
  external_edges <- left_join(external_edges,
                              subtree_info,
                              by=c("other_county"="name"))
  for (i in 1:nrow(external_edges)){
    V(G_v)[external_edges$this_vtd[i]]$pop <-
      V(G_v)[external_edges$this_vtd[i]]$pop +
      external_edges$pop[i]
  }
  
  
  # make spanning tree
  tree_v <- draw_spanning_tree(G_v)
  #plot(tree_v, layout=layout_as_tree, 
  #     vertex.label=paste0(V(tree_v)$name, "\n", V(tree_v)$pop),
  #     vertex.size=1)
  
  # find removable edges
  cuts_v <- V(tree_v)$name[between(V(tree_v)$childpop, 
                                   pop-eps, pop+eps)]
  
  # return edges, tree, external edges
  return(list(edges=cuts_v, tree=tree_v, 
              external_edges=external_edges))
}

get_nodes_to_expand <- function(tree, node, pop, eps,
                                return_bool=FALSE){
  # find parents and children 
  children <- neighbors(tree, node, mode="out")
  parent <- neighbors(tree, node, mode="in")
  
  C_nodes <- c(children, parent)
  C_pops <- c(children$childpop, 
              V(tree)[tree$root]$childpop-V(tree)[node]$childpop)
  if (length(parent)==0){
    C_pops <- C_pops[1:length(children)]
  }
  node_pop <- V(tree)[node]$pop
  subtree_info <- tibble(name=C_nodes$name, pop=C_pops)
  
  # check all subtree combinations to see if worth expanding
  C_list <- list()
  if (all(c(C_pops)<pop+eps)){
    for (i in 1:max((length(C_pops)-1),1)){
      if (i==1){idx <- matrix(1)}
      else {idx <- rbind(1, combn(2:length(C_pops), i-1))}
      for (c in 1:ncol(idx)){
        col <- idx[,c]
        chk_sums  <- c(sum(C_pops[col]), sum(C_pops)-sum(C_pops[col]))
        good_sums <- between(chk_sums, pop-eps-node_pop,  pop+eps)
        if (all(good_sums)){
          C_list <- c(C_list, list(col))        
        }
      }
    }  
  }
  
  expand=length(C_list)>0
  if (return_bool){
    return(expand)  
  } else {
    if (!expand){subtree_info=tibble()}
    return(subtree_info)
  }
}


get_cut_candidates_multi <- function(tree, merged,
                                     pop=NULL, eps=NULL,  
                                     eps_percent=.02){
  if (is.null(pop)){
    pop = inputs$ideal_pop
  }  
  if (is.null(eps)){
    eps=eps_percent*pop
  }
  
  # find county edges
  cuts_c <- V(tree)$name[between(V(tree)$childpop, pop-eps, pop+eps)]
  
  # find node to expand
  expand_bool <- tibble(node=V(tree)$name) %>%
    mutate(subtree_info=map(V(tree)$name, get_nodes_to_expand,
                   tree=tree, pop=pop, eps=eps)) %>%
    filter(map_dbl(subtree_info, nrow)>0)
  
  # expand nodes to find precinct edges
  cuts_v <- expand_bool %>% 
    mutate(res = map2(node, subtree_info, expand_node, 
                tree=tree, pop=pop, eps=eps, merged=merged),
           edge=map(res, ~(.x$edges)),
           tree=map(res, ~(.x$tree)),
           external=map(res, ~(.x$external_edges))) %>%
    select(county=node, edge=edge,tree,external) %>%
    unnest(edge)
  
  bind_rows(tibble(edge=cuts_c, county=cuts_c, level="county"),
            mutate(cuts_v, level="vtd"))
}
