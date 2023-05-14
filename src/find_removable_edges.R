# functions for determining edges to split
expand_node <- function(tree, node, subtree_info, pop, eps, merged,
                        cached_vtrees){
  
  if (!node %in% names(cached_vtrees)){
    # make district tree
    G_v <- make_graph(merged$nodes_v, merged$edges_v,
                      "vtd",node)
    if (length(V(G_v))<2){return(list(tibble()))}
    
    # get external edges
    external_edges <- get_external_edges(merged, tree, node)  
  } else {
    print(paste0("Using cached tree_v: ", node))
    G_v <- cached_vtrees[[node]]$tree_v # graph is prespecified tree
    external_edges <- cached_vtrees[[node]]$edges_vc
  }
  
  # add county pops to neighboring precinct
  external_edges <- left_join(external_edges,
                              subtree_info,
                              by=c("other_county"="name"))
  for (i in 1:nrow(external_edges)){
    V(G_v)[external_edges$this_vtd[i]]$pop <-
      V(G_v)[external_edges$this_vtd[i]]$pop +
      external_edges$pop[i]
  }
  
  if (!node %in% names(cached_vtrees)){
    tree_v <- draw_spanning_tree(G_v)
  } else {
    # update childpop
    tree_v <- add_node_childpop(G_v)
  }
  
  # find removable edges
  cuts_v <- V(tree_v)$name[between(V(tree_v)$childpop, 
                                   pop-eps, pop+eps) &
                             between(sum(V(tree)$pop)-V(tree_v)$childpop, 
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
                                     dont_split = character(0),
                                     cached_vtrees = list(),
                                     eps_percent=.02,
                                     pop=NULL, eps=NULL){
  if (is.null(pop)){
    pop = inputs$ideal_pop
  }  
  if (is.null(eps)){
    eps=eps_percent*pop
  }
  
  # find county edges
  cuts_c <- V(tree)$name[between(V(tree)$childpop, pop-eps, pop+eps) 
                         & between(sum(V(tree)$pop)-V(tree)$childpop, 
                                   pop-eps, pop+eps)]
  
  # find node to expand
  expand_bool <- tibble(node=V(tree)$name) %>%
    mutate(subtree_info=map(V(tree)$name, get_nodes_to_expand,
                   tree=tree, pop=pop, eps=eps)) %>%
    filter(map_dbl(subtree_info, nrow)>0,
           !node %in% dont_split)
  
  # expand nodes to find precinct edges
  cuts_v <- expand_bool %>% 
    mutate(res = map2(node, subtree_info, expand_node, 
                tree=tree, cached_vtrees=cached_vtrees,
                pop=pop, eps=eps, merged=merged),
           edge=map(res, ~(.x$edges)),
           tree=map(res, ~(.x$tree)),
           external=map(res, ~(.x$external_edges))) %>%
    select(county=node, edge=edge,tree,external) %>%
    unnest(edge)
  
  bind_rows(tibble(edge=cuts_c, county=cuts_c, level="county"),
            mutate(cuts_v, level="vtd"))
}
