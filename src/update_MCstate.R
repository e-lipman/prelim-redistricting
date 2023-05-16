update_trees <- function(tree_c, cut_edge,   
                         which_districts, trees){

  if (cut_edge$level=="vtd"){
    # break precinct tree
    edge_v <- c(neighbors(cut_edge$tree[[1]], 
                          cut_edge$edge, mode="in")$name,
                cut_edge$edge)
    tree_v_split <- delete.edges(cut_edge$tree[[1]],
                                 get.edge.ids(cut_edge$tree[[1]], edge_v)) %>%
      decompose()
    
    # break county tree
    tree_c_split <- delete.vertices(tree_c, cut_edge$county)
    
    # add precinct trees for new split county
    cnty <- cut_edge$county
    trees[[which_districts[1]]]$tree_v[[cnty]] <- tree_v_split[[1]]
    trees[[which_districts[2]]]$tree_v[[cnty]] <- tree_v_split[[2]]
    
    v_names <- map(tree_v_split, ~V(.x)$name)
    external1 <- filter(cut_edge$external[[1]],this_vtd %in% v_names[[1]]) %>%
      select(-pop)
    external2 <- filter(cut_edge$external[[1]],this_vtd %in% v_names[[2]]) %>%
      select(-pop)
    trees[[which_districts[1]]]$edges_vc[[cnty]] <- external1
    trees[[which_districts[2]]]$edges_vc[[cnty]] <- external2
    
    # get updated county trees
    tree1_c <- tree_c_split %u% 
      graph_from_data_frame(tibble(e1=external1$other_county, e2=cnty)) %>%
      add_vertices(1, name=cnty) %>%
      decompose()
    tree1_c <- tree1_c[map_lgl(tree1_c, ~(cnty %in% V(.x)$name))][[1]] %>%
      as.undirected() 
    V(tree1_c)[[cnty]]$pop <- sum(V(tree_v_split[[1]])$pop)
    tree1_c <- draw_spanning_tree(tree1_c)
    
    tree2_c <- tree_c_split %u% 
      graph_from_data_frame(tibble(e1=external2$other_county, e2=cnty)) %>%
      add_vertices(1, name=cnty) %>%
      decompose() 
    tree2_c <- tree2_c[map_lgl(tree2_c, ~(cnty %in% V(.x)$name))][[1]] %>%
      as.undirected()
    V(tree2_c)[[cnty]]$pop <- sum(V(tree_v_split[[2]])$pop)
    tree2_c <- draw_spanning_tree(tree2_c)
    
  } else {
    edge_c <- c(neighbors(tree_c, cut_edge$county, mode="in")$name,
                cut_edge$county)  
    tree_c_split <- delete.edges(tree_c, get.edge.ids(tree_c, edge_c)) %>%
      decompose()
    
    tree1_c <- tree_c_split[[1]]
    tree2_c <- tree_c_split[[2]]
    
    cnty <- character(0)
  }
  
  # update county trees
  trees[[which_districts[1]]]$tree_c <- tree1_c
  trees[[which_districts[2]]]$tree_c <- tree2_c
  
  ## remove precinct trees from formerly split county
  comb_county <- map(trees[which_districts], ~names(.x$tree_v)) %>%
    reduce(intersect) %>% setdiff(cnty)
  if (length(comb_county)!=0){
    trees[[which_districts[1]]]$tree_v[[comb_county]] <- NULL
    trees[[which_districts[2]]]$tree_v[[comb_county]] <- NULL
    trees[[which_districts[1]]]$edges_vc[[comb_county]] <- NULL
    trees[[which_districts[2]]]$edges_vc[[comb_county]] <- NULL
  }
  
  # reassign remainig split counties based on new plan
  remaining_split <- map(trees[which_districts], 
                         ~setdiff(names(.x$tree_v), cnty))
  for (node in unlist(remaining_split)){
    old_comp <- ifelse(node %in% remaining_split[[1]], 1, 2)
    new_comp <- ifelse(node %in% V(tree1_c)$name, 1, 2)
    
    if (old_comp!=new_comp){
      trees[[which_districts[new_comp]]]$tree_v[[node]] <- 
        trees[[which_districts[old_comp]]]$tree_v[[node]]  
      trees[[which_districts[old_comp]]]$tree_v[[node]] <- NULL
    }
    
    # nullify external edges to resample
    trees[[which_districts[new_comp]]]$edges_vc[[node]] <- NULL
    trees[[which_districts[old_comp]]]$edges_vc[[node]] <- NULL
    
  }
  return(trees)
}

resample_edges_vc <- function(which_district, trees, plan){
  cntys <- setdiff(names(trees[[which_district]]$tree_v),
                   names(trees[[which_district]]$edges_vc))
  dist_info <- district_info(plan, which_district)
  for (cnty in cntys){
    external <- get_external_edges(dist_info, 
                                   trees[[which_district]]$tree_c, cnty)
    trees[[which_district]]$edges_vc[[cnty]] <- external
  }
  return(trees)
}

update_plan <- function(trees, cut_edge, which_districts, plan){
  county_names <- map(trees[which_districts], ~V(.x$tree_c)$name)
  
  if (cut_edge$level=="vtd"){
    vtd_names <- map(trees[which_districts], 
                     ~V(.x$tree_v[[cut_edge$county]])$name)
    plan %>%
      mutate(district=
               case_when(!district %in% which_districts~district, 
                         vtd %in% vtd_names[[1]]~which_districts[1], 
                         vtd %in% vtd_names[[2]]~which_districts[2], 
                         county %in% county_names[[1]]~which_districts[1],
                         county %in% county_names[[2]]~which_districts[2]
                         ))
  } else {
    plan %>%
      mutate(district=
               case_when(!district %in% which_districts~district, 
                         county %in% county_names[[1]]~which_districts[1],
                         county %in% county_names[[2]]~which_districts[2]
               ))
  }
}

# update linking edges
get_pairs <- function(plan, which_districts=NULL){
  if (is.null(which_districts)){
    which_districts <- 1:configs$num_districts
  }
  pairs_new <- 
    select(inputs$edges_vtd,vtd1,vtd2) %>%
    left_join(plan, by=c("vtd1"="vtd")) %>%
    left_join(plan, by=c("vtd2"="vtd")) %>%
    filter(district.x!=district.y,
           district.x %in% which_districts | 
             district.y %in% which_districts) %>%
    mutate(district1=pmin(district.x,district.y),
           district2=pmax(district.x,district.y),
           county1=pmin(county.x,county.y),
           county2=pmax(county.x,county.y)) %>%
    select(district1, district2, county1, county2, vtd1, vtd2) 
  
  return(pairs_new)
}

update_linking_edges <- function(plan, trees, 
                                 cut_edge=NULL, n_cuts=NULL, merged = NULL,
                                 l_old=NULL, which_districts=NULL){
  
  pairs <- get_pairs(plan, which_districts)
  
  # edge from merging districts
  if (!is.null(which_districts)){
    l1 <- tibble(district1=min(which_districts), 
                 district2=max(which_districts),
                 level=cut_edge$level,
                 county1=cut_edge$county, county2=NA,
                 vtd1=NA,vtd2=NA,
                 n_cuts=n_cuts, merged=list(merged))
    
    # old linking edges from unchanged districts
    l2 <- filter(l_old,
                 !district1 %in% which_districts,
                 !district2 %in% which_districts)  
  } else {
    l1 <- tibble()
    l2 <- tibble()
  }
  
  # remaining split counties
  l3 <- pairs %>%
    filter(county1==county2,
           !district1 %in% which_districts | !district2 %in% which_districts) %>%
    group_by(county1,county2) %>%
    sample_n(min(1, nrow(.))) %>%
    ungroup() %>%
    mutate(merged = map2(district1, district2, 
                         ~district_info(plan, c(.x,.y))),
           level="vtd", n_cuts=NA)
  
  if(nrow(count(l3,district1,district2))!=nrow(l3)){
    return(NULL) # illegal proposal has double linked edges
  }
  l_new <- rbind(l1,l2,l3)
  
  # additional remaining county edges
  n_extra <- configs$max_split - nrow(l_new)
  done_l4=F
  while(!done_l4){
    l4 <- pairs %>% # select uniformly from county pairs
    count(district1,district2,county1,county2) %>%
    select(-n) %>%
    anti_join(l_new, by=c("district1","district2")) %>%
    sample_n(n_extra) %>%
    mutate(merged = map2(district1, district2, 
                         ~district_info(plan, c(.x,.y)))) %>%
    mutate(level="county", vtd1=NA, vtd2=NA,
           n_cuts=NA)
    if (nrow(count(l4,district1,district2))==nrow(l4)){done_l4=T}
  }
  
  l_new <- rbind(l_new,l4) 
  
  # check initial state connected
  if (is.null(which_districts)){
    stopifnot(is.connected(graph_from_data_frame(l_new)))
  }
  
  return(l_new)
}