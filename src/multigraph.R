get_external_edges <- function(dist_info, tree_c,
                               which_county){
  
  neighbors <- neighbors(tree_c, which_county, mode="all")$name
  if (length(neighbors)>0){
    external_edges <- filter(dist_info$edges_v,
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
      mutate(other_county=other_county,
             this_vtd=as.character(this_vtd)) 
  } else {
    selected_edges <- tibble()
  }
  
  
  return(selected_edges)
}

draw_tree_one <- function(dist_info,
                          level="county",
                          which_county=NULL){
  if (level=="county"){
    G <- make_graph(dist_info$nodes_c, dist_info$edges_c,
                    level="county")
  } else (
    G <- make_graph(dist_info$nodes_v, dist_info$edges_v,
                    level="vtd", which_county = which_county)
  )
  draw_spanning_tree(G)  
}

initialize_trees_district <- function(plan, which_district){
  dist_info <- district_info(plan, which_district)
  
  # county tree
  T_c <- draw_tree_one(dist_info)
  
  # precinct trees for split county
  split_counties <- get_pairs(plan, which_district) %>%
    filter(county1==county2,
           district1==which_district | district2==which_district) %>%
    pull(county1) %>% unique()
  T_v <- map(split_counties,
             draw_tree_one, dist_info=dist_info, level="vtd")
  edges_vc <- map(split_counties, get_external_edges, 
      dist_info=dist_info, tree_c=T_c)
  names(T_v) <- split_counties
  names(edges_vc) <- split_counties
  
  list(tree_c = T_c, tree_v = T_v, edges_vc = edges_vc,
       tree_v_nonsplit = list(), edges_vc_nonsplit = list())
}

merge_trees <- function(trees, merged, 
                        which_districts, 
                        level, 
                        which_counties=NULL,
                        which_vtd=NULL){
  
    # county tree
    trees_c <- map(trees[which_districts], ~(.x$tree_c))
    tree_c_merged <- trees_c[[1]] %u% trees_c[[2]]
    
    if (level=="county"){
      tree_c_merged <-  tree_c_merged%>% 
        add_edges(which_counties) %>% as.undirected()
    }
    V(tree_c_merged)$pop <- 
      ifelse(!is.na(V(tree_c_merged)$pop_1), 
             V(tree_c_merged)$pop_1,0) +
      ifelse(!is.na(V(tree_c_merged)$pop_2), 
             V(tree_c_merged)$pop_2,0) 
      
    tree_c_merged <- draw_spanning_tree(tree_c_merged)
    out <- list(tree_c=tree_c_merged)
    
    # vtd tree
    if (level=="vtd"){
      trees_v <- map(trees[which_districts], ~(.x$tree_v[[which_counties]]))
      tree_v_merged <- trees_v[[1]] %u% trees_v[[2]] %>%
        add_edges(which_vtd) %>% as.undirected()
        
      V(tree_v_merged)$pop <- 
        ifelse(!is.na(V(tree_v_merged)$pop_1), 
               V(tree_v_merged)$pop_1,
               V(tree_v_merged)$pop_2) 
      
      out$tree_v <- draw_spanning_tree(tree_v_merged)
      
      out$edges_vc <- map_df(trees[which_districts], 
                         ~(.x$edges_vc[[which_counties]]))

    }
    
    return(out)
}

count_cuts <- function(trees, linking){
  which_districts <- c(linking$district1, linking$district2)
  which_counties <- unique(c(linking$county1, linking$county2))
  merged <- linking$merged[[1]]
  
  dont_split <- unlist(map(trees[which_districts], ~names(.x$tree_v)))
  if (linking$level=="vtd"){
    dont_split <- dont_split[!dont_split %in% which_counties]  
  }
  
  # get cached nonsplit counties
  cached <- map(trees[which_districts], ~names(.x$tree_v_nonsplit))
  dist_i <- c(rep(1,length(cached[[1]])), rep(2,length(cached[[2]])))
  cached_vtrees <- list()
  if (length(dist_i)>0){
    for (i in 1:length(dist_i)){
      cnty = unlist(cached)[i]
      add <- list(
        tree_v = trees[[which_districts[dist_i[i]]]]$tree_v_nonsplit[[cnty]],
        edges_vc = trees[[which_districts[dist_i[i]]]]$edges_vc_nonsplit[[cnty]])
      cached_vtrees[[cnty]] <- add
    }
  }
  
  # get merged
  if (linking$level=="county"){
    tree_merged <- merge_trees(trees, merged,  which_districts, 
                               linking$level, 
                               which_counties=which_counties)  

  } else {
    which_vtd <- as.character(c(linking$vtd1, linking$vtd2))
    tree_merged <- merge_trees(trees, merged,  which_districts, 
                               linking$level, 
                               which_counties=which_counties,
                               which_vtd=which_vtd) 
    cached_vtrees[[which_counties]] <- tree_merged[2:3]
  }
  E_c <- get_cut_candidates_multi(tree_merged$tree_c, 
                                  merged, dont_split, 
                                  cached_vtrees = cached_vtrees)
  return(E_c)
}
