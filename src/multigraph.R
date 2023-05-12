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

initialize_trees_district <- function(plan, linking, which_district){
  dist_info <- district_info(plan, which_district)
  
  # county tree
  T_c <- draw_tree_one(dist_info)
  
  # precinct trees for split county
  split_counties <- linking %>%
    filter(district1==which_district | district2==which_district,
           level=="vtd") %>%
    pull(county)
  T_v <- map(split_counties,
             draw_tree_one, dist_info=dist_info, level="vtd")
  edges_vc <- map(split_counties, get_external_edges, 
      dist_info=dist_info, tree_c=T_c)
  names(T_v) <- split_counties
  names(edges_vc) <- split_counties
  
  list(tree_c = T_c, tree_v = T_v, edges_vc = edges_vc)
}
