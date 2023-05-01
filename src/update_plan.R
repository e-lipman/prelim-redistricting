update_plan <- function(tree_c, cut_edge, plan,
                        which_districts){
  # break county graph
  edge_c <- c(neighbors(tree_c, cut_edge$county, mode="in")$name,
              cut_edge$county)
  tree_c_split <- delete.edges(tree_c, get.edge.ids(tree_c, edge_c)) %>%
    decompose()
  
  if (cut_edge$level=="vtd"){
    # break precinct graph
    edge_v <- c(neighbors(cut_edge$tree[[1]], 
                           cut_edge$edge, mode="in")$name,
                 cut_edge$edge)
    tree_v_split <- delete.edges(cut_edge$tree[[1]],
                                 get.edge.ids(cut_edge$tree[[1]], edge_v)) %>%
      decompose()
    
    # identify precinct components with county components
    linking_county <- cut_edge$external[[1]] %>%
      filter(other_county==edge_c[1])
    county_comp <- linking_county$other_county %in% 
      V(tree_c_split[[2]])$name
    vtd_comp <- linking_county$this_vtd %in% 
      V(tree_v_split[[2]])$name
    flip <- sum(vtd_comp+county_comp)==1
    vtd_assignments <- 
      ifelse(rep(flip, 2), which_districts[2:1], which_districts[1:2])
    
    # assign new districts 
    plan_new <- plan %>%
      mutate(district=case_when(
        !district %in% which_districts ~ district,
        county==cut_edge$county &
          vtd %in% V(tree_v_split[[1]])$name ~ vtd_assignments[1],
        county==cut_edge$county &
          vtd %in% V(tree_v_split[[2]])$name ~ vtd_assignments[2],
        county %in% V(tree_c_split[[1]])$name ~ which_districts[1],
        county %in% V(tree_c_split[[2]])$name ~ which_districts[2]
      )) 
  } else {
    plan_new <- plan %>%
      mutate(district=case_when(
        !district %in% which_districts ~ district,
        county %in% V(tree_c_split[[1]])$name ~ which_districts[1],
        county %in% V(tree_c_split[[2]])$name ~ which_districts[2]
      )) 
  }
  return(plan_new)
}
