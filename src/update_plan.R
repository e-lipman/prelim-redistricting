update_plan <- function(tree_c, cut_edge, plan,
                        which_districts){

  if (cut_edge$level=="vtd"){
    # break county tree
    tree_c_split <- delete.vertices(tree_c, cut_edge$county) %>% 
      decompose()
    
    # break precinct tree
    edge_v <- c(neighbors(cut_edge$tree[[1]], 
                           cut_edge$edge, mode="in")$name,
                 cut_edge$edge)
    tree_v_split <- delete.edges(cut_edge$tree[[1]],
                                 get.edge.ids(cut_edge$tree[[1]], edge_v)) %>%
      decompose()
    
    # identify precinct components with county components
    v_names <- map(tree_v_split, ~V(.x)$name)
    c_names <- map(tree_c_split, ~V(.x)$name)
    edges_cv <- cut_edge$external[[1]]
    
    c_comp <- map(c_names, ~(.x[.x %in% edges_cv$other_county]))
    v_comp <- map(v_names, ~(.x[.x %in% edges_cv$this_vtd]))
    c_which <- map_int(edges_cv$other_county,
                       ~which(sapply(c_comp, function(l){.x %in% l})))
    v_which <- map_int(edges_cv$this_vtd,
                       ~which(sapply(v_comp, function(l){.x %in% l})))
    names(v_which) <- c_which
    
    # assign new districts 
    plan_new <- plan %>%
      mutate(district=case_when(
        # keep other districts samw
        !district %in% which_districts ~ district,
        # assign vtds from split county
        county==cut_edge$county &
          vtd %in% v_names[[1]] ~ which_districts[1],
        county==cut_edge$county &
          vtd %in% v_names[[2]] ~ which_districts[2])) 
    for (i in 1:length(v_which)){
      plan_new$district[plan_new$county %in% c_names[[i]] &
                          is.na(plan_new$district)] =
        which_districts[v_which[as.character(i)]]
    }
  } else {
    edge_c <- c(neighbors(tree_c, cut_edge$county, mode="in")$name,
                cut_edge$county)  
    tree_c_split <- delete.edges(tree_c, get.edge.ids(tree_c, edge_c)) %>%
      decompose()
    
    plan_new <- plan %>%
      mutate(district=case_when(
        !district %in% which_districts ~ district,
        county %in% V(tree_c_split[[1]])$name ~ which_districts[1],
        county %in% V(tree_c_split[[2]])$name ~ which_districts[2]
      )) 
  }
  return(plan_new)
}

update_district_pairs <- function(plan, 
                                  pairs_old=NULL, which_update=NULL){
  if (is.null(which_update)){
    which_update <- unique(plan$district)
  }
  pairs_new <- 
    select(inputs$edges_vtd,vtd1,vtd2) %>%
    left_join(select(plan, vtd, district), by=c("vtd1"="vtd")) %>%
    left_join(select(plan, vtd, district), by=c("vtd2"="vtd")) %>%
    filter(district.x!=district.y,
           district.x %in% which_update | 
             district.y %in% which_update) %>%
    mutate(district1=pmin(district.x,district.y),
           district2=pmax(district.x,district.y)) %>%
    select(district1, district2) %>%
    distinct() %>%
    mutate(cuttable_edges=NA_integer_)
  if (!is.null(pairs_old)){
    pairs_new <- filter(pairs_old, 
           !district1 %in% which_update,
           !district2 %in% which_update) %>%
      bind_rows(pairs_new)
  }
  return(pairs_new)
}

# initialize_trees <- function(plan){
#   # precinct trees by district
#   dist_nodes <- select(plan, vtd, district) %>%
#     left_join(select(inputs$nodes_vtd, vtd, pop), by="vtd") %>%
#     nest(nodes=-district) %>%
#     arrange(district)
#   
#   dist_edges <- select(inputs$edges_vtd,vtd1,vtd2) %>%
#     left_join(select(plan, vtd, district), by=c("vtd1"="vtd")) %>% 
#     left_join(select(plan, vtd, district), by=c("vtd2"="vtd")) %>% 
#     filter(district.x==district.y) %>%
#     select(district=district.x,vtd1,vtd2) %>%
#     mutate(weight=1) %>%
#     nest(edges=-district) %>%
#     arrange(district)
#   
#   dist_trees <- full_join(dist_nodes, dist_edges, by="district") %>%
#     mutate(dist_graph=map2(nodes, edges, 
#                            make_graph, level="vtd"),
#            dist_tree=map(dist_graph, draw_spanning_tree)) %>%
#     select(district, tree=dist_tree)
# }
# 
# initialize_num_cuttable(plan, dist_trees, which_districts){
#   stopifnot(length(which_districts)==2)
# }


