# update district assignments based on cut edge
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

# get_split_counties <- function(plan, which_districts=NULL){
#   if (is.null(which_districts)){
#     which_districts <- 1:configs$num_districts
#   }
#   select(inputs$edges_vtd,vtd1,vtd2) %>%
#     left_join(plan, by=c("vtd1"="vtd")) %>%
#     left_join(plan, by=c("vtd2"="vtd")) %>%
#     filter(district.x!=district.y,
#            district.x %in% which_districts | 
#              district.y %in% which_districts) %>%
#     mutate(district1=pmin(district.x,district.y),
#            district2=pmax(district.x,district.y),
#            county1=pmin(county.x,county.y),
#            county2=pmax(county.x,county.y)) %>%
#     select(district1, district2, county1, county2) %>%
#     distinct()
#   
#   split_counties <-
#     count(plan, county, district) %>%
#     group_by(county) %>%
#     mutate(n_dist=n()) %>%
#     ungroup() %>%
#     filter(n_dist>1) %>%
#     arrange(county, district) %>%
#     mutate(idx=rep(paste0("district",1:2), nrow(.)/2)) %>%
#     select(-n_dist, -n) %>%
#     pivot_wider(names_from=idx, values_from="district") %>%
#     filter(district1 %in% which_districts | 
#              district2 %in% which_districts) %>%
#     select(district1, district2,county) 
#   
#   return(split_counties)
# }

update_linking_edges <- function(plan, trees, 
                                 cut_edge=NULL, n_cuts=NULL, merged = NULL,
                                 l_old=NULL, which_districts=NULL){
  
  #trees_c <- map(trees, ~(.x$tree_c))
  #trees_v <- map(trees, ~(.x$tree_v))
  
  pairs <- get_pairs(plan, which_districts)
  
  # edge from merging districts
  if (!is.null(which_districts)){
    l1 <- tibble(district1=min(which_districts), 
                 district2=max(which_districts),
                 level=cut_edge$level,
                 county=ifelse(level=="county",NA,cut_edge$county),
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
    sample_n(1) %>%
    ungroup() %>%
    mutate(merged = map2(district1, district2, 
                         ~district_info(plan, c(.x,.y))),
           level="vtd")
  n_cuts <- map_int(1:nrow(l3), ~count_cuts(trees, l3[.x,]))
  l3$n_cuts <- n_cuts
  l_new <- rbind(l1,l2,l3)
  
  # additional remaining county edges
  n_extra <- configs$max_split - nrow(l_new)
  l4 <- pairs %>%
    anti_join(l_new, by=c("district1","district2")) %>%
    sample_n(n_extra) %>%
    mutate(merged = map2(district1, district2, 
                         ~district_info(plan, c(.x,.y)))) %>%
    mutate(level="county", vtd1=NA, vtd2=NA)
  n_cuts <- map_int(1:nrow(l4), ~count_cuts(trees, l4[.x,]))
  l4$n_cuts <- n_cuts
  
  l_new <- rbind(l_new,l4) 
  
  # check initial state connected
  if (is.null(which_districts)){
    stopifnot(is.connected(graph_from_data_frame(l_new)))
  }
  
  return(l_new)
}



