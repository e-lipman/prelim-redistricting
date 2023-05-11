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

get_district_pairs <- function(plan, which_districts=NULL){
  if (is.null(which_districts)){
    which_districts <- 1:NUM_DISTRICTS
  }
  pairs_new <- 
    select(inputs$edges_vtd,vtd1,vtd2) %>%
    left_join(select(plan, vtd, district), by=c("vtd1"="vtd")) %>%
    left_join(select(plan, vtd, district), by=c("vtd2"="vtd")) %>%
    filter(district.x!=district.y,
           district.x %in% which_districts | 
             district.y %in% which_districts) %>%
    mutate(district1=pmin(district.x,district.y),
           district2=pmax(district.x,district.y)) %>%
    select(district1, district2) %>%
    distinct() 
  
  return(pairs_new)
}

# functions for managing linking edges

get_split_counties <- function(plan, which_districts=NULL){
  if (is.null(which_districts)){
    which_districts <- 1:NUM_DISTRICTS
  }
  split_counties <- 
    count(plan, county, district) %>%
    group_by(county) %>%
    mutate(n_dist=n()) %>%
    ungroup() %>%
    filter(n_dist>1) %>%
    arrange(county, district) %>%
    mutate(idx=rep(paste0("district",1:2), nrow(.)/2)) %>%
    select(-n_dist, -n) %>%
    pivot_wider(names_from=idx, values_from="district") %>%
    filter(district1 %in% which_districts | 
             district2 %in% which_districts) %>%
    select(district1, district2,county) 
  
  return(split_counties)
}

initialize_linking_edges <- function(plan){
  # split counties
  split_counties <- get_split_counties(plan) %>%
    mutate(level="vtd")
  
  # additional edges
  pairs <- get_district_pairs(plan)
  n_extra <- NUM_SPLIT - nrow(split_counties)
  extra_edges <- pairs %>%
    anti_join(split_counties, by=c("district1","district2")) %>%
    sample_n(n_extra) %>%
    select(district1, district2) %>%
    mutate(level="county", county=NA)
  
  linking_edges <- rbind(split_counties, extra_edges)
  return(linking_edges)
}

update_linking_edges <- function(plan, cut_edge, 
                                 l_old, which_districts){
  
  # edge from merging districts
  l1 <- tibble(district1=min(which_districts), 
               district2=max(which_districts),
               level=cut_edge$level,
               county=ifelse(level=="county",NA,cut_edge$county))
  
  # old linking edges from unchanged districts
  l2 <- filter(l_old,
               !district1 %in% which_districts,
               !district2 %in% which_districts)
  
  # remaining split counties
  l3 <- get_split_counties(plan, which_districts) %>%
    filter(!district1 %in% which_districts | !district2 %in% which_districts) %>%
    mutate(level="vtd")
  
  l_new <- rbind(l1,l2,l3)
  
  # additional remaining county edges
  pairs <- get_district_pairs(plan, which_districts)
  n_extra <- NUM_SPLIT - nrow(l_new)
  l4 <- pairs %>%
    anti_join(l_new, by=c("district1","district2")) %>%
    sample_n(n_extra) %>%
    select(district1, district2) %>%
    mutate(level="county",county=NA)
  
  rbind(l_new,l4)
}
