print_time <- function(str=""){
  print(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), ": ", str))
}

run_sampler <- function(plan_init,
                        iter=10, progress=1){
  
  plan <- select(inputs$nodes_vtd, vtd, county, fips, pop,
                 D_votes=D_pres12, R_votes=R_pres12) %>%
    mutate(district=plan_init)
  print_time("initializing trees")
  trees <- map(1:configs$num_districts, initialize_trees_district,
               plan=plan)
  print_time("initializing linking edges")
  linking <- update_linking_edges(plan, trees)
  cut_info <- NULL
  
  # tracking
  n_accept <- 0
  n_propose <- 0
  percent_dem_curr <- update_votes(plan)
  
  # output quantities
  plans <- matrix(nrow=length(plan_init), ncol=iter)
  num_dem <- rep(NA, iter)
  percent_dem_ord <- matrix(nrow=configs$num_districts, ncol=iter)
  
  start=tic()
  for (i in 1:iter){
    if (i%%progress==0){print_time(i)}
    
    # choose two districts to merge
    which_linking <- sample_n(linking,1)
    which_districts <- which_linking %>% 
      .[,1:2] %>% unlist() %>% unname()
    
    dont_split <- linking %>% # dont split counties that are already split
      filter(level=="vtd",
             !district1 %in% which_districts | 
               !district2 %in% which_districts) %>%
      pull(county1)
    
    merged <- which_linking$merged[[1]]
    
    G_c <- make_graph(merged$nodes_c, merged$edges_c)
    
    # draw tree and find cuttable edges
    T_c <- draw_spanning_tree(G_c, directed=T)
    E_c <- get_cut_candidates_multi(T_c, merged,
                                    dont_split = dont_split)
    if(is.na(which_linking$n_cuts) & nrow(E_c)>0){
      cut_info <- count_cuts(trees, which_linking)
      which_linking$n_cuts <- nrow(cut_info)
      linking$n_cuts[linking$district2==which_districts[1] &
                       linking$district2==which_districts[2]] <-
        which_linking$n_cuts
    }
    
    # acceptance prob
    if (nrow(E_c)>0){
      n_propose <- n_propose + 1
      cuts_curr <- which_linking$n_cuts
      cuts_prop <- nrow(E_c)
      accept_prob <- cuts_prop/cuts_curr  
      accept <- runif(1)<accept_prob
    } else {accept=0}
    
    if (accept){
      n_accept <- n_accept+1
      #print(paste0("merging districts: ", 
      #             paste0(which_districts, collapse=" ")))
      
      # cut edge and update plan
      cut_edge <- sample_n(E_c,1)
      
      trees_bak <- trees
      plan_bak <- plan
      linking_bak <- linking
      
      trees <- update_trees(T_c,cut_edge,which_districts,trees)
      plan <- update_plan(trees, cut_edge, which_districts, plan)
      trees <- trees %>%
        resample_edges_vc(which_district=which_districts[1], plan=plan) %>%
        resample_edges_vc(which_district=which_districts[2], plan=plan)
      
      linking <- update_linking_edges(plan, trees,
                                      cut_edge=cut_edge, 
                                      n_cut=nrow(E_c),
                                      merged=merged,
                                      l_old=linking, 
                                      which_districts=which_districts)
      if (is.null(linking)){ # un-accept
        trees <- trees_bak
        plan <- plan_bak
        linking <- linking_bak
        n_accept <- n_accept - 1
      }
      percent_dem_curr[which_districts] <- 
        update_votes(plan, which_districts)
    }
    
    # cache new counties
    if (accept){
      trees <- cache_vtrees(E_c, which_districts, trees,
                            plan)
    } else if (nrow(E_c)>0 & !accept & !is.null(cut_info)){
      trees <- cache_vtrees(cut_info, which_districts, trees,
                            plan)
    }
    cut_info <- NULL
    
    plans[,i] <- plan$district
    num_dem[i] <- sum(percent_dem_curr>.5)
    percent_dem_ord[,i] <- sort(percent_dem_curr)
  }
  end=toc()
  
  list(plans=plans, 
       num_dem=num_dem,
       percent_dem_ord=percent_dem_ord,
       accept_rate=n_accept/iter,
       propose_rate=n_propose/iter,
       time=end$toc-end$tic)
}

get_one_iter <- function(plan, which_districts){
  trees <- map(1:configs$num_districts, initialize_trees_district,
               plan=plan)
  linking <- update_linking_edges(plan, trees)
  
  # choose two districts to merge
  which_linking <- filter(linking,
                          district1 %in% which_districts,
                          district2 %in% which_districts)
  dont_split <- linking %>% # dont split counties that are already split
    filter(level=="vtd",
           !district1 %in% which_districts | 
             !district2 %in% which_districts) %>%
    pull(county1)
    
  merged <- which_linking$merged[[1]]
    
  G_c <- make_graph(merged$nodes_c, merged$edges_c)
    
  # draw tree and find cuttable edges
  E_c=tibble()
  while(nrow(E_c)==0){
    T_c <- draw_spanning_tree(G_c, directed=T)
    E_c <- get_cut_candidates_multi(T_c, merged,
                                    dont_split = dont_split)  
    vtd_tree <- filter(E_c,level=="vtd") %>%
      group_by(county) %>% sample_n(1) %>% ungroup()
  }
  cut_edge <- sample_n(E_c,1)
  
  trees_old <- map(trees[which_districts], ~(.x$tree_c)) %>%
    reduce(union)
  vtd_old <- map_df(trees[which_districts], 
                 ~tibble(county=names(.x$tree_v),
                         level="Vtd",
                         tree=.x$tree_v,
                         external=.x$edges_vc)) %>%
    group_by(county) %>%
    filter(n()>1)
  
  trees <- update_trees(T_c,cut_edge,which_districts,trees)
  plan <- update_plan(trees, cut_edge, which_districts, plan)
  
  list(T_c=T_c, E_c=E_c, vtd_tree=vtd_tree,
       T_old=trees_old,
       vtd_old=vtd_old,
       plan_new=plan)
}