library(tidyverse)
library(igraph)

set.seed(2943)

for (f in list.files("src", full.names = T)){
  print(f)
  source(f)
}

# load data
inputs <- readRDS(file.path("inputData","model_inputs_NC.RDS"))

plan_init <- read_delim(file.path("inputData","NC", "NC_2016plan.txt"),
                   col_names = c("vtd","district")) %>%
  left_join(inputs$xwalk, by="vtd") %>%
  select(fips,county,county_name,county,vtd,district)

pairs <- update_district_pairs(plan_init)

iter=31
run_sampler <- function(iter, plan_init){
  res <- matrix(nrow=nrow(plan_init), ncol=iter)
  
  plan <- plan_init
  pairs <- update_district_pairs(plan)
  
  for (i in 1:iter){
    # choose two districts to merge
    print(i)
    which_districts <- pairs %>%
      sample_n(1) %>% .[,1:2] %>% unlist() %>% unname()
    merged <- district_info(plan, which_districts)
    plot_plan_districts(plan, which_districts = which_districts)
    G_c <- make_graph(merged$nodes_c, merged$edges_c)
    
    # draw tree and find cuttable edges
    T_c <- draw_spanning_tree(G_c, directed=T)
    E_c <- get_cut_candidates_multi(T_c, merged)
    
    if (nrow(E_c)>0){
      print(i)
      # cut edge and update plan
      cut_edge <- sample_n(E_c,1)
      plan_old <- plan
      plan <- update_plan(T_c, cut_edge, plan, which_districts=which_districts)
      stopifnot(!is.na(plan$district))
      pairs <- update_district_pairs(plan, pairs, which_districts)
      
      # if (any(E_c$level=="vtd")){
      #   vtd_tree <- filter(E_c, level=="vtd") %>% 
      #     group_by(county) %>% 
      #     filter((1:n())==1) %>%
      #     ungroup()  
      # } else {vtd_tree <- NULL}
      
      # plot_plan_districts(plan, which_districts = which_districts,
      #                     county_tree = T_c, vtd_tree = vtd_tree,
      #                     highlight = E_c) %>%
      #   print
    }
    
    # out 
    res[,i] <- plan$district
  }
  
  return(res)
}

res <- run_sampler(50, plan_init)

# check population
plan_init %>%
  mutate(district=res[,50]) %>%
  left_join(select(inputs$nodes_vtd, vtd, pop), by="vtd") %>%
  group_by(district) %>%
  summarise(pop=sum(pop),
            dev=pop/inputs$ideal_pop) %>%
  pull(dev) %>% range()
