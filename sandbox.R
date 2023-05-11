library(tidyverse)
library(igraph)

set.seed(2943)
NUM_DISTRICTS <- 13
NUM_SPLIT <- 13

for (f in list.files("src", full.names = T)){
  print(f)
  source(f)
}

# load data
inputs <- readRDS(file.path("inputData","model_inputs_NC.RDS"))

plan_init <- read_delim(file.path("inputData","NC", 
                                  "HB1029_3rd_Edition.txt"),
                   col_names = c("vtd","district")) %>%
  left_join(inputs$xwalk, by="vtd") %>%
  select(fips,county,county_name,county,vtd,district)

run_sampler <- function(iter, plan_init){
  res <- matrix(nrow=nrow(plan_init), ncol=iter)
  
  plan <- plan_init
  linking <- initialize_linking_edges(plan_init)
  
  for (i in 1:iter){
    print(i)
    
    # choose two districts to merge
    which_districts <- linking %>% 
      sample_n(1) %>% .[,1:2] %>% unlist() %>% unname()
    dont_split <- linking %>% # dont split counties that are already split
      filter(level=="vtd", 
             !district1 %in% which_districts | 
               !district2 %in% which_districts) %>%
      pull(county)
    
    merged <- district_info(plan, which_districts)
    #plot_plan_districts(plan, which_districts)
    G_c <- make_graph(merged$nodes_c, merged$edges_c)
    stopifnot(length(V(G_c))>1)
    
    # draw tree and find cuttable edges
    T_c <- draw_spanning_tree(G_c, directed=T)
    E_c <- get_cut_candidates_multi(T_c, merged,
                                    dont_split = dont_split)
    if (nrow(E_c)>0){
      print(which_districts)
      
      # cut edge and update plan
      cut_edge <- sample_n(E_c,1)
      
      plan <- update_plan(T_c, cut_edge, plan, which_districts=which_districts)
      linking <- update_linking_edges(plan, cut_edge, linking, which_districts)
    }
    
    
    # out 
    res[,i] <- plan$district
  }
  
  return(res)
}

res <- run_sampler(100, plan_init)
