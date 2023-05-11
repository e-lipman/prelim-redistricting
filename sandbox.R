library(tidyverse)
library(yaml)
library(igraph)

for (f in list.files("src", full.names = T)){
  print(f)
  source(f)
}

args <- list(seed_num=2, chain=1)
set.seed(2943)

# load data
inputs <- readRDS(file.path("inputData","model_inputs_NC.RDS"))
configs <- read_yaml("configs.yml")

plan_init <- inputs$seed_plans[[args$seed_num]]

# run sampler function
run_sampler <- function(iter, plan_init){
  res <- matrix(nrow=length(plan_init), ncol=iter)
  
  plan <- select(inputs$nodes_vtd, vtd, county) %>%
    mutate(district=plan_init)
  linking <- initialize_linking_edges(plan)
  trees <- map(1:configs$num_districts, initialize_trees_district,
               plan=plan, linking=linking)
  
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

res <- run_sampler(1000, plan_init)

