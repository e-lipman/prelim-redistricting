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
  
  trees <- map(1:configs$num_districts, initialize_trees_district,
                 plan=plan)
  
  linking <- update_linking_edges(plan, trees)
  
  for (i in 1:iter){
    print(i)
    
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
    
    # acceptance prob
    cuts_curr <- which_linking$n_cuts
    cuts_prop <- nrow(E_c)
    accept <- cuts_prop/cuts_curr
    print(paste("cuts:",cuts_prop, cuts_curr))
    
    if (runif(1)<accept){
      print(which_districts)
      # cut edge and update plan
      cut_edge <- sample_n(E_c,1)
      
      plan <- update_plan(T_c, cut_edge, plan, which_districts=which_districts)
      linking <- update_linking_edges(plan, 
                                      cut_edge=cut_edge, 
                                      n_cut=nrow(E_c),
                                      merged=merged,
                                      l_old=linking, 
                                      which_districts=which_districts)
    }
    
    # out 
    res[,i] <- plan$district
  }
  
  return(res)
}

res <- run_sampler(20, plan_init)

