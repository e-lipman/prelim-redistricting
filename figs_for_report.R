library(tidyverse)
library(yaml)

for (f in list.files("src", full.names = T)){
  print(f)
  source(f)
}

DISTRICTS <- c(1,3)

inputs <- readRDS(file.path("inputData","model_inputs_NC.RDS"))
geo <- readRDS(file.path("inputData","geo_data.RDS"))
configs <- read_yaml("configs.yml")

plan <- inputs$nodes_vtd %>% mutate(district=inputs$seed_plans[[1]])

#################################
#   Figure 1: ReCom proposal    #
#################################
HEIGHT = 3.5
WIDTH = 5

# panel A: graph on precincts for two adjacent districts
plot_full_graph(plan, which_districts = DISTRICTS, tree=F) 
ggsave(file.path("figures","recom_a.jpeg"), 
       height=HEIGHT, width=WIDTH)

# panel B: spanning tree on precincts for two adjacent districts
plot_full_graph(plan, which_districts = DISTRICTS) 
ggsave(file.path("figures","recom_b.jpeg"), 
       height=HEIGHT, width=WIDTH)

# panel C: cuttable edges
plot_full_graph(plan, which_districts = DISTRICTS, highlight=T)  
ggsave(file.path("figures","recom_c.jpeg"),
       height=HEIGHT, width=WIDTH)

# panel D: proposed plan
plot_full_graph(plan, which_districts = DISTRICTS, update_districts = T) 
ggsave(file.path("figures","recom_d.jpeg"),
       height=HEIGHT, width=WIDTH)


#################################
#   Figure 2: Graph hierarchy   #
#################################

# panel A: county-level 
plot_full_graph(plan, which_districts = DISTRICTS, level="county",
                color_districts=F)
ggsave(file.path("figures","graph_hier_a.jpeg"), 
       height=HEIGHT, width=WIDTH)

# panel B: multiscale tree
set.seed(3215)
one_iter <- get_one_iter(plan, DISTRICTS)

plot_plan_districts(plan, which_districts = DISTRICTS,
                    county_tree = one_iter$T_old,
                    vtd_tree = one_iter$vtd_old)
ggsave(file.path("figures","graph_hier_b.jpeg"), 
       height=HEIGHT, width=WIDTH)

#################################
#  Figure 3: Multiscale ReCom   #
#################################

# panel A: cuttable edges and splittable nodes at county-level
plot_plan_districts(plan, which_districts = DISTRICTS,
                    county_tree = one_iter$T_c,
                    color_districts = F,
                    highlight = one_iter$E_c)
ggsave(file.path("figures","mrecom_a.jpeg"), 
       height=HEIGHT, width=WIDTH)

# panel B: cuttable edges at precinct-level
plot_plan_districts(one_iter$plan_new, 
                    which_districts = DISTRICTS,
                    county_tree = one_iter$T_c,
                    vtd_tree = one_iter$vtd_tree,
                    highlight = one_iter$E_c)
ggsave(file.path("figures","mrecom_b.jpeg"), 
       height=HEIGHT, width=WIDTH)
