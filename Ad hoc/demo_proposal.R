library(tidyverse)
library(igraph)
library(sf)
library(tigris)

set.seed(2943)

for (f in list.files("src", full.names = T)){
  print(f)
  source(f)
}

# load data
inputs <- readRDS(file.path("inputData","model_inputs_NC.RDS"))
geo <- readRDS(file.path("inputData","geo_data.RDS"))

plan <- read_delim(file.path("inputData","NC", "NC_2016plan.txt"),
                   col_names = c("vtd","district")) %>%
  left_join(inputs$xwalk, by="vtd") %>%
  select(fips,county,county_name,county,vtd,district)

#plot_plan_districts(plan, which_districts = c(1,3))

######################################
#        (1) merge districts         #
######################################
merged <- district_info(plan, c(1,3))

G_c <- make_graph(merged$nodes_c, merged$edges_c)
#plot(G_c)

######################################
#   (2) Spanning tree on counties    #
######################################
E_c <- tibble()

while(nrow(E_c)==0){
T_c <- draw_spanning_tree(G_c, directed=T)

plot(T_c, layout=layout_as_tree, 
     vertex.label=paste0(V(T_c)$name, "\n", V(T_c)$pop),
     vertex.size=0)


######################################
# (3)      choose edge to cut        #
######################################
E_c <- get_cut_candidates_multi(T_c)
}

######################################
# (4)      make_proposal_plan        #
######################################
cut_edge <- sample_n(E_c,1)
plan_new <- update_plan(T_c, cut_edge, plan, which_districts=c(1,3))

######################################
# (5)           plot plan            #
######################################
vtd_tree <- filter(E_c, level=="vtd") %>% 
  group_by(county) %>% 
  filter((1:n())==1) %>%
  ungroup()
# plot_plan_districts(plan_new,  
#                     which_districts=c(1,3),
#                     which_counties=vtd_tree$county,
#                     county_tree=T_c, 
#                     vtd_tree=vtd_tree)

