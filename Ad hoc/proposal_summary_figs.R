source(file.path("Ad hoc","demo_proposal.R"))
library(scales)

DISTRICTS <- c(1,3)
COLORS <- hue_pal()(length(unique(plan$district)))
COLORS <- COLORS[map_dbl(1:length(COLORS), 
                      ~which(.x==sort(as.character(1:length(COLORS)))))]
OUT_PATH <- file.path("figures","illustrate_proposal")

# manually move a precinct point
geo <- readRDS(file.path("inputData","geo_data.RDS"))
latlong_new <- geo$latlong_vtd[geo$latlong_vtd$vtd=="289",3:4]
geo$latlong_vtd$lat[geo$latlong_vtd$vtd=="289"] <- latlong_new$lat +.03
geo$latlong_vtd$lon[geo$latlong_vtd$vtd=="289"] <- latlong_new$lon +.02

## demo of graph and tree partitions
plot_full_graph(plan_new, which_districts = DISTRICTS, tree=F)
ggsave(file.path(OUT_PATH, "vtd_graph.jpeg"), height=4)

plot_full_graph(plan_new, which_districts = DISTRICTS, tree=T)
ggsave(file.path(OUT_PATH, "vtd_tree.jpeg"), height=4)

#edge <- c(cut_edge$edge, 
#          neighbors(cut_edge$tree[[1]], cut_edge$edge, mode="in")$name)
#tree_split <- delete.edges(cut_edge$tree[[1]],
#                           get.edge.ids(cut_edge$tree[[1]], edge))
plot_plan_districts(plan_new, which_districts = DISTRICTS,
                    county_tree = T_c,
                    vtd_tree = cut_edge) +
  scale_fill_manual(values=c("1"=COLORS[1],"3"=COLORS[3]))
ggsave(file.path(OUT_PATH, "vtd_tree_multi.jpeg"), height=4)

## step 0: Current plan
plot_plan_districts(plan) 
ggsave(file.path(OUT_PATH, "district_plot.jpeg"), height=3)

## step 1: choose two districts to merge
plot_plan_districts(plan, which_districts = DISTRICTS) 
  #scale_fill_manual(values=c("1"=COLORS[7],"3"=COLORS[3]))
ggsave(file.path(OUT_PATH, "step1.jpeg"),
       height=4)

## step 2: draw coarse spanning tree on merged districts
plot_plan_districts(plan, which_districts = DISTRICTS,
                    county_tree = T_c,
                    color_districts = F)
ggsave(file.path(OUT_PATH, "step2.jpeg"),
       height=4)

## step 3: identify cuttable edges and splitable nodes
plot_plan_districts(plan, which_districts = DISTRICTS,
                    county_tree = T_c,
                    color_districts = F,
                    highlight = E_c)
ggsave(file.path(OUT_PATH, "step3.jpeg"),
       height=4)

## step 4: draw spanning trees on expandable nodes
vtd_tree <- filter(E_c, level=="vtd") %>% 
  group_by(county) %>% 
  filter((1:n())==1) %>%
  ungroup()
plot_plan_districts(plan, which_districts = DISTRICTS,
                    county_tree = T_c,
                    vtd_tree = vtd_tree,
                    color_districts = F)
ggsave(file.path(OUT_PATH, "step4.jpeg"),
       height=4)

## step 5: identify cuttable edges and splitable nodes
plot_plan_districts(plan, which_districts = DISTRICTS,
                    county_tree = T_c,
                    vtd_tree = vtd_tree,
                    which_counties = vtd_tree$county,
                    color_districts = F,
                    highlight = E_c)
ggsave(file.path(OUT_PATH, "step5.jpeg"),
       height=3.5)

## step 6: propose new plan by choosing edge to cut
plot_plan_districts(plan_new, which_districts = DISTRICTS,
                    county_tree = T_c,
                    vtd_tree = vtd_tree) +
  scale_fill_manual(values=c("1"=COLORS[1],"3"=COLORS[3]))
ggsave(file.path(OUT_PATH, "step6.jpeg"), height=4)

plot_plan_districts(plan_new, which_districts = DISTRICTS) +
  scale_fill_manual(values=c("1"=COLORS[1],"3"=COLORS[3]))
ggsave(file.path(OUT_PATH, "step6_notree.jpeg"), height=4)



