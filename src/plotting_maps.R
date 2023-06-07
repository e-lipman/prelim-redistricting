#library(tigris)
library(sf)
geo <- readRDS(file.path("inputData","geo_data.RDS"))

plot_edges <- function(g, edge_info, latlong1, latlong2=NULL, 
                      add_nodes = TRUE,   
                      color="black",nodecolor="black", linewidth=1, nodesize=1,
                      weight_edges=F){
  if (is.null(latlong2)){
    latlong2 <- latlong1
  }
  
  # make names consistant
  if (!weight_edges){
    edge_info <- set_names(edge_info, c("id1","id2"))
  } else {
    edge_info <- set_names(edge_info, c("id1","id2","wt"))
  }
  latlong1 <- set_names(latlong1, c("id","lat","lon"))
  latlong2 <- set_names(latlong2, c("id","lat","lon"))
  
  # get latlong for edges
  edges_latlong <- edge_info %>%
    inner_join(latlong1, by=c("id1"="id")) %>%
    inner_join(latlong2, by=c("id2"="id"))
  
  if (!weight_edges){
    out <- g + 
      geom_segment(data=edges_latlong,
                   aes(x=lon.x, y=lat.x, xend=lon.y, yend=lat.y),
                   color=color, linewidth=linewidth)  
  } else {
    out <- g + 
      geom_segment(data=edges_latlong,
                   aes(x=lon.x, y=lat.x, xend=lon.y, yend=lat.y,
                       linewidth=wt),
                   color=color) +
      scale_linewidth_continuous(range=.15*c(1,17))
  }
  if (add_nodes){
    out <- out + 
      geom_point(data=edges_latlong, aes(x=lon.x, y=lat.x), 
                 size=nodesize, color=nodecolor) +
      geom_point(data=edges_latlong, aes(x=lon.y, y=lat.y), 
                 size=nodesize, color=nodecolor)
  }
  return(out)
}

highlight_cut_edges <- function(g, cut_edges, 
                                county_tree=NULL,
                                edges_v2=NULL,
                                level="vtd",
                                color="red", color2="blue"){
  
  edges_c <- filter(cut_edges, level=="county")
  edges_v <- filter(cut_edges, level=="vtd")
  parents_c <- map_chr(edges_c$edge,   
                       ~neighbors(county_tree, .x, mode="in")$name)  
  out <- g
  
  if (level == "vtd"){
    parents_v <- map2_chr(edges_v$edge, edges_v$tree,  
                          ~neighbors(.y, .x, mode="in")$name)  
    edges_v <- tibble(vtd1=edges_v$edge, vtd2=parents_v)
    edges_v2 <- filter(edges_v2, 
           county1 %in% edges_c$county & county2 %in% parents_c) %>%
      select(vtd1, vtd2)
    
    out  <- plot_edges(out, edges_v, select(geo$latlong_vtd, -county), 
              color=color, linewidth=1.5)
    out  <- plot_edges(out, edges_v2, select(geo$latlong_vtd, -county), 
                       color=color2, linewidth=1.5)
    nodes_v <- unique(c(edges_v$vtd1, edges_v$vtd2))
    out <- plot_edges(out, tibble(id1=nodes_v, id2=nodes_v), 
                      select(geo$latlong_vtd,-county))
  } else if (level=="county"){
    edges_c <- tibble(county1=edges_c$edge, county2=parents_c)
    out  <- plot_edges(out, edges_c, geo$latlong_county, 
                      color=color2, linewidth=2)
    
    nodes_c <- unique(edges_v$county)
    out <- plot_edges(out, tibble(county1=nodes_c, county2=nodes_c), 
              geo$latlong_county, color=color, nodesize=4)
  }
}

############################################

plot_plan_districts <- function(plan, 
                                which_districts=NULL,
                                which_counties=NULL,
                                county_tree=NULL,
                                vtd_tree=NULL,
                                color_districts=TRUE,
                                highlight=NULL,
                                vtd_borders=T){
  if (is.null(vtd_tree)){
    vtd_tree <- tibble(county=character(0))
  } 
  if (!is.null(which_counties)){
    vtd_tree <- filter(vtd_tree, county %in% which_counties)
  }
  if (is.numeric(plan)){
    plan <- inputs$nodes_vtd %>% mutate(district=plan)
  }

  plan_subset <- plan
  if (!is.null(which_counties)){
    plan_subset <- filter(plan_subset, county %in% which_counties)
  }
  if (!is.null(which_districts)){
    plan_subset <- filter(plan_subset, district%in% which_districts)
  }
  plan_subset <- plan_subset %>%
    group_by(county) %>%
    mutate(district = as.factor(district)) %>%
    ungroup()    
  
  # district info at county and vtd level
  split_counties <- plan %>%
    count(county, district) %>%
    group_by(county) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    filter(n>1) %>% pull(county)
  plan_counties <- filter(plan_subset) %>%
    mutate(fips=as.character(fips),
           split=county %in% split_counties) %>%
    select(fips, district,split) %>%
    distinct()
  plan_vtd <- filter(plan_subset,
                     county %in% c(vtd_tree$county, split_counties))
  
  # subset shapefiles for plot
  county_subset <- right_join(geo$sf_county, plan_counties, by="fips")
  vtd_subset <- right_join(geo$sf_vtd, plan_vtd, by="vtd")
  
  # make plot
  if (color_districts){
    g <- ggplot() +
      geom_sf(data=filter(county_subset, !split), aes(fill=district)) +
      geom_sf(data=vtd_subset, aes(fill=district),
              color=NA) +
      geom_sf(data=filter(geo$sf_county, fips %in% plan_subset$fips),
              fill=NA) +
      #geom_sf(data=filter(geo$sf_vtd, vtd %in% plan_vtd$vtd & 
      #                      vtd_borders),
      #        fill=NA, linetype="dotted") +
      theme_void() +
      theme(legend.position = "none")  
  } else {
    g <- ggplot() +
      geom_sf(data=filter(geo$sf_county, fips %in% plan_subset$fips),
              fill="gray") +
      #geom_sf(data=filter(geo$sf_vtd, vtd %in% plan_vtd$vtd),
      #        fill="gray", linetype="dotted") +
      theme_void() +
      theme(legend.position = "none")
  }
  
  if (!is.null(county_tree) | !is.null(vtd_tree)){
    g <- add_spanning_trees(g, plan_subset, county_tree, 
                            vtd_tree, highlight)
  }
  
  return(g)
}

add_spanning_trees <- function(g, plan_subset,  
                               #which_districts=NULL, 
                               #which_counties=NULL, 
                               county_tree=NULL, 
                               vtd_tree=NULL, 
                               highlight=NULL){
  # add county tree
  if (!is.null(county_tree)){
    counties_c <- plan_subset$county[!plan_subset$county %in% vtd_tree$county]
    edges_c <- as_data_frame(county_tree) %>%
      select(county1=from, county2=to) %>%
      filter(county1 %in% c(counties_c),
             county2 %in% c(counties_c))
    g <- plot_edges(g, edges_c, geo$latlong_county)  
  }
  
  # add precinct trees
  if (nrow(vtd_tree)>0){
    # add edges within expanded counties
    vtd_v <- plan_subset$vtd[plan_subset$county %in% vtd_tree$county]
    edges_v <- 
      reduce(map(1:nrow(vtd_tree), 
                 ~as_data_frame(vtd_tree$tree[[.x]])), 
             rbind) %>%
      set_names(c("vtd1","vtd2")) %>%
      filter(vtd1 %in% vtd_v, vtd2 %in% vtd_v)
    g <- plot_edges(g, edges_v, select(geo$latlong_vtd, -county),
                    linewidth=.5, color="gray15", nodesize=.5)
    
    # add edges between expanded and non-expanded counties
    edges_vc <- reduce(vtd_tree$external,rbind) %>%
      select(vtd=this_vtd, county=other_county) %>%
      filter(!county %in% vtd_tree$county)
    g <- plot_edges(g, edges_vc, select(geo$latlong_vtd, -county), 
                    geo$latlong_county, linewidth=.75)
    
    # add edges between expanded counties
    if (length(unique(vtd_tree$county))>1){
      county_pairs <- vtd_tree %>%
        select(edge, county, external) %>% unnest(external) %>%
        filter(other_county %in% vtd_tree$county) %>%
        select(county1=county, county2=other_county) %>%
        filter(county1<county2)
      edges_v2 <- county_pairs %>%
        left_join(vtd_tree, by=c("county1"="county")) %>%
        left_join(vtd_tree, by=c("county2"="county")) %>%
        select(county1, county2, vtd1=edge.x, vtd2=edge.y)
      g <- plot_edges(g, 
                      select(edges_v2, vtd1, vtd2), 
                      select(geo$latlong_vtd,-county),
                      add_nodes = F, linewidth=.75)
    }
  }
  
  # highlight cut candidtaes
  if (!is.null(highlight)){
    if (nrow(vtd_tree)==0){
      g <- highlight_cut_edges(g, highlight, county_tree, level="county")  
    } else {
      g <- highlight_cut_edges(g, highlight, county_tree, 
                               edges_v2, level="vtd")  
    }
  }
  return(g)
}

plot_full_graph <- function(plan, 
                            which_districts=NULL,
                            which_counties=NULL,
                            level="vtd",
                            tree=T, 
                            color_districts=T,
                            # these settings illustrate ReCom steps
                            sep_districts=F,
                            highlight=F,
                            update_districts=F){
  # plan_subset <- plan
  # if (!is.null(which_counties)){
  #   plan_subset <- filter(plan_subset, county %in% which_counties)
  # }
  # if (!is.null(which_districts)){
  #   plan_subset <- filter(plan_subset, district%in% which_districts)
  # }
  # plan_subset <- plan_subset %>%
  #   group_by(county) %>%
  #   mutate(split=length(unique(district))>1,
  #          district = as.factor(district)) %>%
  #   ungroup() 
  
  if (level=="county"){
    edges_c <- inputs$edges_vtd %>%
      select(vtd1, vtd2, county1, county2) %>%
      left_join(plan, by=c("vtd1"="vtd")) %>%
      left_join(plan, by=c("vtd2"="vtd")) %>%
      mutate(county1=pmin(county.x,county.y),
             county2=pmax(county.x,county.y)) %>%
      filter((district.x==district.y | !sep_districts),
             district.x %in% which_districts,
             district.y %in% which_districts,
             county1!=county2) %>%
      count(county1,county2)
    
    g <- plot_plan_districts(plan, which_districts, which_counties,
                             color_districts = color_districts)
    g <- plot_edges(g, edges_c,
                    geo$latlong_county,
                    weight_edges = T)
  }
  if (level=="vtd"){
    edges_v <- inputs$edges_vtd %>%
      select(vtd1, vtd2) %>%
      left_join(plan, by=c("vtd1"="vtd")) %>%
      left_join(plan, by=c("vtd2"="vtd")) %>%
      filter((district.x==district.y | !sep_districts),
             district.x %in% which_districts,
             district.y %in% which_districts) %>%
      select(vtd1,vtd2,district=district.x)
    
    if (tree){
      set.seed(12345)
      nodes_v <- filter(inputs$nodes_vtd, 
                        vtd %in% edges_v$vtd1 | 
                          vtd %in% edges_v$vtd2)
      edges_v <- edges_v %>% 
        mutate(weight=1,
               district=ifelse(sep_districts, district, 0)) %>%
        nest(edges=c(-district)) %>%
        mutate(nodes = list(nodes_v),
               graph = map2(nodes, edges, make_graph, level="vtd"),
               tree = map(graph, draw_spanning_tree),
               tree_edges = map(tree, as_data_frame))
      tree_v <- edges_v$tree
      edges_v <- edges_v %>%
        select(tree_edges) %>%
        unnest(tree_edges) %>%
        rename(vtd1=from, vtd2=to)
    }
    
    edges_v <- edges_v %>%
      mutate(vtd1=as.character(vtd1), 
             vtd2=as.character(vtd2))
   
    if ((highlight | update_districts) & !sep_districts){
      pop_bounds <- c(.98, 1.02)*inputs$ideal_pop
      E_child <- V(tree_v[[1]])$name[between(V(tree_v[[1]])$childpop, 
                                             pop_bounds[1], pop_bounds[2])]
      E_parent <- map_chr(E_child, ~neighbors(tree_v[[1]], .x, mode="in")$name)
      
      if (update_districts){
        cut_edge <- c(E_parent[1],E_child[1])
        trees_cut <- delete.edges(tree_v[[1]],
                                  get.edge.ids(tree_v[[1]], cut_edge)) %>%
          decompose()
        plan <- mutate(plan,  
                       district=case_when( 
                         vtd %in% V(trees_cut[[1]])$name~which_districts[1],    
                         vtd %in% V(trees_cut[[2]])$name~which_districts[2],
                         T~as.numeric(district)),
                       district = as.factor(district))
      }
    }
    
    # build plot
    g <- plot_plan_districts(plan, which_districts, which_counties,
                             color_districts = color_districts)
    g <- plot_edges(g, select(edges_v, vtd1, vtd2), select(geo$latlong_vtd, -county),
                  linewidth=.5, nodesize=.5)
    if (tree & highlight){
      g <- plot_edges(g, tibble(id1=E_child,id2=E_parent), 
                      latlong1=select(geo$latlong_vtd, -county),
                      color="gray")
    }
    if (tree & update_districts){
      g <- plot_edges(g, tibble(id1=E_child[1],id2=E_parent[1]), 
                      latlong1=select(geo$latlong_vtd, -county),
                      color="white")
    }
  }
  plot(g)
}

tree_plot <- function(tree){
  plot(tree, layout=layout_as_tree, 
       vertex.label=paste0(V(tree)$name, "\n", 
                           V(tree)$childpop, "\n"),
       vertex.size=0,
       edge.arrow.size=.1)
}

