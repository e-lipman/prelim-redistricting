# get nodes and edges for graph based on districts
options(dplyr.summarise.inform = FALSE)

make_nodes_county <- function(nodes_vtd){
  nodes_county <- group_by(nodes_vtd,county) %>%
    summarise(pop=sum(pop)) %>%
    ungroup()
}

make_edges_county <- function(edges_vtd){
  edges_county <- edges_vtd %>%
    filter(county1!=county2) %>%      # remove edges within counties
    mutate(min=pmin(county1,county2),
           max=pmax(county1,county2),
           county1=min, county2=max) %>%
    group_by(county1, county2) %>%
    summarise(weight=sum(weight), .groups = ) %>%
    ungroup()  
}

district_info <- function(plan, which_districts){
  district <- 
    filter(plan, district %in% which_districts)
  out <- list()
    
  out$nodes_v <- filter(inputs$nodes_vtd, 
                        vtd %in% district$vtd)
  out$edges_v <- filter(inputs$edges_vtd, 
                    vtd1 %in% district$vtd, 
                    vtd2 %in% district$vtd)
  out$nodes_c <- make_nodes_county(out$nodes_v)
  out$edges_c <- make_edges_county(out$edges_v)
  
  return(out)
}

make_graph <- function(nodes, edges, level="county",
                       which_county=NULL){
  
  if (level!="county"){
    if (!is.null(which_county)){
      nodes <- filter(nodes, county==which_county)
      edges <- filter(edges, 
                      county1==which_county,
                      county2==which_county)
    }
  }
  
  G <- graph_from_data_frame(edges, directed=F)
  if (length(V(G))>0){
    V(G)[as.character(nodes[[level]])]$pop <- nodes$pop
  }
  
  return(G)  
}
