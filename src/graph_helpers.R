# make graph
district_info <- function(plan, which_districts,
                          precincts = TRUE){
  district <- 
    filter(plan, district %in% which_districts)
  which_counties <- unique(district$county)
  out <- list()
    
  out$nodes_c <- filter(inputs$nodes_county,   
                        county %in% which_counties)
  out$edges_c <- filter(inputs$edges_county, 
                        county1 %in% which_counties, 
                        county2 %in% which_counties)
    
  if (precincts){
    out$nodes_v <- filter(inputs$nodes_vtd, 
                          vtd %in% district$vtd)
    out$edges_v <- filter(inputs$edges_vtd, 
                      vtd1 %in% district$vtd, 
                      vtd2 %in% district$vtd)
  }
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
  V(G)[as.character(nodes[[level]])]$pop <- nodes$pop
  
  return(G)  
}
