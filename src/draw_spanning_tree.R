library(tidyverse)
library(igraph)

sample1 <- function(options, weights=NULL){
  if (length(options)==1){options}
  else if (is.null(weights)){sample(options,1)}
  else {sample(options,1,prob = weights/sum(weights))}
}

add_node_childpop <- function(tree){
  order <- dfs(tree, tree$root)$order$name[length(V(tree)):1]
  for (node in order){
    children <- neighbors(tree, node, mode="out")$name
    childpop <- sum(V(tree)[children]$childpop)
    V(tree)[node]$childpop <- childpop + V(tree)[node]$pop
  }
  return(tree)
}

loop_erased_walk <- function(G,end,start=NULL){
  if (is.null(start)){
    start=sample1(setdiff(V(G)$name,end))
  }
  if (is.null(end)){
    end=sample1(setdiff(V(G)$name,start))
  }
  curr <- start
  walk <- start
  
  while (!curr %in% end){
    # choose next step
    E_V <- incident(G,curr)
    new_edge <- sample1(E_V, E_V$weight)
    curr <- setdiff(ends(G,new_edge), curr)
    
    # add to walk
    if (!curr %in% walk){
      walk <- c(walk, curr)  
    } else {
      walk <- walk[1:which(walk==curr)]
    }
  }
  return(walk)
}

draw_spanning_tree <- function(G, directed=T, annotate=T){
  stopifnot(is_connected(G))
  
  ST <- make_empty_graph(length(V(G)), directed=directed)
  V(ST)$name <- V(G)$name
  V(ST)$pop <- V(G)$pop
  
  used <- sample1(V(G)$name)
  ST$root <- used[1]
  
  while (length(used)<length(V(G))){
    walk <- loop_erased_walk(G,end=used)  
    used <- unique(c(used, walk))
    for (i in 1:(length(walk)-1)){
      ST <- add_edges(ST, walk[(i+1):i]) # orient edges correctly
    }
  }
  if (annotate){
    ST <- add_node_childpop(ST)
  }
  
  stopifnot(V(ST)$name==V(G)$name)
  stopifnot(sum(V(ST)$pop)==V(ST)[ST$root]$childpop)
  
  return(ST)
}

