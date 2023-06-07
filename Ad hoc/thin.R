library(tidyverse)
library(ggridges)

FLDR <- "res"
iter <- 200000
seeds <- 1:10

THIN <- 20

thin_one <- function(seed, thin){
  print(seed)
  path_orig <- file.path("results",
                         paste0(FLDR,format(iter, scientific = F)), 
                         paste0("seed",seed,"_",iter,".RDS"))
  path_thin <- file.path("results",
                         paste0(FLDR,format(iter, scientific = F),
                                "_thin",thin), 
                         paste0("seed",seed,"_",iter,".RDS"))
  dir.create(file.path(path_thin,".."), showWarnings = F)
  
  res <- readRDS(path_orig)
  keep_iter <- seq(0,iter,thin)
  res$plans <- res$plans[,keep_iter]
  res$num_dem <- res$num_dem[keep_iter]
  res$percent_dem_ord <- res$percent_dem_ord[,keep_iter]
  
  saveRDS(res, path_thin)
}

walk(seeds, thin_one, thin=THIN)
