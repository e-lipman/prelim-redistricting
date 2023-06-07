library(tidyverse)
library(usmap)
library(sf)

FLDR <- "res"
iter <- 200000
seeds <- 1:10
thin <- 20

res <- map(seeds, 
           ~readRDS(file.path("results",
                              paste0(FLDR,format(iter, scientific = F),
                                     "_thin",thin), 
                              paste0("seed",.x,"_",iter,".RDS"))))
inputs <- readRDS(file.path("inputData","model_inputs_NC.RDS")) 

plans <- map(res, ~(.x$plans)) %>% reduce(cbind)
plans_thin <- plans[,seq(0, ncol(plans),200)]
counties <- inputs$nodes_vtd$county

# western-most county: 20
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

get_cluster_probs <- function(plans, c1, c2){
  print(paste(c1,c2))
  plans1 <- plans[counties==c1,]
  plans2 <- plans[counties==c2,]
  
  dist1 <- apply(plans1, 2, getmode)
  dist2 <- apply(plans2, 2, getmode)
  
  mean(dist1==dist2)
}

if (F){
  clust <- cross_df(list(c1=1:100, c2=1:100)) %>%
    filter(c1<c2) %>%
    mutate(p=map2(c1,c2, get_cluster_probs, plans=plans_thin)) %>%
    unnest(p)
  saveRDS(clust, file.path("results","cluster_probs.RDS"))
} else {
  clust <- readRDS(file.path("results","cluster_probs.RDS"))
}

# cluster plot
clust_full <- rbind(clust, 
                    set_names(clust, c("c2","c1","p")),
                    tibble(c1=1:100, c2=1:100, p=1))

corr_mat <- clust_full %>%
  arrange(c1) %>%
  pivot_wider(names_from=c2, values_from=p) %>% 
  select(-c1) %>%
  select_at(as.character(1:100)) %>%
  as.matrix() 
stopifnot(diag(corr_mat)==1)

corr_mat_ord <- lessR::corReorder(corr_mat, heat_map = F)

clust_ord <- as_tibble(corr_mat_ord) %>%
  set_names(as.character(1:nrow(corr_mat_ord))) %>%
  mutate(c1=1:nrow(corr_mat)) %>%
  pivot_longer(-"c1", names_to = "c2", values_to="p") %>%
  mutate(c2=as.numeric(c2))

ggplot(clust_ord, aes(x=c1,y=c2,fill=p)) +
  geom_tile() +
  geom_segment(x=.5,y=.5,xend=.5,yend=100.5) +
  geom_segment(x=.5,y=.5,yend=.5,xend=100.5) +
  geom_segment(x=100.5,y=100.5,xend=.5,yend=100.5) +
  geom_segment(x=100.5,y=100.5,yend=.5,xend=100.5) +
  scale_fill_gradient(low="white", high="black") +
  theme_void() +
  theme(legend.position = "none")
ggsave(file.path("figures","clust_all.jpeg"), 
       height=4,width=4)

# map for specific county
geo <- readRDS(file.path("inputData","geo_data.RDS"))

plot_clust_count <- function(i){
  clusti <- filter(clust_full, c1==i) %>% arrange(c2) %>%
    mutate(p_disc=cut(p, breaks=seq(0,1,.25), include.lowest = T))
  
  plot_dat <- distinct(select(inputs$nodes_vtd, county, fips)) %>%
    mutate(county=as.numeric(county)) %>%
    arrange(county) %>%
    mutate(p=clusti$p)
  plot_dat_geo <- full_join(geo$sf_county, plot_dat, by="fips")
  
  plot_dat_geo %>%
    ggplot() +
    geom_sf(aes(fill=p)) +
    geom_sf(data=filter(plot_dat_geo, county==i), fill="red") +
    scale_fill_gradient(low="white", high="black") +
    #scale_fill_manual(values=c("white","gray50","gray25","black")) +
    theme_void() 
}

dir.create(file.path("figures","clust_maps"), showWarnings = F)
which_counties <- c(20, 49, 92, 28) 
for (i in which_counties){
  print(plot_clust_count(i) + theme(legend.position="none"))
  ggsave(file.path("figures","clust_maps",
                   paste0("clust",str_pad(i, 2, pad="0"),".jpeg")),
         height=1.25, width=3)
}

select(inputs$nodes_vtd, county, county_name) %>%
  distinct() %>% arrange(county) %>% 
  filter(county %in% which_counties)
    

