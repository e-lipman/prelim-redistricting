library(tidyverse)
library(ggridges)

FLDR <- "res"
iter <- 20000
seeds <- setdiff(1:10, 2)

source("src//violin_plot.R")

res <- map(seeds, 
           ~readRDS(file.path("results",
                              paste0(FLDR,iter), 
                              paste0("seed",.x,"_",iter,".RDS"))))

num_dem <- tibble(seed=seeds,
       num_dem=map(res, ~(.x$num_dem))) %>%
  unnest(num_dem) %>%
  mutate(iter=rep(1:iter, length(seeds))) %>%
  rename(k=num_dem)

pdem_district <- tibble(seed=seeds, 
                        p_dem=map(res, ~as_tibble(t(.x$percent_dem_ord))))  %>%
  unnest(p_dem) %>%
  mutate(iter=rep(1:iter, length(seeds))) %>%
  pivot_longer(-c(seed,iter), names_to="dist_rank", values_to="p_dem",
               names_prefix = "V") %>%
  mutate(dist_rank = as.factor(as.numeric(dist_rank)),
         k=cut(p_dem, breaks=seq(0,1,.002), include.lowest = T))

get_tv <- function(dat, which_iter, s1, 
                   s2=NULL){
  props <- filter(dat, 
         iter<=which_iter, seed%in%c(s1,s2)) %>%
    count(seed, k) %>%
    group_by(seed) %>%
    mutate(p=n/sum(n)) %>% 
    ungroup() 
  if (is.null(s2)){
    props_all <- filter(dat, iter<=which_iter) %>% 
      count(k) %>% 
      mutate(p=n/sum(n)) %>%  
      ungroup()
    props <- bind_rows(props, props_all)
  }
  
  props %>% 
    select(-n) %>%
    pivot_wider(names_from=seed, values_from=p, values_fill=0) %>%
    set_names(c("num_dem","chain1","chain2")) %>%
    summarise(TV=.5*sum(abs(chain1-chain2))) %>% pull(TV)
}

get_tv_avg <- function(dat, which_iter, s1, s2=NULL){
  print(which_iter)
  NUM_DIST = 13
  tv_list <- rep(NA,NUM_DIST)
  for (i in 1:NUM_DIST){
    tv_list[i] <- filter(dat, dist_rank==i) %>% 
      get_tv(which_iter, s1, s2)
  } 
  mean(tv_list)
}

#############################################   
#        Posterior for num democrate        #  
#############################################   

# get max TV for num dem 
min_iter <- 1000
step <- 100
rerun <- F
if (rerun){
  TV_all <- cross_df(list(s1=seeds, 
                which_iter=seq(min_iter, iter, step))) %>%
    mutate(TV=pmap(.,get_tv,dat=num_dem)) %>% 
    unnest(TV) 
  TV_max <- group_by(TV_all, which_iter) %>%
    filter(TV==max(TV))
  saveRDS(TV_max, file.path("results","TV_max_ndem.RDS"))
  TV_pairwise <- cross_df(list(s1=seeds, s2=seeds)) %>%
    filter(s1<s2) %>%
    mutate(TV=pmap(.,get_tv, which_iter=iter, dat=num_dem)) %>% 
    unnest(TV)  
  TV_max_pairwise <- filter(TV_pairwise, TV==max(TV))
  saveRDS(TV_max_pairwise, file.path("results","TV_max_ndem_pair.RDS"))
} else {
  TV_max <- readRDS(file.path("results","TV_max_ndem.RDS"))
  TV_max_pairwise <- readRDS(file.path("results","TV_max_ndem_pair.RDS"))
}

# plots max TV versus iter
best_fit <- lm(log(TV_max$TV)~log(TV_max$which_iter)) %>%
  summary() %>% .[["coefficients"]] %>% .[,1] %>% unlist()
best_fit_func <- function(x){exp(best_fit[1])*x^best_fit[2]}

ghost_lines <- tibble(which_iter=c(1000,1000), 
                      TV=c(1,1),
                      label=c("Max total variation",
                              paste0("Best fit x^",round(best_fit[2],2))))

TV_max %>% 
  ggplot(aes(x=which_iter, y=TV)) +
  # ghost lines for legend
  geom_line(data=ghost_lines, aes(linetype=label)) +
  scale_linetype_manual("", values=c("dashed","solid")) +
  # plot results
  geom_line() +
  stat_function(fun=best_fit_func, linetype="dashed") +
  scale_x_log10("Proposals",limits=c(1000,2000000),
                breaks=10^(3:6)) + 
  scale_y_log10(paste0("Largest total variation on average\n",
                       "distribution of elected democrats"),
                breaks=c(.01,.1,1), limits=c(.01,1)) +
  theme_bw()+
  theme(legend.position = "top")
ggsave(file.path("figures","figures_7b.jpeg"),
       height=3.25)

# barplot of num dem for chains of max TV
chains_max_TV <- TV_max_pairwise[1,1:2] %>% unlist()
barplot_by_chain <- num_dem %>%
  filter(seed %in% chains_max_TV) %>%
  count(seed, k) %>%
  mutate(p=n/iter)
barplot_total <- num_dem %>%
  count(k) %>%
  mutate(p=n/(length(seeds)*iter))

barplot_by_chain %>% 
  ggplot(aes(x=k, y=100*p)) +
  geom_col(aes(fill=as.factor(seed)), show.legend = F,
           position=position_dodge()) +
  geom_col(data=barplot_total, fill=NA, color="black") +
  scale_x_continuous("Elected Democrats", limits=c(0,13),
                     breaks=0:13) +
  scale_y_continuous(paste0("Percentage of samples yielding a\n",
                            "given number of elected Democrats"),
                     limits=c(0,60), breaks=seq(0,60,10)) +
  theme_bw()
ggsave(file.path(file.path("figures","figures_7a.jpeg")),
       height = 2.75)

#############################################   
#        Posterior for prop dem vote        #  
#############################################

# get max TV for num dem 
min_iter <- 1000
step <- 1000
rerun <- F
if (rerun){
  TV_all <- cross_df(list(s1=seeds, 
                          which_iter=seq(min_iter, iter, step))) %>%
    mutate(TV=pmap(.,get_tv_avg,dat=pdem_district)) %>% 
    unnest(TV) 
  TV_max <- group_by(TV_all, which_iter) %>%
    filter(TV==max(TV))
  saveRDS(TV_max, file.path("results","TV_max_pdem.RDS"))
  TV_pairwise <- cross_df(list(s1=seeds, s2=seeds)) %>%
    filter(s1<s2) %>%
    mutate(TV=pmap(.,get_tv_avg, 
                   dat=pdem_district,
                   which_iter=iter)) %>% 
    unnest(TV)  
  TV_max_pairwise <- filter(TV_pairwise, TV==max(TV))  
  saveRDS(TV_max_pairwise, file.path("results","TV_max_pdem_pair.RDS"))
} else {
  TV_max <- readRDS(file.path("results","TV_max_pdem.RDS"))
  TV_max_pairwise <- readRDS(file.path("results","TV_max_pdem_pair.RDS"))
}

# plots max TV versus iter
best_fit <- lm(log(TV_max$TV)~log(TV_max$which_iter)) %>%
  summary() %>% .[["coefficients"]] %>% .[,1] %>% unlist()
best_fit_func <- function(x){exp(best_fit[1])*x^best_fit[2]}
best_fit_func2 <- function(x){30*x^(-.48)} # Autry

ghost_lines <- tibble(which_iter=c(1000,1000,1000), 
                      TV=c(1,1,1),
                      label=c("Max total variation",
                              paste0("Best fit x^",round(best_fit[2],2)),
                              "Best fit from Autry et al")) %>%
  .[c(1,3),]

TV_max %>% 
  ggplot(aes(x=which_iter, y=TV)) +
  # ghost lines for legend
  geom_line(data=ghost_lines, aes(linetype=label)) +
  scale_linetype_manual("", values=c("dashed","solid")) +
  # plot results
  geom_line() +
  #stat_function(fun=best_fit_func, linetype="dashed") +
  stat_function(fun=best_fit_func2, linetype="dashed") +
  scale_x_log10("Proposals",limits=c(1000,2000000),
                breaks=10^(3:6)) + 
  scale_y_log10(paste0("Largest average total variation\n",
                       "on ordered marginal distributions"),
                limits=c(.02,1.1),
                breaks=c(.1,1)) +
  #guides(linetype=guide_legend(nrow=2,byrow=TRUE)) +
  theme_bw()+
  theme(legend.position = "top")

ggsave(file.path("figures","figures_8b.jpeg"), height=3.25)

# Violin plot for percent democrats
labels <- c("Most R","2nd R","3rd R","4th R", "5th R", 
            "6th R", "Middle", "6th D",
            "5th D", "4th D","3rd D", "2nd D","Most D")

pdem_district %>%
  filter(seed %in% c(6,8)) %>%
  ggplot(aes(x=dist_rank, y=100*p_dem, fill=as.factor(seed))) +
  geom_split_violin(show.legend = F,
                    width=3) +
  scale_x_discrete("Districts ordered from least to most Democratic",
                   labels=labels,
                   breaks=1:13) +
  scale_y_continuous("Percent of the vote going to the Democrat") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, vjust=.3, hjust=1))

ggsave(file.path("figures","figures_8a.jpeg"), height=2.25)

# Example plans
source("src//plotting_maps.R")
res1 <- res[[1]]$plans
inputs <- readRDS(file.path("inputData","model_inputs_NC.RDS")) 
plan1 <- inputs$seed_plans[[1]]
table(res1[,16], res1[,17])

# acccept iter: 5, 12, 16, 17
iter <- c(1,12,17,10000)
for (i in 1:length(iter)){
  plani <- mutate(inputs$nodes_vtd, district=res1[,iter[i]])  
  plot_plan_districts(plani)
  ggsave(file.path(file.path("figures",
                             paste0("fig6",letters[i],".jpeg"))),
         height=2)
}
