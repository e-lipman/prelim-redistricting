library(tidyverse)
library(yaml)

input_path <- file.path("inputData","NC")
configs <- read_yaml("configs.yml")

######################################
#           make input data          #
######################################

# precinct info
xwalk <- read_delim(file.path(input_path,"NC_Counties.txt"),
                    col_names = c("vtd","county")) %>%
  mutate(county_name=county, 
         county=as.character(as.numeric(as.factor(county)))) %>%
  arrange(vtd)

fips <- read_delim(file.path(input_path,"NC_FIPS.txt"),  
                   col_names = c("fips","county"),  
                   delim="        ") %>%
  mutate(county = gsub(" county","",tolower(county)),
         fips=as.character(fips))

pop <- read_delim(file.path(input_path,"NC_Population.txt"),
                  col_names = c("vtd","pop")) %>%
  arrange(vtd)

votes <- tibble(label=names(configs$voting_data),
                votes=map(configs$voting_data,
                          ~(read_delim(file.path(input_path, .x),
                                       col_names = 1:5) %>%
                              select_at(c(1,3,4)) %>%
                              set_names(c("vtd","D","R"))))) %>%
  unnest(c(votes)) %>%
  pivot_wider(names_from=label, values_from=c("D","R")) %>%
  arrange(vtd)

## merge together
num_vtd = max(xwalk$vtd)
stopifnot(xwalk$vtd==1:num_vtd,
          pop$vtd==1:num_vtd,
          votes$vtd==1:num_vtd)

stopifnot(nrow(fips)==length(unique(xwalk$county_name)))
stopifnot(sort(unique(xwalk$county_name))==sort(fips$county))

vtd_nodes <- 
  left_join(xwalk, fips, by=c("county_name"="county")) %>%
  left_join(pop, by="vtd") %>%
  left_join(votes, by="vtd")

stopifnot(vtd_nodes$vtd==1:num_vtd)

# precinct-level edges
vtd_edges <- read_delim(file.path("inputData","NC","NC_Edges_Lengths.txt"),
                    col_names = c("vtd1","vtd2","length")) %>%
  mutate(min=pmin(vtd1,vtd2),
         max=pmax(vtd1,vtd2),
         vtd1=min, vtd2=max) %>%     # reorder so vt1<vtd2
  select(vtd1,vtd2) %>%
  filter(vtd1!=-1, vtd2!=-1) %>%     # remove edges to outside
  distinct() %>%
  left_join(select(vtd_nodes, vtd, county), by=c("vtd1"="vtd")) %>%
  left_join(select(vtd_nodes, vtd, county), by=c("vtd2"="vtd")) %>%
  rename(county1=county.x, county2=county.y)

# seed plans
plans <- map(configs$plan_list,
             ~(read_delim(file.path(input_path, .x),
                          col_names = c("vtd","district")) %>%
                 arrange(vtd) %>% pull(district)))
stopifnot(map_dbl(plans, length)==num_vtd)
stopifnot(map_dbl(plans, max)==configs$num_districts)

# other quantities
ideal_pop <- sum(vtd_nodes$pop)/configs$num_districts

# write out inputs
inputs <- list(nodes_vtd=vtd_nodes,
               edges_vtd=vtd_edges,
               seed_plans=plans,
               ideal_pop=ideal_pop)

saveRDS(inputs, file.path("inputData","model_inputs_NC.RDS"))

######################################
#          make geo data             #
######################################
library(sf)
library(tigris)
library(housingData)

# load county and precinct shapefiles
sf_vtd <- read_sf(file.path("inputData","NC","NCshapefile")) %>%
  mutate(fips=paste0(STATEFP10, COUNTYFP10)) %>%
  rename(vtd=fidp1)

sf_county <- tigris::counties(state="NC", year=2010) %>%
  mutate(fips=paste0(STATEFP10, COUNTYFP10)) 

# load lat-long data
latlong_vtd <- read_delim(file.path("inputData","NC","NC_latlong.txt"),
                      col_names = c("vtd", "lon","lat")) %>%
  left_join(xwalk, by="vtd") %>%
  mutate(vtd=as.character(vtd),
         county=as.character(county)) %>%
  select(county, vtd, lat, lon)

latlong_county <- geoCounty %>% 
  filter(state=="NC") %>%
  mutate(fips=as.character(fips)) %>%
  select(fips,lat,lon) %>%
  left_join(distinct(select(xwalk, fips, county)), by="fips") %>%
  mutate(county=as.character(county)) %>%
  select(county, lat, lon)

geo <- list(sf_county=sf_county,
            sf_vtd=sf_vtd,
            latlong_county=latlong_county,
            latlong_vtd=latlong_vtd)
saveRDS(geo, file.path("inputData", "geo_data.RDS"))
