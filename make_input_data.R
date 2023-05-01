library(tidyverse)
library(igraph)

input_path <- file.path("inputData","NC")

######################################
#      Read various data sources     #
######################################

# county precinct crosswalk
xwalk <- read_delim(file.path(input_path,"NC_Counties.txt"),
                       col_names = c("vtd","county")) %>%
  mutate(county_name=county, 
         county=as.numeric(as.factor(county)))

fips_xwalk <- read_delim(file.path(input_path,"NC_FIPS.txt"), 
                       col_names = c("fips","county"), 
                       delim="        ") %>%
  mutate(county = gsub(" county","",tolower(county)))
stopifnot(nrow(fips_xwalk)==length(unique(xwalk$county_name)))
stopifnot(sort(unique(xwalk$county_name))==sort(fips_xwalk$county))
xwalk <- left_join(xwalk, fips_xwalk, by=c("county_name"="county")) %>%
  mutate(fips=as.character(fips))

# population
pop <- read_delim(file.path(input_path,"NC_Population.txt"),
                       col_names = c("vtd","pop")) 

# edges
edges <- read_delim(file.path("inputData","NC","NC_Edges_Lengths.txt"),
                    col_names = c("vtd1","vtd2","length")) %>%
  mutate(min=pmin(vtd1,vtd2),
         max=pmax(vtd1,vtd2),
         vtd1=min, vtd2=max) %>%     # reorder so vt1<vtd2
  select(vtd1,vtd2) %>%
  filter(vtd1!=-1, vtd2!=-1) %>%     # remove edges to outside
  distinct()

# check (IDs)
stopifnot(length(unique(xwalk$county))==max(xwalk$county))
stopifnot(length(unique(xwalk$vtd))==max(xwalk$vtd))
stopifnot(length(unique(pop$vtd))==max(pop$vtd))
stopifnot(length(unique(c(edges$vtd1, edges$vtd2)))==max(edges$vtd2))

######################################
#      nodes for each level          #
######################################

nodes_vtd <- full_join(pop, xwalk, by="vtd")
stopifnot(nrow(xwalk)==nrow(pop), nrow(pop)==nrow(nodes_vtd))

nodes_county <- group_by(nodes_vtd,county) %>%
  summarise(pop=sum(pop)) %>%
  ungroup()
stopifnot(nrow(nodes_county)==max(nodes_county$county))
  
#######################################
#      edges for each level          #
######################################

edges_vtd <- mutate(edges, weight=1) %>%
  left_join(xwalk, by=c("vtd1"="vtd")) %>%
  left_join(xwalk, by=c("vtd2"="vtd")) %>%
  rename(county1=county.x, county2=county.y)

edges_county <- edges_vtd %>%
  filter(county1!=county2) %>%      # remove edges within counties
  mutate(min=pmin(county1,county2),
         max=pmax(county1,county2),
         county1=min, county2=max) %>%
  group_by(county1, county2) %>%
  summarise(weight=sum(weight)) %>%
  ungroup()

######################################
#        write out inputs            #
######################################
inputs <- list(xwalk=xwalk,
               nodes_vtd=nodes_vtd,
               nodes_county=nodes_county,
               edges_vtd=edges_vtd,
               edges_county=edges_county)

saveRDS(inputs, file.path("inputData","model_inputs_NC.RDS"))


######################################
#          make geo data             #
######################################
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
