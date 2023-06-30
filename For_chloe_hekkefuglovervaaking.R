#### For Chlo?? ####
#############################################################
#### Start: 1. BIRD DATA PREPARATION     ###################
#############################################################

## Packages for handling database
suppressMessages(require('RPostgres'))
suppressMessages(require('odbc'))
suppressMessages(require(RPostgreSQL))
suppressMessages(require(DBI))
suppressMessages(require(rpostgis))
suppressMessages(require(lubridate))
suppressMessages(require(xtable))
require(rmarkdown)
require(sf)
require(tidyverse)
## End loading required packages


## Connecting to the DB 
sort(unique(odbcListDrivers()[[1]]))

con <- DBI::dbConnect(odbc(),
                      Driver   = "SQL server", 
                      Server   = "ninsql07.nina.no",
                      Database = "TOVTaksering",
                      Trusted_Connection = "True") # need access granted from Roald, I guess
## end connecting to DB


### Getting the data from DB
## 1. Retrieving information about the characteristics of the sampling points
Sampling_point_info <- tbl(con, 'TakseringspunktInfo') %>%
  rename(PointID = PktID,
         RouteID = RuteID) %>%
  as_tibble() %>%
  tidyr::unite('Route_point_id', c('RouteID', 'PointID'), remove = F)



## 2. Getting bird data (point counts)
Bird_data_points <- tbl(con, 'Punkttaksering_Verifisert') %>%
  as_tibble()

# all years?
range(Bird_data_points$YEAR)
# yes! 2006 -> current


## Create a unique ID for the point-route combination (might be useful for some analysis)
## Each route has 12-20 census points 
Bird_data_points2 <- Bird_data_points %>%
  unite(Route_point_id, c('RouteID', 'PointID'), remove = F)

# How many points in total
Bird_data_points2 %>% distinct(Route_point_id)
# 9151 points in Norway.

## Add the site-level information (habitat characteristics, etc...)
Bird_data_points3 <- left_join(Bird_data_points2, Sampling_point_info,
                               by = 'Route_point_id')



## 3. Getting information from Art table (from the DB) and add it to our point data ####
SppID <- tbl(con, 'Art') %>%
  mutate(Species = as.numeric(ArtsID)) %>%
  select(Species, EURINGCode, Artsnavn_Lat, FK_Kode_Flokk) %>%
  as_tibble() %>%
  filter(Species > 0) %>%
  arrange(Species) 
# 342 species (all species ever recorded in Norway)



## How many species do we have in our points?
Bird_data_points3 %>% select(ScientificName) %>% distinct()
# 222 species


## Adding the EURING code from the Art table to the Bird_data
Bird_data_EURING <- Bird_data_points3 %>% left_join(SppID)


## removing the annoying blank space at the end of the names
spp_names <- Bird_data_EURING %>% 
  select(Artsnavn_Lat) %>% 
  distinct() %>% 
  mutate(Spp_name = str_trim(Artsnavn_Lat))


## change it in the bird data object and clean 
Bird_data <- Bird_data_EURING %>% 
  left_join(spp_names) %>%
  select(Route_point_id, YEAR, Spp_name, EURINGCode, `Obs<50m`, `Obs>50m`,
         ObsFlokk, RouteID.x, PointID.x,  Latitude, Longitude, Hoh, Hoh_DEM) %>%
  rename(RouteID = RouteID.x,
         PointID = PointID.x,
         Year = YEAR,
         Species = Spp_name)


#### Dealing with the flocks (flock to pairs) ####
## When birds are observed in flocks, a conversion factor has to be applied:
## Kode_Flokk = 1, dividing by 5 to get num of pairs.
## Kode_Flokk = 2, dividing by 2
## Kode_flokk = 3, nothing is done (gulls, terns, geese, ducks)
FlockInfo <- SppID %>%
  select(EURINGCode, FK_Kode_Flokk)

Bird_data <- Bird_data %>%
  left_join(FlockInfo, by = 'EURINGCode')

Bird_data2 <- Bird_data %>%
  mutate(Flo_to_pairs = case_when(as.numeric(FK_Kode_Flokk) == 1 ~ ObsFlokk/5,
                                  as.numeric(FK_Kode_Flokk) == 2 ~ ObsFlokk/2,
                                  TRUE ~ as.numeric(ObsFlokk)),
         TotalCount = `Obs<50m` + `Obs>50m` + Flo_to_pairs,
         .before = 'RouteID')

Bird_data2


# records per species?
Bird_data2 %>% 
  select(Species, EURINGCode, TotalCount) %>%
  group_by(Species) %>%
  summarise(Freq = n()) %>%
  arrange(desc(Freq)) %>%
  print(n = 50)


## 4. Getting bird data (line transects)
Bird_data_lines <- tbl(con, 'Linjetaksering_Normalisert') %>%
  as_tibble() %>%
  dplyr::rename(YEAR = Aar1)
# The line transect counts are a complementary survey to the main point counts. 
# Only a subset of species are recorded during line transects (as opposed to 
# recording all species in point counts).
# Line transect data are hence not much used, but can be added to the dataset with
# the point counts if needed (via RouteID)

## all years?
range(Bird_data_lines$YEAR, na.rm = T)
# yes! 2005 -> current

################################################################################
#### WITH THE ABOVE CODE YOU GET THE RAW DATA FROM THE DATABASE ################
################################################################################


################################################################################
#### ONE CAN ALSO DO EXTRA WORK ON THAT RAW DATA
#### - ADD THE ZEROES
#### - SUMMARISE BY ROUTE
#### - ...
################################################################################
### not clean yet as now it is too specific for the purpose I had. 

















## 5, Complete the datatable with the missing data
## not all routes are surveyed every year, but in the data base thee is only data
## for the routes surveyed and not the zeroes.
## First, add the missing year-route-species combinations with NAs
complete_site_year_species <- Bird_data2 %>%
  #group_by(RouteID, Species, Year) %>%
  #summarise(Route_count = sum(TotalCount)) %>%
  #mutate(RY = paste0(RouteID, '_', Year)) %>%
  #ungroup() %>%
  arrange(RouteID, Year, Species)%>%
  complete(RouteID, Species, Year = 2006:max(unique(Bird_data2$Year))) 


## A missing species in the dataset may be because (i) the route was not surveyed
## or (ii) the route was surveyed and the species not detected.
## I deal with this in this second step:

## get the route-year (ry) unique IDs
rys<- Bird_data2 %>% select(RouteID, Year) %>%
  mutate(RY = paste0(RouteID, '_', Year)) %>%
  select(RY) %>%
  distinct()

rys %>% arrange() %>% print(n = 2000)


complete_site_year_species2 <- complete_site_year_species %>%
  mutate(RY = paste0(RouteID, '_', Year)) %>%
  mutate(Surveyed = case_when(RY %in% rys$RY ~ 'Yes',
                              TRUE ~ 'No')) %>%
  mutate(Route_count2 = case_when(!is.na(Route_count) ~ Route_count,
                                  is.na(Route_count) & Surveyed == 'Yes' ~ 0,
                                  is.na(Route_count) & Surveyed == 'No' ~ NA_real_)) %>%
  mutate(Occurrence = case_when(Route_count2 == 0 ~ 0,
                                Route_count2 > 0 ~ 1,
                                is.na(Route_count2) ~ NA_real_)) %>%
  left_join(covs, by = 'RouteID') %>%
  left_join(covs_centroids, by = 'RouteID')


complete_site_year_species2 %>% print(n = 50)

# num of rows for complete dataset
17*490*222 # years * routes * species
#[1] 1849260


range(complete_site_year_species$Year)


sum(complete_site_year_species$Route_count, na.rm = T)

complete_site_year_species2 %>% filter(Year == 2010, RouteID == 1101)





#### Making it spatial ####
Bird_data_sf <- complete_site_year_species2 %>%
  filter(!is.na(Lon_avg) | !is.na(Lat_avg)) %>%
  st_as_sf(coords = c('Lon_avg', 'Lat_avg'),
           crs = 4326,
           remove = FALSE) %>%
  st_transform(25833, remove = F) 



Bird_data_sf %>% filter(Year == 2010, RouteID == 1101)

