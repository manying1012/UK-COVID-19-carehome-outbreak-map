# dependencies 

if (!require("dplyr"))
  install.packages("dplyr")
if (!require("odbc"))
  install.packages("odbc")
if (!require("DBI"))
  install.packages("DBI")
install.packages("pivottabler")

library(dplyr)
library(DBI)
library(pivottabler)
library("tidyverse")
#install.packages("leaflet")
library(leaflet)
#install.packages("rgdal")

install.packages("sf")


library(geojsonio)
spdf <- geojson_read("C:/Users/ManyingLoASC/Downloads/Local_Authority_Districts_(December_2021)_GB_BFC/Local_Authority_Districts_(December_2021)_GB_BFC.geojson",  what = "sp")
class(spdf)
names(spdf)


library(sf)
library(leaflet)
library(ggplot)
library(tidyverse)
LAD_boundaries <- read_sf(
  "C:/Users/ManyingLoASC/Downloads/Local_Authority_Districts_(December_2021)_GB_BFC/Local_Authority_Districts_(December_2021)_GB_BFC.geojson"
) 

LAD_boundaries2 <- LAD_boundaries %>%
  filter(stringr::str_detect(LAD21CD, "^E"))

conA <- dbConnect(
  odbc::odbc(),
  driver = "{ODBC Driver 17 for SQL Server};Authentication=ActiveDirectoryInteractive;",
  server = "edge-prd-mart-sql.database.windows.net",
  database = "adultsocialcare",
  UID = "Manying.lo@test-and-trace.nhs.uk"
)

positive_carehome_tests <- DBI::dbGetQuery(
  conn = conA,
  statement = "SELECT 
  test_reported_date,
  test_result,
  utla_id,
  is_care_home,
  care_home_role,
  age
  FROM palantir.all_pillars_snapshot
  WHERE test_reported_date BETWEEN '2021-01-01' AND '2021-03-01'
  AND is_care_home = 1
  AND test_result = 'positive'
  AND care_home_role = 'resident'"
)

# rearrange column names
df1 <- positive_carehome_tests %>% 
  as_tibble() %>%
  select(test_reported_date, utla_id, is_care_home, care_home_role, age_of_patient ="age", test_result) 

library(lubridate)
df2 <- df1 %>% 
  mutate(month = lubridate::month(ymd(test_reported_date))) %>%
  mutate(week = lubridate::week(ymd(test_reported_date))) %>%
  # drop_na(utla_id) %>%
  filter(stringr::str_detect(utla_id, "^E"))

# sum by positive tests
df3 <- df2 %>%
  mutate(test_result_converted = if_else(test_result == "positive", 1, 0, missing = NULL)) %>%
  group_by(week, utla_id) %>%
  summarise(total_positive_test = sum(test_result_converted))

#create full dataframe
list1 <- LAD_boundaries2 %>%
  pull("LAD21CD")

list2 <- df3 %>%
  pull("week") %>%
  unique()

fullcombination <- expand.grid(x = list1, y = list2) %>%
  rename(LAD21CD = "x", week = "y")

df4 <- left_join(fullcombination, df3, by = c("LAD21CD" = "utla_id", 
                                              "week" = "week"))

#generate leaflet map in shiny
lads_eng <- subset(
  x = spdf,  # our data
  subset = grepl(  # subset the data where the following pattern is matched
    x = spdf@data$LAD21CD,  # in this variable in this slot of this SPDF
    pattern = "^E"  # subset anything starting with 'E'
  )
)

library(shiny) 

subset_test <- filter(df4, week == 1 | week == 2)
