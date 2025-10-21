library(dplyr)
library(tidyverse)
library(OpenStreetMap)
library(tmap)
library(tmaptools)
library(sf)

SchoolData<-read.csv("Cleaned_HW2.csv")
Datatype<-SchoolData %>% summarise_all(class) %>% pivot_longer(everything(), 
                                                               names_to="All_variables", 
                                                               values_to="Variable_class")
SchoolData<-edit(SchoolData)

county_summary <- SchoolData %>%
  group_by(County) %>%
  summarise(
    total_students = sum(Count, na.rm = TRUE),
    total_met = sum(Met, na.rm = TRUE),
    overall_rate = total_met / total_students * 100
  )


Washington <- st_read("Washington_Counties_with_Natural_Shoreline___washsh_area.shp")
qtm(Washington)
Washington_merged<-Washington %>%
  left_join(county_summary, by = c("COUNTYLABE" = "County"))

tmap_mode("plot")                             
qtm(Washington_merged, fill= "overall_rate")                             

tmapwashington <- Washington_merged %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "esri", zoom = NULL)

tmap_mode("plot")

tm_shape(tmapwashington) + tm_rgb() +tm_shape(Washington_merged) + tm_polygons(col = "overall_rate", palette="Blues", style="jenks",alpha=0.5, title= "Passed %")+tm_compass(type= "arrow",position = c("left","bottom"))+tm_layout(title = "Distribution of Washington County's Pass rate",title.size=2,title.position=c("center","top"),legend.position = c("right","bottom"))


Washington84 <- Washington_merged %>%
  st_transform(.,4326)

tmap_mode("view")

tm_shape(Washington84) + 
  # add polygon layer
  tm_polygons(col = "overall_rate", palette="Blues", style="jenks",alpha=0.8, title= "Passed %")+
  tm_basemap(server = "OpenStreetMap") +
  tm_compass(type = "arrow", position = c("left", "bottom")) + 
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout(title = "Distribution of Washington County's Pass rate",title.size=2,title.position=c("center","top"))+tm_view(view.legend.position = c("right","bottom"))
