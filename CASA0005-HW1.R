NZ<-read.csv("NEW ZEALAND.csv")
library(sf)
install.packages("tmap")
library(tmap)
shape <- st_read("territorial-authority-2018-generalised.shp")
Hideshape <- shape%>%
  merge(.,
        NZ,
        by.x="TA2018_V1_", 
        by.y="CODE")
tmap_mode("plot")
# change the fill to your column name if different
qtm(Hideshape, fill = "Paid", 
    fill.palette = c("#f7fcf0", "#ccebc5", "#7bccc4", "#2b8cbe", "#084081"))

tm_shape(Hideshape) +
  tm_polygons("Paid",
              palette = c("#f03b20","#bd0026","#ccebc5", "#7bccc4","#41b6c4", "#2c7fb8", "#084081"),
              style = "fixed",
              breaks = c(0, 5000, 10000, 50000, 100000, 200000, 400000, 500000),
              title = "Paid")

Hideshape <- Hideshape[Hideshape$Territorial.authority.description != 
                       "Area Outside Territorial Authority", ]
library(tmaptools)
install.packages("terra")
library(terra)

