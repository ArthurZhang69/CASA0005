library(sf)
library(here)
library(raster)
install.packages("terra")
library(terra)
install.packages("fs")
library(fs)
library(tidyverse)
library(stringr)
library(tibble)
library(ggplot2)

st_layers(here("CASA","gadm41_CHN.gpkg"))
CNoutline <- st_read(here("CASA", "gadm41_CHN.gpkg"), 
                     layer='ADM_ADM_0')    
print(CNoutline)
st_crs(CNoutline)$proj4string

CNoutline<-CNoutline %>%
  st_set_crs(.,4326)

CNSP<-CNoutline %>% as(.,"Spatial")
CNSF<-CNSP %>% st_as_sf()

SSP<-terra::rast(here("CASA","wc2.1_2.5m_tmax_EC-Earth3-Veg_ssp245_2081-2100.tif"))
SSP
plot(SSP)

Projection1<-terra::project(SSP,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
plot(Projection1, col = terrain.colors(20), colNA = "white")

here::here("CASA")
dir_info("C:/Users/Arthur/Documents/R data science/CASA")

listfiles<-dir_info("C:/Users/Arthur/Documents/R data science/CASA") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()
listfiles

SSPtemp <- listfiles %>%
  terra::rast()
SSPtemp[[1]]



month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
names(SSPtemp)<-month
SSPtemp
plot(SSPtemp)
City<-read.csv("World_Cities.csv")
CNCity<-City %>% filter(str_detect(CNTRY_NAME,"China"))

points_sf <- st_as_sf(CNCity, coords = c("X", "Y"), crs = 32650)
points_lonlat <- st_transform(points_sf, 4326)
print(points_lonlat$geometry)

CHINASTR<-c("Beijing","Shanghai","Guangzhou","Chengdu")
lon<-c(-165.0153,-165.7401,-168.8843,-170.2158)
lat<-c(10.86278,7.800193,6.67001,10.39078)
CHINASF<-points_sf %>% filter(str_detect(CITY_NAME,"Beijing|Shanghai|Guangzhou|Chengdu"))
CHINASF_CRS<-st_transform(CHINASF,4326)
pts <- vect(CHINASF_CRS)
CNsamples <- data.frame(CHINASTR, lon, lat, row.names="CHINASTR")
class(CNsamples)
CNcitytemp<- terra::extract(SSPtemp, pts,, method = "bilinear", ID = FALSE)
st_bbox(st_transform(CHINASF, 4326))


out <- cbind(st_drop_geometry(CHINASF_CRS), CNcitytemp)
head(out)

crs(SSPtemp)
st_crs(CHINASF)

ext(SSPtemp)
st_bbox(CHINASF)

plot(CNoutline$geom)
CNSIMPLE <- CNoutline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()
print(CNoutline)
crs(SSPtemp)

Chinatemp <- CNoutline %>%
  # now crop our temp data to the extent
  terra::crop(SSPtemp,.)

# plot the output
plot(Chinatemp)

exactCN<-terra::mask(Chinatemp, CNoutline)
plot(exactCN)

hist(exactCN[[3]], col="red", main ="March temperature")


exactCNdf <- exactCN %>%
  as.data.frame()

gghist <- ggplot(exactCNdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Chinese March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))

squishdata<-exactCNdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )

twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")

meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

meantwomonths

ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Chinese Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Chinese temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))


library(plotly)
# split the data for plotly based on month

jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")

jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")

# give axis titles
x <- list (title = "Temperature")
y <- list (title = "Frequency")

# set the bin width
xbinsno<-list(start=0, end=40, size = 2.5)

# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist


meanofall <- squishdata %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE))

# print the top 1
head(meanofall, n=1)


# standard deviation per month
sdofall <- squishdata %>%
  group_by(Month) %>%
  summarize(sd = sd(Temp, na.rm=TRUE))

# maximum per month
maxofall <- squishdata %>%
  group_by(Month) %>%
  summarize(max = max(Temp, na.rm=TRUE))

# minimum per month
minofall <- squishdata %>%
  group_by(Month) %>%
  summarize(min = min(Temp, na.rm=TRUE))

# Interquartlie range per month
IQRofall <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE))

# perhaps you want to store multiple outputs in one list..
lotsofstats <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE), 
            max=max(Temp, na.rm=T))

# or you want to know the mean (or some other stat) 
#for the whole year as opposed to each month...

meanwholeyear=squishdata %>%
  summarize(meanyear = mean(Temp, na.rm=TRUE))

CHINASF_ll <- st_transform(CHINASF_3857, 4326)
st_bbox(CHINASF_ll)

pts <- vect(CHINASF_ll)
vals <- terra::extract(SSPtemp, pts, method = "bilinear", ID = FALSE)
View(vals)
