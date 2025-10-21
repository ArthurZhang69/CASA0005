library(readr)
library(dplyr)
library(stringr)
library(sf)
install.packages("countrycode")
library(countrycode)
library(tmap)
library(tmaptools)


GII <- read_csv("HDR25_Statistical_Annex_GII_Table.csv")
colnames(GII)
GII_Cleaned<-GII %>% select(...1,'Table 5. Gender Inequality Index',...3)
GII_Cleaned<-GII_Cleaned %>% filter(!is.na(...1))
GII_Cleaned<-GII_Cleaned %>% filter(!is.na(...3))

GII_Cleaned<-GII_Cleaned %>% mutate( GII_Cleaned=as.numeric(...3)) %>% filter(!is.na(GII_Cleaned))

World<-st_read("World_Countries_Generalized.shp")

custom_map <- c(
  "Congo (Democratic Republic of the)" = "COD",
  "Democratic Republic of the Congo"   = "COD",
  "Congo"                              = "COG",
  "Hong Kong, China (SAR)"             = "HKG",
  "Macao, China (SAR)"                 = "MAC",
  "United States"                      = "USA",
  "United Kingdom"                     = "GBR",
  "Viet Nam"                           = "VNM",
  "Côte d’Ivoire"                      = "CIV",
  "Cote d Ivoire"                      = "CIV",
  "Eswatini"                           = "SWZ",
  "Türkiye"                            = "TUR",
  "Timor-Leste"                        = "TLS",
  "Cabo Verde"                         = "CPV",
  "Micronesia (Federated States of)"   = "FSM",
  "Moldova (Republic of)"              = "MDA",
  "State of Palestine"                 = "PSE",
  "Russian Federation"                 = "RUS",
  "Venezuela (Bolivarian Republic of)" = "VEN",
  "Tanzania (United Republic of)"      = "TZA",
  "Bolivia (Plurinational State of)"   = "BOL",
  "Brunei Darussalam"                  = "BRN",
  "Iran (Islamic Republic of)"         = "IRN",
  "Syrian Arab Republic"               = "SYR",
  "Lao People’s Democratic Republic"   = "LAO",
  "Korea (Republic of)"                = "KOR",
  "Korea (Democratic People’s Republic of)" = "PRK",
  "The Gambia"                         = "GMB",
  "Czechia"                            = "CZE",
  "United Arab Emirates"               = "ARE"
)
GII_Cleaned<-GII_Cleaned %>% rename(COUNTRYAFF='country')
class(GII_Cleaned)

GII_Joined<- World %>% left_join(GII_Cleaned,by = 'COUNTRYAFF')
not_matched <- GII_Cleaned %>%
  anti_join(World, by = "COUNTRYAFF")

print(not_matched$COUNTRYAFF)

GII_fixed <- GII_Cleaned %>%
  mutate(
    COUNTRYAFF = dplyr::recode(
      COUNTRYAFF,
      "Korea (Republic of)" = "Korea, Republic of",
      "Türkiye" = "Turkiye",
      "Iran (Islamic Republic of)" = "Iran, Islamic Republic of",
      "Moldova (Republic of)" = "Moldova, Republic of",
      "Bolivia (Plurinational State of)" = "Bolivia, Plurinational State of",
      "Venezuela (Bolivarian Republic of)" = "Venezuela, Bolivarian Republic of",
      "Eswatini (Kingdom of)" = "Eswatini",
      "Sao Tome and Principe" = "	São Tomé and Príncipe",
      "Tanzania (United Republic of)" = "Tanzania, United Republic of",
      "Congo (Democratic Republic of the)" = "Congo, The Democratic Republic of the"
    )
  )
GII_Joined<- World %>% left_join(GII_fixed,by = 'COUNTRYAFF')
not_matched <- GII_fixed %>%
  anti_join(World, by = "COUNTRYAFF")

GII_Joined<-GII_Joined %>% rename(GII=...3)

tmap_mode("plot")
qtm(GII_Joined,fill = "GII")

GII_Joined<-GII_Joined %>% dplyr::filter(ISO != "AQ")
GII_Joined<-GII_Joined %>% dplyr::mutate(GII= as.numeric(GII))

breaks6 <- c(0, 0.1, 0.2, 0.3, 0.45, 0.6, 0.85)
labels6 <- c("<0.1", "0.1–0.2", "0.2–0.3", "0.3–0.45", "0.45–0.6", ">0.6")

tm_shape(GII_Joined) +
  tm_polygons(
    col = "GII",
    style = "cont",                        
    palette = "-RdBu",                     
    colorNA = "black",
    title = "Gender Inequality Index"
  ) +
  tm_borders(col = "grey40", lwd = 0.3) +
  tm_layout(
    main.title = "The Global Gender Inequality Index (GII) Distribution In 2023",
    main.title.size = 1.4,
    main.title.fontface= "bold",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.position = c("right","top"),
    legend.outside.size = 0.28,
    legend.title.size = 3.2,
    legend.title.fontface = "bold",          
    legend.text.size = 0.8,                  
    legend.text.fontface = "bold",           
    legend.height = 10,                     
    inner.margins = c(0.02,0.02,0.02,0.02),
    frame = FALSE
  ) +
  tm_credits(
    text = "
    Data Source: 
    United Nations Development Programme(2025). 
    Table 5: Gender Inequality Index. 
    Human Development Reports Data Center.",
    position = c("left", "bottom"),   
    align = "left",
    size = 0.8,
    col = "black",
    fontface = "italic",
  )
