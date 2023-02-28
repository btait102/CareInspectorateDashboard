library(tidyverse)

data <- read_csv("C:/Users/btait/OneDrive/Documents/CodeProjects/CareInspectorateDashboard/Data/MDSF_data_31 January 2023.csv")

postcode <- read_csv("C:/Users/btait/OneDrive/Documents/CodeProjects/CareInspectorateDashboard/Data/PostcodeLookup.csv") %>% 
  select(Postcode, Latitude, Longitude)


map_data <- data %>% 
  filter(CareService == "Day Care of Children") %>% 
  mutate(TotalScore = KQ_Setting + KQ_Staff_Team + KQ_Leadership,
         TotalScoreColour = case_when(TotalScore <9 ~ "red",
                                      TotalScore <= 12, "orange",
                                      TRUE ~ "green")) %>% 
  select(ServiceName,Postcode=Service_Postcode, Health_Board_Name, TotalScore,
         TotalScoreColour) %>% 
  left_join(postcode)





icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'white',
  library = 'ion',
  markerColor = case_when(TotalScore <9 ~ "red",
                          TotalScore <= 12, "orange",
                          TRUE ~ "green")
)

library(leaflet)
library(htmltools)



m <- leaflet(data = map_data) %>% 
  addTiles() %>% 
  addMarkers(~Longitude, ~Latitude,
             clusterOptions = markerClusterOptions(),
             label = ~htmlEscape(ServiceName))
  
  
m

postcode <- read_csv("C:/Users/btait/OneDrive/Documents/CodeProjects/CareInspectorateDashboard/Data/PostcodeLookup.csv")

