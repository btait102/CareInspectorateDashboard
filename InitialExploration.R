library(tidyverse)
library(leaflet)
library(htmltools)

data <- read_csv("C:/Users/btait/OneDrive/Documents/CodeProjects/CareInspectorateDashboard/Data/MDSF_data_31 January 2023.csv")

postcode <- read_csv("C:/Users/btait/OneDrive/Documents/CodeProjects/CareInspectorateDashboard/Data/PostcodeLookup.csv") %>% 
  select(Postcode, Latitude, Longitude) %>% 
  group_by(Postcode) %>% 
  summarise(Latitude=mean(Latitude),
         Longitude=mean(Longitude))%>% 
  ungroup()


map_data <- data %>% 
  filter(Subtype == "Day Care of Children (under 3s)" | Subtype =="Nursery",
         ServiceStatus == "Active") %>% 
  mutate(TotalScore = KQ_Setting + KQ_Staff_Team + KQ_Leadership,
         TotalScoreColour = case_when(is.na(TotalScore)~ "black",
                                      TotalScore <9 ~ "red",
                                      TotalScore <= 12~ "orange",
                                      TRUE ~ "green")) 

map_data_pc <- map_data %>% 
  select(ServiceName,Postcode=Service_Postcode, Health_Board_Name, TotalScore,
         TotalScoreColour,KQ_Setting , KQ_Staff_Team , KQ_Leadership) %>% 
  left_join(postcode,"Postcode") %>% 
  mutate(label = )

blank_pc <- map_data_pc %>% 
  filter(is.na(Latitude))







m <- leaflet(data = map_data_pc) %>% 
  addTiles() %>% 
  addAwesomeMarkers(~Longitude, ~Latitude,
             clusterOptions = markerClusterOptions(),
             icon = awesomeIcons(
               icon = 'ios-close',
               iconColor = 'white',
               library = 'ion',
               markerColor = ~TotalScoreColour
             ),
             popup = ~paste0(paste0("Service Name - ",ServiceName," <br/> ",
                                    "Total Score - ",as.character(TotalScore)," <br/> ",
                                    "Setting - ",as.character(KQ_Setting),
                                    ",  Staff/Team - ",as.character(KQ_Staff_Team),
                                    ",  Leadership - ",as.character(KQ_Leadership))))
  
  
m


