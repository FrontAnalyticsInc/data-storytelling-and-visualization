install.packages('leaflet')

orstationc <- read.csv(file = 'http://geog.uoregon.edu/bartlein/old_courses/geog414s05/data/orstationc.csv' ) 

str(orstationc)
library(leaflet)

pal <- colorQuantile("YlOrRd", NULL , n = 8)
leaflet(orstationc) %>% 
  addTiles() %>%
  addCircleMarkers(color = ~pal(tann))
