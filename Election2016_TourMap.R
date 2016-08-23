

library(leaflet)

# Icons to be used as markers on map
repub.icon <- makeIcon(iconUrl="https://upload.wikimedia.org/wikipedia/commons/9/9b/Republicanlogo.svg",
                       iconWidth = 25, iconHeight = 25)
dem.icon <- makeIcon(iconUrl="http://www.standupamericaus.org/sua/wp-content/uploads/2012/05/Democrat-Logo-psd355111.png", 
                     iconWidth = 25, iconHeight = 25)

# Create basic template for a map
# W/o template, if no data is fed into map 
#   (e.g. no scheduled rallies on a certain day), then viewer becomes blank -
#   no map appears.  
# W/ template, if no data is fed into map, then at least 
#   a bare map will appear.
map.blank <- leaflet() %>% 
  addTiles() %>%
  setView(lng = -97, lat = 38, zoom = 4) %>%
  addLayersControl(overlayGroups=c("Clinton (D)", "Kaine (D)", 
                                   "Trump (R)", "Pence (R)"), 
                   options = layersControlOptions(collapsed = FALSE))

###############################################################################
############ Today
###############################################################################

map.today <- map.blank

if (filter(schedule.df, date == Sys.Date() & clinton==TRUE) %>% nrow > 0) {
  map.today <- map.today %>%
    addMarkers(data=filter(schedule.df, date==Sys.Date() & clinton==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=dem.icon,
               group="Clinton (D)")
} 

if (filter(schedule.df, date == Sys.Date() & kaine==TRUE) %>% nrow > 0) {
  map.today <- map.today %>%
    addMarkers(data=filter(schedule.df, date==Sys.Date() & kaine==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=dem.icon,
               group="Kaine (D)")
} 

if (filter(schedule.df, date == Sys.Date() & trump==TRUE) %>% nrow > 0) {
  map.today <- map.today %>%
    addMarkers(data=filter(schedule.df, date==Sys.Date() & trump==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=repub.icon,
               group="Trump (R)")
}

if (filter(schedule.df, date == Sys.Date() & pence==TRUE) %>% nrow > 0) {
  map.today <- map.today %>%
    addMarkers(data=filter(schedule.df, date==Sys.Date() & pence==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=repub.icon,
               group="Pence (R)")
}

###############################################################################
############ Yesterday
###############################################################################

map.yesterday <- map.blank

if (filter(schedule.df, date == Sys.Date() - 1 & clinton==TRUE) %>% nrow > 0) {
  map.yesterday <- map.blank %>%
    addMarkers(data=filter(schedule.df, date==Sys.Date() - 1 & clinton==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=dem.icon,
               group="Clinton (D)")
} 

if (filter(schedule.df, date == Sys.Date() - 1 & kaine==TRUE) %>% nrow > 0) {
  map.yesterday <- map.yesterday %>%
    addMarkers(data=filter(schedule.df, date==Sys.Date() - 1 & kaine==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=dem.icon,
               group="Kaine (D)")
} 

if (filter(schedule.df, date == Sys.Date() - 1 & trump==TRUE) %>% nrow > 0) {
  map.yesterday <- map.yesterday %>%
    addMarkers(data=filter(schedule.df, date==Sys.Date() - 1 & trump==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=repub.icon,
               group="Trump (R)")
}

if (filter(schedule.df, date == Sys.Date() - 1 & pence==TRUE) %>% nrow > 0) {
  map.yesterday <- map.yesterday %>%
    addMarkers(data=filter(schedule.df, date==Sys.Date() - 1 & pence==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=repub.icon,
               group="Pence (R)")
}




###############################################################################
############ Last 3 Days
###############################################################################

map.last3days <- map.blank

if (filter(schedule.df, date >= Sys.Date() - 2 & clinton==TRUE) %>% nrow > 0) {
  map.last3days <- map.blank %>%
    addMarkers(data=filter(schedule.df, date >= Sys.Date() - 2 & clinton==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=dem.icon,
               group="Clinton (D)")
} 

if (filter(schedule.df, date >= Sys.Date() - 2 & kaine==TRUE) %>% nrow > 0) {
  map.last3days <- map.last3days %>%
    addMarkers(data=filter(schedule.df, date >= Sys.Date() - 2 & kaine==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=dem.icon,
               group="Kaine (D)")
} 

if (filter(schedule.df, date >= Sys.Date() - 2 & trump==TRUE) %>% nrow > 0) {
  map.last3days <- map.last3days %>%
    addMarkers(data=filter(schedule.df, date >= Sys.Date() - 2 & trump==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=repub.icon,
               group="Trump (R)")
}

if (filter(schedule.df, date >= Sys.Date() - 2 & pence==TRUE) %>% nrow > 0) {
  map.last3days <- map.last3days %>%
    addMarkers(data=filter(schedule.df, date >= Sys.Date() - 2 & pence==TRUE),
               lng=~longitude, lat=~latitude,
               popup=~popup_label,
               icon=repub.icon,
               group="Pence (R)")
}





###############################################################################
############ Last 7 Days
###############################################################################

map.last7days <- leaflet() %>% addTiles() %>%
  addMarkers(data=filter(schedule.df, date >= Sys.Date() - 6 & clinton==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=dem.icon,
             group="Clinton (D)") %>%
  addMarkers(data=filter(schedule.df, date >= Sys.Date() - 6 & kaine==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=dem.icon,
             group="Kaine (D)") %>%
  addMarkers(data=filter(schedule.df, date >= Sys.Date() - 6 & trump==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=repub.icon,
             group="Trump (R)") %>%
  addMarkers(data=filter(schedule.df, date >= Sys.Date() - 6 & pence==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=repub.icon,
             group="Pence (R)") %>%
  addLayersControl(overlayGroups=c("Clinton (D)", "Kaine (D)", 
                                   "Trump (R)", "Pence (R)"), 
                   options = layersControlOptions(collapsed = FALSE))


###############################################################################
############ Last 30 Days
###############################################################################

map.last30days <- leaflet() %>% addTiles() %>%
  addMarkers(data=filter(schedule.df, date >= Sys.Date() - 29 & clinton==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=dem.icon,
             group="Clinton (D)") %>%
  addMarkers(data=filter(schedule.df, date >= Sys.Date() - 29 & kaine==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=dem.icon,
             group="Kaine (D)") %>%
  addMarkers(data=filter(schedule.df, date >= Sys.Date() - 29 & trump==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=repub.icon,
             group="Trump (R)") %>%
  addMarkers(data=filter(schedule.df, date >= Sys.Date() - 29 & pence==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=repub.icon,
             group="Pence (R)") %>%
  addLayersControl(overlayGroups=c("Clinton (D)", "Kaine (D)", 
                                   "Trump (R)", "Pence (R)"), 
                   options = layersControlOptions(collapsed = FALSE))



###############################################################################
############ Since July 31, 2016
###############################################################################

map.100days <- map.blank %>%
  addMarkers(data=filter(schedule.df, clinton==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=dem.icon,
             group="Clinton (D)") %>%
  addMarkers(data=filter(schedule.df, kaine==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=dem.icon,
             group="Kaine (D)") %>%
  addMarkers(data=filter(schedule.df, trump==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=repub.icon,
             group="Trump (R)") %>%
  addMarkers(data=filter(schedule.df, pence==TRUE),
             lng=~longitude, lat=~latitude,
             popup=~popup_label,
             icon=repub.icon,
             group="Pence (R)")




