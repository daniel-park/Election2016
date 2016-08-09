



library(rvest)
library(dplyr)
library(ggmap)
library(RgoogleMaps)

setwd("~/Dropbox/DataScience/Projects/Election2016")
schedule.df <- read.csv(file="Election2016_TourSchedule.csv", stringsAsFactors=FALSE)
schedule.df$date <- as.Date(schedule.df$date)

# Scrape from willhillarywin.com
# produces list of 2
clinton.url <- read_html("https://www.willhillarywin.com/hillary-clintons-events/")

# extracting relevant nodes re: event info
# Used the Chrome app "SelectorGadget" to identify the nodes
# produces list of 128
clinton.schedule.list <- html_nodes(x=clinton.url, 
                                    css=".start,  
                                    .evcal_event_title, .evo_location p")
# extracting text from list
# Is a vector of strings
# Generally, each event has 5 items associated with it
clinton.schedule.vec <- html_text(clinton.schedule.list)
# Result:
# clinton.schedule.vec[1:5]

# Create an index of entries that match today's date
event.date.idx <- which(as.Date(clinton.schedule.vec, format="%d%b")==Sys.Date())
# Result:
# event.date.idx

for (i in event.date.idx) {
  
  event.title <- clinton.schedule.vec[i + 2]
  
  if (grepl("Clinton", event.title) & grepl("Kaine", event.title)) {
    speaker <- "Hillary Clinton & Tim Kaine"
  } else if (grepl("Clinton", event.title)) {
    speaker <- "Hillary Clinton"
  } else if (grepl("Kaine", event.title)) {
    speaker <- "Tim Kaine"
  } else {
    speaker <- "nobody"
  }
  
  if (speaker != "nobody") {
    # Creating an empty row which will be populated by information about today's event
    temp.df <- data.frame(date=as.Date(NA), 
                          clinton=logical(1), # default is FALSE
                          kaine=logical(1), # default is FALSE
                          trump=logical(1), # default is FALSE
                          pence=logical(1), # default is FALSE
                          speaker=character(1), 
                          party=character(1),
                          city_state=character(1),
                          location=character(1), 
                          address=character(1), 
                          time=character(1),
                          latitude=numeric(1),
                          longitude=numeric(1),
                          popup_label=character(1),
                          stringsAsFactors=FALSE)
    # Bind empty row to data frame
    # We will fill in the blank row with new info from the i^th event
    schedule.df <- rbind(schedule.df, temp.df)
    
    # `last.row.idx` records number of rows
    # Last row is the blank row - will tell us which row to populate.
    last.row.idx <- nrow(schedule.df)  
    
    
    
    
    # add date
    schedule.df$date[last.row.idx] <- as.Date(clinton.schedule.vec[i], format="%d%b")
    
    # add booleans for speaker
    if (speaker=="Hillary Clinton & Tim Kaine") {
      schedule.df$clinton[last.row.idx] <- TRUE
      schedule.df$kaine[last.row.idx]   <- TRUE
    } else if (speaker=="Hillary Clinton") {
      schedule.df$clinton[last.row.idx] <- TRUE
    } else if (speaker=="Tim Kaine") {
      schedule.df$kaine[last.row.idx]   <- TRUE
    }
    
    # add speaker names
    schedule.df$speaker[last.row.idx] <- speaker
    
    # add party
    schedule.df$party[last.row.idx] <- "democrat"
    
    # add location name
    schedule.df$location[last.row.idx] <- clinton.schedule.vec[i + 3]
    
    # add city and state
    # Extract city and state
    city.state <- clinton.schedule.vec[i + 2] %>%
      stringr::str_extract(pattern="^.*: ") %>%
      stringr::str_sub(1, -3)
    
    schedule.df$city_state[last.row.idx] <- city.state
    
    # add event time
    schedule.df$time[last.row.idx] <- clinton.schedule.vec[i + 1]
    
    # Rest of info comes from a google query
    # Output from query is a data frame
    temp.geocode <- ggmap::geocode(location=clinton.schedule.vec[i + 4], 
                                   output="more")
    
    # Using output to fill in rest of info
    schedule.df$address[last.row.idx] <- temp.geocode$address[1]
    schedule.df$longitude[last.row.idx] <- temp.geocode$lon[1]
    schedule.df$latitude[last.row.idx] <- temp.geocode$lat[1]
    
    schedule.df$popup_label[last.row.idx] <- paste(format(schedule.df$date[last.row.idx], format="%B %d, %Y"), "<br>",
                                                   schedule.df$time[last.row.idx], "<br>",
                                                   schedule.df$speaker[last.row.idx], "<br>",
                                                   schedule.df$location[last.row.idx], "<br>",
                                                   schedule.df$city_state[last.row.idx], "<br>"
                                                   )
  }
}






# Trump scrape
# produces list of 2
trump.url <- read_html("https://www.donaldjtrump.com/schedule")

# extracting relevant nodes re: event info
# Used the Chrome app "SelectorGadget" to identify the nodes
# produces list of 128
trump.schedule.list <- html_nodes(x=trump.url, 
                                  css="h2~ p+ p , h5 , h2+ p , .event_item h2 , h6")

# extracting text from list
# Is a vector of strings
# Generally, each event has 5 items associated with it
trump.schedule.vec <- html_text(trump.schedule.list)
# Result:
# trump.schedule.vec[1:5]


event.date.idx <- which(as.Date(trump.schedule.vec, format="- %A, %B %d, %Y -")==Sys.Date())


# Trump schedule doesn't identify when both candidates are at same event
# Must figure how to identify
# Possibly deal with after adding rows and then compare; then collapse rows


for (i in event.date.idx) {  
  # Creating an empty row which will be populated by information about today's event
  temp.df <- data.frame(date=as.Date(NA),
                        clinton=logical(1),
                        kaine=logical(1),
                        trump=logical(1),
                        pence=logical(1),
                        speaker=character(1), 
                        party=character(1),
                        location=character(1), 
                        city_state=character(1),
                        address=character(1), 
                        longitude=numeric(1),
                        latitude=numeric(1),
                        time=character(1),
                        popup_label=character(1),
                        stringsAsFactors=FALSE)
  
  # Bind empty row to data frame
  schedule.df <- rbind(schedule.df, temp.df)
  
  # `last.row.idx` records number of rows
  # new row will be added to row number `total.rows + 1`
  last.row.idx <- nrow(schedule.df)  
  
  
  # new row to be added
  schedule.df$date[last.row.idx] <- as.Date(trump.schedule.vec[i], format="- %A, %B %d, %Y -")
  
  # speaker
  speaker <- trump.schedule.vec[i + 2]
  # add booleans for speaker
  if (speaker=="Donald J. Trump") {
    schedule.df$trump[last.row.idx]   <- TRUE
    speaker <- "Donald Trump"
  } else if (speaker=="Governor Mike Pence") {
    schedule.df$pence[last.row.idx] <- TRUE
    speaker <- "Mike Pence"
  }
  
  schedule.df$speaker[last.row.idx] <- speaker
  schedule.df$party[last.row.idx] <- "republican"
  schedule.df$location[last.row.idx] <- trump.schedule.vec[i + 3]
  schedule.df$city_state[last.row.idx] <- trump.schedule.vec[i + 1]
  schedule.df$time[last.row.idx] <- trump.schedule.vec[i + 4] %>% tolower
  
  # Trump's campaign website does not list complete address of event.
  # `ggmap::geocode()` not very accurate when address is vague.
  # `RgoogleMaps::getGeoCode()` is a little better but doesn't offer addresses.
  
  # Output from query is a data frame
  temp.geocode <- RgoogleMaps::getGeoCode(gcStr=paste0(trump.schedule.vec[i + 3], 
                                                       ", ",
                                                       trump.schedule.vec[i + 1])
  )
  
  # Using output to fill in rest of info
  schedule.df$longitude[last.row.idx] <- temp.geocode["lon"]
  schedule.df$latitude[last.row.idx]  <- temp.geocode["lat"]

  schedule.df$popup_label[last.row.idx] <- paste(format(schedule.df$date[last.row.idx], format="%B %d, %Y"), "<br>",
                                                 schedule.df$time[last.row.idx], "<br>",
                                                 schedule.df$speaker[last.row.idx], "<br>",
                                                 schedule.df$location[last.row.idx], "<br>",
                                                 schedule.df$city_state[last.row.idx], "<br>"
                                                 )    
}

 


#write.csv(x=schedule.df, file="Election2016_TourSchedule.csv", row.names=FALSE)
