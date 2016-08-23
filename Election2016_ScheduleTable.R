
library(DT)
library(dplyr)

# Create file name which contains today's date
file.name <- paste0("Election2016_csv/Election2016_TourSchedule", 
                    format(Sys.Date(), "%b%d"), 
                    ".csv")

schedule.df <- read.csv(file=file.name, stringsAsFactors=FALSE)
schedule.df$date <- as.Date(schedule.df$date)



if (schedule.state=="US") {
  schedule.subset.df <- filter(schedule.df, 
                               date <=schedule.date.end & 
                                 date >= schedule.date.begin) %>%
    select(date, speaker, city_state, location, time) %>%
    dplyr::rename(Date=date, Candidate=speaker, Location=city_state, Venue=location, Time=time)
} else {
  schedule.subset.df <- filter(schedule.df, state==schedule.state) %>%
    select(date, speaker, party, city, location, time) %>%
    dplyr::rename(Date=date, Candidate=speaker, Party=party, Location=city, Venue=location, Time=time)
}

# make date more readable
schedule.subset.df$Date <- format(schedule.subset.df$Date, format="%B %d, %Y")

schedule.DT <- DT::datatable(data=schedule.subset.df, options=list())
