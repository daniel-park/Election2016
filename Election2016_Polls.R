

library(plotly)
library(reshape2)
library(dplyr)

# Data frame of various polling results.
# Each row represents a poll.
#all.polls <- pollster_chart_data(slug)

# List with 2 data frames
# $estimates is today's polling estimate by HuffPost
# $estimates_by_date is record of daily polling estimates by HuffPost
#est <- pollster_chart(slug)

# Daily estimates
# View(est$estimates_by_date)
#est.daily <- est$estimates_by_date

# May not need above

###############################################################################
# `all.polls`, `est.daily` were created in the 
#   `Election2016_EstTable.R` script.

# Plotting individual poll results.
# Polls are taken over a period of days.
# To plot each poll result by date, we'll use the median date.

# Create column for median date
# (can't figure out how to use apply())
# Start with empty vector as date object.
median.date <- as.Date(rep(NA, nrow(all.polls)), format="%Y-%m-%d")

#doesn't work
#pply(data.frame(all.polls$start_date, all.polls$end_date), median)

for (i in 1:nrow(all.polls)) {
  median.date[i] <- median(c(all.polls$start_date[i], all.polls$end_date[i]))
}

all.polls <- mutate(all.polls, med_date=median.date)




# `all.polls` is a wide table
# Reshape to long
all.polls.long <- reshape2::melt(data=all.polls,
                                    id.vars=c("pollster", "start_date", 
                                              "end_date", "med_date"),  
                                    measure.vars=c("Clinton", "Trump"),
                                    variable.name="choice",
                                    value.name="value")




# Chart progression of Clinton and Trump's daily polling estimates
# Filter out other candidates
est.daily.top2 <- filter(est.daily, 
                            choice=="Clinton" | choice=="Trump")

# In time series plot, we want blank space for future days where data
#  doesn't yet exist.
# Must create empty data frame with future dates but NA values for the data

# Most recent date where data is available
most.recent.date <- max(est.daily.top2$date)

# Create vector of successive dates starting from `most.recent.date` until 
#   election day
day.by.date <- seq(most.recent.date + 1,
                   as.Date("2016-11-08"), 
                   by="days") %>% 
  
  # Next, reverse order:
  sort(decreasing=TRUE)
# See result:
# day.by.date 

# Replicate each date to match pattern of `est.daily.top2`
day.by.date.2X <- rep(day.by.date, each=2)
# See result:
# tail(day.by.date.2X)
# Compare with date column for daily estimates:
# head(est.daily.top2$date)


# use day.by.date to create empty data frame
empty.date.df <- data.frame(date=day.by.date.2X, 
                            choice=c("Clinton", "Trump"), 
                            value=NA)
# See result:
# Earliest date of `empty.date.frame` will follow most recent date of
#   `est.daily.top2`:
# tail(empty.date.df); head(est.daily.top2)

# Merge two data frames
est.daily.top2 <- rbind(empty.date.df, est.daily.top2)

# Add a new column
# For plotly, change the presentation of hover info
all.polls.long <- all.polls.long %>% 
  mutate(hover_info=paste(value,                             # poll value
                          pollster,                          # poll organization
                          paste(
                            format(start_date, "%m/%d"), # when poll began
                            "-",                         # 
                            format(end_date, "%m/%d")),  # when poll ended
                          sep="<br>"))
# head(all.polls.long)

# For empty data frame, create and rename columns to match `all.polls.long`
empty.date.df <- mutate(empty.date.df, 
                        pollster="", 
                        start_date=as.Date(NA), 
                        end_date=as.Date(NA), 
                        hover_info="") %>%
  dplyr::rename(med_date=date)
# Result
# head(empty.date.df)

all.polls.long <- merge(empty.date.df, all.polls.long, by=intersect(names(empty.date.df), names(all.polls.long)), all=TRUE)
# Result:
# head(all.polls.long)


all.polls.long$choice_color <- ifelse(test=all.polls.long$choice=="Clinton",
                                         yes="blue", no="red")
est.daily.top2$choice_color <- ifelse(test=est.daily.top2$choice=="Clinton",
                                         yes="blue", no="red")
# Result:
# head(all.polls.long); head(est.daily.top2)

# Create plot label where vertical line denotes election day
# Finding the middle of the plot for polling average
# Label will appear in the middle
midpoint <- mean(c(max(est.daily.top2$value, na.rm=TRUE),
                   min(est.daily.top2$value, na.rm=TRUE)))

vert.line <- list(type="line", line=list(color="gray"),
                  x0="2016-11-08", x1="2016-11-08", xref="x",
                  y0=min(all.polls.long$value, na.rm=TRUE),
                  y1=max(all.polls.long$value, na.rm=TRUE) - 1, yref="y")

# marker1a will denote election day
marker1a <- list(x="2016-11-25",
                 y=max(all.polls.long$value, na.rm=TRUE),
#                   midpoint,
                 text="Election Day <br> November 11",
                 font=list(size=10),
                 showarrow=FALSE)

marker1a <- list(x="2016-11-08", 
                y=max(all.polls.long$value, na.rm=TRUE) - 0.75,
                text="Election Day <br> November 11",
                arrowsize=0.7,
                ay=-20,
                ax=0,
                font=list(size=10),
                showarrow=TRUE)  

plot.labels <- list(marker1a)

# Polling data
plot_ly(data=all.polls.long, 
        x=med_date, 
        y=value, 
        mode="markers", 
        marker=list(color=choice_color),
        hoverinfo="text",
        text=hover_info,
        color=choice) %>% 
  #colors=c("red", "blue")
  layout(title="National Polls", 
         annotations=plot.labels,
         shapes=list(vert.line),
         xaxis=list(title="Date", tickangle=-45, rangeslider=list(type="date")),
         yaxis=list(title="", ticksuffix="%"))

