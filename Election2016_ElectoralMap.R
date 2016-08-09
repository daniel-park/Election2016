

map.df <- data.frame(state=state.abb, 
                     name=state.name, 
                     clinton_value=numeric(50),
                     trump_value=numeric(50),
                     leader=character(50), 
                     lead=numeric(50),
                     stringsAsFactors=FALSE)


# slugs for states that have polling data
state.slug.df <- filter(charts$charts, topic=="2016-president") %>%
  filter(state!="US") %>%
  select(slug, state)

map.df <- merge(map.df, state.slug.df, by="state", all=TRUE)

map.df$slug <- ifelse(test=is.na(map.df$slug), 
                      yes="",
                      no=map.df$slug)


# for every row in `map.df`
for (i in 1:nrow(map.df)) {
  
  # if there exists estimates for that state
  if (any(map.df$slug[i]==state.slug.df$slug, na.rm=TRUE)) {
    
    tryCatch({
      
      # extract the data for that state
      state.est.today <- pollster_chart(map.df$slug[i])$estimates # today's estimates
      
      # take today's estimated voter support for each candidate
      # record in `map.df`
      map.df$clinton_value[i] <- filter(state.est.today, choice=="Clinton")$value
      map.df$trump_value[i] <- filter(state.est.today, choice=="Trump")$value
      
      # take the candidate leading the state (always first entry)
      # record candidate's name in `map.df`
      map.df$leader[i] <- state.est.today$choice[1]
      
      # take difference between voter support for both candidates
      # record difference in `map.df`
      map.df$lead[i]   <- state.est.today$value[1] - state.est.today$value[2]
    }, error=function(e) {
      
      #cat("\n", "Error related to slug", map.df$slug[i],
      #    ":",conditionMessage(e), "\n")
    }
    )
  }
}



# MAP

# add column for hover labels on map
map.df <- mutate(map.df, hover=paste(state, "<br>", "Clinton", clinton_value, "<br>",
                                     "Trump", trump_value))

# Need to superimpose two maps, so separate data
clinton.map.df <- filter(map.df, leader=="Clinton")
trump.map.df <- filter(map.df, leader=="Trump")

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)


election.map <- plot_ly(data=clinton.map.df, 
                        z = lead, 
                        text = hover, 
                        hoverinfo="text",
                        locations=state,
                        type = 'choropleth',
                        locationmode = 'USA-states', 
                        color = lead, 
                        colors = 'Blues', 
                        showscale=FALSE) %>%
  add_trace(data=trump.map.df, 
            z = lead,
            text=hover,
            hoverinfo="text",
            locations=state,
            type='choropleth',
            locationmode='USA-states',
            color=lead, 
            colors='Reds', 
            showscale=FALSE) %>%
  layout(title="Electoral Map",
         geo=g)
