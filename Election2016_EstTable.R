
library(pollstR)
library(dplyr)
library(DT)

# Data frame of various polling results.
# Each row represents a poll.
all.polls <- pollster_chart_data(slug)

# List with 2 data frames
# $estimates is today's polling estimate by HuffPost
# $estimates_by_date is record of daily polling estimates by HuffPost
est <- pollster_chart(slug)

# Daily estimates
est.daily <- est$estimates_by_date

est.today <- select(est$estimates, choice, value, party) %>%
  rename(Candidate=choice, Support=value, Party=party)

for (i in 1:nrow(est.today)) {
  if (est.today$Candidate[i]=="Clinton") {
    est.today$Candidate[i] <- "Hillary Clinton"
  } else if (est.today$Candidate[i]=="Trump") {
    est.today$Candidate[i] <- "Donald Trump"
  } else if (est.today$Candidate[i]=="Johnson") {
    est.today$Candidate[i] <- "Gary Johnson"
  }
  if (est.today$Party[i]=="Dem" & !is.na(est.today$Party[i])) {
    est.today$Party[i] <- "Democratic"
  } else if (est.today$Party[i]=="Rep" & !is.na(est.today$Party[i])) {
    est.today$Party[i] <- "Republican"
  } else if (est.today$Party[i]=="Lib" & !is.na(est.today$Party[i])) {
    est.today$Party[i] <- "Libertarian"
  }
}


