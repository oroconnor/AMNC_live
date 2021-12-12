library(tidyverse)
library(lubridate)
library(openair)

wdata <- read_csv("20989094_Apr2021_AMNC.csv")

wdata <- wdata %>%
  mutate(
    Date = mdy_hms(Date)
  ) %>%
  rename(
    time = Date,
    ws = `Wind Speed (S-WSB 20989094:20977362-1), m/s, AMNC, Kingston, NY`,
    wd = `Wind Direction (S-WDA 20989094:20975986-1), *, AMNC, Kingston, NY`
  )

most_recent_time <- max(wdata$time) # calculate most recent time in dataset

recent_wdata <- wdata %>%
  filter(
    time == most_recent_time
  )


fourhour_wdata <- tail(wdata, 4) 
 
windRose(recent_wdata, paddle = FALSE)
