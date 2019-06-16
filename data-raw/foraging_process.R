library(dplyr)

d <- read.csv("data-raw/AgtaForaging.csv")

# Aggregating by trip
d_trip <- d %>% group_by(trip_id) %>% summarise(harvest=mean(harvest), group_size=n(), pooled=mean(pooled))

d_trip$zero_return <- ifelse(d_trip$harvest == 0, 1, 0)
d_trip <- d_trip[d_trip$group_size < 9,] # dropping one outlier hunt with 9 people
AgtaForaging <- d_trip

# Export data object
devtools::use_data(AgtaForaging, overwrite = T)
