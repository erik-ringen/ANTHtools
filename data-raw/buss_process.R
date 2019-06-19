library(tidyverse)

buss1 <- read.csv("data-raw/buss1989_finan.csv")
buss2 <- read.csv("data-raw/buss1989_agediff.csv")
buss3 <- read.csv("data-raw/buss1989_indust.csv")
buss4 <- read.csv("data-raw/buss1989_looks.csv")

d <- bind_rows(buss1, buss2, buss3, buss4)
colnames(d)[1] <- "Region"
d$question <- rep( c("Financial Prospects", "Age Difference", "Industry/Ambition", "Good Looks"), each=nrow(buss1) )

Buss1989 <- d %>% gather(key="Measure", value="est", -c(Sample, Region, question)) %>% group_by(Sample, Region, Measure, question) %>% summarize(est=mean(est)) %>% spread(Measure, est)

# Export data object
usethis::use_data(Buss1989, overwrite = T)
