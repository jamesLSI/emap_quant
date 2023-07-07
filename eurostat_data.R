library(eurostat)
library(tidyverse)
library(magrittr)

brexit_dates <- tibble(time = c(ymd("2016-06-23"),
                                ymd("2017-3-29"),
                                ymd("2019-5-23"),
                                ymd("2019-9-05"),
                                ymd("2020-1-29"),
                                ymd("2020-1-31"),
                                ymd("2020-12-31")),
                       event = c("Vote",
                                 "Article 50",
                                 "May resigns",
                                 "Benn bill",
                                 "Divorce terms agreed",
                                 "Officially leaves",
                                 "Tranisition ends"))

# tables <- get_eurostat_toc()

prc_hicp <- get_eurostat("ei_cphi_m")
prc_hicp_labelled <- label_eurostat(prc_hicp)%>% 
  mutate(time = lubridate::ymd(time))

# prc_hicp_labelled %>% 
#   count(indic) %>% 
#   view()

# prc_hicp_labelled %>% 
#   filter(Label == "HICP - All items excluding energy") %>% 
#   view()
