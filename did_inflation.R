source("get_data.R")

hicp_cpi_post_brexit %>% 
  count(geo) %>% 
  view()

# common trend visual ####
hicp_cpi_post_brexit %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  mutate(geo = if_else(geo == "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
                       "EU",
                       geo)) %>% 
  filter(geo %in% c("United Kingdom",
                    "Germany",
                    "France",
                    "Canada",
                    "EU")) %>% 
  filter(time > min(brexit_dates$time)) %>% 
  plot_ly(type = "scatter",
          mode = "lines",
          x = ~time,
          y = ~values,
          split = ~geo) %>% 
  layout(title = "CPI Indexed at 2015 - All items excluding energy",
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         shapes = list(vline("2020-12-31"),
                       vline("2022-02-28"))) %>% 
  add_text(showlegend = F,
           x = "2020-12-31",
           y = 130,
           text = ~"End of Transition") %>% 
  add_text(showlegend = F,
           x = "2022-02-28",
           y = 130,
           text = paste("Russian Invasion", "of Ukraine", sep="\n"))


# generate data for model  ####
data <- hicp_cpi_post_brexit %>% 
  ## filter post 1996 to align values in various datasets
  filter(time > as.Date(ymd("1996-01-01"))) %>% 
  ## focus on CPI excluding energy
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  ## gilter only geogrpahies of interest
  filter(geo %in% c("United Kingdom",
                    "Germany",
                    "France",
                    "Canada",
                    "Euro area - 19 countries  (2015-2022)")) %>% 
  ## dummy for post-brexit transition period
  mutate(brexit_time = if_else(time > brexit_dates$time[7],
                               1,
                               0)) %>% 
  ## dummy for UK or not
  mutate(treated = if_else(geo == "United Kingdom",
                           1,
                           0))

# Fit DiD regression models ####
## effect of just being UK ####
did_model_uk_only <- lm(values ~ treated, data = data)
## effect added of post transition period ####
did_model_uk_brexit_date <- lm(values ~ treated + brexit_time, data = data)
## effect added of being UK AND post transition period (interaction effect) ####
did_model_all <- lm(values ~ treated + brexit_time + treated * brexit_time, data = data)

# Print model summaries ####
## effect of just being UK ####
summary(did_model_uk_only)
## effect added of post transition period ####
summary(did_model_uk_brexit_date)
## effect added of being UK AND post transition period (interaction effect) ####
summary(did_model_all)



## play on numbers

{fg_mean_pre <- hicp_cpi_post_brexit %>% 
  filter(geo %in% c("Germany",
                    "France")) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(time < brexit_dates$time[7]) %>% 
  filter(time == max(time)) %>% 
  summarise(mean = mean(values))

fg_mean_post <- hicp_cpi_post_brexit %>% 
  filter(geo %in% c("Germany",
                    "France")) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(time == max(time)) %>% 
  summarise(mean = mean(values))

f_pre <- hicp_cpi_post_brexit %>% 
  filter(geo %in% c("France")) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
       indic == "HICP - All items excluding energy") %>%
  filter(time < brexit_dates$time[7]) %>% 
  filter(time == max(time)) %>% 
  summarise(mean = mean(values))

f_post <- hicp_cpi_post_brexit %>% 
  filter(geo %in% c("France")) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(time == max(time)) %>% 
  summarise(mean = mean(values))

g_pre <- hicp_cpi_post_brexit %>% 
  filter(geo %in% c("Germany")) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(time < brexit_dates$time[7]) %>% 
  filter(time == max(time)) %>% 
  summarise(mean = mean(values))

g_post <- hicp_cpi_post_brexit %>% 
  filter(geo %in% c("Germany")) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(time == max(time)) %>% 
  summarise(mean = mean(values))

uk_pre <- hicp_cpi_post_brexit %>% 
  filter(geo %in% c("United Kingdom")) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(time < brexit_dates$time[7]) %>% 
  filter(time == max(time)) %>% 
  summarise(mean = mean(values))

uk_post <- hicp_cpi_post_brexit %>% 
  filter(geo %in% c("United Kingdom")) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(time == max(time)) %>% 
  summarise(mean = mean(values))}

(uk_pre - uk_post)-(fg_mean_pre - fg_mean_post) 
(uk_pre - fg_mean_pre)-(uk_post - fg_mean_post)

(uk_pre - uk_post)-(f_pre - f_post) 
(uk_pre - f_pre)-(uk_post - f_post)

(uk_pre - uk_post)-(g_pre - g_post) 
(uk_pre - g_pre)-(uk_post - g_post)

