source("get_data.R")

data <- hicp_cpi_post_brexit %>% 
  filter(time > as.Date(ymd("1996-01-01"))) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  filter(geo %in% c("United Kingdom",
                    "Germany",
                    "France")) %>% 
  mutate(brexit_time = if_else(time > brexit_dates$time[7],
                               1,
                               0)) %>% 
  mutate(treated = if_else(geo == "United Kingdom",
                           1,
                           0))

# Fit DiD regression model
did_model_uk_only <- lm(values ~ treated, data = data)
did_model_uk_brexit_date <- lm(values ~ treated + brexit_time, data = data)
did_model_all <- lm(values ~ treated + brexit_time + treated * brexit_time, data = data)

# Print model summary
summary(did_model_uk_only)
summary(did_model_uk_brexit_date)
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

f_mean_post <- hicp_cpi_post_brexit %>% 
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

