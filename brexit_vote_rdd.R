source("eurostat_data.R")
library(plotly)

## plot data ####
prc_hicp_labelled %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  # filter(!str_detect(geo,
  #                    "Euro area|European Union")) %>% 
  mutate(geo = if_else(geo == "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
                       "EU",
                       geo)) %>% 
  filter(geo %in% c("Germany (until 1990 former territory of the FRG)",
                    "United Kingdom",
                    "France",
                    "Italy",
                    "Spain",
                    "EU")) %>%
  plot_ly(type = "scatter",
          mode = "lines",
          x = ~time,
          y = ~values,
          split = ~geo) %>% 
  layout(title = "HICP Indexed at 2015 - All items excluding energy",
         xaxis = list(title = ""),
         yaxis = list(title = ""))

## linear model diff slope ####
lm_different_slope <- prc_hicp_labelled %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  filter(geo == "United Kingdom") %>%
  mutate(threshold = ifelse(time > brexit_dates$time[1], 1, 0)) %$%
  lm(values ~ threshold + I(time - brexit_dates$time[1]) + threshold:I(time - brexit_dates$time[1]))

summary(lm_different_slope)

prc_hicp_labelled %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  filter(geo == "United Kingdom") %>%
  select(time, values) %>%
  mutate(threshold = as.factor(ifelse(time > brexit_dates$time[1], 1, 0))) %>% 
  # mutate(values = as.factor(values)) %>% 
  ggplot(aes(x = time, y = values, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 21, color = "red",
             size = 1, linetype = "dashed") +
  labs(title = "UK HICP Indexed at 2015 - All items excluding energy",
       subtitle = "RDD from Brexit vote 23rd June 2016",
       y = "",
       x = "")



## linear model diff slope multiple countries ####

lm_different_slope <- prc_hicp_labelled %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  filter(geo %in% c("United Kingdom",
                    "Germany (until 1990 former territory of the FRG)",
                    "France")) %>%
  mutate(state = if_else(geo == "United Kingdom",
                         1,
                         if_else(geo == "France",
                                 2,
                                 3))) %>% 
  mutate(threshold = ifelse(time > brexit_dates$time[1], 1, 0)) %$%
  lm(values ~ threshold + I(time - brexit_dates$time[1]) + threshold:I(time - brexit_dates$time[1]) + state)

summary(lm_different_slope)

prc_hicp_labelled %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  filter(geo %in% c("United Kingdom",
                    "Germany (until 1990 former territory of the FRG)",
                    "France")) %>%
  select(time, values, geo) %>%
  mutate(state = as.factor(if_else(geo == "United Kingdom",
                                   1,
                                   if_else(geo == "France",
                                           2,
                                           3)))) %>%
  mutate(threshold = as.factor(ifelse(time > brexit_dates$time[1], 1, 0))) %>% 
  # mutate(values = as.factor(values)) %>% 
  ggplot(aes(x = time, y = values, color = interaction(threshold, geo, sep=':'))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 21, color = "red",
             size = 1, linetype = "dashed") +
  labs(title = "UK HICP Indexed at 2015 - All items excluding energy",
       subtitle = "RDD from Brexit vote 23rd June 2016",
       y = "",
       x = "")
