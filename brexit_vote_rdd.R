source("get_data.R")
library(plotly)

vline <- function(x = 45, color = "black") {
  list(
    type = "line",
    x0 = x,
    x1 = x,
    yref = "paper",
    y0 = 0,
    y1 = 0.9,
    line = list(color = color)
  )
}

## plot data ####
hicp_cpi_post_brexit %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  # filter(!str_detect(geo,
  #                    "Euro area|European Union")) %>% 
  mutate(geo = if_else(geo == "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
                       "EU",
                       geo)) %>% 
  filter(geo %in% c("Germany",
                    "United Kingdom",
                    "France")) %>%
  filter(time > min(brexit_dates$time)) %>% 
  plot_ly(type = "scatter",
          mode = "lines",
          x = ~time,
          y = ~values,
          split = ~geo) %>% 
  layout(title = "CPI Indexed at 2015 - All items excluding energy",
         xaxis = list(title = ""),
         yaxis = list(title = ""),
         shapes = list(vline("2020-12-31"))) %>% 
  add_text(showlegend = F,
           x = "2020-12-31", y = 130,
           text = "End of Transition")

## linear model diff slope ####
### brexit vote ####
lm_different_slope <- hicp_cpi_post_brexit %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  filter(geo == "United Kingdom") %>%
  filter(time > dmy("31-12-2011")) %>% 
  filter(time < brexit_dates$time[[7]]) %>% 
  mutate(threshold = ifelse(time > brexit_dates$time[1], 1, 0)) %$%
  lm(values ~ threshold + I(time - brexit_dates$time[1]) + threshold:I(time - brexit_dates$time[1]))

summary(lm_different_slope)

hicp_cpi_post_brexit %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  filter(geo == "United Kingdom") %>%
  select(time, values) %>%
  filter(time > dmy("31-12-2011")) %>% 
  filter(time < brexit_dates$time[[7]]) %>% 
  mutate(threshold = as.factor(ifelse(time > brexit_dates$time[1], 1, 0))) %>% 
  # mutate(values = as.factor(values)) %>% 
  ggplot(aes(x = time, y = values, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 21, color = "red",
             size = 1, linetype = "dashed") +
  labs(title = "UK CPI Indexed at 2015 - All items excluding energy",
       subtitle = "RDD from Brexit vote 23rd June 2016",
       y = "",
       x = "")

### transition period ####
lm_different_slope_transition <- hicp_cpi_post_brexit %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  filter(geo == "United Kingdom") %>%
  mutate(threshold = ifelse(time > brexit_dates$time[7], 1, 0)) %$%
  lm(values ~ threshold + I(time - brexit_dates$time[7]) + threshold:I(time - brexit_dates$time[1]))

summary(lm_different_slope_transition)

viz <- hicp_cpi_post_brexit %>% 
  filter(time > as.Date(ymd("2009-12-31"))) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  filter(geo == "United Kingdom") %>%
  select(time, values) %>%
  mutate(threshold = as.factor(ifelse(time > brexit_dates$time[7], 1, 0))) %>% 
  # mutate(values = as.factor(values)) %>% 
  ggplot(aes(x = time, y = values, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 21, color = "red",
             size = 1, linetype = "dashed") +
  labs(title = "UK HICP Indexed at 2015 - All items excluding energy",
       subtitle = "RDD from Transition end 2020",
       y = "",
       x = "")

ggplotly(viz)


## linear model diff slope multiple countries ####

lm_different_slope <- hicp_cpi_post_brexit %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  filter(geo %in% c("United Kingdom",
                    "Germany",
                    "France")) %>%
  mutate(state = if_else(geo == "United Kingdom",
                         1,
                         if_else(geo == "France",
                                 2,
                                 3))) %>% 
  mutate(threshold = ifelse(time > brexit_dates$time[1], 1, 0)) %$%
  lm(values ~ threshold + I(time - brexit_dates$time[1]) + threshold:I(time - brexit_dates$time[1]) + state)

summary(lm_different_slope)

hicp_cpi_post_brexit %>% 
  filter(time > as.Date(ymd("1996-01-01"))) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  filter(geo %in% c("United Kingdom",
                    "Germany",
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
  guides(color = guide_legend(title="Country")) +
  # geom_vline(xintercept = 21, color = "red",
  #            size = 1, linetype = "dashed") +
  labs(title = "UK HICP Indexed at 2015 - All items excluding energy",
       subtitle = "RDD from Brexit vote 23rd June 2016",
       y = "",
       x = "")
