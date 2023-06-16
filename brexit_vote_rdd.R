

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


