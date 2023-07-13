source("get_data.R")

# common trend visual ####

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

hicp_cpi_post_brexit %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
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
                           0)) %>% 
  filter(time < brexit_dates$time[7]+366)

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



## table ####

diff_in_diff_table <- hicp_cpi_post_brexit %>% 
  filter(geo %in% c("Germany",
                    "France",
                    "United Kingdom",
                    "EU",
                    "Canada")) %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  mutate(brexit = if_else(time == (brexit_dates$time[7]-30),
                          "Transition Ends",
                          if_else(time == (brexit_dates$time[7]-30)+365,
                                  "Transition plus 1",
                                  "other"))) %>% 
  filter(!brexit == "other") %>% 
  select(geo,
         values,
         brexit) %>% 
  pivot_wider(names_from = brexit,
              values_from = values) %>% 
  mutate(difference = `Transition plus 1` - `Transition Ends`)


diff_in_diff_table %>% 
  write_csv("diff_in_diff_table.csv")
