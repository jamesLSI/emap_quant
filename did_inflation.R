source("get_data.R")
source("current_account_data.R")
library(plotly)

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
  ## add in current account data in usd and usd rate
  left_join(curr_acc_combined %>% 
              filter(str_detect(unit,
                                "Million")) %>%
              select(geo,
                     bop_item,
                     usd_curr_acc_values,
                     time,
                     usd_rate,
                     usd_rate_base_2015)) %>% 
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
  ## filter out one year after transition and so avoid russian invasion of ukraine
  filter(time < brexit_dates$time[7]+366) #%>% 
  # filter(time > brexit_dates$time[7]-366)

data %>% 
  summarise(min = min(time),
            max = max(time))

# Fit DiD regression models ####
## effect of just being UK ####
did_model_uk_only <- lm(values ~ treated, data = data)
## effect of just being UK + post ####
did_model_uk_post <- lm(values ~ treated + brexit_time, data = data)
## effect of just being UK + post + interaction ####
did_model_uk_post_interact <- lm(values ~ treated + brexit_time + treated * brexit_time, data = data)
## effect added for current account balance ####
did_model_uk_curr_acc <- lm(values ~ treated + usd_curr_acc_values, data = data)
## effect added of usd rate
did_model_uk_curr_acc_usd <- lm(values ~ treated + usd_curr_acc_values + usd_rate_base_2015, data = data)
## effect added of post transition period ####
did_model_uk_curr_acc_usd_brexit_date <- lm(values ~ treated + usd_curr_acc_values + usd_rate_base_2015 + brexit_time , data = data)
## effect added of being UK AND post transition period (interaction effect) ####
did_model_all <- lm(values ~ treated + usd_curr_acc_values + usd_rate_base_2015 + brexit_time + treated * brexit_time, data = data)

options(digits=10,
        maxsum = 10)
# Print model summaries ####
## effect of just being UK ####
summary(did_model_uk_only)
## effect of just being UK + post ####
summary(did_model_uk_post)
## effect of just being UK + post + interaction ####
summary(did_model_uk_post_interact)
## effect added for current account balance ####
summary(did_model_uk_curr_acc)
## effect added of used rate
summary(did_model_uk_curr_acc_usd)
## effect added of post transition period ####
summary(did_model_uk_curr_acc_usd_brexit_date)
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
