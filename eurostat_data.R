library(eurostat)
library(UsefulFunctions)

tables <- get_eurostat_toc()

prc_hicp <- get_eurostat("ei_cphi_m")
prc_hicp_labelled <- label_eurostat(prc_hicp)

prc_hicp_labelled %>% 
  count(indic) %>% 
  view()

prc_hicp_labelled %>% 
  filter(Label == "HICP - All items excluding energy") %>% 
  view()

prc_hicp_labelled %>% 
  filter(unit == "Harmonized consumer price index, 2015=100",
         indic == "HICP - All items excluding energy") %>%
  filter(!str_detect(geo,
                     "Euro area|European Union")) %>% 
  plot_ly(type = "scatter",
          mode = "lines",
          x = ~time,
          y = ~values,
          split = ~geo) %>% 
  layout(title = "HICP Indexed at 2015 - All items excluding energy",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
