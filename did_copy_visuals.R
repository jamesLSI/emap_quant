source("did_copy.R")

## distribution ####
card_krueger_1994_mod %>%
  select(chain, state) %>%
  table() %>%
  prop.table(margin = 2)  %>%
  apply(MARGIN = 2,
        FUN = scales::percent_format(accuracy = 0.1)) %>%
  noquote


## pre-treatment means ####

card_krueger_1994_mod %>%
  filter(observation == "February 1992") %>%
  group_by(state) %>%
  summarise(emptot = mean(emptot, na.rm = TRUE),
            pct_fte  = mean(pct_fte, na.rm = TRUE),
            wage_st = mean(wage_st, na.rm = TRUE),
            hrsopen = mean(hrsopen, na.rm = TRUE)) %>%
  pivot_longer(cols=-state, names_to = "variable") %>%
  pivot_wider(names_from = state, values_from = value)

## post-treatment means ####

card_krueger_1994_mod %>%
  filter(observation == "November 1992") %>%
  group_by(state) %>%
  summarise(emptot = mean(emptot, na.rm = TRUE),
            pct_fte  = mean(pct_fte, na.rm = TRUE),
            wage_st = mean(wage_st, na.rm = TRUE),
            hrsopen = mean(hrsopen, na.rm = TRUE)) %>%
  pivot_longer(cols=-state, names_to = "variable") %>%
  pivot_wider(names_from = state, values_from = value)

## histograms ####

hist.feb <- card_krueger_1994_mod %>%
  filter(observation == "February 1992") %>%
  ggplot(aes(wage_st, fill = state)) +
  geom_histogram(aes(y=c(..count..[..group..==1]/sum(..count..[..group..==1]),
                         ..count..[..group..==2]/sum(..count..[..group..==2]))*100),
                 alpha=0.5, position = "dodge", bins = 23) +
  labs(title = "February 1992", x = "Wage range", y = "Percent of stores", fill = "") +
  scale_fill_grey()

hist.nov <- card_krueger_1994_mod %>%
  filter(observation == "November 1992") %>%
  ggplot(aes(wage_st, fill = state)) +
  geom_histogram(aes(y=c(..count..[..group..==1]/sum(..count..[..group..==1]),
                         ..count..[..group..==2]/sum(..count..[..group..==2]))*100),
                 alpha = 0.5, position = "dodge", bins = 23) +
  labs(title = "November 1992", x="Wage range", y = "Percent of stores", fill="") +
  scale_fill_grey()

ggarrange(hist.feb, hist.nov, ncol = 2, 
          common.legend = TRUE, legend = "bottom")
