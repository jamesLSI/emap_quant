source("did_copy.R")

## first difference ####

differences <- card_krueger_1994_mod %>%
  group_by(observation, state) %>%
  summarise(emptot = mean(emptot, na.rm = TRUE))

# Treatment group (NJ) before treatment
njfeb <- differences[1,3]

# Control group (PA) before treatment
pafeb <- differences[2,3]

# Treatment group (NJ) after treatment
njnov <- differences[3,3]

# Control group (PA) after treatment
panov <- differences[4,3]

## ATT ####
(njnov-njfeb)-(panov-pafeb) 
(njnov-panov)-(njfeb-pafeb)

## DID plot ####

nj_counterfactual <- tibble(
  observation = c("February 1992","November 1992"), 
  state = c("New Jersey (Counterfactual)","New Jersey (Counterfactual)"),
  emptot = as.numeric(c(njfeb, njfeb-(pafeb-panov)))
) 

# Data points for treatment event
intervention <- tibble(
  observation = c("Intervention", "Intervention", "Intervention"),
  state = c("New Jersey", "Pennsylvania", "New Jersey (Counterfactual)"),
  emptot = c(19.35, 22.3, 19.35)
) 

# Combine data
did_plotdata <- bind_rows(differences, 
                          nj_counterfactual, 
                          intervention)

did_plotdata %>%
  mutate(label = if_else(observation == "November 1992", as.character(state), NA_character_)) %>%
  ggplot(aes(x=observation,y=emptot, group=state)) +
  geom_line(aes(color=state), size=1.2) +
  geom_vline(xintercept = "Intervention", linetype="dotted", 
             color = "black", size=1.1) + 
  scale_color_brewer(palette = "Accent") +
  scale_y_continuous(limits = c(17,24)) +
  ggrepel::geom_label_repel(aes(label = label),
                            nudge_x = 0.5, nudge_y = -0.5,
                            na.rm = TRUE) +
  guides(color=FALSE) +
  labs(x="", y="FTE Employment (mean)") +
  annotate(
    "text",
    x = "November 1992",
    y = 19.6,
    label = "{Difference-in-Differences}",
    angle = 90,
    size = 3
  )

## DID model ####

card_krueger_1994_mod <- mutate(card_krueger_1994_mod,
                                time = ifelse(observation == "November 1992", 1, 0),
                                treated = ifelse(state == "New Jersey", 1, 0)
)

did_model <- lm(emptot ~ time * treated,  data = card_krueger_1994_mod)
summary(did_model)

# declare as panel data
panel <- pdata.frame(card_krueger_1994_mod, "sheet")

# Within model
did.reg <- plm(emptot ~ time + treated + time:treated, 
               data = panel, model = "within")

# obtain clustered standard errors
coeftest(did.reg, vcov = function(x) 
  vcovHC(x, cluster = "group", type = "HC1"))
