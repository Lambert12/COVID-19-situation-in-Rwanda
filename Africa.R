####################################
# NZUNGIZE Labert                  #
# Email: nzulapa@outlook.com       #
# https://github.com/NzungizeL      #
####################################



setwd("C:/Users/Zeky/Desktop/R/age breakdown on COVID-19")

library(tidyverse)
library(scales)
library(wpp2019)


sex_cols <- c(Male = "#5d3bf4", Female =  "#f43bd2")

#Italian fatality rates
italy_rates <-tibble(
  age_grp = rep(c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+'), 2),
  sex = rep(c("Male", "Female"), each = 10),
  cfr = c(0, 0, 0, 0.6, 0.7,   1.7, 6.0, 17.8, 26.4, 32.5,
          0, 0, 0, 0.2,   0.4, 0.6, 2.8,  10.7, 19.1,   22.3) / 100,
  age_midpt = rep(c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95), 2)
)

italy_rates %>%
  ggplot(aes(x = age_midpt, y = cfr, colour = sex)) +
  geom_point() +
  geom_text(data = filter(italy_rates, cfr > 0.01),
            aes(label = percent(cfr), y = cfr + 0.012), size = 3) +
  geom_line() +
  scale_x_continuous(breaks = italy_rates$age_midpt, 
                     labels = italy_rates$age_grp) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_colour_manual(values = sex_cols) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(x = "Age group", colour = "", y = "Observed case fatality rate",
       title = "Observed fatality rate of diagnosed COVID-19 cases in italy to 19 March 2020",
       subtitle = "20,686 men and boys with case fatality rate of 10.3%; 14,378 women and girls with case fatality rate of 6.2%",
       caption = "Source: Istituto Superiore di Sanità, Roma")


#Population rates

data(popF)
data(popM)

selected_countries <- c("Italy","Algeria","Angola","Benin","Burkina Faso", "Botswana","Cameroon","Cabo Verde","Central African Republic","Chad","Congo","Cote d'Ivoire","Djibouti","Dem. Republic of the Congo","Egypt","Equatorial Guinea","Eswatini","Ethiopia","Eritrea","Gabon","Gambia","Ghana","Guinea-Bissau","Guinea","Kenya","Liberia","Libya","Madagascar","Mali","Mauritius","Mauritania","Morocco","Mozambique", "Namibia","Niger","Nigeria","Rwanda","Senegal",
"Seychelles","Sierra Leone","Somalia","South Africa","Sudan","United Republic of Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe","Mayotte","Reunion" )

age_lu <- tibble(age = unique(popF$age),
                 age_grp = c(rep(unique(italy_rates$age_grp), each = 2), "90+")) %>%
  mutate(age_grp = factor(age_grp, levels = unique(age_grp)))


# Visual check that this shorthand worked ok
# View(age_lu)

pop_2020 <- popF %>%
  mutate(sex = "Female") %>%
  rbind(mutate(popM, sex = "Male")) %>%
  select(country = name, age, pop = `2020`, sex) %>%
  left_join(age_lu, by = "age") %>%
  group_by(country, age_grp, sex) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  filter(country %in% selected_countries) %>%
  mutate(country = fct_drop(country)) %>%
  group_by(country) %>%
  mutate(prop = pop / sum(pop)) %>%
  ungroup()


# check no misspellings in countries
stopifnot(sum(!selected_countries %in% unique(pop_2020$country)) == 0)

pop_2020 %>%
  ggplot(aes(x = as.numeric(age_grp), y = prop, colour = sex)) +
  geom_line() +
  facet_wrap(~country) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 1:10, labels = levels(pop_2020$age_grp)) +
  scale_colour_manual(values = sex_cols) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Age group",
       y = "",
       colour = "",
       title = "Estimated proportion of the population in 2020",
       subtitle = "Age profiles of African countries with COVID-19",
       caption = "Source: UN World Population Prospects 2019")

#Combine fatality rate with population

the_caption = "Source:UN World Population Prospects 2019 and Istituto Superiore di Sanità."

the_caption2 = "Source:UN World Population Prospects 2019 and Istituto Superiore di Sanità." 

projected_cfr <- pop_2020 %>%
  mutate(age_grp = as.character(age_grp)) %>%
  left_join(italy_rates, by = c("age_grp", "sex")) %>%
  group_by(country) %>%
  summarise(cfr = sum(cfr * prop) /  sum(prop)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, -cfr))

xlabel <- "Case fatality rate in Italy.\n"

# Version 1:
projected_cfr %>%
  ggplot(aes(y = country, x = cfr)) +
  geom_point(colour = "#f4ba3b") +
  geom_text(aes(label = percent(cfr, accuracy = 0.1)), nudge_x = 0.001, size = 3) +
  geom_segment(aes(yend = country, xend = 0), colour = "#f4ba3b") +
  scale_x_continuous(label = percent_format(accuracy = 0.1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  theme_bw() +
  labs(subtitle = xlabel,
       y = "",
       title = "Fatality rates in Italy with age profiles in Africa",
       x = "Case fatality ratio",
       caption = the_caption)

# Version 2, calibrated to actual Italy case fatality rate so far
projected_cfr %>%
  mutate(cfr_adj = cfr / cfr[country == "Italy"] * 0.113) %>%
  ggplot(aes(y = country, x = cfr_adj)) +
  geom_point(colour = "#f4ba3b") +
  geom_text(aes(label = percent(cfr_adj, accuracy = 0.1)), nudge_x = 0.002, size = 3) +
  geom_segment(aes(yend = country, xend = 0), colour = "#f4ba3b") +
  scale_x_continuous(label = percent_format(accuracy = 0.1)) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(subtitle = xlabel,
       y = "",
       title = "Prediction of COVID-19 with age profiles and fatality rates in Africa",
       x = "Estimation of fatality ratio in Africa",
       caption = the_caption2)

