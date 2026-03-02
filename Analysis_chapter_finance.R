###Financial inclusion chapter

###Ilgi Bozdag - GDS###


#Load Packages 
if(!require(pacman)) install.packages('pacman')

pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl,remotes, haven, readxl, waffle )

##load data

data <- read_excel("2025_Global_Survey_on_Livelihoods_and_Economic_Inclusion_GSLEI_-_all_versions_-_English_-_2025-09-15-15-35-21 (1).xlsx")


###Question 20.1 Does the legislation of the host country recognize any UNHCR or government-issued ID credentials as a valid document to open an account at a financial institution (may it be a bank, or a non-bank institution)?


table(data$`20.1.1 Refugees`)

refugees_20_1_1 <- data %>%
  count(`20.1.1 Refugees`, name = "count") %>%
  filter(!is.na(`20.1.1 Refugees`)) %>%
  mutate(
    percentage = round((count / sum(count)) * 100, 2),
    pct_label = paste0(percentage, "%")
  ) %>%
  rename(response = `20.1.1 Refugees`) %>%
  arrange(desc(percentage))

bar_chart_20_1 <- ggplot(refugees_20_1_1, 
                    aes(x = reorder(response, percentage), 
                        y = percentage)) +
  geom_bar(stat = "identity", 
           fill = "#0072BC",  # UNHCR Blue
           width = 0.6) +
  geom_text(aes(label = pct_label),
            hjust = -0.2,
            size = 4,
            fontface = "bold") +
  scale_y_continuous(
    limits = c(0, max(refugees_20_1_1$percentage) * 1.15),
    labels = scales::percent_format(scale = 1)
  ) +
  coord_flip() +
  theme_unhcr() +
  labs(
    title = "Question 20.1.1: Opening an account at financial institution",
    x = NULL,
    y = "Percentage (%)",
    caption = "Source: UNHCR Livelihoods Survey 2025"
  ) +
  theme_unhcr()


###Question 21.1 Are forcibly displaced and stateless persons able to register a SIM card and personal connectivity services?

table(data$`21.1.1 Refugees`)

refugees_21_1_1 <- data %>%
  count(`21.1.1 Refugees`, name = "count") %>%
  filter(!is.na(`21.1.1 Refugees`)) %>%
  mutate(
    percentage = round((count / sum(count)) * 100, 2),
    pct_label = paste0(percentage, "%")
  ) %>%
  rename(response = `21.1.1 Refugees`) %>%
  arrange(desc(percentage))

bar_chart_21_1 <- ggplot(refugees_21_1_1, 
                         aes(x = reorder(response, percentage), 
                             y = percentage)) +
  geom_bar(stat = "identity", 
           fill = "#0072BC",  # UNHCR Blue
           width = 0.6) +
  geom_text(aes(label = pct_label),
            hjust = -0.2,
            size = 4,
            fontface = "bold") +
  scale_y_continuous(
    limits = c(0, 60),
    labels = scales::percent_format(scale = 1)
  ) +
  coord_flip() +
  theme_unhcr() +
  labs(
    title = "Question 21.1.1: Registering a SIM card and personal connectivity services",
    x = NULL,
    y = "Percentage (%)",
    caption = "Source: UNHCR Livelihoods Survey 2025"
  ) +
  theme_unhcr()


##22.1 Does the country’s legislation recognize any UNHCR-issued and/or government-issued ID credentials as valid for opening an account with mobile money service providers (i.e., providers offering financial services via mobile phones and mobile network operators)?

table(data$`22.1 Does the country’s legislation recognize any UNHCR-issued and/or government-issued ID credentials as valid for opening an account with mobile money service providers (i.e., providers offering financial services via mobile phones and mobile network operators)?`
      )

refugees_22_1_1 <- data %>%
  count(`22.1 Does the country’s legislation recognize any UNHCR-issued and/or government-issued ID credentials as valid for opening an account with mobile money service providers (i.e., providers offering financial services via mobile phones and mobile network operators)?`
        , name = "count") %>%
  filter(!is.na(`22.1 Does the country’s legislation recognize any UNHCR-issued and/or government-issued ID credentials as valid for opening an account with mobile money service providers (i.e., providers offering financial services via mobile phones and mobile network operators)?`
  )) %>%
  mutate(
    percentage = round((count / sum(count)) * 100, 2),
    pct_label = paste0(percentage, "%")
  ) %>%
  rename(response = `22.1 Does the country’s legislation recognize any UNHCR-issued and/or government-issued ID credentials as valid for opening an account with mobile money service providers (i.e., providers offering financial services via mobile phones and mobile network operators)?`
  ) %>%
  arrange(desc(percentage))

bar_chart_22_1 <- ggplot(refugees_22_1_1, 
                         aes(x = reorder(response, percentage), 
                             y = percentage)) +
  geom_bar(stat = "identity", 
           fill = "#0072BC",  # UNHCR Blue
           width = 0.6) +
  geom_text(aes(label = pct_label),
            hjust = -0.2,
            size = 4,
            fontface = "bold") +
  scale_y_continuous(
    limits = c(0, 60),
    labels = scales::percent_format(scale = 1)
  ) +
  coord_flip() +
  theme_unhcr() +
  labs(
    title = "Question 22.1.1: Opening an account with mobile money service providers",
    x = NULL,
    y = "Percentage (%)",
    caption = "Source: UNHCR Livelihoods Survey 2025"
  ) +
  theme_unhcr()


##23.1 If you answered that forcibly displaced and stateless persons have legal access to open an account with financial service providers or mobile money service providers, can they open accounts in practice?

##The rest of the questions from 23,,25,26, and 27 there is no answer - no data !
table(data$`25.1 Has UNHCR facilitated access to opening accounts in the names of forcibly displaced and stateless persons or access to other financial products, such as loans for forcibly displaced and stateless persons?`)




ggsave("bar_chart_20_1.png", plot = figure1, width = 10, height = 6, dpi = 300)
ggsave("bar_chart_21_1.png", plot = figure3, width = 10, height = 6, dpi = 300)
ggsave("bar_chart_22_1.png", plot = figure4_1, width = 10, height = 6, dpi = 300)
