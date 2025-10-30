#####Livelihood Analysis####
##load packages

#Load Packages 
if(!require(pacman)) install.packages('pacman')

pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr,
  readr,labelled,pastecs,psych,tableone, outbreaks, ggplot2, unhcrthemes,
  scales, gt,webshot2, sjlabelled, waffle, writexl,remotes, haven, readxl, waffle )

##load data

data <- read_excel("2025_Global_Survey_on_Livelihoods_and_Economic_Inclusion_GSLEI_-_all_versions_-_English_-_2025-09-15-15-35-21 (1).xlsx")



###Figure 1

##Number of countries 

table(data$country)
length(unique(data$country))

###There are soe countries coded more than once  DRC, Kenya

### 2019 111, 2021 123, 2023 132, 2025 63


# Create the data
df <- data.frame(
  year = c(2019, 2021, 2023, 2025),
  value = c(111, 123, 132, 63)
)

# Plot
figure1 <- ggplot(df, aes(x = year, y = value, color = "trend")) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_unhcr_d(guide = "none") +
  scale_x_continuous(
    breaks = c(2019, 2021, 2023, 2025),  # precise years on x-axis
    labels = c("2019", "2021", "2023", "2025")
  ) +
  scale_y_continuous(
    limits = c(60, 135),
    breaks = seq(60, 135, 15),
    labels = number_format(accuracy = 1)
  ) +
  labs(
    title = "Number of surveyed countries, 2019-2025",
    y = "Number of countries"
  ) +
  theme_unhcr()



###Analysis from survey 

##Figure 3 - 
##Percentage of refugees living in countries with unresricted access in practice to formal employment

##variable for refugees

table(data$`4.1.1 Refugees`) ### full legal right for wage earning activities 

percentage_table_1 <- prop.table(table(data$`4.1.1 Refugees`)) * 100

##The results sow that 18.5% no access, 34,28% with minimal or no challenges and 47.14% can access with restrictions

table(data$`8.1.1 Refugees`)### access to formal wage earning employment in practice 

percentage_table_2 <- prop.table(table(data$`8.1.1 Refugees`)) * 100

##The results 30% with minimal or no challenges in practice, 52.85 with restriction and 17.14% do not have access


##Combine both


data$legal_practice <- ifelse(
  data$`8.1.1 Refugees` %in% c("Yes, with minimal or no challenges", "Yes with minimal challenge") &
    data$`4.1.1 Refugees` %in% c("Yes, with minimal or no challenges", "Yes with minimal challenge"),
  1, 0
)

table(data$legal_practice)


percentage_table_3 <- prop.table(table(data$legal_practice)) * 100


##22,85% have access to both 



# Create the data
df_fig3 <- data.frame(
  year = c(2019, 2021, 2023, 2025),
  value = c(18, 38, 45, 30)
)




figure3 <- ggplot(df_fig3, aes(x = factor(year), y = value, fill = factor(year))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(value, "%")), vjust = -0.5, size = 4) +
  scale_fill_unhcr_d() +
  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
  theme_unhcr() +
  labs(
    x = "Year",
    y = "Percentage"
  )



###Figure 4


# Data for 2025
legal_right <- 34.28
in_practice <- 30

# Convert percentages to number of squares (e.g., 100 squares total)
legal_squares <- round(legal_right)
practice_squares <- round(in_practice)


# Create waffle chart
figure4_1 <- waffle(
  c(`Legal right to labour market` = legal_squares,
    `Remaining` = 100 - legal_squares),
  rows = 10,
  colors = c("#0072BC", "#D3D3D3"),  # UNHCR blue + grey for remaining
  title = "Legal Right to the Labour Market"
) + theme_unhcr() +
   theme(
    axis.text.x = element_blank(),  # Remove X axis labels
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),  # Remove Y axis labels
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

figure4_2 <- waffle(
  c(`Legal right to labour market` = legal_squares,
    `Remaining` = 100 - legal_squares),
  rows = 10,
  colors = c("#D25A45", "#D3D3D3"),  # UNHCR red + grey for remaining
  title = "Legal Right to the Labour Market"
) + theme_unhcr() +
  theme(
    axis.text.x = element_blank(),  # Remove X axis labels
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),  # Remove Y axis labels
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )



####Figure 5

table(data$`9.1.2 Select Administrative Barriers, all that apply/Administrative/practical restrictions (e.g. business registration, bank account regulation)`, useNA = "ifany")

table(data$`9.1.4 Select Skill and Qualification Barriers, all that apply/No recognition of skills, diplomas, certification, etc.`, useNA = "ifany")

table(data$`9.1.2 Select Administrative Barriers, all that apply/Employers/firms' lack of knowledge about refugees' rights, and non-recognition of ID credentials`, useNA = "ifany")

table(data$`9.1.1 Select Legal Barriers, all that apply/Necessity of permits (e.g. residence, work)`, useNA = "ifany")

table(data$`9.1.3 Select Economic Barriers, all that apply/High unemployment/underemployment in the host economy`, useNA = "ifany")

table(data$`9.1.4 Select Skill and Qualification Barriers, all that apply/Refugees lack language skills for the national labour market`, useNA = "ifany")

table(data$`9.1.1 Select Legal Barriers, all that apply/No or restricted right to work`, useNA = "ifany")


library(dplyr)

# List of variables
vars <- c(
  "9.1.2 Select Administrative Barriers, all that apply/Administrative/practical restrictions (e.g. business registration, bank account regulation)",
  "9.1.4 Select Skill and Qualification Barriers, all that apply/No recognition of skills, diplomas, certification, etc.",
  "9.1.2 Select Administrative Barriers, all that apply/Employers/firms' lack of knowledge about refugees' rights, and non-recognition of ID credentials",
  "9.1.1 Select Legal Barriers, all that apply/Necessity of permits (e.g. residence, work)",
  "9.1.3 Select Economic Barriers, all that apply/High unemployment/underemployment in the host economy",
  "9.1.4 Select Skill and Qualification Barriers, all that apply/Refugees lack language skills for the national labour market",
  "9.1.1 Select Legal Barriers, all that apply/No or restricted right to work"
)

# Create a combined data frame
df_all <- lapply(vars, function(v) {
  freq <- table(data[[v]], useNA = "ifany")
  df <- as.data.frame(freq)
  colnames(df) <- c("Response", "Count")
  df$Variable <- v
  df$Percentage <- (df$Count / sum(df$Count)) * 100
  return(df)
}) %>% bind_rows()


df_filtered <- df_all %>%
  filter(Response == "1")


###rename variables


# Apply short names to df_filtered
df_filtered <- df_filtered %>%
  mutate(Variable = recode(Variable,
                           "9.1.2 Select Administrative Barriers, all that apply/Administrative/practical restrictions (e.g. business registration, bank account regulation)" ~ "Administrative/practical restrictions",
                           "9.1.4 Select Skill and Qualification Barriers, all that apply/No recognition of skills, diplomas, certification, etc." ~ "No Recognition of skills, diplomas, certification etc.",
                           "9.1.2 Select Administrative Barriers, all that apply/Employers/firms' lack of knowledge about refugees' rights, and non-recognition of ID credentials" ~ "Employers' lack of awareness of refugees' rights and IDs",
                           "9.1.1 Select Legal Barriers, all that apply/Necessity of permits (e.g. residence, work)" ~ "Necessity of permits",
                           "9.1.3 Select Economic Barriers, all that apply/High unemployment/underemployment in the host economy" ~ "High unemployment in the host economy",
                           "9.1.4 Select Skill and Qualification Barriers, all that apply/Refugees lack language skills for the national labour market" ~ "Refugees lack language skills for labor market",
                           "9.1.1 Select Legal Barriers, all that apply/No or restricted right to work" ~ "No or restricted right to work"
  ))




# Order by Percentage (descending)
df_filtered <- df_filtered %>%
  arrange(desc(Percentage))


# Bar chart with UNHCR blue only
figure5 <- ggplot(df_filtered, aes(x = reorder(Variable, Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "#0072BC") +  # UNHCR blue
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.1, size = 4) +
  coord_flip() +  # Horizontal bars for readability
  scale_y_continuous(limits = c(0, 100), labels = scales::percent_format(scale = 1)) +
  theme_unhcr() +
  labs(title = "Barriers (Selected Responses)", 
       x = NULL, 
       y = NULL,
       caption="Note: Multiple answers possible")

