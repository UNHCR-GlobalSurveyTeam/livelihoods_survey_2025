
#Livelihood and Economic inclusion survey 


#delete everyting from console
rm(list = ls())

# 1 to export
# 0 not to export
bExport = 1
bExport = 0


# Load Libraries
source('C:/Users/HUMBERSET/OneDrive - UNHCR/Documents/R Studio/Libraries/LoadLibraries.R')

# options(digits = 22)



# =============================================================================	*  
# ----------------------------------------------------------------------------- *  



# 0) Refugee data ----
RefDat <- read_csv("Data/Raw_Data/RDF/RefPopulation_RDF_2000_2024_Janis.csv")


RefDat1 <- RefDat %>%
  rename(yr          = Year,
         cnt         = 'Country of asylum',
         iso3        = 'Country of asylum (ISO)',
         n_ref       = "Refugees under UNHCR's mandate",
         n_asylum    = 'Asylum-seekers',
         n_others    = 'Other people in need of international protection') %>%
  mutate(n_others = ifelse(n_others == "-", 0, n_others),
         n_ref    = ifelse(n_ref    == '.', 0 , n_ref),
         n_asylum = ifelse(n_asylum == '.', 0 , n_asylum),
         n_others = ifelse(n_others == '.', 0 , as.numeric(n_others)),
         n_poc    = n_ref + n_others,
         cnt      = ifelse(cnt      == 'TÃ¼rkiye', 'Turkiye', cnt),
         bAll     = 1) %>%
  dplyr::select(yr, iso3, n_ref, n_asylum, n_others, n_poc)


RefDat1 <- RefDat1 %>%
  mutate(n_poc =  n_others+n_ref) %>% 
  group_by(yr, iso3) %>% 
  summarise(s_ref = sum(n_ref),
            s_asylum = sum(n_asylum), 
            s_others = sum(n_others),
            s_poc = sum(n_poc)) %>% ungroup() %>% 
  mutate(bAll = 1)

contr <- RefDat1 %>% 
  group_by(yr) %>% 
  summarise(s_poc = sum(s_poc))

# _0.1) 2025 data ----
RefDat_2025 <- read_excel("Data/Raw_Data/RDF/MYT25_as_of_now_20250818.xlsx", sheet = '2_BackfilledMYSR25') %>% 
  clean_names()


RefDat_2025a <- RefDat_2025 %>% 
  dplyr::select(iso3_country_code, unsd_name_asylum, refugees, asylum_seekers, other_people_in_need_of_international_protection) %>% 
  rename(iso3 = iso3_country_code,
         cnt = unsd_name_asylum, 
         n_ref = refugees, 
         n_asylum = asylum_seekers,
         n_others = other_people_in_need_of_international_protection) %>% 
  mutate(s_others = ifelse(n_others == "-", 0, n_others),
         s_ref    = ifelse(n_ref    == '.', 0 , n_ref),
         s_asylum = ifelse(n_asylum == '.', 0 , n_asylum),
         s_others = ifelse(n_others == '.', 0 , as.numeric(n_others)),
         s_poc    = n_ref + n_others,
         bAll     = 1,
         yr       = 2025) %>%
  dplyr::select(yr, iso3, bAll, s_ref, s_asylum, s_others, s_poc)



RefDat1a <- bind_rows(RefDat1, RefDat_2025a)

summary(RefDat1a)


contr <- RefDat1a %>% 
  group_by(yr) %>% 
  summarise(s_poc = sum(s_poc))
#OK






# 		1) 2023 data ----
#  _1.1) Original LEI data ----
PrevLiveli_allDat <- read_excel("Data/Raw_Data/Livelihood_EconomicSurvey/2023_GSLEI_Refugees_database and analysis_finalised.xlsx",
                                sheet = '2023 GSLEI', skip = 3) %>% clean_names() %>%
  dplyr::select(c(4, 5, 10, 81:85)) #Col E, J, CC:CG


nrow(PrevLiveli_allDat)

Quest2023 <- as.data.frame(ls(PrevLiveli_allDat))



tempPrevLiveli_allDat <- PrevLiveli_allDat %>% 
  rename(category = x13_refugees,
         n_ref = total_82) %>% 
  mutate(n_ref = as.numeric(n_ref))


StatLEI2023 <- tempPrevLiveli_allDat %>% 
  filter(is.na(category) == F) %>% 
  group_by(category) %>% 
  summarise(n_cnt = n(),
            s_ref = sum(n_ref, na.rm = T)) %>% ungroup() %>% 
  mutate(tot_cnt = sum(n_cnt),
         tot_ref = sum(s_ref),
         perc_ref = round(100*s_ref/tot_ref, 2))
#OK same as in stats


Orig2023 <- StatLEI2023 %>% 
  filter(category == 'Yes, without any significant restrictions') %>% 
  rename(access_in_practice = category, 
         `2023` = perc_ref) %>% 
  dplyr::select(access_in_practice, n_cnt, tot_cnt, `2023`)


#The original version of 2023
Corr2023 <- Orig2023 %>% 
  rename(perc_ref = `2023`)



# _1.2) Change in methodology ----
# The year is correctly mapped
# Duplicates are removed

RegCntNames1 <- readRDS("Data/Final_Data/R_Data/countries_names_regions.rds")

RegCntNames1a <- RegCntNames1 %>% 
  dplyr::select(cnt, iso3, unhcr_region, bAll)


df23_corrected <- tempPrevLiveli_allDat %>% 
  rename(cnt = x1_2_country_operation,
       access_in_practice = category) %>%
  filter(is.na(access_in_practice) == F) %>% 
  mutate(bAll = 1) %>% 
  dplyr::select(cnt, access_in_practice, bAll) %>% 
  mutate(cnt = ifelse(cnt == "2021 - Côte d'Ivoire", "Côte d'Ivoire",
               ifelse(cnt == '2021 - Guinea', 'Guinea',
               ifelse(cnt == '2021 - GUINEA BISSAU', 'Guinea-Bissau',
               ifelse(cnt == '2021 - Kazakhstan', 'Kazakhstan',
               ifelse(cnt == '2021 - Togo', 'Togo',
               ifelse(cnt == 'Aruba', 'Aruba (K. of the Netherlands)',
               ifelse(cnt == 'Bolivia', 'Plurinational State of Bolivia',
               ifelse(cnt == 'Congo', 'Republic of the Congo',
               ifelse(cnt == 'Congo, Democratic Republic of', 'Democratic Republic of the Congo',
               ifelse(cnt == 'Hong Kong', 'Hong Kong (CHN)',
               ifelse(cnt == 'Iran, Islamic Republic of', 'Islamic Republic of Iran',
               ifelse(cnt == 'Korea, Republic of', 'Republic of Korea',
               ifelse(cnt == 'Macedonia', 'North Macedonia',
               ifelse(cnt == 'Moldova', 'Republic of Moldova',
               ifelse(cnt == 'Netherlands, The', 'Netherlands (Kingdom of the)',
               ifelse(cnt == 'Serbia', 'Serbia*',
               ifelse(cnt == 'Syria', 'Syrian Arab Republic',
               ifelse(cnt == 'Tanzania', 'United Republic of Tanzania',
               ifelse(cnt == 'Turkey', 'Türkiye',
               ifelse(cnt == 'United Kingdom', 'United Kingdom of Great Britain and Northern Ireland',
               ifelse(cnt == 'Venezuela', 'Bolivarian Republic of Venezuela', cnt))))))))))))))))))))))


df23_correctedReg <- merge(df23_corrected, RegCntNames1a, by = 'cnt', all.x = T)

contrmerge <- df23_correctedReg %>% 
  group_by(bAll.x, bAll.y) %>% 
  tally()
contrmerge
# bAll.x bAll.y     n
#   1      1      139
#   1     NA       13 #Kosovo and NA


# Check NA
nrow(df23_correctedReg) #135
nrow(distinct(df23_correctedReg, cnt)) #132
#NOK


contr <- df23_correctedReg %>% 
  group_by(cnt) %>% 
  mutate(seq = seq_along(cnt),
         maxSeq = max(seq)) %>% ungroup() %>% filter(maxSeq >1)
#same categories for duplicates



df23_correctedReg1 <- df23_correctedReg %>% 
  group_by(cnt) %>% 
  mutate(seq = seq_along(cnt),
         maxSeq = max(seq)) %>% ungroup() %>% 
  filter(maxSeq == seq) %>% dplyr::select(-maxSeq, -seq)

# Check NA
nrow(df23_correctedReg1) #132
nrow(distinct(df23_correctedReg1, cnt)) #132
#OK




# _1.3) Merge with REF ----
df23_correctedReg1a <- df23_correctedReg1 %>% 
  mutate(iso3 = ifelse(cnt == 'Kosovo (S/RES/1244 (1999))', 'KOSovo', iso3))
# %>% 
#   filter(is.na(iso3) == F) #remove Kosovo as already in Serbia*
#132


df23_correctedReg1b <- df23_correctedReg1a %>% 
  dplyr::select(iso3, access_in_practice, -bAll.x, -bAll.y) %>%
  mutate(bAll = 1,
         yr = 2023)


nrow(df23_correctedReg1b) #132
nrow(distinct(df23_correctedReg1b, yr, iso3)) #132



Dat2023ref <- RefDat1a %>% 
  filter(yr == 2022) %>%
  mutate(yr = 2023) 

Dat2023ref <- merge(Dat2023ref, df23_correctedReg1b, by = c('yr', 'iso3'), all = T) 


contrmerge <- Dat2023ref %>% 
  group_by(bAll.x, bAll.y) %>% 
  tally()
contrmerge
# bAll.x bAll.y     n
#   1      1      130
#   1     NA       48
#  NA      1        2 #MUS and Kosovo



StatDat2023ref <- Dat2023ref %>% 
  group_by(yr, access_in_practice) %>% 
  summarise(n_cnt = n(),
            s_poc = sum(s_poc, na.rm = T)) %>% ungroup() %>%
  mutate(TOT_POC = sum(s_poc)) %>% 
  filter(is.na(access_in_practice) == F) %>% 
  mutate(tot_cnt = sum(n_cnt),
         tot_poc = sum(s_poc),
         perc_poc = s_poc/tot_poc,
         coverage = tot_poc/TOT_POC,
         coverage_cnt = n_cnt/tot_cnt)


StatDat2023ref1 <- StatDat2023ref %>% 
  filter(access_in_practice == 'Yes, without any significant restrictions')



# 		2) 2025 data ----
liveli <- read_excel("Data/Raw_Data/Livelihood_EconomicSurvey/2025_Global_Survey_on_Livelihoods_and_Economic_Inclusion_GSLEI_-_all_versions_-_English_-_2025-08-20-10-30-11.xlsx", range =  cell_cols("J:XA"), na = " ") %>% 
  clean_names

Quest <- as.data.frame(ls(liveli))



liveli1 <- liveli %>%
  clean_names() %>% 
  rename(country      = x1_2_country_operation_select_from_list_of_countries_filtered_by_region_if_country_not_in_the_list_specify_below,
         region       = x1_1_what_region_is_your_operation_located_in,
         OtherCountry = x1_2_1_if_other_country_please_specify,
         accesstowork = x8_1_1_refugees) %>% #Access to work in practice
  dplyr::select(country, accesstowork) %>% 
  mutate(bAll = 1)



nrow(liveli1) #56
nrow(distinct(liveli1, country)) #52
#DRC mulitiple times




liveli1a <- liveli1 %>% 
  group_by(country) %>% 
  mutate(seq = seq_along(country),
         maxSeq = max(seq)) %>% ungroup() %>% 
  filter(seq == maxSeq) %>% dplyr::select(-maxSeq, -seq) %>% 
  mutate(country = ifelse(country == 'Bolivia', 'Plurinational State of Bolivia',
                   ifelse(country == 'Iran, Islamic Republic of', 'Islamic Republic of Iran',
                   ifelse(country == 'Korea, Republic of', 'Republic of Korea',
                   ifelse(country == 'Syria', 'Syrian Arab Republic',
                   ifelse(country == 'Tanzania', 'United Republic of Tanzania', 
                   ifelse(country == 'Congo, Democratic Republic of', 'Democratic Republic of the Congo', country)))))))


nrow(liveli1a) #52
nrow(distinct(liveli1a, country)) #52


# Add regions
liveli1b <- merge(liveli1a, RegCntNames1, by.x = 'country', by.y = 'cnt', all.x = T) 

contrmerge <- liveli1b %>% 
  group_by(bAll.x, bAll.y) %>% 
  tally()
contrmerge
# bAll.x bAll.y     n
#   1      1       52


liveli1c <- liveli1b %>% 
  dplyr::select(-bAll.x, -bAll.y) %>% 
  mutate(bAll = 1,
         year = 2025,
         category_modif_liveli_new = ifelse(accesstowork == 'No', 'NoAccess',
                                     ifelse(accesstowork == 'Yes, with restriction (restriction, obstacles, ...)', 'PartialAccess',
                                     ifelse(accesstowork == 'Yes, with minimal or no challenges', 'FullAccess', NA)))) %>% 
  rename(cnt = country) %>% 
  dplyr::select(-accesstowork)



#_2.1) Add Europe 2025 ----
LEI_Eu <- read_excel("Data/Raw_Data/Livelihood_EconomicSurvey/Refugees' legal and practical access to employment and self-employment.xlsx") %>% 
  clean_names() %>% 
  dplyr::select(7, 10) %>% #access to work in practice
  rename(country = country_operation,
         accesswork = do_refugees_have_access_in_practice_to_formal_wage_earning_employment_meaning_the_practical_restrictions_for_refugees_engaging_in_formal_wage_earning_employment_are_low_or_non_existent) %>% 
  mutate(country = ifelse(country == 'Macedonia', 'North Macedonia',
                   ifelse(country == 'Moldova', 'Republic of Moldova',
                   ifelse(country == 'Netherlands, The', 'Netherlands (Kingdom of the)',
                   ifelse(country == 'Serbia', 'Serbia*',
                   ifelse(country == 'United Kingdom', 'United Kingdom of Great Britain and Northern Ireland',
                   ifelse(country == 'Turkiye', 'Türkiye', country)))))),
         bAll = 1)


LEI_Euiso <- merge(LEI_Eu, RegCntNames1, by.x = 'country', by.y = 'cnt', all.x = T)

contrmerge <- LEI_Euiso %>% 
  group_by(bAll.x, bAll.y) %>% 
  tally()
contrmerge
# bAll.x bAll.y     n
#   1      1       46
#   1     NA        2 #Kosovo, ref data already into Serbia*

nrow(LEI_Euiso) #48
nrow(distinct(LEI_Euiso, country)) #46


LEI_Euiso1 <- LEI_Euiso %>% 
  group_by(country) %>% 
  mutate(seq = seq_along(country),
         maxSeq = max(seq)) %>%  ungroup() %>% 
  filter(seq == maxSeq) %>% dplyr::select(-seq, -maxSeq) %>% 
  mutate(iso3       = ifelse(country == 'Kosovo (S/RES/1244 (1999))', 'KOSovo', iso3),
         accesswork = ifelse(country == 'Türkiye', "Yes, with minimal or no challenges", accesswork)) %>% # 20.08.2025: Change asked by Nada 
  filter(is.na(iso3) == F) %>% 
  mutate(year = 2025,
         bAll = 1) %>% 
  rename(category_modif_liveli_new = accesswork,
         cnt = country) %>% 
  dplyr::select(-bAll.x, -bAll.y)
#45 countries


liveli2 <- bind_rows(liveli1c, LEI_Euiso1) %>% 
  rename(yr = year)


nrow(liveli2) #97
nrow(distinct(liveli2, iso3)) #97
#OK


# _2.2) Merge Ref with values ----

Liveli_25ref <- RefDat1a %>% 
  filter(yr == 2024) %>% 
  mutate(yr = 2025) 

Liveli_25ref <- merge(Liveli_25ref, liveli2 , by = c('iso3', 'yr'), all = T)

contrmerge <- Liveli_25ref %>% 
  group_by(bAll.x, bAll.y) %>% 
  tally()
contrmerge
# bAll.x bAll.y     n
#   1      1       96
#   1     NA       85
#  NA      1        1 #Kosovo



Liveli_25refa <- Liveli_25ref %>% 
  rename(access_in_practice = category_modif_liveli_new) %>% 
  mutate(access_in_practice = ifelse(access_in_practice %in% c('Yes, with restrictions'), 'PartialAccess',
                              ifelse(access_in_practice %in% c('Yes, with minimal or no challenges', 'Yes, without any significant restrictions'), 'FullAccess',
                              ifelse(access_in_practice %in% c('No'), 'NoAccess', access_in_practice))))



StatDat2025ref <- Liveli_25refa %>% 
  group_by(yr, access_in_practice) %>% 
  summarise(n_cnt = n(),
            s_poc = sum(s_poc, na.rm = T)) %>% ungroup() %>%
  mutate(TOT_POC = sum(s_poc)) %>% 
  filter(is.na(access_in_practice) == F) %>% 
  mutate(tot_cnt = sum(n_cnt),
         tot_poc = sum(s_poc),
         perc_poc = s_poc/tot_poc,
         coverage = tot_poc/TOT_POC,
         coverage_cnt = n_cnt/tot_cnt)


StatDat2025ref1 <- StatDat2025ref %>% 
  filter(access_in_practice == 'FullAccess')%>% 
  mutate(access_in_practice = ifelse(access_in_practice == 'FullAccess', 'Yes, without any significant restrictions', access_in_practice))




# 3) 2021 Data ----
# _3.1) Original LEI data ----
LEI2021 <- read_excel("Data/Raw_Data/Livelihood_EconomicSurvey/2019_2021 livelihoods global survey raw data_comments.xlsx", sheet = '2021') %>% 
  clean_names() %>% 
  dplyr::select(operational_context, x7, x8, x9, access_in_practice) %>% 
  mutate(bAll = 1)



nrow(LEI2021) #131
nrow(distinct(LEI2021, operational_context)) #127
#Duplicates


contr <- LEI2021 %>% 
  group_by(operational_context) %>% 
  mutate(seq = seq_along(operational_context),
         maxSeq = max(seq)) %>%  ungroup()
# NOK


#Trying to replicate the data from pdf results
LEI2021_a <- LEI2021 %>% 
  mutate(x8 = as.numeric(x8),
         x7 = as.numeric(x7)) %>% 
  filter(is.na(access_in_practice) == F) %>% 
  rename(n_poc = x7, 
         n_ref = x8,
         region = x9)


LEI2021_b <- LEI2021_a[-c(1,2),]


#Without additional data Portugal (added after results were published)
StatLEI2021_workPract <- LEI2021_b %>%
  filter(operational_context != 'Portugal') %>%
  group_by(access_in_practice) %>% 
  summarise(s_poc = sum(n_poc, na.rm = T),
            s_ref = sum(n_ref, na.rm = T)) %>% ungroup() %>% 
  mutate(tot_poc = sum(s_poc),
         perc_poc = round(100*s_poc/tot_poc, 2),
         tot_ref = sum(s_ref),
         perc_ref = round(100*s_ref/tot_ref, 2))


Orig2021 <- StatLEI2021_workPract %>% 
  filter(access_in_practice == 'Yes, without any significant restrictions') %>% 
  rename(`2021` = perc_ref) %>% 
  dplyr::select(access_in_practice, `2021`)




# _3.2) Change in methodology ----
summary(LEI2021_b)

#Duplicate Switzerland and Liechtenstein as their are in the same category

sub_LIE_LEI2021_b <- LEI2021_b %>% 
  filter(operational_context == 'Switzerland & Liechstenstein') %>% 
  rename(cnt = operational_context) %>% 
  mutate(cnt = 'Liechtenstein')


Dat2021 <- LEI2021_b %>% 
  rename(cnt = operational_context) %>% 
  dplyr::select(cnt, n_poc, n_ref, access_in_practice, bAll) %>% 
  mutate(cnt = ifelse(cnt == 'Bolivia', 'Plurinational State of Bolivia',
               ifelse(cnt == "Cote d'Ivoire", "Côte d'Ivoire",
               ifelse(cnt == "Democratic Republic of Congo", "Democratic Republic of the Congo",
               ifelse(cnt == "GUINEA BISSAU", 'Guinea-Bissau',
               ifelse(cnt == "Haïti", 'Haiti',
               ifelse(cnt == "Iran", 'Islamic Republic of Iran',
               ifelse(cnt == 'Macedonia', 'North Macedonia',
               ifelse(cnt == 'Moldova', 'Republic of Moldova',
               ifelse(cnt == 'Netherlands', 'Netherlands (Kingdom of the)',
               ifelse(cnt == 'Republic of Congo', 'Republic of the Congo',
               ifelse(cnt == 'Serbia', 'Serbia*',
               ifelse(cnt == 'Syria', 'Syrian Arab Republic',
               ifelse(cnt == 'Tanzania', 'United Republic of Tanzania',
               ifelse(cnt == 'Turkey', 'Türkiye',
               ifelse(cnt == 'United States', 'United States of America', 
               ifelse(cnt == 'Botswana - Dukwi', 'Botswana', 
               ifelse(cnt == 'CAMEROON', 'Cameroon', 
               ifelse(cnt == "China (responses related to China's mainland)", 'China',
               ifelse(cnt == 'Congo', 'Republic of the Congo', 
               ifelse(cnt == 'ECUADOR', 'Ecuador', 
               ifelse(cnt == 'Korea', 'Republic of Korea', 
               ifelse(cnt == 'Switzerland & Liechstenstein', 'Switzerland', # 2 countries in 1
               ifelse(cnt == 'United Kingdom & Northern Ireland', 'United Kingdom of Great Britain and Northern Ireland',cnt)))))))))))))))))))))))) %>% 
  bind_rows(sub_LIE_LEI2021_b)


Dat2021a <- merge(Dat2021, RegCntNames1a, by = 'cnt', all.x = T)

contrmerge <- Dat2021a %>% 
  group_by(bAll.x, bAll.y) %>% 
  tally()
contrmerge
# bAll.x bAll.y     n
#   1      1      127
#   1     NA        1



Dat2021b <- Dat2021a %>% 
  filter(is.na(access_in_practice) == F) %>% 
  mutate(iso3 = ifelse(cnt == 'Kosovo', 'KOSovo', iso3))
#ok 128, Kosovo removed (already in Serbia*)


Dat2021c <- Dat2021b %>% 
  dplyr::select(iso3, access_in_practice) %>% 
  mutate(bAll = 1,
         yr = 2021)


nrow(Dat2021c) #128
nrow(distinct(Dat2021c, yr, iso3)) #123
#Duplicates


contr <- Dat2021c %>% 
  group_by(yr, iso3) %>% 
  mutate(seq = seq_along(iso3),
         maxSeq = max(seq)) %>% filter(maxSeq > 1)
#only SSD is changing (2 times no, 1 time Yes, with restrictions)
#just takes No



Dat2021d <- Dat2021c %>% 
  group_by(yr, iso3) %>% 
  mutate(seq = seq_along(iso3),
         maxSeq = max(seq)) %>% 
  filter(maxSeq == seq)
#123


Dat2021ref <- RefDat1a %>% 
  filter(yr == 2020) %>% 
  mutate(yr = 2021) 

Dat2021ref <- merge(Dat2021ref, Dat2021d, by = c('yr', 'iso3'), all = T) 


contrmerge <- Dat2021ref %>% 
  group_by(bAll.x, bAll.y) %>% 
  tally()
contrmerge
# bAll.x bAll.y     n
#   1      1      122
#   1     NA       55
#  NA      1        1


StatDat2021ref <- Dat2021ref %>% 
  group_by(yr, access_in_practice) %>% 
  summarise(n_cnt = n(),
            s_poc = sum(s_poc, na.rm = T)) %>% ungroup() %>%
  mutate(TOT_POC = sum(s_poc)) %>% 
  filter(is.na(access_in_practice) == F) %>% 
  mutate(tot_cnt = sum(n_cnt),
         tot_poc = sum(s_poc),
         perc_poc = s_poc/tot_poc,
         coverage = tot_poc/TOT_POC,
         coverage_cnt = n_cnt/tot_cnt)


StatDat2021ref1 <- StatDat2021ref %>% 
  filter(access_in_practice == 'Yes, without any significant restrictions') 




# 4) 2019 Data ----
# _4.1) Original LEI data ----
LEI2019 <- read_excel("Data/Raw_Data/Livelihood_EconomicSurvey/2019_2021 livelihoods global survey raw data_comments.xlsx", sheet = '2019') %>% 
  clean_names() %>% 
  dplyr::select(operational_context, x8, x9, legal_rights, access_in_practice, x24) %>% 
  mutate(bAll = 1)


nrow(LEI2019) #122
nrow(distinct(LEI2019, operational_context)) #121


contr <- LEI2019 %>% 
  group_by(operational_context) %>% 
  mutate(seq = seq_along(operational_context),
         maxSeq = max(seq)) %>%  ungroup() %>% filter(maxSeq > 1)
# OK, no duplicates (Only NA)



contr <- LEI2019 %>% 
  filter(access_in_practice == 'Yes, without any significant restrictions' | operational_context == 'France') #in other file, france is included into full access to work

nrow(contr) #53
nrow(distinct(contr, operational_context)) #53

contr1 <- contr %>%
  mutate(x8 = as.numeric(x8),
         x9 = as.numeric(x9)) %>% 
  summarise(s_poc = sum(x8, na.rm = T),
            s_ref = sum(x9, na.rm = T)) 
#diff comes from France


LEI2019_a <- LEI2019 %>%
  mutate(x8 = as.numeric(x8),
         x9 = as.numeric(x9)) %>% 
  rename(n_poc = x8, 
         n_ref = x9)

LEI2019_b <- LEI2019_a[-c(1,2),]



#Original results from pdf
LEI2019_c <- LEI2019_b %>% 
  mutate(access_in_practice = ifelse(operational_context == 'France', 'Yes, without any significant restrictions', access_in_practice))



StatLEI2019_workInPractice <- LEI2019_c %>%
  filter(is.na(access_in_practice) == F) %>% 
  group_by(access_in_practice) %>% 
  summarise(s_poc = sum(n_poc, na.rm = T),
            s_ref = sum(n_ref, na.rm = T)) %>% ungroup() %>% 
  mutate(tot_poc = sum(s_poc),
         perc_poc = round(100*s_poc/tot_poc, 2),
         tot_ref = sum(s_ref),
         perc_ref = round(100*s_ref/tot_ref, 2))
#OK same as in pdf --> FRANCE
#The main difference comes from the classification for France

Orig2019 <- StatLEI2019_workInPractice %>% 
  filter(access_in_practice == 'Yes, without any significant restrictions') %>% 
  rename(`2019` = perc_ref) %>% 
  dplyr::select(access_in_practice, `2019`)


#_4.2) Change in methodology ----
summary(LEI2019_b)

Dat2019 <- LEI2019_b %>% 
  rename(cnt = operational_context) %>% 
  dplyr::select(cnt, access_in_practice, bAll) %>% 
  mutate(cnt = ifelse(cnt == 'Bolivia', 'Plurinational State of Bolivia',
               ifelse(cnt == "Cote d'Ivoire", "Côte d'Ivoire",
               ifelse(cnt == "DRC", "Democratic Republic of the Congo",
               ifelse(cnt == "Guinea Bissau", 'Guinea-Bissau',
               ifelse(cnt == "Haïti", 'Haiti',
               ifelse(cnt == "Islamic republic of Iran", 'Islamic Republic of Iran',
               ifelse(cnt == 'Macedonia', 'North Macedonia',
               ifelse(cnt == 'Moldova', 'Republic of Moldova',
               ifelse(cnt == 'Netherlands', 'Netherlands (Kingdom of the)',
               ifelse(cnt == 'Republic of Congo', 'Republic of the Congo',
               ifelse(cnt == 'Serbia', 'Serbia*',
               ifelse(cnt == 'Syria', 'Syrian Arab Republic',
               ifelse(cnt == 'Tanzania', 'United Republic of Tanzania',
               ifelse(cnt == 'Turkey', 'Türkiye',
               ifelse(cnt == 'United States', 'United States of America', cnt))))))))))))))))


Dat2019a <- merge(Dat2019, RegCntNames1a, by = 'cnt', all.x = T)

contrmerge <- Dat2019a %>% 
  group_by(bAll.x, bAll.y) %>% 
  tally()
contrmerge
# bAll.x bAll.y     n
#   1      1      118
#   1     NA        2


Dat2019b <- Dat2019a %>% 
  mutate(iso3 = ifelse(cnt == 'Kosovo', 'KOSovo', iso3)) %>% 
  filter(is.na(access_in_practice) == F)
#ok 108


Dat2019c <- Dat2019b %>% 
  dplyr::select(iso3, access_in_practice) %>% 
  mutate(bAll = 1,
         yr = 2019)



Dat2019ref <- RefDat1a %>% 
  filter(yr == 2018) %>%
  mutate(yr = 2019) 

Dat2019ref <- merge(Dat2019ref, Dat2019c, by = c('yr', 'iso3'), all = T) 


contrmerge <- Dat2019ref %>% 
  group_by(bAll.x, bAll.y) %>% 
  tally()
contrmerge
# bAll.x bAll.y     n
#   1      1      107
#   1     NA       69
#  NA      1        1


StatDat2019ref <- Dat2019ref %>% 
  group_by(yr, access_in_practice) %>% 
  summarise(n_cnt = n(),
            s_poc = sum(s_poc, na.rm = T)) %>% ungroup() %>%
  mutate(TOT_POC = sum(s_poc)) %>% 
  filter(is.na(access_in_practice) == F) %>% 
  mutate(tot_cnt = sum(n_cnt),
         tot_poc = sum(s_poc),
         perc_poc = s_poc/tot_poc,
         coverage = tot_poc/TOT_POC,
         coverage_cnt = n_cnt/tot_cnt)


StatDat2019ref1 <- StatDat2019ref %>% 
  filter(access_in_practice == 'Yes, without any significant restrictions')





# 5) Results together ----
# _5.1) Originals ----

OrigData <- Orig2019 %>%
  full_join(Orig2021, by = "access_in_practice") %>%
  full_join(Orig2023, by = "access_in_practice")



# _5.2) Bind data ----
FullDat <- bind_rows(StatDat2019ref1, StatDat2021ref1, StatDat2023ref1, StatDat2025ref1)






# 6) DATA CHECK ----
#Compairing 2023 with 2025

Changliveli2 <- liveli2 %>% 
  rename(access_in_practice_25 = category_modif_liveli_new) %>% 
  mutate(access_in_practice_25 = ifelse(access_in_practice_25 %in% c('Yes, with minimal or no challenges', 'FullAccess'), 'Yes, without any significant restrictions', 
                                 ifelse(access_in_practice_25 == 'PartialAccess', 'Yes, with restrictions',
                                 ifelse(access_in_practice_25 == 'NoAccess', 'No', access_in_practice_25))))

Check23_25 <- merge(Changliveli2, df23_correctedReg1b, by = 'iso3', all.x = T)

contrmerge <- Check23_25 %>% 
  group_by(bAll.x, bAll.y) %>% 
  tally()
contrmerge
# bAll.x bAll.y     n
#   1      1       95
#   1     NA        2

Check23_25a <- Check23_25 %>% 
  rename(access_in_practice_23 = access_in_practice) %>% 
  mutate(bCheck = ifelse(access_in_practice_25 != access_in_practice_23, 1, 0)) %>% 
  dplyr::select(iso3, cnt, access_in_practice_23, access_in_practice_25, bCheck)


table(Check23_25a$bCheck)
# 0  1 
# 82 13 


if (bExport == 1){
  write.xlsx(Check23_25a, 'Analysis/21_Results/ControlDoc_AccWorkPract_LEI23_LEI25_20250820huae.xlsx')
}