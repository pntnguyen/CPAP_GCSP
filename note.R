library(skimr)

setwd("../Data")

raw_df <- read_excel("Cancer_Prevention_Awareness_Raw_Data_251203.xlsx", sheet = "DATA")

## Screening question

raw_df |>
  skim

## Select only variables have completion rate is 1


# complete_vars <- skim(raw_df) |>
#   filter(complete_rate == 1) |>
#   pull(skim_variable)
#
# raw_df %<>%
#   select(complete_vars)

## Recode variables

raw_df %<>%
  mutate(
    gender = case_when(SQ1 == 1 ~ "Male", .default = "Female"),
    age = SQ2,
    city = case_when(
      SQ3 == 1  ~ "Seoul",
      SQ3 == 2  ~ "Busan",
      SQ3 == 3  ~ "Daegu",
      SQ3 == 4  ~ "Incheon",
      SQ3 == 5  ~ "Gwangju",
      SQ3 == 6  ~ "Daejeon",
      SQ3 == 7  ~ "Ulsan",
      SQ3 == 8  ~ "Sejong",
      SQ3 == 9  ~ "Gyeonggi",
      SQ3 == 10 ~ "Gangwon",
      SQ3 == 11 ~ "Chungcheongbuk",
      SQ3 == 12 ~ "Chungcheongnam",
      SQ3 == 13 ~ "Jeollabuk",
      SQ3 == 14 ~ "Jeollanam",
      SQ3 == 15 ~ "Gyeongsangbuk",
      SQ3 == 16 ~ "Gyeongsangnam",
      SQ3 == 17 ~ "Jeju"
    ),
    city_size = case_when(
      SQ3_1 == 1 ~ "Large City (Metropolitan)",
      SQ3_1 == 2 ~ "Medium/Small City",
      SQ3_1 == 3 ~ "County Area (including Eup/Myeon/Dong)"
    )
  )


## Question 3: The following questions are about your perception of cancer

raw_df |>
  select(paste0("Q3_",1:14))

## Question 4: To what extent do you agree with the following statements regarding "cancer"?

raw_df |>
  select(paste0("Q4_",1:4))

## Q7: What are your thoughts on "practicing lifestyle habits for cancer prevention"?

raw_df |>
  select(Q7)

## Q8: Aware of Cancer prevention guildeline

raw_df |>
  select(Q8)

## Q9_1 : 3 rules in order that you think are the easiest to follow among the cancer prevention rules

raw_df |>
  select(Q9_1_1,Q9_1_2,Q9_1_3)

## Q9_2 : 3 rules in order that you think are the most difficult to follow among the cancer prevention rules

raw_df |>
  select(Q9_2_1,Q9_2_2,Q9_2_3)

## Q9_3 : 3 rules in order that you think do not help much with cancer prevention among the cancer prevention rules

raw_df |>
  select(Q9_3_1,Q9_3_2,Q9_3_3)


raw_df |>
  pull(DQ1_2_1_1) |>
  unique()


###

raw_df |>
  filter(Q14 == 3) |>
  select(paste0("Q20_", 1:4)) |>
  skim()

raw_df |>
  mutate(row_id = row_number()) |>
  filter(Q14 == 3) |>
  filter(if_all(paste0("Q20_", 1:4), is.na)) |>
  select(row_id, Q14, paste0("Q20_", 1:4))


raw_df |>
  group_by(city,Q101) |>
  count() |>
  ungroup() |>
  group_by(city) |>
  mutate(per = n/sum(n)) |>
  filter(Q101 == 1)

###

library(ggplot2)
library(sf)
library(stringr)


korea_map <- st_read(dsn = "./gadm41_KOR.gpkg", layer = "ADM_ADM_1") |>
  sf::st_transform(5179) |>
  mutate(city = str_extract(NAME_1, "^[^-]+"))

korea_map |>
  ggplot() +
  geom_sf()+
  theme_void()

### Population characteristic

library(dplyr)
library(gtsummary)

df |>
  mutate(
    gender         = factor(gender, levels = c("Male", "Female")),
    city_size      = factor(
      city_size,
      levels = c(
        "Large City (Metropolitan)",
        "Medium/Small City",
        "County Area (including Eup/Myeon/Dong)"
      )
    ),
    marital_status = factor(
      marital_status,
      levels = c("Single", "Married", "Disvorced/Separated/Widowed")
    ),
    education      = factor(
      education,
      levels = c("Middle school or less", "High school", "College or more")
    ),
    monthly_income = factor(monthly_income, levels = c("<3", "3-6", "6 ≤"))
  ) |>
  tbl_summary(
    label = list(gender = "Gender",
                 city_size = "Region",
                 marital_status = "Marital Status",
                 education = "Education level",
                 monthly_income = "Monthly income (million KRW)"),
    include = c(gender, city_size, marital_status, education, monthly_income)
  )


## age distribution people smoking

df |>
  filter(Q14 == 3) |>
  group_by(city,Q16) |>
  count() |>
  ungroup() |>
  group_by(city) |>
  mutate(per = n/sum(n)) |>
  ggplot(aes(y = per, x = factor(Q16)))+
  geom_col()+
  facet_wrap(~city)+
  coord_flip()

## pack year

### Number of pack year by age

df |>
  mutate(
    ## pack year of ex-smoker
    ex_pack_year = (ex_smkr_daily_avg/20)*ex_smkr_years,
    ## pack year of current smoker
    cr_pack_year = (cr_smkr_daily_avg/20)*cr_smkr_years,
  ) |>
  filter(Q14 == 2) |>
  ggplot(aes(x = age, y = ex_pack_year))+
  geom_point()+
  theme_minimal()

df |>
  mutate(
    ## pack year of ex-smoker
    ex_pack_year = (ex_smkr_daily_avg/20)*ex_smkr_years,
    ## pack year of current smoker
    cr_pack_year = (cr_smkr_daily_avg/20)*cr_smkr_years,
  ) |>
  filter(Q14 == 3) |>
  ggplot(aes(x = age, y = cr_pack_year))+
  geom_point()+
  theme_minimal()

df |>
  mutate(
    ex_pack_year = (ex_smkr_daily_avg / 20) * ex_smkr_years,
    cr_pack_year = (cr_smkr_daily_avg / 20) * cr_smkr_years,
    pack_year = case_when(Q14 == 2 ~ ex_pack_year,
                          Q14 == 3 ~ cr_pack_year),
    smoking_status = case_when(Q14 == 2 ~ "Ex-smoker",
                               Q14 == 3 ~ "Current smoker")
  ) |>
  filter(Q14 %in% c(2, 3)) |>
  ggplot(aes(x = age, y = pack_year)) +
  geom_point() +
  facet_wrap(~ smoking_status) +
  labs(x = "Age", y = "Pack-years") +
  theme_bw()

## pack year by city

pack_by_city <- df |>
  mutate(
    ex_pack_year = (ex_smkr_daily_avg / 20) * ex_smkr_years,
    cr_pack_year = (cr_smkr_daily_avg / 20) * cr_smkr_years,
    pack_year = case_when(Q14 == 2 ~ ex_pack_year,
                          Q14 == 3 ~ cr_pack_year),
    smoking_status = case_when(Q14 == 2 ~ "Ex-smoker",
                               Q14 == 3 ~ "Current smoker")
  ) |>
  filter(Q14 %in% c(2, 3)) |>
  group_by(city, smoking_status) |>
  summarise(
    mean_pack  = mean(pack_year, na.rm = TRUE),
    .groups = "drop"
  )

pack_by_city |>
  left_join(korea_map, ., by = "city") |>
  ggplot(aes(
    fill = mean_pack,
    geometry = geom,
    label = city
  )) +
  geom_sf() +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Classic Blue",
    na.value = "white",
    name = "Mean of pack years"
  ) +
  geom_sf_text(size = 3, color = "black") +
  facet_wrap(~smoking_status) +
  theme_bw() +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"
  )

### awareness by city

## Q13 Do you think smoking causes cancer? (1: Yes, 2: No)

## Practice is ex-smoker?

df |>
  group_by(city,Q13) |>
  count() |>
  ungroup() |>
  group_by(city) |>
  mutate(per = n/sum(n)) |>
  filter(Q13 == 1) |>
  ggplot(aes(x = city, y = per))+
  geom_col()

df |>
  mutate(
    smk_aw = case_when(Q13 == 2 ~ 0,.default = 1),
    practice = case_when(Q14 == 2 ~ 1, .default = 0)
  ) |>
  ggplot(aes(x = age, y = smk_aw))+
  geom_smooth(method = "glm",
              method.args = list(family = binomial),
              se = TRUE) +
  # facet_wrap(~city)+
  scale_y_continuous(limits = c(0,1))+
  theme_bw()

library(tidyr)
df |>
  mutate(
    Awareness = case_when(Q13 == 2 ~ 0, .default = 1),
    Practice  = case_when(Q14 == 2 ~ 1, .default = 0)
  ) |>
  pivot_longer(c(Awareness, Practice),
               names_to = "outcome",
               values_to = "value") |>
  ggplot(aes(x = age, y = value, color = outcome, fill = outcome)) +
  geom_smooth(method = "glm",
              method.args = list(family = binomial),
              se = TRUE) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
  # facet_wrap(~city)+
  labs(x = "Age", y = "Predicted probability", color = NULL, fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")


#### Spatial distribution

df |>
  group_by(city) |>
  count() |>
  ungroup() |>
  mutate(per = n/sum(n)) |>
  left_join(korea_map,.,by = "city") |>
  ggplot(aes(fill = per,geometry = geom,label = city)) +
  geom_sf() +
  theme_void()+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    na.value="white",
                                    labels = scales::label_percent(),
                                    name = "Percentage of samples"
                                    )+
  geom_sf_text(size = 3, color = "black")+
  theme_void()+
  theme(legend.position="bottom")



#### Age distribution

raw_df |>
  select(age,city) |>
  ggplot(aes(age))+
  geom_histogram(bins = 20)+
  facet_wrap(~city)


#### cancer smoking percentage

raw_df |>
  group_by(city,Q14) |>
  count() |>
  ungroup() |>
  group_by(city) |>
  mutate(per = n/sum(n)) |>
  filter(Q14 == 3) |>
  ungroup() |>
  select(-c(Q14,n)) |>
  left_join(korea_map,.,by = "city") |>
  ggplot(aes(fill = per,geometry = geom,label = city)) +
  geom_sf() +
  theme_void()+
  paletteer::scale_fill_paletteer_c("ggthemes::Classic Red",
                                    na.value="white",
                                    labels = scales::label_percent(),
                                    name = "Percentage of current smoking"
  )+
  geom_sf_text(size = 3, color = "yellow")+
  theme_void()+
  theme(legend.position="bottom")

### Knowledge

## Q8: Are you aware that there are guidelines for cancer prevention?

### 1.	I have heard of the cancer prevention rules, but I do not know the details well.
### 2.	I have heard of the cancer prevention rules and I know their contents.
### 3.	I have never heard of the cancer prevention rules.

knlge <- raw_df |>
  group_by(city, Q8) |>
  count() |>
  ungroup() |>
  group_by(city) |>
  mutate(knlge_per = n / sum(n)) |>
  ungroup() |>
  filter(Q8 == 2) |>
  select(-c(Q8, n))

# knlge |>
#   left_join(korea_map, ., by = "city") |>
#   ggplot(aes(
#     fill = per,
#     geometry = geom,
#     label = city
#   )) +
#   geom_sf() +
#   paletteer::scale_fill_paletteer_c(
#     "ggthemes::Classic Blue",
#     na.value = "white",
#     labels = scales::label_percent(),
#     name = "Percentage of \nsmoking prevention \nknowledge"
#   ) +
#   geom_sf_text(size = 3, color = "black") +
#   theme_void() +
#   theme()

### Awareness

### Q13: Do you think smoking causes cancer?
### 1: Yes, 2: No

smk_aw <- raw_df |>
  group_by(city, Q13) |>
  count() |>
  ungroup() |>
  group_by(city) |>
  mutate(smk_aw_per = n / sum(n)) |>
  ungroup() |>
  filter(Q13 == 1) |>
  select(-c(Q13, n))

smk_aw |>
  left_join(korea_map, ., by = "city") |>
  ggplot(aes(
    fill = per,
    geometry = geom,
    label = city
  )) +
  geom_sf() +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Classic Blue",
    na.value = "white",
    labels = scales::label_percent(),
    name = "Percentage of \nsmoking prevention \nawareness"
  ) +
  geom_sf_text(size = 3, color = "black") +
  theme_void() +
  theme()

### Secondhand smooking

# Q23 Are you aware that exposure to other people's tobacco smoke (secondhand smoke)
# is a Group 1 carcinogen?

secsmoke_aw <- raw_df |>
  group_by(city, Q23) |>
  count() |>
  ungroup() |>
  group_by(city) |>
  mutate(secsmoke_aw_per = n / sum(n)) |>
  ungroup() |>
  filter(Q23 == 1) |>
  select(-c(Q23, n))

# secsmoke_aw |>
#   left_join(korea_map, ., by = "city") |>
#   ggplot(aes(
#     fill = per,
#     geometry = geom,
#     label = city
#   )) +
#   geom_sf() +
#   paletteer::scale_fill_paletteer_c(
#     "ggthemes::Classic Blue",
#     na.value = "white",
#     labels = scales::label_percent(),
#     name = "Percentage of \n2nd hand smoking \nprevention awareness"
#   ) +
#   geom_sf_text(size = 3, color = "black") +
#   theme_void() +
#   theme()

## Electronic cigarettes
# Do you think nicotine-free electronic cigarettes are also harmful?
# 1 Yes 2 No

elec_aw <- raw_df |>
  group_by(city, Q21) |>
  count() |>
  ungroup() |>
  group_by(city) |>
  mutate(elect_aw_per = n / sum(n)) |>
  ungroup() |>
  filter(Q21 == 1) |>
  select(-c(Q21, n))

## practice

raw_df |>
  group_by(city, Q14) |>
  count() |>
  ungroup() |>
  group_by(city) |>
  mutate(cur_smk_per = n / sum(n)) |>
  ungroup() |>
  # filter(Q14 == 2) |>
  select(-c(n)) |>
  mutate(smoker = case_when(Q14 == 1 ~ "Non-smoker",
                            Q14 == 2 ~ "Ex-smoker",
                            .default = "Current_smoker")) |>
  filter(Q14 != 1) |>
  left_join(korea_map, ., by = "city") |>
  ggplot(aes(
    fill = cur_smk_per,
    geometry = geom,
    label = city
  )) +
  geom_sf() +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Classic Blue",
    na.value = "white",
    labels = scales::label_percent(),
    name = "Percentage of \nex-smoker \ncurrent-smoker"
  ) +
  geom_sf_text(size = 3, color = "black") +
  facet_wrap(~smoker) +
  theme_bw() +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  )



df |>
  mutate(
    gender         = factor(gender, levels = c("Male", "Female")),
    city_size      = factor(
      city_size,
      levels = c(
        "Large City (Metropolitan)",
        "Medium/Small City",
        "County Area (including Eup/Myeon/Dong)"
      )
    ),
    marital_status = factor(
      marital_status,
      levels = c("Single", "Married", "Disvorced/Separated/Widowed")
    ),
    education      = factor(
      education,
      levels = c("Middle school or less", "High school", "College or more")
    ),
    monthly_income = factor(monthly_income, levels = c("<3", "3-6", "6 ≤")),
    awareness = case_when(Q13 == 1 ~ "Aware",.default = "No aware"),
    smoker = case_when(Q14 == 1 ~ "Non-smoker",
                       Q14 == 2 ~ "Ex-smoker",
                       .default = "Current smoker")
  ) |>
  filter(smoker %in% c("Ex-smoker","Current smoker")) |>
  tbl_summary(
    by = smoker,
    label = list(
      gender = "Gender",
      city_size = "Region",
      marital_status = "Marital Status",
      education = "Education level",
      monthly_income = "Monthly income (million KRW)",
      awareness = "Awareness"
    ),
    include = c(gender, city_size, marital_status, education, monthly_income, awareness)
  ) |>
  add_p() |>
  bold_labels()


##

df |>
  filter(Q14 %in% c(2,3)) |>
  group_by(Q14) |>
  count(Q18)



df |>
  mutate(smoker = case_when(Q14 == 1 ~ "Non-smoker",
                            Q14 == 2 ~ "Ex-smoker",
                            .default = "Current smoker")) |>
  select(Q15,smoker) |>
  filter(smoker != "Non-smoker") |>
  group_by(smoker) |>
  count(Q15)

## Smoking status

## Pack-year = $\frac{Cigarrettes \ smoked \ per \ day}{20} \times Years \ smoked$

### By age



df |>
  mutate(
    ex_pack_year = (ex_smkr_daily_avg / 20) * ex_smkr_years,
    cr_pack_year = (cr_smkr_daily_avg / 20) * cr_smkr_years,
    pack_year = case_when(Q14 == 2 ~ ex_pack_year,
                          Q14 == 3 ~ cr_pack_year),
    smoking_status = case_when(Q14 == 2 ~ "Ex-smoker",
                               Q14 == 3 ~ "Current smoker")
  ) |>
  filter(Q14 %in% c(2, 3)) |>
  ggplot(aes(x = age, y = pack_year)) +
  geom_point() +
  facet_wrap(~ smoking_status) +
  labs(x = "Age", y = "Pack-years") +
  theme_bw()


### By city

## Mean of pack year by city

pack_by_city <- df |>
  mutate(
    ex_pack_year = (ex_smkr_daily_avg / 20) * ex_smkr_years,
    cr_pack_year = (cr_smkr_daily_avg / 20) * cr_smkr_years,
    pack_year = case_when(Q14 == 2 ~ ex_pack_year,
                          Q14 == 3 ~ cr_pack_year),
    smoking_status = case_when(Q14 == 2 ~ "Ex-smoker",
                               Q14 == 3 ~ "Current smoker")
  ) |>
  filter(Q14 %in% c(2, 3)) |>
  group_by(city, smoking_status) |>
  summarise(
    mean_pack  = mean(pack_year, na.rm = TRUE),
    .groups = "drop"
  )

## Mean packs of ex-smoker


pack_by_city |>
  filter(smoking_status == "Ex-smoker") |>
  left_join(korea_map, ., by = "city") |>
  ggplot(aes(
    fill = mean_pack,
    geometry = geom,
    label = city
  )) +
  geom_sf() +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Classic Blue",
    na.value = "white",
    name = "Mean of pack years"
  ) +
  geom_sf_text(size = 3, color = "black") +
  theme_bw() +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"
  )


## Mean packs of current smoker

pack_by_city |>
  filter(smoking_status == "Current smoker") |>
  left_join(korea_map, ., by = "city") |>
  ggplot(aes(
    fill = mean_pack,
    geometry = geom,
    label = city
  )) +
  geom_sf() +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Classic Blue",
    na.value = "white",
    name = "Mean of pack years"
  ) +
  geom_sf_text(size = 3, color = "black") +
  theme_bw() +
  theme(
    axis.text  = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"
  )


### who ever-smoke

ever_smoke_df <- df |>
  filter(Q14 != 1) |>
  mutate(
    smoker = case_when(Q14 == 2 ~ "Ex-smoker", .default = "Current smoker"),
    quitting_status = case_when(smoker == "Current smoker" & attitude_to_quit == "Yes" ~ "Fail to quit",
                                smoker == "Ex-smoker" & attitude_to_quit == "Yes" ~ "Success to quit",
                                .default = "Current smoker & dont want"),
    failed_cess = case_when(quitting_status == "Fail to quit" ~ 1,
                            .default = 0)
  ) |>
  filter(quitting_status != "Current smoker & dont want")

ever_smoke_df |>
  mutate(
    pack_year = case_when(
      smoker == "Ex-smoker" ~ (ex_smkr_daily_avg / 20) * ex_smkr_years,
      smoker == "Current smoker" ~ (cr_smkr_daily_avg / 20) * cr_smkr_years
    )
  ) |>
  tbl_summary(
    by = quitting_status,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    label = list(smk_educated = "Received education of smoking cessation",
                 pack_year = "Pack year"),
    include = c(age, pack_year, quitting_status, smk_educated)
  ) |>
  add_p()

ever_smoke_df |>
  pull(smk_educated) |>
  unique()

