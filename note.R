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
    ),
    bmi = weight / (height / 100)^2,
    current_ocupation = case_when(
      DQ1_3_1 == 1 ~ "Day shift",
      DQ1_3_1 == 2 ~ "Shift work",
      DQ1_3_1 == 3 ~ "Night shift",
      .default = "Not applicable"
    ),
    smk_educated = factor(smk_educated, levels = c("Yes", "No")),
    personal_cc_exp = factor(
      personal_cc_exp,
      levels = c(1, 0),
      labels = c("Yes", "No")
    ),
    family_cc_exp = factor(
      family_cc_exp,
      levels = c(1, 0),
      labels = c("Yes", "No")
    ),
    close_to_cc_exp = factor(
      close_to_cc_exp,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
  ) |>
  tbl_summary(
    by = quitting_status,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    label = list(
      smk_educated = "Received education of smoking cessation",
      pack_year = "Pack year",
      current_ocupation = "Current ocupation",
      personal_cc_exp = "Personal experienced with cancer",
      family_cc_exp = "Family experienced with cancer",
      close_to_cc_exp = "Close relationship experienced with cancer"
    ),
    type  = list(
      c(smk_educated, personal_cc_exp, family_cc_exp, close_to_cc_exp) ~ "categorical"
    ),
    include = c(
      age,
      bmi,
      pack_year,
      quitting_status,
      smk_educated,
      current_ocupation,
      personal_cc_exp,
      family_cc_exp,
      close_to_cc_exp
    )
  ) |>
  add_p()


ever_smoke_df |>
  pull(smk_educated) |>
  unique()

## practice

## Q37. Do you currently have a colorful, balanced diet?
## Q53. Where do you think your drinking frequency and the amount you drink fall under the following categories?
# 1.	I drink a light/small amount.
# 2.	I drink a moderate/reasonable amount.
# 3.	I drink a lot/heavy amount.
# 4.	It is at an addiction level.
# 5.	I do not drink alcohol.

## Q64. Including all physical activities and exercise in your daily life,
## do you walk or exercise for more than 30 minutes a day at least 5 times a week,
## enough to sweat?

### Q65_1_1,Q65_1_2: high-intensity physical activity days/week, minutes/days
### Q65_2_1,Q65_2_2: moderate-intensity physical activity days/week, minutes/days

### Q70 Are you maintaining an appropriate healthy weight?

ever_smoke_df |>
  mutate(
    diet_practice = case_when(Q37 == 1 ~ "Yes", .default = "No"),
    drinking_practice = case_when(
      Q53 %in% c(1, 2) ~ "Light/Moderate",
      Q53 %in% c(3, 4) ~ "Heavy/Addiction",
      .default = "Not drink"
    ),
    exercise_practice = case_when(Q64 == 1 ~ "Yes", .default = "No"),
    high_inten_min_week = Q65_1_1 * Q65_1_2,
    moderate_inten_min_week = Q65_2_1 * Q65_2_2,
    healthy_weight_practice = case_when(Q70 == 1 ~ "Yes", .default = "No")
  ) |>
  mutate(across(
    c(diet_practice, exercise_practice, healthy_weight_practice),
    ~ factor(.x, levels = c("Yes", "No"))
  ),
  drinking_practice = factor(
    drinking_practice,
    levels = c("Not drink", "Light/Moderate", "Heavy/Addiction")
  )) |>
  tbl_summary(
    by = quitting_status,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"
    ),
    type  = list(
      c(
        diet_practice,
        drinking_practice,
        exercise_practice,
        healthy_weight_practice
      ) ~ "categorical"
    ),
    label = list(
      diet_practice = "Diet practice",
      drinking_practice = "Drinking practice",
      exercise_practice = "Exercise practice",
      high_inten_min_week = "High-intensity physical activity minutes per week",
      moderate_inten_min_week = "Moderate-intensity physical activity minutes per week",
      healthy_weight_practice = "Healthy weight practice"
    ),
    digits = all_continuous() ~ 2,
    include = c(
      diet_practice,
      drinking_practice,
      exercise_practice,
      high_inten_min_week,
      moderate_inten_min_week,
      healthy_weight_practice
    )
  ) |>
  add_p()


###

korea_map_district <- st_read(dsn = "../Data/gadm41_KOR.gpkg", layer = "ADM_ADM_2") |>
  sf::st_transform(5179) |>
  mutate(city = str_extract(NAME_1, "^[^-]+"))

korea_map_district |>
  ggplot()+
  geom_sf()

df |>
  select(city,SQ3_2_1,SQ3_2_2,city_size) |>
  unique() |>
  # filter(city == "Busan") |>
  pull(SQ3_2_2)

korea_map_district |>
  select(city,NAME_1,NL_NAME_1,NAME_2,TYPE_2) |>
  # filter(city == "Busan") |>
  pull(NAME_2)

df |>
  select(city,SQ3_2_2,city_size) |>
  unique() |>
  mutate(
    district_en = case_when(
      SQ3_2_2 == "북구"     ~ "Buk",
      SQ3_2_2 == "사상구"   ~ "Sasang",
      SQ3_2_2 == "해운대구" ~ "Haeundae",
      SQ3_2_2 == "동래구"   ~ "Dongnae",
      SQ3_2_2 == "부산진구" ~ "Busanjin",
      SQ3_2_2 == "사하구"   ~ "Saha",
      SQ3_2_2 == "중구"     ~ "Jung",
      SQ3_2_2 == "연제구"   ~ "Yeonje",
      SQ3_2_2 == "동구"     ~ "Dong",
      SQ3_2_2 == "서구"     ~ "Seo",
      SQ3_2_2 == "수영구"   ~ "Suyeong",
      SQ3_2_2 == "금정구"   ~ "Geumjeong",
      SQ3_2_2 == "남구"     ~ "Nam",
      SQ3_2_2 == "강서구"   ~ "Gangseo",
      SQ3_2_2 == "기장군"   ~ "Gijang",
      SQ3_2_2 == "영도구"   ~ "Yeongdo",
      .default = NA_character_
    ),
    district_type = case_when(
      str_sub(SQ3_2_2, -1) == "구" ~ "Gu",
      str_sub(SQ3_2_2, -1) == "군" ~ "Gun",
      str_sub(SQ3_2_2, -1) == "시" ~ "Si",
      .default = NA_character_
    )
  ) |>
  filter(city == "Busan") |> View()

library(dplyr)
library(stringr)

district_lookup <- c(
  # Seoul
  "중"="Jung","종로"="Jongno","도봉"="Dobong","중랑"="Jungnang","광진"="Gwangjin",
  "강북"="Gangbuk","강남"="Gangnam","구로"="Guro","성북"="Seongbuk","강동"="Gangdong",
  "관악"="Gwanak","동작"="Dongjak","서대문"="Seodaemun","양천"="Yangcheon",
  "강서"="Gangseo","서초"="Seocho","송파"="Songpa","노원"="Nowon","동대문"="Dongdaemun",
  "영등포"="Yeongdeungpo","성동"="Seongdong","금천"="Geumcheon","은평"="Eunpyeong",
  "용산"="Yongsan","마포"="Mapo",
  # Busan
  "북"="Buk","사상"="Sasang","해운대"="Haeundae","동래"="Dongnae","부산진"="Busanjin",
  "사하"="Saha","연제"="Yeonje","동"="Dong","서"="Seo","수영"="Suyeong",
  "금정"="Geumjeong","남"="Nam","기장"="Gijang","영도"="Yeongdo",
  # Daegu, Incheon, Gwangju, Daejeon, Ulsan, Sejong
  "달성"="Dalseong","달서"="Dalseo","수성"="Suseong",
  "남동"="Namdong","미추홀"="Michuhol","계양"="Gyeyang","부평"="Bupyeong",
  "연수"="Yeonsu","강화"="Ganghwa","광산"="Gwangsan","대덕"="Daedeok",
  "유성"="Yuseong","울주"="Ulju","세종"="Sejong",
  # Gyeonggi
  "성남"="Seongnam","오산"="Osan","수원"="Suwon","평택"="Pyeongtaek",
  "남양주"="Namyangju","하남"="Hanam","안성"="Anseong","의정부"="Uijeongbu",
  "용인"="Yongin","포천"="Pocheon","파주"="Paju","양평"="Yangpyeong",
  "고양"="Goyang","안산"="Ansan","화성"="Hwaseong","부천"="Bucheon","양주"="Yangju",
  "이천"="Icheon","구리"="Guri","광명"="Gwangmyeong","군포"="Gunpo","안양"="Anyang",
  "김포"="Gimpo","시흥"="Siheung","광주"="Gwangju",
  # Gangwon
  "원주"="Wonju","고성"="Goseong","춘천"="Chuncheon","강릉"="Gangneung",
  "동해"="Donghae","철원"="Cheorwon","인제"="Inje",
  # Chungbuk / Chungnam
  "청주"="Cheongju","음성"="Eumseong","진천"="Jincheon",
  "금산"="Geumsan","공주"="Gongju","부여"="Buyeo","천안"="Cheonan","논산"="Nonsan",
  "서산"="Seosan",
  # Jeonbuk / Jeonnam
  "익산"="Iksan","정읍"="Jeongeup","김제"="Gimje","장수"="Jangsu","순창"="Sunchang",
  "화순"="Hwasun","순천"="Suncheon","담양"="Damyang","장성"="Jangseong","나주"="Naju",
  "목포"="Mokpo",
  # Gyeongbuk / Gyeongnam
  "구미"="Gumi","안동"="Andong","경산"="Gyeongsan","경주"="Gyeongju","포항"="Pohang",
  "고령"="Goryeong","김천"="Gimcheon","칠곡"="Chilgok","문경"="Mungyeong","성주"="Seongju",
  "사천"="Sacheon","밀양"="Miryang","창원"="Changwon","양산"="Yangsan","의령"="Uiryeong",
  "함안"="Haman","진주"="Jinju","김해"="Gimhae",
  # Jeju
  "서귀포"="Seogwipo","제주"="Jeju"
)

df |>
  mutate(
    # "성남시 분당구" / "창원시마산합포구" -> "성남시" / "창원시"
    district_clean = str_remove(SQ3_2_2, "(?<=시)\\s*[^시]+구$"),
    district_base  = str_sub(district_clean, 1, -2),
    # drop the 구/군/시
    district_en    = unname(district_lookup[district_base]),
    district_type  = case_when(
      str_sub(district_clean, -1) == "구" ~ "Gu",
      str_sub(district_clean, -1) == "군" ~ "Gun",
      str_sub(district_clean, -1) == "시" ~ "Si",
      .default = NA_character_
    ),
    across(
      c(
        city,
        district_clean,
        district_base,
        district_en,
        district_type
      ),
      str_squish
    )
  ) |>
  select(SQ3_2_2,
         city,
         district_clean,
         district_base,
         district_en,
         district_type,
         city_size) |>
  distinct(city, district_en, district_type, .keep_all = TRUE) |>
  select(city, district_en, district_type, city_size) |>
  right_join(
    korea_map_district,
    .,
    by = join_by(
      "city" == city,
      "district_en" == "NAME_2",
      "district_type" == "TYPE_2"
    )
  ) |>
  ggplot(aes(geometry = geom, fill = city_size)) +
  scale_fill_discrete(na.value = "white")+
  geom_sf()+
  theme_void()+
  theme(legend.position = "bottom")

korea_map_district |>
  ggplot(aes(geometry = geom)) +
  geom_sf()




df |>
  select(city,city_size) |>
  unique() |> View()


