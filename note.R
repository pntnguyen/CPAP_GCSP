library(skimr)

setwd("../Data")



raw_df <- read_excel("Cancer_Prevention_Awareness_Raw_Data_251203.xlsx", sheet = "DATA")

### 2023 data

df_2023 <- read_excel("Cancer_Prevention_Awareness_Raw_Data_2023.xlsx", sheet = "data")


# install.packages("haven")

# Load the package
library(haven)
library(skimr)
library(dplyr)
# Import the .sas7bdat file
df <- read_sas("../data/CHS/chs25_all.sas7bdat")
colnames(df)

df |>
  pull(sma_08z1) |>
  unique()

## wt_h: household weight
## wt_p: individual weight
## mbhld_co: total number of household member

library(readr)

admin_address1 <- read_delim("../Data/CHS/admin_address.txt",
                            delim = "\t",
                            locale = locale(encoding = "CP949"))

admin_address <- read_tsv("../Data/CHS/signgu_code_english.txt",
                          col_types = cols(signgu_code = "c"))

colnames(admin_address1) <- c("code", "address", "city")

admin_address1[grepl("0{4}$", admin_address1$code), , drop = FALSE] |> View()


df |>
  pull(signgu_code) |>
  unique()

df |>
  pull(CTPRVN_CODE) |>
  unique()

admin_address |>
  select(signgu_code, sido_en, map_name) |>
  pull(sido_en) |>
  unique()

korea_map_district |>
  select(NAME_1,NAME_2) |>
  pull(NAME_1) |>
  unique()



## Variables code book
#
# | Variable | English meaning |
#   |:----|:----------------|
#   | EXAMIN_YEAR | Survey year |
#   | exmprs_no | Respondent ID |
#   | age | Age (completed years) |
#   | sex | Sex |
#   | CTPRVN_CODE | Province/metropolitan city (si-do) code |
#   | PBHLTH_CODE | Public health center code |
#   | SPOT_NO | Sample point number |
#   | HSHLD_CODE | Household number |
#   | MBHLD_CODE | Household member number |
#   | DONG_TY_CODE | Residential area type (dong vs. eup/myeon) |
#   | HOUSE_TY_CODE | Housing type |
#   | signgu_code | Administrative district (si-gun-gu) code |
#   | kstrata | Stratification variable |
#   | wt_h | Household weight |
#   | wt_p | Individual weight |
#   | mbhld_co | Total number of household members |
#   | rspns_adult_co | Number of adults in household |
#   | fma_19z3 | Household type (recoded) |
#   | fma_04z1 | Basic livelihood security recipient |
#   | fma_12z1 | Household income reporting unit (annual or monthly) |
#   | fma_13z1 | Household income, annual amount |
#   | fma_14z1 | Household income, monthly amount |
#   | fma_24z2 | Household income, monthly amount (conditional) |
#   | nue_01z1 | Household food security |
#   | fmb_01z1 | Change in total income |
#   | qoa_01z1 | Self-rated health |
#   | sma_01z1 | Lifetime conventional cigarette smoking |
#   | smc_08z2 | Secondhand smoke exposure at home (indoors) |
#   | smc_09z2 | Experience of secondhand smoke at home (indoors) |
#   | smc_10z2 | Experience of secondhand smoke at workplace (indoors) |
#   | sma_03z2 | Current conventional cigarette smoking |
#   | smb_01z1 | Average daily cigarettes among daily smokers |
#   | sma_36z1 | Lifetime heated tobacco product use |
#   | sma_37z1 | Current heated tobacco product use |
#   | sma_08z1 | Lifetime use of nicotine-containing liquid e-cigarettes |
#   | sma_11z1 | Current use of nicotine-containing liquid e-cigarettes |
#   | smd_02z3 | Quit attempt among current smokers |
#   | smd_01z3 | Quit plan among current smokers |
#   | dra_01z1 | Lifetime alcohol drinking |
#   | drb_01z3 | Annual drinking frequency |
#   | drb_03z1 | Amount consumed per drinking occasion |
#   | drb_16z1 | Amount consumed per occasion among those drinking 10+ drinks |
#   | drb_04z1 | Monthly binge drinking (male) |
#   | drb_05z1 | Monthly binge drinking (female) |
#   | drg_01z1 | Attempt to cut down or abstain from alcohol in past year |
#   | sfa_02z3 | Car seat belt use |
#   | sfb_05z2 | Drunk driving experience (car) |
#   | sfa_12z2 | Seat belt use in rear seat |
#   | sfa_06z2 | Motorcycle helmet use |
#   | sfb_03z2 | Drunk driving experience (motorcycle) |
#   | sfa_14z1 | Helmet use when riding personal mobility devices |
#   | pha_04z1 | Days of vigorous physical activity |
#   | pha_05z1 | Vigorous activity duration (hours) |
#   | pha_06z1 | Vigorous activity duration (minutes) |
#   | pha_07z1 | Days of moderate physical activity |
#   | pha_08z1 | Moderate activity duration (hours) |
#   | pha_09z1 | Moderate activity duration (minutes) |
#   | phb_01z1 | Days of walking |
#   | phb_02z1 | Walking duration (hours) |
#   | phb_03z1 | Walking duration (minutes) |
#   | nua_01z2 | Days per week eating breakfast (past year) |
#   | nuc_02z1 | Awareness of nutrition labeling |
#   | nuc_01z2 | Reading nutrition labels |
#   | nuc_03z1 | Using nutrition labels |
#   | oba_02z1 | Height |
#   | oba_03z1 | Weight |
#   | oba_01z1 | Self-perceived body shape |
#   | obb_01z1 | Weight control attempt |
#   | cva_11z2 | Awareness of early stroke symptom: limb weakness |
#   | mya_15z1 | Awareness of early myocardial infarction symptom: shortness of breath |
#   | cva_12z1 | Awareness of early stroke symptom: slurred speech |
#   | cva_14z2 | Awareness of early stroke symptom: vision loss in one eye |
#   | cva_16z2 | Awareness of early stroke symptom: difficulty balancing |
#   | cva_17z1 | Awareness of early stroke symptom: severe headache |
#   | mya_10z2 | Awareness of early MI symptom: jaw, neck, or back pain |
#   | mya_11z2 | Awareness of early MI symptom: dizziness |
#   | mya_12z2 | Awareness of early MI symptom: chest pain |
#   | mya_14z1 | Awareness of early MI symptom: arm or shoulder pain |
#   | ora_01z1 | Self-rated oral health |
#   | orb_01z1 | Chewing difficulty |
#   | ord_01d2 | Toothbrushing yesterday: after lunch |
#   | ord_01f3 | Toothbrushing yesterday: after dinner or before sleep |
#   | ore_02z2 | Unmet dental care need (past year) |
#   | ore_03z3 | Reason for unmet dental care need |
#   | mtc_17z1 | Average sleep duration, weekdays |
#   | mtb_07d1 | Depressive symptom: tiredness, low energy |
#   | mtb_07e1 | Depressive symptom: poor appetite or overeating |
#   | mtb_07f1 | Depressive symptom: feeling like a bad person |
#   | mtb_07g1 | Depressive symptom: difficulty concentrating (newspaper, TV) |
#   | mtb_07h1 | Depressive symptom: slowed movement or speech |
#   | mtb_07i1 | Depressive symptom: thoughts that death would be better |
#   | mtd_01z1 | Suicidal ideation |
#   | mtd_02z1 | Mental health counseling for suicidal ideation |
#   | mtd_05z1 | Reason for suicidal thoughts |
#   | mtd_03z1 | Suicide attempt |
#   | mtc_18z1 | Average sleep duration, weekends |
#   | mtd_04z1 | Mental health counseling after suicide attempt |
#   | mtj_05z2 | Experience of cognitive impairment |
#   | mta_01z1 | Perceived stress level |
#   | mta_02z1 | Mental health counseling for stress |
#   | mtb_01z1 | Experience of depressed mood |
#   | mtb_02z1 | Mental health counseling for depressive symptoms |
#   | mtb_07a1 | Depressive symptom: little interest or pleasure |
#   | mtb_07b1 | Depressive symptom: feeling down or hopeless |
#   | mtb_07c1 | Depressive symptom: trouble falling asleep or sleeping too much |
#   | sca_01z1 | Annual influenza vaccination |
#   | hya_19z1 | Awareness of own blood pressure value |
#   | dia_05z1 | Time of first diabetes diagnosis |
#   | dia_13c2 | Diabetes treatment: non-drug therapy |
#   | dia_13b2 | Diabetes treatment: diabetes medication |
#   | dia_13a2 | Diabetes treatment: insulin |
#   | dia_09z1 | Completed diabetes management education |
#   | dia_22z2 | Number of HbA1c tests |
#   | dia_14z1 | Diabetic eye complication exam (fundus exam) |
#   | dia_15z1 | Annual diabetic kidney complication test (microalbuminuria) |
#   | hya_04z1 | Hypertension diagnosis |
#   | hya_05z1 | Time of first hypertension diagnosis |
#   | hya_14c2 | Blood pressure management: non-drug therapy |
#   | hya_14a2 | Blood pressure management: taking antihypertensive medication |
#   | hya_14b2 | Blood pressure management: days of medication use |
#   | hya_10z1 | Completed hypertension management education |
#   | dia_19z1 | Awareness of own blood glucose value |
#   | dia_04z1 | Diabetes diagnosis |
#   | sra_01z3 | Unmet medical need (past year) |
#   | sra_02z3 | Reason for unmet medical need |
#   | ira_01z1 | Accident or poisoning experience (past year) |
#   | i4a_10z1 | Injury (4th): treatment facility |
#   | i5a_04z2 | Injury (5th): mechanism of occurrence |
#   | i5a_10z1 | Injury (5th): treatment facility |
#   | ira_02z1 | Number of accidents or poisonings (past year) |
#   | i1a_04z2 | Injury (1st): mechanism of occurrence |
#   | i1a_10z1 | Injury (1st): treatment facility |
#   | i2a_04z2 | Injury (2nd): mechanism of occurrence |
#   | i2a_10z1 | Injury (2nd): treatment facility |
#   | i3a_04z2 | Injury (3rd): mechanism of occurrence |
#   | i3a_10z1 | Injury (3rd): treatment facility |
#   | i4a_04z2 | Injury (4th): mechanism of occurrence |
#   | qoc_07z1 | Happiness index |
#   | qoc_01z1 | EQ-5D: mobility |
#   | qoc_02z1 | EQ-5D: self-care |
#   | qoc_03z1 | EQ-5D: usual activities |
#   | qoc_04z1 | EQ-5D: pain/discomfort |
#   | qoc_05z1 | EQ-5D: anxiety/depression |
#   | hma_01z3 | Use of health care facilities |
#   | soe_01z1 | Length of residence in province/metropolitan city |
#   | soe_02z1 | Length of residence in city/county/district |
#   | cpr_01z1 | Awareness of CPR |
#   | cpr_02z1 | CPR training experience |
#   | cpr_03a2 | CPR practice experience: mannequin |
#   | cpr_03b2 | CPR practice experience: automated external defibrillator (AED) |
#   | cpr_04z1 | Self-rated ability to perform CPR |
#   | sob_01z1 | Education level |
#   | sob_02z1 | Graduation status |
#   | soa_01z1 | Economic activity |
#   | soa_06z2 | Occupation (recoded) |
#   | soa_07z1 | Employment status |
#   | sod_02z3 | Marital status |
#   | cog_01z1 | COVID-19 concern: infection |
#   | cog_03z1 | COVID-19 concern: blame from others |
#   | cog_05z1 | COVID-19 concern: financial damage |
#   | coi_01z2 | COVID-19 vaccination experience |
#
#   : Variable codebook {#tbl-codebook}


