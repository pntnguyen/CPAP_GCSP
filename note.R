library(skimr)

setwd("../Data")

raw_df <- read_excel("Cancer_Prevention_Awareness_Raw_Data_251203.xlsx", sheet = "DATA")

### 2023 data

df_2023 <- read_excel("Cancer_Prevention_Awareness_Raw_Data_2023.xlsx", sheet = "data")
