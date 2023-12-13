### Subset dataframe for public data
library(tidyverse)

covidestim_df <- read_csv("data-products/covidestim-dataframe.csv")

covidestim_public <- covidestim_df %>%
  transmute(sewershed_ID = shedID, 
            date = date, 
            reported_cases_by_week = cases, 
            Rt_qPCR_N1_average_copies_per_ml_wastewater = N1, 
            ratio_Rt_qPCR_N1_over_Rt_qPCR_PMMoV = ratio, 
            ratio_Rt_N1_qPCR_N1_over_flow_millions_gallons_per_day = N1_by_flow, 
            population_size = pop)

write_csv(covidestim_public, "data-products/public-data.csv")

sero_df <- read_csv("data-products/serosurvey-by-date.csv")
write_csv(sero_df, "data-products/public-data-sero.csv")
