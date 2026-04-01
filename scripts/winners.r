# total winners region wise

library(readxl)
library(tidyverse)

basedata <- read_excel("/home/moridin/Personal/Paalam Naangu/Data Repository/ForAnalysis_Primary.xlsx")


# making the smaller dataframe for analysis

winner <- basedata

winner <- winner |>
  select(region,subregion,district,alliance_one)

# making sure that DMK comes first for ease of entry
winner$alliance_one <- factor(winner$alliance_one,c("DMK+","AIADMK+"))

# whole TN
table(winner$region,winner$alliance_one)

winner_north <- winner |> filter(region=="North")
table(winner_north$subregion,winner_north$alliance_one)

winner_kongu <- winner |> filter(region=="Kongu")
table(winner_kongu$subregion,winner_kongu$alliance_one)

winner_delta <- winner |> filter(region=="Delta")
table(winner_delta$subregion,winner_delta$alliance_one)

winner_south <- winner |> filter(region=="South")
table(winner_south$subregion,winner_south$alliance_one)

# looking at it district wise

table(winner_north$district,winner_north$alliance_one)

table(winner_kongu$district,winner_kongu$alliance_one)

table(winner_delta$district,winner_delta$alliance_one)

table(winner_south$district,winner_south$alliance_one)



