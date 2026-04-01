# calculating the basic frequencies for the maps

library(readxl)
library(tidyverse)

basedata <- read_excel("/home/moridin/Personal/Paalam Naangu/Data Repository/ForAnalysis_Primary.xlsx")

freqanalysis <- basedata

freqanalysis <- freqanalysis |>
  select(constituency,district,region,subregion)


count(freqanalysis,region) # number of constituencies in each region

Chennai <- freqanalysis |> filter(region=="Greater Chennai")
rapply(Chennai, function(x) length(unique(x)))

North <- freqanalysis |> filter(region=="North")
rapply(North, function(x) length(unique(x)))
count(North,subregion)

Kongu <- freqanalysis |> filter(region=="Kongu")
rapply(Kongu, function(x) length(unique(x)))
count(Kongu,subregion)

Delta <- freqanalysis |> filter(region=="Delta")
rapply(Delta, function(x) length(unique(x)))
count(Delta,subregion)

South <- freqanalysis |> filter(region=="South")
rapply(South, function(x) length(unique(x)))
count(South,subregion)






