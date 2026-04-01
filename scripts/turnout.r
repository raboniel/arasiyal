# Looking at the election turnouts 

library(readxl)
library(tidyverse)
library(RColorBrewer)

basedata <- read_excel("/home/moridin/Personal/Paalam Naangu/Data Repository/ForAnalysis_2021.xlsx")


# 0. making a smaller dataframe for analysis
voters <- basedata

voters <- voters |>
  select(constituency,region,subregion,district,alliance_one,turnoutp,urbanism)


# 1 . looking at the highest and lowest vote shares
voters |> arrange(turnoutp) # from lowest

voters |> arrange(desc(turnoutp))

# 2. looking regional level
median_turnout = median(voters$turnoutp)

voters |> group_by(region) |> summarise(median_turnout=median(turnoutp))

chennai_voters <- voters |> filter(region == "Chennai")
print(min(chennai_voters$turnoutp))
print(max(chennai_voters$turnoutp))

north_voters <- voters |> filter(region == "North")
print(min(north_voters$turnoutp))
print(max(north_voters$turnoutp))

kongu_voters <- voters |> filter(region == "Kongu")
print(min(kongu_voters$turnoutp))
print(max(kongu_voters$turnoutp))

delta_voters <- voters |> filter(region == "Delta")
print(min(delta_voters$turnoutp))
print(max(delta_voters$turnoutp))

south_voters <- voters |> filter(region == "South")
print(min(south_voters$turnoutp))
print(max(south_voters$turnoutp))

# 3. looking at urbanism

voters |> group_by(urbanism) |> summarise(median_turnout=median(turnoutp))

metro_voters <- voters |> filter(urbanism == "Metro")
print(min(metro_voters$turnoutp))
print(max(metro_voters$turnoutp))
print(median(metro_voters$turnoutp))

city_voters <- voters |> filter(urbanism == "City")
print(min(city_voters$turnoutp))
print(max(city_voters$turnoutp))
print(median(city_voters$turnoutp))

town_voters <- voters |> filter(urbanism == "Town")
print(min(town_voters$turnoutp))
print(max(town_voters$turnoutp))
print(median(town_voters$turnoutp))

rural_voters <- voters |> filter(urbanism == "Rural")
print(min(rural_voters$turnoutp))
print(max(rural_voters$turnoutp))
print(median(rural_voters$turnoutp))


# making the different boxplots

# 4.region based
voters$region <- factor(voters$region,c("Chennai","North","Kongu","Delta","South"))

plot_turnout_region <- ggplot(data=voters, aes(x=region,y=turnoutp,fill=region))+
  geom_boxplot() + 
  labs(title="Regionwise turnouts in the 2021 elections")+
  theme(legend.position = "none")+
  scale_y_continuous(limits=c(50,90)) +
 # scale_x_discrete(labels=c("Greater Chennai"="Chennai","North"="North","Kongu"="Kongu","Delta"="Delta","South"="South"))+
  scale_fill_manual(values=c("Chennai"="#f2f2f285","North"="#0a036e85","Kongu"="#0aa81785","Delta"="#ff050585","South"="#e0c80d85")) 
  

plot_turnout_region + spice_boxplot_voters()

# 5.urbanism based

voters$urbanism <- factor(voters$urbanism,c("Rural","Town","City","Metro"))

plot_turnout_urbanism <- ggplot(data=voters, aes(x=urbanism,y=turnoutp,fill=urbanism))+
  geom_boxplot() + 
  labs(title="Urbanism and turnouts in the 2021 elections")+
  theme(legend.position = "none")+
  scale_y_continuous(limits=c(50,90)) +
  scale_fill_brewer(palette="GnBu")

plot_turnout_urbanism + spice_boxplot_voters()


# 6. Comparing to the older elections

threekingdoms <- read_excel("/home/moridin/Personal/Paalam Naangu/Data Repository/ForAnalysis_threekingdoms.xlsx")

threekingdoms <- threekingdoms |> 
  select(region,subregion,district,turnoutp,urbanism,year)

threekingdoms$year <- factor(threekingdoms$year,c("2011","2016","2021"))

plot_turnout_threekingdoms <- ggplot(data=threekingdoms, aes(x=year,y=turnoutp,fill=year))+
  geom_boxplot() + 
  labs(title="Turnouts in the last three elections")+
  theme(legend.position = "none")+
  scale_y_continuous(limits=c(50,90)) +
  scale_fill_brewer(palette = "PRGn") 

plot_turnout_threekingdoms + spice_boxplot_voters()

# 7. regional over three years

threekingdoms$region <- factor(threekingdoms$region,c("Chennai","North","Kongu","Delta","South"))

plot_turnout_threekingdoms_region <- ggplot(data=threekingdoms, aes(x=year,y=turnoutp,fill=year))+
  geom_boxplot() + 
  labs(title="Regionwise turnouts in the last three elections")+
  theme(legend.position = "none")+
  scale_y_continuous(limits=c(50,90)) +
  scale_fill_brewer(palette = "PRGn") +
  theme(axis.title.x = element_blank())+
  facet_wrap(~region,nrow=1)

plot_turnout_threekingdoms_region + spice_boxplot_voters_threekingdoms()


# 8. urbanism over three years

threekingdoms$urbanism <- factor(threekingdoms$urbanism,c("Rural","Town","City","Metro"))

plot_turnout_threekingdoms_urbanism <- ggplot(data=threekingdoms, aes(x=year,y=turnoutp,fill=year))+
  geom_boxplot() + 
  labs(title="Urbanism and turnouts in the last three elections")+
  theme(legend.position = "none")+
  scale_y_continuous(limits=c(50,90)) +
  scale_fill_brewer(palette = "PRGn") +
  theme(axis.title.x = element_blank())+
  facet_wrap(~urbanism,nrow=1)

plot_turnout_threekingdoms_urbanism + spice_boxplot_voters_threekingdoms()

