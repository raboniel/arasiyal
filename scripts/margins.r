# looking at margins of victory

# Looking at the election turnouts 

library(readxl)
library(tidyverse)
library(RColorBrewer)

basedata <- read_excel("/home/moridin/Personal/Paalam Naangu/Data Repository/ForAnalysis_2021.xlsx")


# 0. making a smaller dataframe for analysis
margins <- basedata

margins <- margins |>
  select(constituency,region,subregion,alliance_one,alliance_two,party_one,party_two,urbanism,margin,marginp)

# 1 . looking at the highest and lowest margins
margins |> arrange(marginp) # from lowest

margins |> arrange(desc(marginp))

#looking at margins for the last three elections
threekingdoms <- read_excel("/home/moridin/Personal/Paalam Naangu/Data Repository/ForAnalysis_threekingdoms.xlsx")

threekingdoms <- threekingdoms |>
  select(constituency,year,party_one,party_two,margin,marginp)

threekingdoms |> arrange(marginp)

threekingdoms |> arrange(desc(marginp))

# 2. looking at alliance and partywise margins

margins |> group_by(party_one) |> summarise(median_margins=median(marginp))

margins |> summarise(median_margins=median(marginp))

margins |> group_by(alliance_one) |> summarise(median_margins=median(marginp))

margins |> group_by(region) |> summarise(median_margins=median(marginp))

margins |> group_by(alliance_one,region) |> summarise(median_margins=median(marginp))

# looking at them individually
margins$region <- factor(margins$region,c("Chennai","North","Kongu","Delta","South"))

# alliancewise and regions
plot_margin_region <- ggplot(data=margins, aes(x=region,y=marginp,fill=alliance_one))+
  geom_boxplot() + 
  labs(title="Regionwise victory margins in the 2021 elections")+
  theme(legend.position = "none")+
  # scale_y_continuous(limits=c(50,90)) +
  # scale_x_discrete(labels=c("Greater Chennai"="Chennai","North"="North","Kongu"="Kongu","Delta"="Delta","South"="South"))+
  scale_fill_manual(values=c("DMK+"="#6d2423","AIADMK+"="#2c5225")) 

plot_margin_region + spice_boxplot_voters()



# 3. looking at urbanism and alliance margins

margins |> group_by(urbanism) |> summarise(median_margins=median(marginp))

margins |> group_by(alliance_one,urbanism) |> summarise(median_margins=median(marginp))

#alliancewise and urbanism
plot_margin_urbanism <- ggplot(data=margins, aes(x=urbanism,y=marginp,fill=alliance_one))+
  geom_boxplot() + 
  labs(title="Urbanism and victory margins in the 2021 elections")+
  theme(legend.position = "none")+
  # scale_y_continuous(limits=c(50,90)) +
  # scale_x_discrete(labels=c("Greater Chennai"="Chennai","North"="North","Kongu"="Kongu","Delta"="Delta","South"="South"))+
  scale_fill_manual(values=c("DMK+"="#6d2423","AIADMK+"="#2c5225")) 

plot_margin_urbanism + spice_boxplot_voters()

# 4. looking at the impact of People's Front on AIADMK losses

pf_impact <- basedata

attach(pf_impact)

votes_pf <-  ifelse(alliance_one == "PF", vote_one,
                     ifelse(alliance_two == "PF", vote_two,
                            ifelse(alliance_three == "PF", vote_three,
                                   ifelse(alliance_four == "PF",vote_four,
                                          ifelse(alliance_five == "PF",vote_five,0)))))

pf_impact$votes_pf <- votes_pf

pf_spoiler <- pf_impact |>
  filter (alliance_one == "DMK+") |>
  filter (votes_pf > margin) |>
  select (constituency,region,party_one,party_two,margin,party_three,party_four,party_five,votes_pf)

pf_spoiler

#5. looking at historical margins

threekingdoms <- threekingdoms |>
  select(year,alliance_one,alliance_two,margin,marginp)

threekingdoms$year <- factor(threekingdoms$year,c("2011","2016","2021"))

plot_margin_threekingdoms <- ggplot(data=threekingdoms, aes(x=year,y=marginp,fill=alliance_one))+
  geom_boxplot() + 
  labs(title="Victory margins in the last three elections")+
  theme(legend.position = "none")+
 # scale_y_continuous(limits=c(50,90)) +
  # scale_fill_brewer(palette = "PRGn") 
  scale_fill_manual(values=c("DMK+"="#6d2423","AIADMK+"="#2c5225")) 


plot_margin_threekingdoms + spice_boxplot_voters()

