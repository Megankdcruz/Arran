setwd("C:/Users/megdc/OneDrive/Documents/Arran/Arran_results")
NBats<-read.csv("Data/North_Bats.csv")
SBats<-read.csv("Data/South_Bats.csv")
NMam<-read.csv("Data/North_Mammals.csv")
SMam<-read.csv("Data/South_Mammals.csv")
Nbirds<-read.csv("Data/North_Birds.csv")
Sbirds<-read.csv("Data/South_Birds.csv")

#I want to make an overall graph for vertebrate richness/diversity on the two sites 
#And potentially make a separate bats diversity index and graph as all bats are protected so knowing which hillside has a more diverse population might be important

library("tidyverse")
library("ggplot2")
library("readr")

#First I will make the bats graphs and index and then focus on combining the relevant data from all the vertebrate categories into two dataframes (1 for each site)
#To plot species richness I am going to isolate the number of different species at each site into two objects which I will later join in a data frame in order to plot 

North_BA_richness<- NBats %>%
  summarise(species_richness=n_distinct(scientificName))
North_BA_richness
South_BA_richness<- SBats %>%
  summarise(species_richness=n_distinct(scientificName))
South_BA_richness
#Sense checking - listing the unique scientific names to make sure there isn't any double counting 
sort(unique(NBats$scientificName))
#There is a mistake - R is counting pipistrellus pygmaeus twice so we need to fix that there are some names with and underscore but some with just a space between the two words 
#I will fix this by replacing any spaces in the 'scientific name' column with _'s 
NBats$scientificName <- gsub(" ", "_", NBats$scientificName)
#then we will re-print the number of unique names 
sort(unique(NBats$scientificName))
#now only 2 unique names so we will update the species richness object above by re-running it
#I am going to perform the same sense check for the south plot 
sort(unique(SBats$scientificName))
#there does not seem to be any repeats so no need to fix anything. 
#now we can make a dataframe of species richness to plot 
Batrichness<- data.frame(
  site= c("North", "South"),
  richness=c(North_BA_richness$species_richness, South_BA_richness$species_richness))
Batrichness
#now to plot 
ggplot(Batrichness,aes(x=site,y=richness,fill=site))+
  geom_col() +
  scale_fill_manual(values = c("North"="#8E8A98",
                               "South"="#CCE1E4"
  ))+
  theme_minimal()+
  labs(
    title = "Bat Species Richness by Site",
    x="Site",
    y= "Number of species"
  ) + 
  theme(legend.position = "none")
ggsave("Results/Bat_SPrichness.png")
#due to the small amount of species at each site - I do not think a diversity index will provide any additional insights that this figure doesnt give esspecially since - individual counts for bats are unreliable when working with audiomoths 
#Now we can combine them to do an overall Vertebrate diversity for each slope 
library("dplyr")
All_vert<- bind_rows(
  NBats %>% select(scientificName, eventID) %>% mutate(site = "North", group = "Bats"),
  SBats %>% select(scientificName, eventID) %>% mutate(site = "South", group = "Bats"),
  Nbirds %>% select(scientificName, eventID) %>% mutate(site = "North", group = "Birds"),
  Sbirds %>% select(scientificName, eventID) %>% mutate(site = "South" , group = "Birds"),
  NMam %>% select(scientificName, eventID) %>% mutate(site = "North", group = "Mammals"),
  SMam %>% select(scientificName, eventID) %>% mutate(site = "South", group = "Mammals")
)
#had to change the code a little bit because when it tried to bind all rows - bird data has words in it's individual count column while almost everthing else has numbers 
# so instead I am only binding the scientific name and eventID rows - because to make a species richness plot that is really all we need 
#some rows in all_verts are empty rows could be due to empty rows in the CSV file so i am going to remove them 
All_vert <- All_vert %>%
  mutate(
    scientificName = trimws(scientificName),
    eventID = trimws(eventID)
  ) %>%
  filter(
    scientificName != "" & !is.na(scientificName),
    eventID != "" & !is.na(eventID)
  )
#this code basically just filters for any rows where scientificname or event id are empty or are filled with NA and removes them 
#now that we have a big  data frame with all the vertebrates we can make a species richness plot 
#first like before we have to get species richness by counting the unique names
vert_richness_site <- All_vert %>%
  group_by(site) %>%
  summarise(species_richness = n_distinct(scientificName))

vert_richness_site
#now to plot 
ggplot(vert_richness_site,aes(x=site,y=species_richness,fill=site))+
  geom_col() +
  scale_fill_manual(values = c("North"="#8E8A98",
                               "South"="#CCE1E4"
  ))+
  theme_minimal()+
  labs(
    title = "Vertebrate Species Richness by Site",
    x="Site",
    y= "Number of species"
  ) + 
  theme(legend.position = "none")
ggsave("Results/Vert_sprichness1.png")
#Just going to run an alternate plot - which also showing the groupings of bird mammal and bat 
# so first doing a site and group species richness 

richness_by_site_group <- All_vert %>% 
  group_by(site, group) %>% 
  summarise(richness = n_distinct(scientificName), .groups = "drop")
#now plotting it to see if it looks better - and better represents the data 
ggplot(richness_by_site_group,
       aes(x = site, y = richness, fill = group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Birds"   = "#8E8A98",
    "Bats"    = "#CCE1E4",
    "Mammals" = "#A9C47F"   
  )) +
  theme_minimal() +
  labs(
    title = "Vertebrate Species Richness by Site and Group",
    x = "Site",
    y = "Number of species",
    fill = "Group"
  )
ggsave("Results/SG_Vert_Sprichness.png")
