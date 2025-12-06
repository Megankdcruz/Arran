install.packages("ggplot2")
library("ggplot2")
install.packages("tidyverse")
library("tidyverse")
North<-read.csv("NorthSlopePlants.csv")
South<-read.csv("SouthSlopePlants.csv")
#Separated the data for both sites to make it easier when analysing. 
#Species richness trying to calculate the number of unique species observed
#calculating North Species richness 
North_richness<- North %>%
  summarise(species_richness=n_distinct(scientificName))
North_richness
#North richness = 25 - 25 unique species. 
#Calculating South species richness 
South_richness<- South %>% 
  summarise(species_richness = n_distinct(scientificName))
South_richness

#combine them into a table to make plotting easier 
richness<- tibble(
  site= c("North", "South"),
  species_richness=c(
    n_distinct(North$scientificName),
    n_distinct(South$scientificName)
  )
)
richness
#I want to plot the species richness for each site 
#to do this I will use the package ggplot2 
ggplot(richness,aes(x=site,y=species_richness,fill=site))+
  geom_col() +
  theme_minimal()+
  labs(
    title = "Species Richness by Site",
    x="Site",
    y= "Number of species"
  ) + 
  theme(legend.position = "none")
  