#This script is analysing the moth data as part of my 'invertebrate' results 
library("tidyverse")
library("ggplot2")
library("readr")
NorthM<-read.csv("Data/NorthSlopeMoths.csv")
SouthM<-read.csv("Data/SouthSlopeMoths.csv")
#starting off bycalculating species richness - I will only do species richness per slope for the moths 
#North species richness 
Northmoth_richness<- NorthM %>%
  summarise(species_richness=n_distinct(scientificName))
Northmoth_richness
#checking number of unique names to make sure nothing is being counted twice 
unique_names<- sort(unique(NorthM$scientificName))
unique_names
#Moving onto south richness 
Southmoth_richness<- SouthM %>%
  summarise(species_richness=n_distinct(scientificName))
Southmoth_richness
rm(Southmouth_richness)
#checking no of unique names for south 
unique_namesS<- sort(unique(SouthM$scientificName))
unique_namesS
#Now we can combine them into one dataframe to plot them 
Mothrichness<- data.frame(
  site= c("North", "South"),
  richness=c(Northmoth_richness$species_richness, Southmoth_richness$species_richness))
Mothrichness
rm(richness)
#Plotting 
ggplot(Mothrichness,aes(x=site,y=richness,fill=site))+
  geom_col() +
  scale_fill_manual(values = c("North"="sienna3",
                               "South"="sienna4"
  ))+
  theme_minimal()+
  labs(
    title = "Moth Species Richness by Site",
    x="Site",
    y= "Number of species"
  ) + 
  theme(legend.position = "none")
ggsave("Results/Moth_speciesrichness.png")

#I also want to make a graph showing the most abundant - least abundant species across the two sites 
#to do this I am going to merge the two sites and add a "site" column 
north<- NorthM %>%
  mutate(site="North")
south<- SouthM %>%
  mutate(site = "South")

moths<- bind_rows(north,south)

#Now I want to find out abundance per species and site 
moths_summary<- moths %>%
  group_by(scientificName,site) %>%
  summarise(total_abundance = sum(individualCount),
            .groups = "drop")%>%
  group_by(scientificName)%>%
  mutate(species_total=sum(total_abundance))%>%
  ungroup() %>%
  arrange(desc(species_total)) %>%
  mutate(scientificName=factor(scientificName, levels = unique(scientificName)))

ggplot(moths_summary,
       aes(x = scientificName, y = total_abundance, fill = site)) +
  geom_col(position = "dodge") +   # North vs South side-by-side
  coord_flip() +                   # flip so names are readable
  labs(x = "Species",
       y = "Abundance",
       fill = "Site",
       title = "Moth abundance by species – North vs South") +
  theme_minimal()
ggsave("Results/MothAbundance_plot.png")

#I am going to try and look at the shared species between each site
Shared_species <- intersect(NorthM$scientificName, SouthM$scientificName)
length(Shared_species)


shared_summary <- moths %>%
  filter(scientificName %in% Shared_species) %>%      
  group_by(scientificName, site) %>%
  summarise(total_abundance = sum(individualCount),
            .groups = "drop") %>%
  group_by(scientificName) %>%
  mutate(species_total = sum(total_abundance)) %>%    
  ungroup() %>%
  arrange(desc(species_total)) %>%
  mutate(scientificName = factor(scientificName,
                                 levels = unique(scientificName)))

ggplot(shared_summary,
       aes(x = scientificName, y = total_abundance, fill = site)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "Shared species",
       y = "Abundance",
       fill = "Site",
       title = "Abundance of moth species shared between Sites") +
  theme_minimal()
ggsave("Results/Shared_Moth_species_abundance.png")

#trying to make a venn diagram to make it clearer between shared and unique species for each plot 
#first we create new species sets for the unique species 
north_species<- unique(NorthM$scientificName)
south_species<- unique(SouthM$scientificName)
#Using the venn diagram package 
install.packages("ggVennDiagram")

library(ggVennDiagram)

# the code below creates a venn-diagram separating shared species which we created for the last plot and unique species 
#I then added the code for label_alpha = 0 to remove the dark background as it was hard to see the numbers - I changed the colours for a similar reason. 

ggVennDiagram(
  venn_list,
  category.names = c("North", "South"),
  label_alpha = 0          
) +
  scale_fill_gradient(low = "#e6f2ff", high = "#5dade2") +  
  labs(title = "Shared vs unique moth species\nNorth vs South") +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 40, 10, 60)
  ) +
  coord_cartesian(clip = "off")

ggsave("Results/Venn diagram for moth species.png")
