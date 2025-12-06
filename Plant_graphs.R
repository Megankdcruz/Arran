install.packages("ggplot2")
library("ggplot2")
install.packages("tidyverse")
library("tidyverse")
North<-read.csv("Data/NorthSlopePlants.csv")
library(readr)
North <- read_csv("Data/NorthSlopePlants.csv")
View(North)
South<-read.csv("Data/SouthSlopePlants.csv")
View(South)
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

#combine them into a dataframe to make plotting easier 
richness<- data.frame(
  site= c("North", "South"),
  richness=c(North_richness$species_richness, South_richness$species_richness))
richness
#I want to plot the species richness for each site 
#to do this I will use the package ggplot2 
ggplot(richness,aes(x=site,y=richness,fill=site))+
  geom_col() +
  scale_fill_manual(values = c("North"="coral3",
                               "South"="darkgoldenrod1"
                               ))+
  theme_minimal()+
  labs(
    title = "Species Richness by Site",
    x="Site",
    y= "Number of species"
  ) + 
  theme(legend.position = "none")
#save figures in case I want to upload to Github 
ggsave("Results/Species_richness_graph1.png")

#I want to see species richness per transect to relate to habitats and split up the data for the slops a little bit 
#To do this I will first extract the eventID using the first two characters which denote the transect
NorthTran<- North %>%
  mutate(transect=substr(eventID,1,2)) 
SouthTran<- South %>% 
  mutate(transect = substr(eventID,1,2))
NorthTran
#combining the data sets to make it easier to plot 
CombinedTrans<- rbind(NorthTran,SouthTran)
#calculate species richness for each transect 
transect_richness<- CombinedTrans %>%
  group_by(transect)%>%
  summarise(richness=n_distinct(scientificName))
transect_richness
#plot transect richness 
ggplot(transect_richness,aes(x=transect, y=richness, fill = transect))+
  geom_col()+
  scale_fill_manual(values = c(
    "N1"="coral3",
    "N2"="coral2",
    "N3"="coral1",
    "S1"="goldenrod1",
    "S2"="goldenrod2",
    "S3"="goldenrod3"
    ))+
  theme_minimal()+
  labs(
    title="Species Richness per Transect (North & South)",
    x="Transect",
    y="Species Richness"
  )
#save plot 
ggsave("Results/Species_Richness_Transects.png")
#I also want to calculate the most dominant plant species per transect 
#we will use the combined data set for the transects which I created for the last plot 
#To get the most abundant/dominant species we need to could the species occurences per transect 
species_counts<- CombinedTrans %>%
  group_by(transect, scientificName) %>%
  summarise(count=n(), .groups = "drop")
species_counts
#now I can extract the most 'dominant' species which will be the species with the highest count. 
dominant_species<-species_counts %>%
  group_by(transect) %>%
  slice_max(order_by = count, n=1) %>%
  ungroup()
dominant_species
#plotting the dominant species per transect 
ggplot(dominant_species, aes(x = transect, y = count, fill = scientificName)) +
  geom_col()+
  scale_fill_manual(values = c(
      "Erica_cinerea"              = "mediumpurple",
      "Rhytidadelphus_triquetrus"  = "olivedrab1",
      "Galium_mollugo"             = "ivory3",
      "Pteridium_aquilinum"        = "lightsalmon3",
      "Nardus_stricta"             = "darkseagreen3"
  ))+
  theme_minimal() +
  labs(
    title = "Most Dominant Species by Transect",
    x = "Transect",
    y = "Number of Records",
    fill = "Dominant Species"
  ) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  )
ggsave("Results/Dominant_sp_transect.png")



#Running a Shannon's diversity index on both slopes to determine which slope has higher diversity 
install.packages("vegan")
library("vegan")
library("dplyr")
#making another north and south dataset from which I can create a joint table and matrix 
North2<-North %>%
  mutate(site = "North")
South2<-South %>%
  mutate(site = "South")
#now combining them 
all_sites<- bind_rows(North2, South2)
#Error in `bind_rows()`:
#! Can't combine `..1$eventDate` <date> and `..2$eventDate` <character>.
#Run `rlang::last_trace()` to see where the error occurred.
#because of the above error, need to chance the types of data to be the same otherwise they cant be combined 
North2 <- North %>%
  mutate(
    site = "North",
    eventDate = as.Date(eventDate)   # if already Date, it stays Date; if character, it converts
  )

South2 <- South %>%
  mutate(
    site = "South",
    eventDate = as.Date(eventDate)   # may need a format if dates look like "01/06/2024"
  )

all_sites <- bind_rows(North2, South2)
#Now that there are in 1 data frame we can create a site matrix to use for the shannons index 
site_matrix<- all_sites %>%
  group_by(site,scientificName) %>%
  summarise(count= n(), .groups = "drop") %>%
  pivot_wider(
    names_from = scientificName,
    values_from = count,
    values_fill = 0
  )
#to run a shannons index we need only numerical information so I need to remove the first column which is 'site' 
sites<- site_matrix$site
species_counts<- as.matrix(site_matrix[,-1])
#now I can run the diversity index 
Shannon_values<- diversity(species_counts, index = "shannon")
shannon_results<- data.frame(
  sites = sites,
  shannon = Shannon_values)
shannon_results
#Running another graph to show top three dominant species for each site north and south 
#I can do this using my all sites dataframe
#I will count the species per site and pick the 3 with the highest counts 
sp_count_site<- all_sites %>%
  group_by(site,scientificName) %>%
  summarise(count = n(), .groups="drop")

top3_site<- sp_count_site %>% 
  group_by(site) %>%
  slice_max(order_by = count, n=3, with_ties = FALSE) %>%
  ungroup()

top3_site
#now to plot them 
ggplot(top3_site, aes(x = site, y = count, fill = scientificName)) +
  geom_col()+
  scale_fill_manual(values = c(
    "Rhytidadelphus_triquetrus" = "olivedrab3",
    "Galium_mollugo"             = "ivory3",
    "Pteridium_aquilinum"        = "lightsalmon3",
    "Nardus_stricta"             = "darkseagreen3",
    "Potentilla_erecta"          = "gold2",
    "Pteridium_aquilinum"        = "lightsalmon3"
  ))+
  theme_minimal() +
  labs(
    title = "3 Most Dominant species by Site",
    x = "Site",
    y = "Number of Records",
    fill = "Top 3 Dominant Species"
  ) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  )
ggsave("Results/3_Dom_Site.png")  

#Removing unimportant or incorrect values and data frames from my environment 
rm(plant)
rm(shannon_values)
rm(PlantData)
rm(PlantData_fixed)
