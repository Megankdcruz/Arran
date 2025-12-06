library("tidyverse")
library("ggplot2")
library("readr")
#In this Rscript I am going to make some figures to represent the diversity and species richness of invertebrates on both of the study sites 
# first to load in the data 
setwd("C:/Users/megdc/OneDrive/Documents/Arran/Arran_results")
Nkick<-read.csv("Data/North_Kicksample.csv")
Skick<-read.csv("Data/South_Kicksample.csv")
Nmoth<-read.csv("Data/NorthSlopeMoths.csv")
Smoth<-read.csv("Data/SouthSlopeMoths.csv")
Ninv<-read.csv("Data/NorthInverts.csv")
Sinv<-read.csv("Data/South_Invertebrates.csv")
# these data sets represent the invertebrates from both the sites across - streams, land, flying 
# however not all of the organisms found were identified to species level so the first thing I want to do is filter for that and separate those ordered to species and those not into different data frmes 
All_invert<- bind_rows(
  Nkick %>% select(scientificName, eventID, taxonRank,Order) %>% mutate(site = "North", group = "Stream"),
  Skick %>% select(scientificName, eventID, taxonRank,Order) %>% mutate(site = "South", group = "Stream"),
  Nmoth %>% select(scientificName, eventID, taxonRank,Order) %>% mutate(site = "North", group = "Flying"),
  Smoth %>% select(scientificName, eventID, taxonRank,Order) %>% mutate(site = "South" , group = "Flying"),
  Ninv %>% select(scientificName, eventID, taxonRank,Order) %>% mutate(site = "North", group = "Pit/sweep"),
  Sinv %>% select(scientificName, eventID, taxonRank,Order) %>% mutate(site = "South", group = "Pit/sweep")
)
# I included order as well as scientific name because - the organisms identified to order or family sometimes do not have that field filled in 
# taxon rank is also included as this is what I am going to use to separate species from order and family 

All_invert<- All_invert %>%
  mutate(taxonRank = trimws(tolower(taxonRank)))
#this removes and spaces in the words and makes all the words in taxon rank column lower case - to make sure nothing is missed by the next part of code
#Now I am going to pull only the rows which have been identified to species level into a separate dataframe 
invert_species<- All_invert %>%
  filter(taxonRank == "species")
#Then just putting the organisms that have been identified to order, family or any other level in another data frame 

invert_order<- All_invert %>%
  filter(taxonRank %in% c("order","family","super-family","suborder","infraorder","genus"))

#Now that we have got our data sets we want to work with I can look at species diversity 
Invert_Sprichness_site <- invert_species %>%
  group_by(site) %>%
  summarise(species_richness = n_distinct(scientificName))
Invert_Sprichness_site
#Plot 
ggplot(Invert_Sprichness_site,aes(x=site,y=species_richness,fill=site))+
  geom_col() +
  scale_fill_manual(values = c("North"="#8E8A98",
                               "South"="#CCE1E4"
  ))+
  theme_minimal()+
  labs(
    title = "Invertebrate Species Richness by Site",
    x="Site",
    y= "Number of species"
  ) + 
  theme(legend.position = "none")
ggsave("Results/Invert_sprichness_site.png")

# Now I am going to do the same for the orders and do an order richness plot
Invert_orderrichness_site<- invert_order %>%
  group_by(site) %>%
             summarise(order_richness = n_distinct(Order))
#plot
ggplot(Invert_orderrichness_site,aes(x=site,y=order_richness,fill=site))+
  geom_col() +
  scale_fill_manual(values = c("North"="#8E8A98",
                               "South"="#CCE1E4"
  ))+
  theme_minimal()+
  labs(
    title = "Invertebrate Order Richness by Site",
    x="Site",
    y= "Number of orders"
  ) + 
  theme(legend.position = "none")
ggsave("Results/invert_Orderrich_site.png")
# I can also categorise them by group to give more detail 
richness_by_site_group <- invert_species %>% 
  group_by(site, group) %>% 
  summarise(richness = n_distinct(scientificName), .groups = "drop")
#plot
ggplot(richness_by_site_group,
       aes(x = site, y = richness, fill = group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Flying"   = "#8E8A98",
    "Stream"    = "#CCE1E4",
    "Pit/sweep" = "#A9C47F"   
  )) +
  theme_minimal() +
  labs(
    title = "Invertebrate Species Richness by Site and Group",
    x = "Site",
    y = "Number of species",
    fill = "Group"
  )
ggsave("Results/Invert_spRich_SG.png")
#doing the same for the order/family species 
Ordrichness_by_site_group <- invert_order %>% 
  group_by(site, group) %>% 
  summarise(richness = n_distinct(Order), .groups = "drop")
#plot
ggplot(Ordrichness_by_site_group,
       aes(x = site, y = richness, fill = group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Flying"   = "#8E8A98",
    "Stream"    = "#CCE1E4",
    "Pit/sweep" = "#A9C47F"   
  )) +
  theme_minimal() +
  labs(
    title = "Invertebrate Order Richness by Site and Group",
    x = "Site",
    y = "Number of Orders",
    fill = "Group"
  )
ggsave("Results/Orderrich_invert_SG.png")

#the order richness look better for these - I think I will do an order richness for all the observations to see if that is a better graph to use - instead of two seperate ones 
#for this I will use my All inverts data frame 
AllOrdrichness_by_site_group <- All_invert %>% 
  group_by(site, group) %>% 
  summarise(richness = n_distinct(Order), .groups = "drop")
#now plotting it 
ggplot(AllOrdrichness_by_site_group,
       aes(x = site, y = richness, fill = group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    "Flying"   = "#8E8A98",
    "Stream"    = "#CCE1E4",
    "Pit/sweep" = "#A9C47F"   
  )) +
  theme_minimal() +
  labs(
    title = "Invertebrate Order Richness by Site and Group",
    x = "Site",
    y = "Number of Orders",
    fill = "Group"
  )
ggsave("Results/Allinvt_orderrich.png")

#The last thing I want to do is looking at the BWMP scores for the freshwater invertebrates and look at the different scores for each slope and if tey are different for either site 
view(Skick)
#There are a few scores missing so I am going to fill those in first using the dplyr package and the case_when
#Corixidae, zygoptera,anisoptera, Notonectidae

Skick$BMWPscore<- as.numeric(Skick$BMWPscore)

Skick<- Skick %>%
  mutate(
  BMWPscore = case_when(
    scientificName == "Corixidae"~ 5,
    scientificName == "Notonectidae" ~ 5,
    scientificName == "zygoptera" ~ 8,
    scientificName == "anisoptera" ~ 8, 
    TRUE ~ BMWPscore
  )
)

view(Skick)

#THIS WOULD NOT WORK 
#Because BMWP scores are assigned by family - after further research it is not advised to enter scores for order as it is too broad 
#therefore I cannot make a plot because - the scores are in ranges for most of the invertebrates 
#im going to instead try and make a table with the scores including ranges for organisms identified to order 
# "Corixidae",    "Family",   "5",        "Lesser Water Boatman",
BMWP_Table<- tribble(
  ~Taxon,       ~Level,    ~BMWPscore,   ~Notes,                                    ~Count,        ~Site,
  "Plecoptera", "Order",    "8-10",       "Stonefly larvae, mostly high scoring",   "7",              "North",
   "Ephemeroptera", "Order", "4-10",    "Mayfly larvae vary widely",                "6",               "North",
  "coleoptera",  "Order",  "2-10",        "Large variation between families both larvae and adult found",   "5",  "North",
  "Oligochaeta", "Order",  "1-4",         "Worms: low pollution-tolerant taxa",     "7",               "North",
  "Odonata",     "Family",  "8",          " anisoptera: Dragonfly larvae",           "2",               "North",
  "Trichoptera",  "Order",   "5-10",      "Vary a lot",                              "5",               "North",
  "Trichoptera",  "Family",  "6",          " Hydroptillidae: Caddisfly larvae",      "1",               "North",
  "Trichoptera",  "Family",   "7",        " Philopotamidae: Caddisfly family larvae", "1",               "North",
  "Oligochaeta",  "Family",   "1-4",      "Worms: low pollution-tolerant taxa",       "1",               "South",
  "Diptera",      "Order",    "1-5",      "Flies generally low scoring unless family identified", "1",    "South",
  "Coleoptera",   "Family",   "5",      "Dytiscidae: diving beetle",                 "1",                "South",
  "Hemiptera",    "Family",   "5",       "Boatman",                                   "4",                "South",
  "Odonata",      "Family",    "8",      "Damselfly larvae",                           "2",              "South",
  "Odonata",      "Family",     "8",      "Dragonfly larvae",                         "3",                "South",
  "Hemiptera",    "Family",   "5",        "Boatman",                                   "2",             "South",
  "Plecoptera",    "Order",    "8-10",      "Stoneflies: mostly high scoring",         "9",               "South",
  "Coleoptera",    "Order",    "2-10",     "Large variation between families larvae and adult found",  "2",   "South",
  "Trichoptera",   "Order",    "5-10",    "Caddisflies vary a lot",                    "2",                "South",
  "Ephemeroptera",  "Order",    "4-10",    "Mayflies vary widely",                      "4",             "South",
  "Diptera",        "Family",   "5",       "Blackfly larvae",                          "1",               "South",
 
  
)
#that took so long 
#I am now going to save the table in a format that will allow it to be pasted into word 
write.csv(BMWP_Table, "Results/BMWP_table.csv", row.names = FALSE)
