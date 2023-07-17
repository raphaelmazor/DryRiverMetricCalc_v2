#Gossner et al. 2015. A summary of 8 traits of Coleoptera, Hemiptera, Orthoptera, and Araneae in German Grasslands
library(tidyverse)
library(readxl)
gossner_df<-read_csv("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Metric calculator for dry stream data/Misc traits/Beetles/Gossner et al 2016_ArthropodSpeciesTraits.csv")

arthropods_df<-read_xlsx("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - RB9 SWAMP 2020/Reports/Arthropod STE/STE_Arthropods_July 2023.xlsx",
                         sheet="Sheet1")

###BODY SIZE
# Body size was defined as the mean body length in mm over males and females. We standardized available information within 
# one taxon by using information given in one standard compilation, i.e., Freude et al.20 for Coleoptera, Biedermann and 
# Niedringhaus27 for Hemiptera: Auchenorrhyncha, Wachmann et al.29 for Hemiptera: Heteroptera, and the spiders of Europe 
# website50 for Araneae. In Orthoptera, sizes were averaged across values given in different sources52–54.

gossner_bodysize<-gossner_df %>%
  filter(!is.na(Body_Size)) %>%
  group_by(Family) %>%
  summarise(Gossner_BodySizeMax=max(Body_Size, na.rm = T),
            Gossner_BodySizeMin=min(Body_Size, na.rm = T)) %>%
  ungroup()

length(gossner_bodysize$Family)== unique(length(gossner_bodysize$Family))


####DISPERSAL
# Dispersal ability, ranging from 0 to 1 by steps of 0.25, was defined differently for the groups, depending on available information.
# For Hemiptera and most Coleoptera, wing dimorphism between males and females was used, assigning 1 for species with fully developed 
# wings in both sexes; 0.75 for predominantly macropterous species; 0.5 for equally brachypterous and macropterous species; 
# 0.25 for predominantly brachypterous species and 0 for always brachypterous species. For other Coleoptera, dispersal ability was 
# based on descriptions of flying ability20,26. Species of Araneae were assigned to the five dispersal groups taking into account 
# activity ranges and dispersal strategies (e.g., ballooning and migration), given in different sources41–48. 
# Species which are known to be very frequent ‘aeronauts’ and which are observed very frequently outside their main habitat were assigned 1.
# In contrast, class 0 would include species which never show ballooning behavior, but this did not apply to any sampled species. 
# Species which show ballooning only over a few meter distance and were never observed outside their main habitat were assigned 0.25. 
# When species observations outside the main habitat have been observed more frequently, species were assigned to 0.5 or 0.75 depending 
# on the relative frequency of these observations. For Orthoptera, active dispersal ability is estimated on the basis of the size of 
# the hind wings (alae), the occurrence of macropterous forms and studies of individual movement and colonization dynamics. 
# We followed the classification of Reinhardt et al.59 for this trait, which takes these three criteria into account.
# It is largely based on the (a) wing development of the adults57, complemented by (b) results from population studies
# on individual movements reviewed in55,56 and long-term observations of local and regional colonization dynamics reviewed in55,56.
# Additionally, (c) passive dispersal behavior, e.g., by eggs attached to stipes and spread by flooding was considered51,52.
# All flightless species were assigned very low (0) dispersal ability and low (0.25) dispersal ability if they were using passive 
# dispersal behavior. The other species which are able to fly were assigned to the higher dispersal abilities depending on their 
# dispersal behavior considering criteria (b); minor individual movements and colonization dynamics was assigned medium (0.5) 
# dispersal ability, average individual movements and colonization dynamics was assigned high (0.75) dispersal ability and 
# high individual movements and colonization dynamics was assigned very high (1) dispersal ability.

gossner_dispersal<-gossner_df %>%
  filter(!is.na(Dispersal_ability)) %>%
  group_by(Order, Suborder, Family, Dispersal_ability) %>%
  tally() %>%
  group_by(Order, Suborder, Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  slice_max(order_by = Dispersal_ability, with_ties = F) %>%
  ungroup() %>%
  select(Family, 
         Gossner_DispersalAbility=Dispersal_ability) #Most common dispersal ability, ranked 0, .25, .5, .75, and 1. Ties favor greater dispersal category.

length(gossner_dispersal$Family)==length(unique(gossner_dispersal$Family))
####FEEDING GUILD
#Short: (h)erbivores, (p)redators or (c) carnivores, (f)ungivores, (d)etritivores, (o)mnivores

gossner_feedingguildshort<-gossner_df %>%
  group_by(Family, Feeding_guild_short ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  #There was a tie for Buprestidae (h and d). I assigned h
  filter(Family!="Buprestidae" | Feeding_guild_short=="h") %>%
  ungroup() %>%
  select(Family, Gossner_FeedingGuild_short=Feeding_guild_short)

length(gossner_feedingguildshort$Family)==length(unique(gossner_feedingguildshort$Family))
#long: 
gossner_feedingguild<-gossner_df %>%
  group_by(Family, Feeding_guild ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # group_by(Family) %>% tally() %>% filter(n>1) #Again, ties for Buprestidae
  #There was a tie for Buprestidae (h and d). I assigned h
  filter(Family!="Buprestidae" | Feeding_guild=="h") %>%
  ungroup() %>%
  select(Family, Gossner_FeedingGuild=Feeding_guild)

length(gossner_feedingguild$Family)==length(unique(gossner_feedingguild$Family))


#long2: Ignore parentheses
gossner_df$Feeding_guild %>% unique() %>%sort()
gossner_feedingguild2<-
  gossner_df %>%
  mutate(Feeding_guild2 = str_remove_all(Feeding_guild,"[()]")  ) %>%
  group_by(Family, Feeding_guild2 ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # group_by(Family) %>% tally() %>% filter(n>1) #Again, ties for Buprestidae
  #There was a tie for Buprestidae (h and d). I assigned h
  filter(Family!="Buprestidae" | Feeding_guild2=="h") %>%
  ungroup() %>%
  select(Family, Gossner_FeedingGuild2=Feeding_guild2)

length(gossner_feedingguild2$Family)==length(unique(gossner_feedingguild2$Family))
gossner_feedingguild2$Gossner_FeedingGuild2 %>% unique()
gossner_feedingguild$Gossner_FeedingGuild %>% unique()

###FEEDING MODE
# Feeding mode was defined as the way nutrients are ingested. We distinguished between three different modes: 
# (e)xtraintestinal digestion, which is common in predacious spiders; 
# (c)hewing of plant or animal tissue by beetles and grasshoppers; 
# (s)ucking on plant or animal sap by Hemiptera.
gossner_df$Feeding_mode %>% unique() %>% sort()

gossner_feedingmode<-
  gossner_df %>%
  group_by(Family, Feeding_mode ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # group_by(Family) %>% tally() %>% filter(n>1) #No ties
  ungroup() %>%
  select(Family, Gossner_FeedingMode=Feeding_mode)
length(gossner_feedingmode$Family)==length(unique(gossner_feedingmode$Family))

###FEEDING SPECIALIZATION
# Feeding specialization was defined as host plant niche breadth of herbivores based on the main higher plant lineages60. 
# We classified (m)onophages as species feeding on one plant genus, 
# (o)ligophages as species feeding on one higher plant lineage (i.e., bryophytes, ferns, gymnosperms, angiosperms: monocots, 
# angiosperms: basal eudicots, angiosperms: eurosids, angiosperms: euasterids), 
# and (p)olyphages as species feeding on more than one higher plant lineage. Generally, the classification is based on larval 
# stages. Exceptions are Coleoptera species that develop in dead wood, but feed on plant pollen as adults, in which the 
# specialization of adults was used. Classification is based on Böhme23,24, Koch61 and Rheinheimer & Hassler26 for 
# herbivorous Coleoptera, Nickel28 for Auchenorrhyncha, Wachmann et al.29 for herbivorous Heteroptera, and Baur et al.53,
# Detzel58, Ingrisch & Köhler55, and Maas et al.52 for herbivorous Orthoptera.

gossner_df$Feeding_specialization %>% unique() %>% sort()

gossner_feedingspecialization<-
  gossner_df %>%
  filter(!is.na(Feeding_specialization)) %>%
  group_by(Family, Feeding_specialization ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # group_by(Family) %>% tally() %>% filter(n>1) #Several ties
  # filter(Family %in% c("Acrididae","Aphrophoridae")) These are O and P. Default to P
  mutate(Feeding_specialization=case_when(Family %in% c("Acrididae","Aphrophoridae")~"p",T~Feeding_specialization)) %>%
  ungroup() %>%
  select(Family, Gossner_FeedingSpecialization=Feeding_specialization) %>%
  unique()

length(gossner_feedingspecialization$Family)==length(unique(gossner_feedingspecialization$Family))


###FEEDING TISSUE
# For sucking herbivores we list the feeding tissue on which the species are sucking during the larval and 
# during the adult stage. We distinguished between xylem-, phloem-, and mesophyll-suckers and additionally 
# assigned species that suck on reproduction organs (flowers and unripe seed on the plant), which is common 
# in the Heteroptera families Miridae, Pentatomidae and Rhopalidae and on hard lipid-rich ripe seeds on the 
# ground, which is common in the Heteroptera family Lygaeidae. Classification is based on Nickel28 for Auchenorrhyncha 
# and Wachmann et al.29 for Heteroptera.
# x: xylem
# p: phloem
# m: mesophyll
# r: reproductive organs (flowers and unripe seed on plant)
# se: hard lipid seeds on the ground

gossner_df$Feeding_tissue %>% unique() %>% sort()

gossner_feedingtissue<-
  gossner_df %>%
  filter(!is.na(Feeding_tissue)) %>%
  group_by(Family, Feeding_tissue ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # group_by(Family) %>% tally() %>% filter(n>1) #No ties
  ungroup() %>%
  select(Family, Gossner_FeedingTissue=Feeding_tissue) %>%
  unique()

length(gossner_feedingtissue$Family)==length(unique(gossner_feedingtissue$Family))

# feedingtissue2: ignore parentheses
gossner_feedingtissue2<-
  gossner_df %>%
  filter(!is.na(Feeding_tissue)) %>%
  mutate(Feeding_tissue2 = str_remove_all(Feeding_tissue,"[()]")  ) %>%
  
  group_by(Family, Feeding_tissue2 ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # group_by(Family) %>% tally() %>% filter(n>1) #No ties
  ungroup() %>%
  select(Family, Gossner_FeedingTissue2=Feeding_tissue2) %>%
  unique()

length(gossner_feedingtissue2$Family)==length(unique(gossner_feedingtissue2$Family))

###FEEDING PLANT PART
# For chewing herbivores we list the plant parts on which the species are feeding on during the larval and during the adult stage.
# We distinguished between root-feeders, stem-feeders, leaf-feeders and feeders on flowers, pollen and unripe seeds on the plant
# (combined to feeders of reproductive organs). Classification is based on Böhme23,24, Koch61 and Rheinheimer & Hassler26 for 
# herbivorous Coleoptera, and Baur et al.53, Detzel58, Ingrisch & Köhler55 and Maas et al.52 for Orthoptera.
# 
# ro: roots
# l: leaves
# s: stems
# r: reproductive organs (flowers, pollen, and unripe seeds on the plant)

gossner_df$Feeding_plant_part %>% unique() %>% sort()

gossner_feedingplantpart<-
  gossner_df %>%
  filter(!is.na(Feeding_plant_part)) %>%
  group_by(Family, Feeding_plant_part ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # group_by(Family) %>% tally() %>% filter(n>1) #two ties
  # filter(Family %in% c("Conocephalidae","Tettigoniidae")) #overwrite with more inclusive category
  mutate(Feeding_plant_part = case_when(Family=="Conocephalidae"~"l-r-s",
                                        Family=="Tettigoniidae"~"l-r",
                                        T~Feeding_plant_part)) %>%

  ungroup() %>%
  select(Family, Gossner_FeedingPlantPart=Feeding_plant_part) %>%
  unique()

length(gossner_feedingplantpart$Family)==length(unique(gossner_feedingplantpart$Family))

#plant part 2: ignore parentheses


gossner_feedingplantpart2<-
  gossner_df %>%
  filter(!is.na(Feeding_plant_part)) %>%
  mutate(Feeding_plant_part2 = str_remove_all(Feeding_plant_part,"[()]")  ) %>%
  
  group_by(Family, Feeding_plant_part2 ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # group_by(Family) %>% tally() %>% filter(n>1) #two ties
  # filter(Family %in% c("Conocephalidae","Tettigoniidae")) #overwrite with more inclusive category
  mutate(Feeding_plant_part2 = case_when(Family=="Conocephalidae"~"l-r-s",
                                        Family=="Tettigoniidae"~"l-r",
                                        T~Feeding_plant_part2)) %>%
  
  ungroup() %>%
  select(Family, Gossner_FeedingPlantPart2=Feeding_plant_part2) %>%
  unique()

length(gossner_feedingplantpart2$Family)==length(unique(gossner_feedingplantpart2$Family))



###ENDOPHAGOUS LIFESTYLE
# Within herbivorous species we additionally classified species with an endophagous lifestyle of their larvae
# into gall-inducers and miners following information given in Koch61 and Rheinheimer & Hassler26 for herbivorous 
# Coleoptera, Nickel28 and Nickel & Remane62 for Auchenorrhyncha, and Wachmann et al.29 for herbivorous Heteroptera. 
# We additionally define the tissue were the larvae are feeding, e.g., leaf or stem miners.

#
# gx: gall inducer on X plant part (see above)
# mx: miners in x plant part (see above)

gossner_df$Endophagous_lifestyle %>% unique()

gossner_endophagouslifestyle<-
  gossner_df %>%
  filter(!is.na(Endophagous_lifestyle)) %>%
  group_by(Family, Endophagous_lifestyle ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # group_by(Family) %>% tally() %>% filter(n>1) #no ties
  ungroup() %>%
  select(Family, Gossner_Endophagy=Endophagous_lifestyle) %>%
  unique()

length(gossner_endophagouslifestyle$Family)==length(unique(gossner_endophagouslifestyle$Family))


#Endo2: Ignore parentheses


gossner_endophagouslifestyle2<-
  gossner_df %>%
  filter(!is.na(Endophagous_lifestyle)) %>%
  mutate(Endophagous_lifestyle2 = str_remove_all(Endophagous_lifestyle,"[()]")  ) %>%
  
  group_by(Family, Endophagous_lifestyle2 ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # group_by(Family) %>% tally() %>% filter(n>1) #no ties
  ungroup() %>%
  select(Family, Gossner_Endophagy2=Endophagous_lifestyle2) %>%
  unique()

length(gossner_endophagouslifestyle2$Family)==length(unique(gossner_endophagouslifestyle2$Family))


###STRATUM USE
# Vertical stratum use was selected as a trait of habitat preference and defined as the main vegetation layer (stratum) 
# in which the species is usually observed as juvenile and adult. A coarser as well as a finer classification is given. 
# We distinguished between soil-dwelling, ground-dwelling, herb-, shrub and/or tree-layer species, and species linked to water bodies.
# In the finer classification all strata which a particular species uses as habitat are given, with less frequent assignments given 
# in brackets. In the coarser classification, all species that use more than one stratum during their life cycle were categorized as
# unspecific, thus ignoring different larval and adult habitat niches. For example, several Coleoptera of the family Curculionidae 
# (e.g., genera Otiorhynchus, Phyllobius, Sitona) feed on plant roots in the soil in their larval phase but on leaves in the herb 
# layer as adults26. Those species were hence assigned as unspecific in the coarse classification and as soil-and herb-layer species
# in the finer classification. Where specific information was missing (several Auchenorrhyncha & Coleoptera), the stratum was assigned 
# from the main feeding source or the association with certain plant species. Where information on the species level was missing, 
# information on genera or family level was used, provided that the trait value of the higher taxonomic unit was assumed to be equal
# for all species based on information given in literature or based on authors’ experience. Classification is based on Platen49 and 
# Platen et al.32,36, and adapted with information in Maurer & Hänggi31, Malten33,34, Malten & Blick35, Blick38,39 and the expert’s 
# experience for Araneae; on Böhme24, Freude et al.20, Homburg et al.14, and Rheinheimer and Hassler26 for Coleoptera; on Nickel28 for 
# Auchenorrhyncha; on Wachmann et al.29 for Heteroptera, and on Köhler51, Baur et al.53, and Maas et al.52 for Orthoptera.

#s: soil layer
#g: ground
#h: herbaceous layer
#t: tree layer
#w: water
#u: unspecific

gossner_df$Stratum_use %>% unique() %>% sort()


gossner_stratumuse<-
  gossner_df %>%
  filter(!is.na(Stratum_use)) %>%
  group_by(Family, Stratum_use ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # filter(Family=="Dictynidae") %>%
  # group_by(Family) %>% tally() %>% filter(n>1) #several ties
  mutate(Stratum_use = case_when(Family=="Berytidae"~"g-h", #alts were g-(h) and (g)-h
                                 Family=="Buprestidae"~"h-t", #alts were h and t
                                 Family=="Coreidae"~"g-h-(t)", #alts were g, g-(h), g-(h)-(t), and h
                                 Family=="Dictynidae"~"g-h-t",#Alts were g, (h)-t, h-(t), and t
                                 T~Stratum_use)) %>%
  ungroup() %>%
  select(Family, Gossner_Stratum=Stratum_use) %>%
  unique()

length(gossner_stratumuse$Family)==length(unique(gossner_stratumuse$Family))


#stratumuse2: ignore parentheses

gossner_stratumuse2<-
  gossner_df %>%
  filter(!is.na(Stratum_use)) %>%
  mutate(Stratum_use2 = str_remove_all(Stratum_use,"[()]")  ) %>%
  
  group_by(Family, Stratum_use2 ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # filter(Family=="Aphrophoridae")
  # group_by(Family) %>% tally() %>% filter(n>1) #several ties
  mutate(Stratum_use2 = case_when(Family=="Theridiidae"~"h-t", #alts were h and h-t
                                 Family=="Buprestidae"~"h-t", #alts were h and t
                                 Family=="Coreidae"~"g-h-t", #alts were g, g-(h), g-(h)-(t), and h
                                 Family=="Aphrophoridae"~"h-t",#Alts were h and h-t
                                 T~Stratum_use2)) %>%
  ungroup() %>%
  select(Family, Gossner_Stratum=Stratum_use2) %>%
  unique()

length(gossner_stratumuse2$Family)==length(unique(gossner_stratumuse2$Family))



gossner_df$Stratum_use_short %>% unique()

gossner_stratumuseshort<-
  gossner_df %>%
  filter(!is.na(Stratum_use_short)) %>%
  group_by(Family, Stratum_use_short ) %>%
  tally() %>%
  group_by( Family) %>%
  mutate(ntot=sum(n),
         prop_n=n/ntot) %>%
  slice_max(order_by = prop_n,with_ties = T) %>%
  select(-n) %>%
  # filter(Family=="Buprestidae")
  # group_by(Family) %>% tally() %>% filter(n>1) #several ties
  mutate(Stratum_use_short = case_when(Family=="Aphrophoridae"~"u", #alts were h and u
                                 Family=="Araneidae"~"u", #alts were h and u
                                 Family=="Buprestidae"~"h", #alts were h and t
                                 T~Stratum_use_short)) %>%
  ungroup() %>%
  select(Family, Gossner_Stratum_short=Stratum_use_short) %>%
  unique()

length(gossner_stratumuseshort$Family)==length(unique(gossner_stratumuseshort$Family))


#######################
###Stitch them together

gossner_traits<-
  gossner_df %>%
  select(Order, Family) %>%
  unique() %>%
  left_join(gossner_bodysize) %>%
  left_join(gossner_dispersal) %>%
  left_join(gossner_feedingguild) %>%
  left_join(gossner_feedingguild2) %>%
  left_join(gossner_feedingguildshort) %>%
  left_join(gossner_feedingmode) %>%
  left_join(gossner_feedingspecialization) %>%
  left_join(gossner_feedingtissue) %>%
  left_join(gossner_feedingtissue2) %>%
  left_join(gossner_feedingplantpart) %>%
  left_join(gossner_feedingplantpart2) %>%
  left_join(gossner_endophagouslifestyle) %>%
  left_join(gossner_endophagouslifestyle2) %>%
  left_join(gossner_stratumuse) %>%
  left_join(gossner_stratumuse2) %>%
  left_join(gossner_stratumuseshort) 

length(gossner_traits$Family)  ==  length(unique(gossner_traits$Family))

gossner_traits[duplicated(gossner_traits$Family ),]

setdiff(arthropods_df$Family, gossner_traits$Family)
setdiff(gossner_traits$Family, arthropods_df$Family)

write_csv(gossner_traits, "Outputs/gossner_traits.csv")            


library(clipr)
write_clip(arthropods_df %>%
             left_join(gossner_traits)
)


arthropods_df %>%
  left_join(gossner_traits) %>%
  ggplot(aes(x=Ubick_BodySizeMax, y=Gossner_BodySizeMax ))+
  geom_point()+
  geom_smooth(method=lm)+
  geom_abline()
