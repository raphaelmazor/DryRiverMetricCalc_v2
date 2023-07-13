library(tidyverse)

ants<-read_csv("Data/GlobalAnts.csv")
# arthropods_df<-read_xlsx("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - RB9 SWAMP 2020/Reports/Arthropod STE/STE_Arthropods_July 2023.xlsx",
#                          sheet="Sheet1")


names(ants)
ants_df<-ants %>%
  transmute (Genus, Species = paste(Genus, Species), Size=`Whole body length (mm)`, Diet)

ants_sp<-ants_df %>%
  group_by(Genus, Species) %>%
  summarise(MeanSize=mean(Size, na.rm=T),
            MaxSize=max(Size, na.rm=T),
            MinSize=min(Size, na.rm=T)) 


ants_gen<-ants_sp %>%
  group_by(Genus) %>%
  summarise(MeanSize_Genus=mean(MeanSize, na.rm=T),
            MaxSize_Genus=max(MeanSize, na.rm=T),
            MinSize_Genus=min(MeanSize, na.rm=T)) 



ants_combined<-ants_sp %>%
  inner_join(ants_gen)
write_clip(ants_combined)

arthropods_df %>%
  filter(Family=="Formicidae")


ants_df %>%
  group_by(Genus, Species, Diet) %>%
  tally() %>%
  write_clip()
