library(tidyverse)
library(readxl)
library(clipr)

#Import metadata
meta_arth_df<-read_xlsx("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - RB9 SWAMP 2020/Reports/Arthropod STE/STE_Arthropods_July 2023.xlsx",
                        sheet="Sheet1")


#Import sampledata
sample_data<-read_xls("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Dry phase bioassessment RCN/Data/4_FullData/Taxonomy/Bug_Data/Edited_RB4_2021_SWAMP_MASTER_Template_RD_DP.xls",
                      sheet="BenthicResults") %>%
  bind_rows(
    read_xls("C:/Users/Raphaelm/SCCWRP/Ephemeral Stream Assessment tools - Dry phase bioassessment RCN/Data/4_FullData/Taxonomy/Bug_Data/Edited_RB9_2021_SWAMP_MASTER_Template_RD_DP.xls",
             sheet="BenthicResults")
    
  ) %>%
  #Standard format for sample ID generation for arthropod samples
  mutate(SampleID=paste(StationCode, SampleDate, CollectionMethodCode, Replicate, sep="_"))


arth_tax<-c("STE1_ID","TaxonomicLevelCode",
            "Class","Subclass","Order","Suborder","Superfamily","Family","Subfamily","Tribe","Genus","Species"
)

#Make a long-format dataframe of taxonomic data
arth_taxa_long<-meta_arth_df %>% 
  select(all_of(arth_tax)) %>%
  pivot_longer(cols=c( "Class","Subclass","Order","Suborder","Superfamily","Family","Subfamily","Tribe","Genus","Species")) %>%
  na.omit() %>%
  rename(Original_LevelCode = TaxonomicLevelCode) %>%
  mutate(Parent_LevelCode = case_when(name == "Phylum" ~ 10, #Standard SWAMP levels
                                      name == "Subphylum" ~13,
                                      name == "Superclass" ~ 16,
                                      name == "Class" ~ 20,
                                      name == "Subclass" ~ 23,
                                      name == "Superorder" ~ 26,
                                      name == "Order" ~ 30,
                                      name == "Suborder" ~ 33,
                                      name == "SuperFamily" ~ 36, name == "Superfamily" ~ 36, #SWAMP uses capital F, but our data doesn't.
                                      name == "Family" ~ 40,
                                      name == "Subfamily" ~ 42,
                                      name == "Tribe" ~ 45,
                                      name == "Genus" ~ 50,
                                      name == "Species" ~ 60,
                                      name == "Subspecies" ~ 63,
                                      name == "Variety" ~ 66,
                                      T~ -88
  ))



generate_sample_id<-function(x){
  sample_data_x = x %>%
    mutate(SampleID=paste(StationCode, SampleDate, CollectionMethodCode, Replicate, sep="_"))
  sample_data_x
}

sample_data %>%
  select(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, FinalID, Result) %>%
  left_join(meta_arth_df) %>%
  group_by(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, STE1_ID) %>%
  summarise(Result=sum(Result)) %>%
  ungroup()  %>%
  left_join(arth_taxa_long)


meta_arth_df %>%
  group_by(FinalID) %>%
  tally() %>%
  filter(n>1)

flag_distinct_taxa<-function(x) {
  sample_data_x = x %>%
    select(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, FinalID, Result) %>%
    left_join(meta_arth_df) %>%
    group_by(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, STE1_ID) %>%
    summarise(Result=sum(Result)) %>%
    ungroup()  %>%
    
    left_join(arth_taxa_long, relationship = "many-to-many") %>%
    filter(STE1_ID!=value) %>%
    group_by(SampleID) %>%
    mutate(DistinctTaxon = !STE1_ID %in% value) %>%
    select(SampleID, StationCode, SampleDate, CollectionMethodCode, Replicate, STE1_ID, Result, DistinctTaxon) %>%
    unique() 
  sample_data_x
}

arth_traits<-
  c(
    #Invasiveness
    "Nonnative","Nonnative_Synanthropic",
    #Habitat
    "Aquatic","Stratum_final",
    #Feeding
    "FFG_final_simple","FFG_final_detailed",
    #Body size
    "BodySizeMin","BodySizeMean","BodySizeMax",
    #Other
    "AntWiki_ColonySizeBin",
    "Gossner_DispersalAbility",
    "Ubick_HuntingStyle",
    #Temperature
    "TempMin_final","TempMean_final","TempMax_final"
  )

# 
sample_data_aggregated<-sample_data %>%
  flag_distinct_taxa()

sample_data_aggregated %>% write_clip()


# 
# #Calculate these metrics
# # 
# # meta_arth_df %>%
# #   select(STE1_ID,
# #          Class, Order, Family,
# #          all_of(arth_traits)
# #   ) %>%
# #   unique() %>%
# #   group_by(STE1_ID) %>%tally() %>% filter(n>1)
# 
# 
# 
# 
# 
metrics_output<-sample_data_aggregated %>%
  rename(FinalID=STE1_ID) %>%
  left_join(meta_arth_df %>%
              select(FinalID,
                     Class, Order, Family, Genus, Species,
                     all_of(arth_traits)
                     ) %>%
              unique()
            ) %>%
  # mutate(Nonnative_Syn) %>%
  group_by(SampleID, StationCode, SampleDate, CollectionMethodCode, Replicate) %>%
  summarise(
    #Arthrhopods
    Arth_Rich = sum(DistinctTaxon),
    Arth_Abund = sum(Result),
    ##Taxonomic metrics
    #Insects
    Insect_Rich=sum(DistinctTaxon & Class=="Insecta", na.rm=T),
    Insect_RelRich=Insect_Rich/Arth_Rich,
    Insect_Abund = sum(Result[Class=="Insecta"], na.rm=T),
    Insect_RelAbund=Insect_Abund/Arth_Abund,
    #Noninsects
    Noninsect_Rich=sum(DistinctTaxon & Class!="Insecta", na.rm=T),
    Noninsect_RelRich=Noninsect_Rich/Arth_Rich,
    Noninsect_Abund = sum(Result[Class!="Insecta"], na.rm=T),
    Noninsect_RelAbund=Noninsect_Abund/Arth_Abund,
    #Coleoptera
    Coleo_Rich=sum(DistinctTaxon & Order=="Coleoptera", na.rm = T),
    Coleo_RelRich=Coleo_Rich/Arth_Rich,
    Coleo_Abund = sum(Result[Order=="Coleoptera"], na.rm = T),
    Coleo_RelAbund=Coleo_Abund/Arth_Abund,
    #Araneae
    Araneae_Rich=sum(DistinctTaxon & Order=="Araneae" , na.rm=T),
    Araneae_RelRich=Araneae_Rich/Arth_Rich,
    Araneae_Abund = sum(Result[Order=="Araneae"], na.rm=T),
    Araneae_RelAbund=Araneae_Abund/Arth_Abund,
    #Formicidae
    Formicidae_Rich=sum(DistinctTaxon & Family=="Formicidae", na.rm = T),
    Formicidae_RelRich=Formicidae_Rich/Arth_Rich,
    Formicidae_Abund = sum(Result[Family=="Formicidae"], na.rm = T),
    Formicidae_RelAbund=Formicidae_Abund/Arth_Abund,
    #CAF
    CAF_Rich= Coleo_Rich + Araneae_Rich + Formicidae_Rich,
    CAF_RelRich=Coleo_RelRich + Araneae_RelRich + Formicidae_RelRich,
    CAF_Abund=Coleo_Abund + Araneae_Abund + Formicidae_Abund,
    CAF_RelAbund=Coleo_RelAbund + Araneae_RelAbund + Formicidae_RelAbund,
    #Nativeness metrics
    #Nonnative
    Nonnat_KRich=sum(DistinctTaxon & !is.na(Nonnative), na.rm=T),
    Nonnat_Rich=sum(DistinctTaxon & Nonnative=="Nonnative", na.rm=T),
    Nonnat_RelRich=Nonnat_Rich/Nonnat_KRich,
    Nonnat_KAbund = sum(Result[!is.na(Nonnative)], na.rm=T),
    Nonnat_Abund = sum(Result[Nonnative=="Nonnative"], na.rm=T),
    Nonnat_RelAbund=Nonnat_Abund/Nonnat_KAbund,
    #Nonnative or synanthrophic
    NonnatSynanth_KRich=sum(DistinctTaxon & !is.na(Nonnative_Synanthropic), na.rm=T),
    NonnatSynanth_Rich=sum(DistinctTaxon & Nonnative_Synanthropic==1, na.rm=T),
    NonnatSynanth_RelRich=NonnatSynanth_Rich/NonnatSynanth_KRich,
    NonnatSynanth_KAbund = sum(Result[!is.na(Nonnative_Synanthropic)], na.rm=T),
    NonnatSynanth_Abund = sum(Result[Nonnative_Synanthropic==1], na.rm=T),
    NonnatSynanth_RelAbund=NonnatSynanth_Abund/NonnatSynanth_KAbund,
    #Argentine Ant
    LinEpi_Rich=sum(DistinctTaxon & Species=="Linepithema humile", na.rm=T),
    LinEpi_RelRich=LinEpi_Rich/Arth_Rich,
    LinEpi_Abund = sum(Result[Species=="Linepithema humile"], na.rm=T),
    LinEpi_RelAbund=LinEpi_Abund/Arth_Abund,
    ##Habitat metrics
    #Aquatic
    Aquatic_KRich=sum(DistinctTaxon & !is.na(Aquatic), na.rm=T),
    Aquatic_Rich=sum(DistinctTaxon & Aquatic=="Aquatic", na.rm=T),
    Aquatic_RelRich=Aquatic_Rich/Aquatic_KRich,
    Aquatic_KAbund = sum(Result[!is.na(Aquatic)], na.rm=T),
    Aquatic_Abund = sum(Result[Aquatic=="Aquatic"], na.rm=T),
    Aquatic_RelAbund=Aquatic_Abund/Aquatic_KAbund,
    #Stratum final metrcs
    StratumFinal_KRich=sum(DistinctTaxon & !is.na(Stratum_final), na.rm=T),
    StratumFinal_KAbund = sum(Result[!is.na(Stratum_final)], na.rm=T),
    ##Ground
    Ground_Rich=sum(DistinctTaxon & Stratum_final=="Ground or soil layer", na.rm=T),
    Ground_RelRich=Ground_Rich/StratumFinal_KRich,
    Ground_Abund = sum(Result[Stratum_final=="Ground or soil layer"], na.rm=T),
    Ground_RelAbund=Ground_Abund/StratumFinal_KAbund,
    ##Herbaceous
    Herbaceous_Rich=sum(DistinctTaxon & Stratum_final=="Herbaceous layer", na.rm=T),
    Herbaceous_RelRich=Herbaceous_Rich/StratumFinal_KRich,
    Herbaceous_Abund = sum(Result[Stratum_final=="Herbaceous layer"], na.rm=T),
    Herbaceous_RelAbund=Herbaceous_Abund/StratumFinal_KAbund,
    ##Tree/arboreal
    Arboreal_Rich=sum(DistinctTaxon & Stratum_final=="Tree layer", na.rm=T),
    Arboreal_RelRich=Arboreal_Rich/StratumFinal_KRich,
    Arboreal_Abund = sum(Result[Stratum_final=="Tree layer"], na.rm=T),
    Arboreal_RelAbund=Arboreal_Abund/StratumFinal_KAbund,
    ##FFG simple metrics
    FFG_KRich=sum(DistinctTaxon & !is.na(FFG_final_simple), na.rm=T),
    FFG_KAbund = sum(Result[!is.na(FFG_final_simple)], na.rm=T),
    #Detritivore
    Detritivore_Rich=sum(DistinctTaxon & FFG_final_simple=="Detritivore", na.rm=T),
    Detritivore_RelRich=Detritivore_Rich/FFG_KRich,
    Detritivore_Abund = sum(Result[FFG_final_simple=="Detritivore"], na.rm=T),
    Detritivore_RelAbund=Detritivore_Abund/FFG_KAbund,
    #Fungivore
    Fungivore_Rich=sum(DistinctTaxon & FFG_final_simple=="Fungivore", na.rm=T),
    Fungivore_RelRich=Fungivore_Rich/FFG_KRich,
    Fungivore_Abund = sum(Result[FFG_final_simple=="Fungivore"], na.rm=T),
    Fungivore_RelAbund=Fungivore_Abund/FFG_KAbund,
    #Detrritivore + Fungivore
    DetrFung_Rich=Detritivore_Rich + Fungivore_Rich,
    DetrFung_RelRich=Detritivore_RelRich + Fungivore_RelRich,
    DetrFung_Abund = Detritivore_Abund + Fungivore_Abund,
    DetrFung_RelAbund=Detritivore_RelAbund + Fungivore_RelAbund,
    #Predator
    Predator_Rich=sum(DistinctTaxon & FFG_final_simple=="Predator", na.rm=T),
    Predator_RelRich=Fungivore_Rich/FFG_KRich,
    Predator_Abund = sum(Result[FFG_final_simple=="Predator"], na.rm=T),
    Predator_RelAbund=Fungivore_Abund/FFG_KAbund,
    ##FFG detailed metrics
    FFG_d_KRich=sum(DistinctTaxon & !is.na(FFG_final_detailed), na.rm=T),
    FFG_d_KAbund = sum(Result[!is.na(FFG_final_detailed)], na.rm=T),
    #Predator_Ground
    PredatorGround_Rich=sum(DistinctTaxon & FFG_final_detailed=="Predator_Ground", na.rm=T),
    PredatorGround_RelRich=Fungivore_Rich/FFG_d_KRich,
    PredatorGround_Abund = sum(Result[FFG_final_detailed=="Predator_Ground"], na.rm=T),
    PredatorGround_RelAbund=Fungivore_Abund/FFG_d_KAbund,
    ##Body size
    BodySize_KRich=sum(DistinctTaxon & !is.na(BodySizeMean), na.rm=T),
    BodySize_KAbund = sum(Result[!is.na(BodySizeMean)], na.rm=T),
    #Small-bodied (<9 mm, based on Vieira's classifications)
    BodySizeSmall_Rich=sum(DistinctTaxon & BodySizeMean<=9, na.rm=T),
    BodySizeSmall_RelRich=BodySizeSmall_Rich/BodySize_KRich,
    BodySizeSmall_Abund = sum(Result[BodySizeMean<=9], na.rm=T),
    BodySizeSmall_RelAbund=BodySizeSmall_Abund/BodySize_KAbund,
    #Medium-bodied (9-16 mm, based on Vieira's classifications)
    BodySizeMedium_Rich=sum(DistinctTaxon & BodySizeMean>9 & BodySizeMean<=16, na.rm=T),
    BodySizeMedium_RelRich=BodySizeMedium_Rich/BodySize_KRich,
    BodySizeMedium_Abund = sum(Result[BodySizeMean>9 & BodySizeMean<=16], na.rm=T),
    BodySizeMedium_RelAbund=BodySizeMedium_Abund/BodySize_KAbund,
    #large-bodied (16+ mm, based on Vieira's classifications)
    BodySizeLarge_Rich=sum(DistinctTaxon & BodySizeMean>16, na.rm=T),
    BodySizeLarge_RelRich=BodySizeLarge_Rich/BodySize_KRich,
    BodySizeLarge_Abund = sum(Result[BodySizeMean>16], na.rm=T),
    BodySizeLarge_RelAbund=BodySizeLarge_Abund/BodySize_KAbund,
    #Largest size
    BodySizeLargest = max(BodySizeMean, na.rm = T),
    #Average body size
    BodySizeAverage = weighted.mean(BodySizeMean, na.rm=T, w=Result),
    ##Gossner dispersal
    Dispersal_KRich=sum(DistinctTaxon & !is.na(Gossner_DispersalAbility), na.rm=T),
    Dispersal_KAbund = sum(Result[!is.na(Gossner_DispersalAbility)], na.rm=T),
    #Poor dispersers
    DisperserPoor_Rich=sum(DistinctTaxon & Gossner_DispersalAbility<=.25, na.rm=T),
    DisperserPoor_RelRich=DisperserPoor_Rich/Dispersal_KRich,
    DisperserPoor_Abund = sum(Result[Gossner_DispersalAbility<=.25], na.rm=T),
    DisperserPoor_RelAbund=DisperserPoor_Abund/Dispersal_KAbund,
    #Good dispersers
    DisperserGood_Rich=sum(DistinctTaxon & Gossner_DispersalAbility==1, na.rm=T),
    DisperserGood_RelRich=DisperserGood_Rich/Dispersal_KRich,
    DisperserGood_Abund = sum(Result[Gossner_DispersalAbility==1], na.rm=T),
    DisperserGood_RelAbund=DisperserGood_Abund/Dispersal_KAbund,
    ##Temp tolerance
    TempTol_KRich=sum(DistinctTaxon & !is.na(TempMax_final), na.rm=T),
    TempTol_KAbund = sum(Result[!is.na(TempMax_final)], na.rm=T),
    #High temp tolerance
    TempTolHigh_Rich=sum(DistinctTaxon & TempMax_final>=35, na.rm=T),
    TempTolHigh_RelRich=TempTolHigh_Rich/TempTol_KRich,
    TempTolHigh_Abund = sum(Result[TempMax_final>=35], na.rm=T),
    TempTolHigh_RelAbund=TempTolHigh_Abund/TempTol_KAbund,
    #Average temp preference
    TempTolAverage = weighted.mean(TempMean_final, na.rm=T, w=Result),
    #Max temp tolerance
    TempTolMax = max(TempMax_final, na.rm=T)
    ) %>%
  ungroup()
# 
# 
# metrics_output %>% skimr::skim()
# metrics_output %>% write_clip()
# 
# metrics_output %>%
#   filter(SampleID=="402COZYDL_2021-07-10_TerInvt_T_DS_1") %>%
#   select(Arth_Rich, Nonnat_Rich)
# 
# meta_arth_df$Nonnative_Synanthropic
# meta_arth_df %>%
#   group_by(FFG_final_detailed) %>%
#   tally()
# 
# meta_arth_df$BodySizeMean %>% summary()
# 
# ggplot(data=meta_arth_df, aes(x=TempMax_final))+
#   geom_histogram()
# 
# 
# read_clip()
# ###############

#Create a function
calculate_bug_metrics<-function(x) {
  x %>%
    rename(FinalID=STE1_ID) %>%
    left_join(meta_arth_df %>%
                select(FinalID,
                       Class, Order, Family, Genus, Species,
                       all_of(arth_traits)
                ) %>%
                unique()
    ) %>%
    # mutate(Nonnative_Syn) %>%
    group_by(SampleID, StationCode, SampleDate, CollectionMethodCode, Replicate) %>%
    summarise(
      #Arthrhopods
      Arth_Rich = sum(DistinctTaxon),
      Arth_Abund = sum(Result),
      ##Taxonomic metrics
      #Insects
      Insect_Rich=sum(DistinctTaxon & Class=="Insecta", na.rm=T),
      Insect_RelRich=Insect_Rich/Arth_Rich,
      Insect_Abund = sum(Result[Class=="Insecta"], na.rm=T),
      Insect_RelAbund=Insect_Abund/Arth_Abund,
      #Noninsects
      Noninsect_Rich=sum(DistinctTaxon & Class!="Insecta", na.rm=T),
      Noninsect_RelRich=Noninsect_Rich/Arth_Rich,
      Noninsect_Abund = sum(Result[Class!="Insecta"], na.rm=T),
      Noninsect_RelAbund=Noninsect_Abund/Arth_Abund,
      #Coleoptera
      Coleo_Rich=sum(DistinctTaxon & Order=="Coleoptera", na.rm = T),
      Coleo_RelRich=Coleo_Rich/Arth_Rich,
      Coleo_Abund = sum(Result[Order=="Coleoptera"], na.rm = T),
      Coleo_RelAbund=Coleo_Abund/Arth_Abund,
      #Araneae
      Araneae_Rich=sum(DistinctTaxon & Order=="Araneae" , na.rm=T),
      Araneae_RelRich=Araneae_Rich/Arth_Rich,
      Araneae_Abund = sum(Result[Order=="Araneae"], na.rm=T),
      Araneae_RelAbund=Araneae_Abund/Arth_Abund,
      #Araneae, non-synahthropic only
      Araneae_Nat_Rich=sum(DistinctTaxon & Order=="Araneae" & Nonnative_Synanthropic==0, na.rm=T),
      Araneae_Nat_RelRich=Araneae_Nat_Rich/Arth_Rich,
      Araneae_Nat_Abund = sum(Result[Order=="Araneae" & Nonnative_Synanthropic==0], na.rm=T),
      Araneae_Nat_RelAbund=Araneae_Nat_Abund/Arth_Abund,
      #Formicidae
      Formicidae_Rich=sum(DistinctTaxon & Family=="Formicidae", na.rm = T),
      Formicidae_RelRich=Formicidae_Rich/Arth_Rich,
      Formicidae_Abund = sum(Result[Family=="Formicidae"], na.rm = T),
      Formicidae_RelAbund=Formicidae_Abund/Arth_Abund,
      #Formicidae, native only
      Formicidae_Nat_Rich=sum(DistinctTaxon & Family=="Formicidae" & Nonnative_Synanthropic==0, na.rm = T),
      Formicidae_Nat_RelRich=Formicidae_Nat_Rich/Arth_Rich,
      Formicidae_Nat_Abund = sum(Result[Family=="Formicidae" & Nonnative_Synanthropic==0], na.rm = T),
      Formicidae_Nat_RelAbund=Formicidae_Nat_Abund/Arth_Abund,
      #CAF
      CAF_Rich= Coleo_Rich + Araneae_Rich + Formicidae_Rich,
      CAF_RelRich=Coleo_RelRich + Araneae_RelRich + Formicidae_RelRich,
      CAF_Abund=Coleo_Abund + Araneae_Abund + Formicidae_Abund,
      CAF_RelAbund=Coleo_RelAbund + Araneae_RelAbund + Formicidae_RelAbund,
      #CAF, Native only
      CAF_Nat_Rich= Coleo_Rich + Araneae_Nat_Rich + Formicidae_Nat_Rich,
      CAF_Nat_RelRich=Coleo_RelRich + Araneae_Nat_RelRich + Formicidae_Nat_RelRich,
      CAF_Nat_Abund=Coleo_Abund + Araneae_Nat_Abund + Formicidae_Nat_Abund,
      CAF_Nat_RelAbund=Coleo_RelAbund + Araneae_Nat_RelAbund + Formicidae_Nat_RelAbund,
      #Nativeness metrics
      #Nonnative
      Nonnat_KRich=sum(DistinctTaxon & !is.na(Nonnative), na.rm=T),
      Nonnat_Rich=sum(DistinctTaxon & Nonnative=="Nonnative", na.rm=T),
      Nonnat_RelRich=Nonnat_Rich/Nonnat_KRich,
      Nonnat_KAbund = sum(Result[!is.na(Nonnative)], na.rm=T),
      Nonnat_Abund = sum(Result[Nonnative=="Nonnative"], na.rm=T),
      Nonnat_RelAbund=Nonnat_Abund/Nonnat_KAbund,
      #Nonnative or synanthrophic
      NonnatSynanth_KRich=sum(DistinctTaxon & !is.na(Nonnative_Synanthropic), na.rm=T),
      NonnatSynanth_Rich=sum(DistinctTaxon & Nonnative_Synanthropic==1, na.rm=T),
      NonnatSynanth_RelRich=NonnatSynanth_Rich/NonnatSynanth_KRich,
      NonnatSynanth_KAbund = sum(Result[!is.na(Nonnative_Synanthropic)], na.rm=T),
      NonnatSynanth_Abund = sum(Result[Nonnative_Synanthropic==1], na.rm=T),
      NonnatSynanth_RelAbund=NonnatSynanth_Abund/NonnatSynanth_KAbund,
      #Argentine Ant
      LinEpi_Rich=sum(DistinctTaxon & Species=="Linepithema humile", na.rm=T),
      LinEpi_RelRich=LinEpi_Rich/Arth_Rich,
      LinEpi_Abund = sum(Result[Species=="Linepithema humile"], na.rm=T),
      LinEpi_RelAbund=LinEpi_Abund/Arth_Abund,
      ##Habitat metrics
      #Aquatic
      Aquatic_KRich=sum(DistinctTaxon & !is.na(Aquatic), na.rm=T),
      Aquatic_Rich=sum(DistinctTaxon & Aquatic=="Aquatic", na.rm=T),
      Aquatic_RelRich=Aquatic_Rich/Aquatic_KRich,
      Aquatic_KAbund = sum(Result[!is.na(Aquatic)], na.rm=T),
      Aquatic_Abund = sum(Result[Aquatic=="Aquatic"], na.rm=T),
      Aquatic_RelAbund=Aquatic_Abund/Aquatic_KAbund,
      #Stratum final metrcs
      StratumFinal_KRich=sum(DistinctTaxon & !is.na(Stratum_final), na.rm=T),
      StratumFinal_KAbund = sum(Result[!is.na(Stratum_final)], na.rm=T),
      ##Ground
      Ground_Rich=sum(DistinctTaxon & Stratum_final=="Ground or soil layer", na.rm=T),
      Ground_RelRich=Ground_Rich/StratumFinal_KRich,
      Ground_Abund = sum(Result[Stratum_final=="Ground or soil layer"], na.rm=T),
      Ground_RelAbund=Ground_Abund/StratumFinal_KAbund,
      ##Herbaceous
      Herbaceous_Rich=sum(DistinctTaxon & Stratum_final=="Herbaceous layer", na.rm=T),
      Herbaceous_RelRich=Herbaceous_Rich/StratumFinal_KRich,
      Herbaceous_Abund = sum(Result[Stratum_final=="Herbaceous layer"], na.rm=T),
      Herbaceous_RelAbund=Herbaceous_Abund/StratumFinal_KAbund,
      ##Tree/arboreal
      Arboreal_Rich=sum(DistinctTaxon & Stratum_final=="Tree layer", na.rm=T),
      Arboreal_RelRich=Arboreal_Rich/StratumFinal_KRich,
      Arboreal_Abund = sum(Result[Stratum_final=="Tree layer"], na.rm=T),
      Arboreal_RelAbund=Arboreal_Abund/StratumFinal_KAbund,
      ##FFG simple metrics
      FFG_KRich=sum(DistinctTaxon & !is.na(FFG_final_simple), na.rm=T),
      FFG_KAbund = sum(Result[!is.na(FFG_final_simple)], na.rm=T),
      #Detritivore
      Detritivore_Rich=sum(DistinctTaxon & FFG_final_simple=="Detritivore", na.rm=T),
      Detritivore_RelRich=Detritivore_Rich/FFG_KRich,
      Detritivore_Abund = sum(Result[FFG_final_simple=="Detritivore"], na.rm=T),
      Detritivore_RelAbund=Detritivore_Abund/FFG_KAbund,
      #Fungivore
      Fungivore_Rich=sum(DistinctTaxon & FFG_final_simple=="Fungivore", na.rm=T),
      Fungivore_RelRich=Fungivore_Rich/FFG_KRich,
      Fungivore_Abund = sum(Result[FFG_final_simple=="Fungivore"], na.rm=T),
      Fungivore_RelAbund=Fungivore_Abund/FFG_KAbund,
      #Detrritivore + Fungivore
      DetrFung_Rich=Detritivore_Rich + Fungivore_Rich,
      DetrFung_RelRich=Detritivore_RelRich + Fungivore_RelRich,
      DetrFung_Abund = Detritivore_Abund + Fungivore_Abund,
      DetrFung_RelAbund=Detritivore_RelAbund + Fungivore_RelAbund,
      #Predator
      Predator_Rich=sum(DistinctTaxon & FFG_final_simple=="Predator", na.rm=T),
      Predator_RelRich=Fungivore_Rich/FFG_KRich,
      Predator_Abund = sum(Result[FFG_final_simple=="Predator"], na.rm=T),
      Predator_RelAbund=Fungivore_Abund/FFG_KAbund,
      ##FFG detailed metrics
      FFG_d_KRich=sum(DistinctTaxon & !is.na(FFG_final_detailed), na.rm=T),
      FFG_d_KAbund = sum(Result[!is.na(FFG_final_detailed)], na.rm=T),
      #Predator_Ground
      PredatorGround_Rich=sum(DistinctTaxon & FFG_final_detailed=="Predator_Ground", na.rm=T),
      PredatorGround_RelRich=Fungivore_Rich/FFG_d_KRich,
      PredatorGround_Abund = sum(Result[FFG_final_detailed=="Predator_Ground"], na.rm=T),
      PredatorGround_RelAbund=Fungivore_Abund/FFG_d_KAbund,
      ##Body size
      BodySize_KRich=sum(DistinctTaxon & !is.na(BodySizeMean), na.rm=T),
      BodySize_KAbund = sum(Result[!is.na(BodySizeMean)], na.rm=T),
      #Small-bodied (<9 mm, based on Vieira's classifications)
      BodySizeSmall_Rich=sum(DistinctTaxon & BodySizeMean<=9, na.rm=T),
      BodySizeSmall_RelRich=BodySizeSmall_Rich/BodySize_KRich,
      BodySizeSmall_Abund = sum(Result[BodySizeMean<=9], na.rm=T),
      BodySizeSmall_RelAbund=BodySizeSmall_Abund/BodySize_KAbund,
      #Medium-bodied (9-16 mm, based on Vieira's classifications)
      BodySizeMedium_Rich=sum(DistinctTaxon & BodySizeMean>9 & BodySizeMean<=16, na.rm=T),
      BodySizeMedium_RelRich=BodySizeMedium_Rich/BodySize_KRich,
      BodySizeMedium_Abund = sum(Result[BodySizeMean>9 & BodySizeMean<=16], na.rm=T),
      BodySizeMedium_RelAbund=BodySizeMedium_Abund/BodySize_KAbund,
      #large-bodied (16+ mm, based on Vieira's classifications)
      BodySizeLarge_Rich=sum(DistinctTaxon & BodySizeMean>16, na.rm=T),
      BodySizeLarge_RelRich=BodySizeLarge_Rich/BodySize_KRich,
      BodySizeLarge_Abund = sum(Result[BodySizeMean>16], na.rm=T),
      BodySizeLarge_RelAbund=BodySizeLarge_Abund/BodySize_KAbund,
      #Largest size
      BodySizeLargest = max(BodySizeMean, na.rm = T),
      #Average body size
      BodySizeAverage = weighted.mean(BodySizeMean, na.rm=T, w=Result),
      ##Gossner dispersal
      Dispersal_KRich=sum(DistinctTaxon & !is.na(Gossner_DispersalAbility), na.rm=T),
      Dispersal_KAbund = sum(Result[!is.na(Gossner_DispersalAbility)], na.rm=T),
      #Poor dispersers
      DisperserPoor_Rich=sum(DistinctTaxon & Gossner_DispersalAbility<=.25, na.rm=T),
      DisperserPoor_RelRich=DisperserPoor_Rich/Dispersal_KRich,
      DisperserPoor_Abund = sum(Result[Gossner_DispersalAbility<=.25], na.rm=T),
      DisperserPoor_RelAbund=DisperserPoor_Abund/Dispersal_KAbund,
      #Good dispersers
      DisperserGood_Rich=sum(DistinctTaxon & Gossner_DispersalAbility==1, na.rm=T),
      DisperserGood_RelRich=DisperserGood_Rich/Dispersal_KRich,
      DisperserGood_Abund = sum(Result[Gossner_DispersalAbility==1], na.rm=T),
      DisperserGood_RelAbund=DisperserGood_Abund/Dispersal_KAbund,
      ##Temp tolerance
      TempTol_KRich=sum(DistinctTaxon & !is.na(TempMax_final), na.rm=T),
      TempTol_KAbund = sum(Result[!is.na(TempMax_final)], na.rm=T),
      #High temp tolerance
      TempTolHigh_Rich=sum(DistinctTaxon & TempMax_final>=35, na.rm=T),
      TempTolHigh_RelRich=TempTolHigh_Rich/TempTol_KRich,
      TempTolHigh_Abund = sum(Result[TempMax_final>=35], na.rm=T),
      TempTolHigh_RelAbund=TempTolHigh_Abund/TempTol_KAbund,
      #Average temp preference
      TempTolAverage = weighted.mean(TempMean_final, na.rm=T, w=Result),
      #Max temp tolerance
      TempTolMax = max(TempMax_final, na.rm=T)
    ) %>%
    ungroup()
}


# test function


junk<-sample_data %>%
  generate_sample_id() %>%
  flag_distinct_taxa() %>%
  calculate_bug_metrics()
write_csv(junk, "C:/Users/Raphaelm/Documents/Repositories/DryRiverMMIDevelopment/Data/BioData/Arthros/arthropod_metircs_07252023.csv")


skimr::skim(junk)
junk$Araneae_NonSynanthropic_Abund

#Vector of bug metric names:



bug_mets<-c("Arth_Rich", "Arth_Abund", 
            "Insect_Rich", "Insect_RelRich","Insect_Abund", "Insect_RelAbund", 
            "Noninsect_Rich", "Noninsect_RelRich", "Noninsect_Abund", "Noninsect_RelAbund",
            "Coleo_Rich", "Coleo_RelRich", "Coleo_Abund", "Coleo_RelAbund", 
            "Araneae_Rich", "Araneae_RelRich", "Araneae_Abund", "Araneae_RelAbund", 
            "Araneae_Nat_Rich", "Araneae_Nat_RelRich", "Araneae_Nat_Abund", "Araneae_Nat_RelAbund",
            "Formicidae_Rich", "Formicidae_RelRich", "Formicidae_Abund", "Formicidae_RelAbund",
            "Formicidae_Nat_Rich", "Formicidae_Nat_RelRich", "Formicidae_Nat_Abund", "Formicidae_Nat_RelAbund", 
            "CAF_Rich", "CAF_RelRich", "CAF_Abund", "CAF_RelAbund", 
            "CAF_Nat_Rich", "CAF_Nat_RelRich", "CAF_Nat_Abund", "CAF_Nat_RelAbund", 
            "Nonnat_KRich", "Nonnat_Rich", "Nonnat_RelRich", "Nonnat_KAbund", "Nonnat_Abund", "Nonnat_RelAbund", 
            "NonnatSynanth_KRich", "NonnatSynanth_Rich", "NonnatSynanth_RelRich", 
            "NonnatSynanth_KAbund", "NonnatSynanth_Abund", "NonnatSynanth_RelAbund", 
            "LinEpi_Rich", "LinEpi_RelRich", "LinEpi_Abund", "LinEpi_RelAbund", 
            "Aquatic_KRich", "Aquatic_Rich", "Aquatic_RelRich", "Aquatic_KAbund", "Aquatic_Abund", "Aquatic_RelAbund", 
            "StratumFinal_KRich", "StratumFinal_KAbund",
            "Ground_Rich", "Ground_RelRich", "Ground_Abund", "Ground_RelAbund", 
            "Herbaceous_Rich", "Herbaceous_RelRich", "Herbaceous_Abund", "Herbaceous_RelAbund", 
            "Arboreal_Rich", "Arboreal_RelRich", "Arboreal_Abund", "Arboreal_RelAbund", 
            "FFG_KRich", "FFG_KAbund", 
            "Detritivore_Rich", "Detritivore_RelRich", "Detritivore_Abund","Detritivore_RelAbund", 
            "Fungivore_Rich", "Fungivore_RelRich", "Fungivore_Abund", "Fungivore_RelAbund",
            "DetrFung_Rich", "DetrFung_RelRich","DetrFung_Abund", "DetrFung_RelAbund", 
            "Predator_Rich", "Predator_RelRich", "Predator_Abund", "Predator_RelAbund", "FFG_d_KRich", "FFG_d_KAbund", "PredatorGround_Rich", "PredatorGround_RelRich", "PredatorGround_Abund","PredatorGround_RelAbund", 
            "BodySize_KRich", "BodySize_KAbund", "BodySizeSmall_Rich", "BodySizeSmall_RelRich", "BodySizeSmall_Abund", "BodySizeSmall_RelAbund", "BodySizeMedium_Rich", "BodySizeMedium_RelRich", "BodySizeMedium_Abund", "BodySizeMedium_RelAbund", "BodySizeLarge_Rich", "BodySizeLarge_RelRich", "BodySizeLarge_Abund", "BodySizeLarge_RelAbund", "BodySizeLargest", "BodySizeAverage", 
            "Dispersal_KRich", "Dispersal_KAbund", "DisperserPoor_Rich", "DisperserPoor_RelRich", "DisperserPoor_Abund", "DisperserPoor_RelAbund", "DisperserGood_Rich", "DisperserGood_RelRich", "DisperserGood_Abund", "DisperserGood_RelAbund", 
            "TempTol_KRich", "TempTol_KAbund", "TempTolHigh_Rich", "TempTolHigh_RelRich", "TempTolHigh_Abund", "TempTolHigh_RelAbund", "TempTolAverage", "TempTolMax")

