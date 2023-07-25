library(tidyverse)
library(readxl)

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

setdiff(meta_arth_df$FinalID, sample_data$FinalID)
setdiff(sample_data$FinalID, meta_arth_df$FinalID)

#Vector of trait names
#Not used for flagging ambiguous taxa

# arth_traits<-c(
#   #Invasiveness
#   "Nonnative","Nonnative_Synanthropic",
#   #Habitat
#   "Aquatic","Stratum_final",
#   #Feeding
#   "FFG_final_simple","FFG_final_detailed",
#   #Body size
#   "BodySizeMin","BodySizeMean","BodySizeMax",
#   #Other
#   "AntWiki_ColonySizeBin",
#   "Gossner_DispersalAbility",
#   "Ubick_HuntingStyle",
#   #Temperature
#   "TempMin_final","TempMean_final","TempMax_final"
#   
#   )
# setdiff(arth_traits, names(meta_arth_df))

#Taxonomy hierarchy columns
#Could add others, like phylum, infraorder, etc, but probably not necessary

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




sample_data_x<-sample_data %>%
  filter(SampleID=="403TEXCYN_2021-07-06_TerInvt_T_DS_1") %>%
  select(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, FinalID, Result) %>%
  left_join(meta_arth_df) %>%
  group_by(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, STE1_ID) %>%
  summarise(Result=sum(Result)) %>%
  ungroup()

# sample_data_flagged<-sample_data_x %>%
#   left_join(arth_taxa_long) %>%
#   filter(SampleID=="403KLEINE_2021-07-05_TerInvt_V_DS_1") %>%
#   
#   group_by(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, STE1_ID) %>%
#   mutate(NumTaxa=sum(STE1_ID == value),
#          DistinctTaxon=NumTaxa==1) %>%
#   ungroup() %>% 
#   select(SampleID, StationCode, SampleDate, CollectionMethodCode, Replicate, STE1_ID, Result, DistinctTaxon) %>%
#   unique() 



sample_data_flagged<-sample_data_x %>%
  left_join(arth_taxa_long) %>%
  # filter(SampleID=="403KLEINE_2021-07-05_TerInvt_V_DS_1") %>%
  filter(STE1_ID!=value) %>%
  group_by(SampleID) %>%
  mutate(DistinctTaxon = !STE1_ID %in% value) %>%
  select(SampleID, StationCode, SampleDate, CollectionMethodCode, Replicate, STE1_ID, Result, DistinctTaxon) %>%
  unique() 

  # group_by(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, STE1_ID) %>%
  # mutate(NumTaxa=sum(STE1_ID == value),
  #        DistinctTaxon=NumTaxa==1) %>%
  # ungroup() %>% 
  # select(SampleID, StationCode, SampleDate, CollectionMethodCode, Replicate, STE1_ID, Result, DistinctTaxon) %>%
  # unique() 


#######
#Make into a function
#

generate_sample_id<-function(x){
  sample_data_x = x %>%
    mutate(SampleID=paste(StationCode, SampleDate, CollectionMethodCode, Replicate, sep="_"))
  sample_data_x
}

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


junk<-sample_data %>%
  generate_sample_id() %>%
  flag_distinct_taxa() 
junk %>%write_clip()

junk %>%
  filter(SampleID=="403TEXCYN_2021-07-06_TerInvt_T_DS_1") 

meta_arth_df %>% filter(FinalID=="Latridiidae")

fake_data_1 <-structure(list(StationCode = c("403TEXCYN", "403TEXCYN", "403TEXCYN"),
                             SampleDate = c("7/6/2021", "7/6/2021", "7/6/2021"),
                             CollectionMethodCode = c("TerInvt_T_DS", "TerInvt_T_DS", "TerInvt_T_DS"), 
                             Replicate = c(1L, 1L, 1L),
                             FinalID = c("Isopoda", "Malacostraca", "Manica"), 
                             Result = c(1L, 1L, 1L)),
                        class = "data.frame", row.names = c(NA, -3L))

fake_data_1 %>%
  generate_sample_id() %>%
  flag_distinct_taxa()
