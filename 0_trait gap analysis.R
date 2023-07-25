#Bug trait coverage
library(clipr)
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


flag_distinct_taxa<-function(x) {
  sample_data_x = x %>%
    select(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, FinalID, Result) %>%
    left_join(meta_arth_df) %>%
    group_by(SampleID,StationCode, SampleDate, CollectionMethodCode, Replicate, STE1_ID) %>%
    summarise(Result=sum(Result)) %>%
    ungroup()  %>%
    
    left_join(arth_taxa_long) %>%
    filter(STE1_ID!=value) %>%
    group_by(SampleID) %>%
    mutate(DistinctTaxon = !STE1_ID %in% value) %>%
    select(SampleID, StationCode, SampleDate, CollectionMethodCode, Replicate, STE1_ID, Result, DistinctTaxon) %>%
    unique() 
  sample_data_x
}

sample_data_aggregated<-sample_data %>%
  flag_distinct_taxa()


arth_traits<-c(
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




meta_arth_df %>%
  ungroup() %>%
  select(FinalID,
         all_of(arth_traits),
         Class, Order, Family) %>%
  unique() %>%
  left_join(
    sample_data_aggregated %>%
      ungroup() %>%
      select(SampleID,CollectionMethodCode, FinalID=STE1_ID) %>%
      unique() %>%
      group_by(CollectionMethodCode,FinalID) %>%
      tally(name="Freq"  )
    ) %>%
  ungroup() %>%
  mutate(Freq=case_when(is.na(Freq)~0,T~Freq)) %>%
  arrange(-Freq) %>%
  write_clip()
sample_data_aggregated$SampleID %>% unique() %>% length()
  