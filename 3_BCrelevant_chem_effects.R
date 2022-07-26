# AUTHOR: Jenny Kay
# PURPOSE: compiling gentox, hormone synth, and ER agonist results for 
#    mammary carcinogens and breast cancer-relevant chemicals
# STARTED: January 2021
# Last update: 2022-07-26
# written in version: R version 4.1.0 (2021-05-18)


library(tidyverse)
library(readxl)
library(stringr)

# Assign the folder where the R script lives to working directory
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)


options(stringsAsFactors = FALSE)


## glossary matching CASRNs to DTXSIDs and chem names
# downloaded version 2021r1 on Jan 19, 2022 from CompTox Dashboard 
# https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=616dd716e4b0a5ca8aeea68e&page=0
chemids <- read.csv("./inputs/DSSTox_Identifiers_and_CASRN_2021r1.csv") %>%
  rename(CASRN = casrn, DTXSID = dtxsid, preferred_name = preferredName) %>% 
  select(CASRN, DTXSID, preferred_name)%>% 
  # some characters showed up weird for 1,2-benzenediol
  mutate(preferred_name = ifelse(CASRN == "120-80-9", "1,2-Benzenediol", preferred_name))


#### Mammary carcinogens and chemicals with rodent cancer studies ####

#List of mammary carcinogens from in vivo studies compiled in 1_MCList_refs.R
MCList <- read.csv("./outputs/MCList_refs.csv") 


#List of chemicals tested in bioassays with NTP technical reports
#    downloaded 4/12/22 from ICE (https://ice.ntp.niehs.nih.gov/Search, select "NTP Cancer Bioassay Chemicals") 
ICE <- read.table("./inputs/ICE_NTP.txt") %>% rename(CASRN = V1) %>% 
  mutate(ICE = "NTP") %>% 
  unique()


#Chemicals with carcinogenicity conclusions from EPA Office of Pesticide Programs
#    provided by Richard Judson April 20, 2022 by email (file on github; also included in 1_MCList_refs.R)
ToxVal <- read_excel("./inputs/toxval_cancer_details_with_references_dev_toxval_v9_2022-04-20.xlsx") %>% 
  select(-c(toxval_id, toxval_type:toxval_type_category, habitat:strain_original, exposure_route:media_original)) %>% 
  filter(str_detect(common_name, "Rat|Mouse") | str_detect(species_original, "rat|mice")) %>% 
  filter(risk_assessment_class == "carcinogenicity" | risk_assessment_class == "chronic") %>% 
  rename(CASRN = casrn) %>% 
  select(CASRN) %>% 
  mutate(ToxVal = "ToxValDB") %>% 
  unique()


# ToxRefDB 
# EPA's Toxicity Reference Database (ToxRefDB)
# Downloaded SQL database 6/23/22 from https://gaftp.epa.gov/comptox/High_Throughput_Screening_Data/Animal_Tox_Data/current
#   version dated 4/21/20

# In SQL database, rodent cancer studies where mammary gland was assessed for gross and/or microscopic pathology 
ToxRef_MG_assessed <- read.csv("./inputs/ToxRef_MG_tested.csv") %>% 
  select(casrn) %>% 
  rename(CASRN = casrn) %>% 
  unique() %>% 
  mutate(ToxRef = "ToxRefDB")


Bioassays <- full_join(ICE, ToxVal, by = "CASRN") %>% 
  full_join(ToxRef_MG_assessed, by = "CASRN") %>% 
  unite(ICE, ToxVal, ToxRef, col = "ref", na.rm = TRUE, sep = ", ") %>% 
  mutate(Bioassay = "Bioassay")




#### Genotoxicity ####
# Genotoxicity results compiled in 2_gentox_chems.R
gentox_chems <- read.csv("./outputs/gentox_ccris_ecvam_ntp_echem_toxnet.csv") 


 
#### Steroidogens ####
# Chemicals that significantly increased estradiol (E2) or progesterone (P4) synthesis in EPA HT-H295R screens
#    Karmaus 2016 tested chemicals in a single high dose, DOI: 10.1093/toxsci/kfw002
#    Haggard 2018 tested chemicals in concentration response, DOI: 10.1093/toxsci/kfx274
#    Cardona 2021 classified strength of E2/P4 induction from Haggard 2018 based on potency/efficacy, DOI: 10.1289/EHP8608 

# single high dose hit calls from Karmaus 2016 H295R
H295R_onedose <- read.csv("./inputs/Karmaus_toxsci-15-0570-File009.csv") %>% 
  rename(CASRN = casn, E2P4_onedose_chem = chnm) %>% 
  select(-c(spid, chid, code, aeid, s2id:max_med, coff, resp_unit)) %>% 
  filter(CASRN != "NA") %>% 
  filter(str_detect(aenm, "_ESTRADIOL_up|_PROG_up")) %>% 
  unique() %>% 
  pivot_wider(names_from = aenm, values_from = hitc) %>% 
  rename(E2_onedose_up = CEETOX_H295R_ESTRADIOL_up, P4_onedose_up = CEETOX_H295R_PROG_up) %>% 
  # Indicate hormones and hormone synthesis substrates as NA, as these do not necessarily increase
  #    de novo synthesis of E2/P4, but rather reflect metabolism of the substrate added. 
  #    Since they were tested but we're not considering results, don't want to filter out
  mutate(E2_onedose_up = case_when(str_detect(E2P4_onedose_chem, 
                                              "estosterone|Progesterone|17alpha|Estradiol|Equilin|Estriol|Estrone|[Aa]ndro") ~ "_NA",
                                   E2_onedose_up == "1" ~ "positive",
                                   E2_onedose_up == "0" ~ "negative",
                                   E2_onedose_up == "NULL" ~ "no data")) %>% 
  mutate(P4_onedose_up = case_when(str_detect(E2P4_onedose_chem, 
                                              "estosterone|Progesterone|17alpha|Estradiol|Equilin|Estriol|Estrone|[Aa]ndro") ~ "_NA",
                                   P4_onedose_up == "1" ~ "positive",
                                   # PFOS was tested 3x and P4-negative in two, so assigning it "negative"
                                   # (otherwise it comes up NA)
                                   str_detect(P4_onedose_up, "0") ~ "negative",
                                   P4_onedose_up == "NULL" ~ "no data")) %>% 
  unique()


# E2/P4 concentration-response results from Haggard 2018  
#    Includes all chemicals tested, with only E2/P4 results as reported in Cardona 2021

H295R_CR <- read_excel("./inputs/H295R_CR_Cardona_EPsummary.xlsx") %>% 
  rename(E2P4_CR_chem = Chem_name, E2_CR_up = E2_effect, P4_CR_up = P4_effect) %>% 
  # As above, indicate hormones and hormone synthesis substrates as NA, as these do not necessarily 
  #    increase de novo synthesis of E2/P4, but rather reflect metabolism of the substrate added. 
  #    Since they were tested but we're not considering results, don't want to filter out
  mutate(E2_CR_up = ifelse(str_detect(E2P4_CR_chem, 
                                              "estosterone|Progesterone|17alpha|Estradiol|Equilin|Estriol|Estrone|[Aa]ndro"), "_NA", E2_CR_up)) %>% 
  mutate(P4_CR_up = ifelse(str_detect(E2P4_CR_chem, 
                                      "estosterone|Progesterone|17alpha|Estradiol|Equilin|Estriol|Estrone|[Aa]ndro"), "_NA", P4_CR_up)) %>% 
  unique()

# Merge single dose and CR H295R results
Hormonesynth <- full_join(H295R_onedose, H295R_CR, by = "CASRN") %>% 
  mutate(H295R_chem = coalesce(E2P4_onedose_chem, E2P4_CR_chem)) %>% 
  select(CASRN, H295R_chem, E2_onedose_up, P4_onedose_up, E2_CR_up, P4_CR_up) %>% 
  unique() %>% 
  mutate(E2Summary = case_when(E2_onedose_up == "_NA" ~ "_NA",
                               (is.na(E2_onedose_up) | str_detect(E2_onedose_up, "negative|no")) & 
                                 (E2_CR_up == "ns effect" | is.na(E2_CR_up)) ~ "negative",
                               E2_onedose_up == "positive" & E2_CR_up == "ns effect" ~ "negative",
                               E2_onedose_up == "positive" & is.na(E2_CR_up) ~ "*E2",
                               E2_CR_up == "borderline" ~ "*E2",
                               TRUE ~ "E2")) %>% 
  
  mutate(P4Summary = case_when(P4_onedose_up == "_NA" ~ "_NA",
                               (is.na(P4_onedose_up) | str_detect(P4_onedose_up, "negative|no")) & 
                                 (P4_CR_up == "ns effect" | is.na(P4_CR_up)) ~ "negative",
                               P4_onedose_up == "positive" & P4_CR_up == "ns effect" ~ "negative",
                               P4_onedose_up == "positive" & is.na(P4_CR_up) ~ "*P4",
                               P4_CR_up == "borderline" ~ "*P4",
                               TRUE ~ "P4")) %>% 
  
  mutate(HormoneSummary = case_when(E2_onedose_up == "_NA" ~ "_NA",
                                    E2Summary == "E2" & P4Summary == "P4" ~ "E2, P4",
                                    E2Summary == "*E2" & P4Summary == "P4" ~ "*E2, P4",
                                    E2Summary == "E2" & P4Summary == "*P4" ~ "E2, *P4",
                                    E2Summary == "*E2" & P4Summary == "*P4" ~ "*E2, *P4",
                                    E2Summary == "negative" ~ P4Summary,
                                    P4Summary == "negative" ~ E2Summary,
                                    TRUE ~ "check")) %>% 
  select(-c(E2Summary, P4Summary)) %>% 
  unique()

write_csv(Hormonesynth, "./outputs/H295R_hormonesynthesis_summary.csv")



#### ER active ####
# Computational integration of in vitro assays for ER activity
#    Downloaded from Judson et al 2015 supplement, DOI: 10.1093/toxsci/kfv168

ERagonist <- read_excel("./inputs/Judson_toxsci-15-0258-File002.xlsx") %>%
  # fix a broken CASRN
  mutate(CASRN = ifelse(Name == "1,3,5,7-Tetramethyl-1,3,5,7-tetravinylcyclotetrasiloxane", '2554-06-5', CASRN)) %>%
  mutate(ERactivity = case_when(AUC.Agonist >= 0.1 ~ "agonist",
                                AUC.Antagonist >= 0.1 ~ "antagonist",
                                AUC.Agonist >= 0.01 & AUC.Antagonist >= 0.01 ~ "mixed_weak",
                                AUC.Agonist >= 0.01 ~ "weak_agonist",
                                AUC.Antagonist >= 0.01 ~ "weak_antagonist",
                                TRUE ~ "inactive")) %>%
  select(CASRN, Name, ERactivity, AUC.Agonist, AUC.Antagonist)

write_csv(ERagonist, "./outputs/ERagonists.csv")



#### Combined lists ####

# Merge mammary carcinogens and complete results for hormone synthesis and ER agonism
# Add gentox results only for those chemicals (because gentox list is long 
#     and we only need info for MCs and endocrine-active chems)
# Clean up chem names and IDs 
# filter out anything that's not "breast cancer-relevant" (i.e., MC, E2/P4 steroidogen, or ER agonist)
# Label radiation as genotoxic

BCrelList <- full_join(MCList, Hormonesynth, by = "CASRN") %>% 
  full_join(ERagonist, by = "CASRN") %>% 
  left_join(gentox_chems, by = "CASRN") %>%
  left_join(chemids, by = "CASRN") %>%
  mutate(preferred_name = coalesce(preferred_name.y, chem_name, H295R_chem, Name, preferred_name.x)) %>% 
  left_join(chemids, by = "preferred_name") %>% 
  mutate(CASRN = coalesce(CASRN.y, CASRN.x)) %>% 
  mutate(DTXSID = coalesce(DTXSID.y.y, DTXSID.x.x, DTXSID.x, DTXSID.y)) %>%
  arrange(CASRN) %>% 
  arrange(desc(MC)) %>% 
  
  left_join(Bioassays, by = "CASRN") %>%  
  mutate(MC = ifelse(is.na(MC), Bioassay, MC)) %>% 
  mutate(MC_references = ifelse(is.na(MC_references), ref, MC_references)) %>% 
  mutate(Genotoxicity = ifelse(grepl('radiation', preferred_name, fixed = TRUE), 'positive', Genotoxicity)) %>% 
  
  select(CASRN, DTXSID, preferred_name, MC, MC_references, E2_onedose_up:HormoneSummary, ERactivity, Genotoxicity) %>%
  mutate(across(.cols = DTXSID:Genotoxicity, .fns = str_replace_na, replacement = "-")) %>%
  
  mutate(BCrel = ifelse(MC == "MC" |
                          str_detect(HormoneSummary, "E2|P4") |
                          ERactivity == "agonist" | ERactivity == "weak_agonist" | ERactivity == "mixed_weak",
                        "relevant", "not")) %>% 
  filter(BCrel != "not") %>%
  select(-BCrel) %>% 
  
  mutate(EDC = case_when(str_detect(HormoneSummary, "-|NA") & (ERactivity == "-") ~ "-",
                         str_detect(HormoneSummary, "negative|-") & str_detect(ERactivity, "inactive|-|antag") ~ "EDC-",
                         (str_detect(HormoneSummary, "negative|-|NA") | HormoneSummary == "*E2" | 
                            HormoneSummary == "*P4" | HormoneSummary == "*E2, *P4") & 
                           str_detect(ERactivity, "inactive|-|weak|antag") ~ "EDC~",
                         TRUE ~ "EDC+")) %>% 
  select(CASRN:ERactivity, EDC, Genotoxicity) %>%
  unique()


write_csv(BCrelList, "./outputs/BCRelList.csv")

