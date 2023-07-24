# AUTHOR: Jenny Kay
# PURPOSE: compiling gentox, hormone synth, and ER agonist results for 
#    mammary carcinogens and breast cancer-relevant chemicals
# STARTED: January 2021
# last update: 2023-07-24
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
# https://epa.figshare.com/articles/dataset/Chemistry_Dashboard_Data_DSSTox_Identifiers_Mapped_to_CAS_Numbers_and_Names/5588566
# no need to reload if running after one of the previous scripts
chemids <- read.csv("inputs/DSSTox_Identifiers_and_CASRN_2021r1.csv") %>%
  rename(CASRN = casrn, DTXSID = dtxsid, preferred_name = preferredName) %>% 
  select(CASRN, DTXSID, preferred_name)%>% 
  # some characters showed up weird for 1,2-benzenediol
  mutate(preferred_name = ifelse(CASRN == "120-80-9", "1,2-Benzenediol", preferred_name))


#### Mammary carcinogens and chemicals with rodent cancer studies ####

#List of mammary carcinogens from in vivo studies compiled in 1_MCList_refs.R
MCList <- read.csv("outputs/MCList_refs.csv") 


#List of chemicals tested in bioassays with NTP technical reports
#    downloaded 5/2/23 from ICE (https://ice.ntp.niehs.nih.gov/Search: 
#    under Chemical Input -> Select Chemicals -> "NTP Cancer Bioassay Chemicals") 
ICE <- read_excel("inputs/NTP_Cancer_Bioassay_Chemicals.xlsx") %>% 
  mutate(ICE = "NTP") %>% 
  select(CASRN, ICE) %>% unique()


# ToxValDB v9 
# Chemicals with carcinogenicity conclusions listed in ToxValDB
#    provided by Richard Judson April 20, 2022 by email (file on github; also loaded in 1_MCList_refs.R)
ToxVal <- read_excel("inputs/toxval_cancer_details_with_references_dev_toxval_v9_2022-04-20.xlsx") %>% 
  select(-c(toxval_id, toxval_type:toxval_type_category, habitat:strain_original, exposure_route:media_original)) %>% 
  filter(str_detect(common_name, "Rat|Mouse") | str_detect(species_original, "rat|mice")) %>% 
  filter(risk_assessment_class == "carcinogenicity") %>% 
  rename(CASRN = casrn) %>% 
  select(CASRN) %>% 
  mutate(ToxVal = "ToxValDB") %>% 
  unique()


# ToxRefDB 
# EPA's Toxicity Reference Database (ToxRefDB 2.0)
# Downloaded SQL database 6/23/22 from https://gaftp.epa.gov/comptox/High_Throughput_Screening_Data/Animal_Tox_Data/current
#   version dated 4/21/20
#   pulled rodent cancer studies where mammary gland was assessed for gross and/or microscopic pathology 
ToxRef_MG_assessed <- read.csv("inputs/ToxRef_MG_tested04062023.csv") %>% 
  # Chronic toxicity studies are different from carcinogenicity
  filter(study_type_guideline != "Chronic toxicity") %>% 
  select(casrn, dsstox_substance_id, preferred_name) %>% 
  rename(CASRN = casrn, DTXSID = dsstox_substance_id) %>% 
  unique() %>% 
  mutate(ToxRef = "ToxRefDB")


## Combine NTP, ToxVal, and ToxRef bioassay list
Bioassays <- full_join(ICE, ToxVal, by = "CASRN") %>% 
  left_join(chemids, by = "CASRN") %>% 
  full_join(ToxRef_MG_assessed, by = "DTXSID") %>% 
  mutate(CASRN = coalesce(CASRN.y, CASRN.x)) %>% 
  mutate(preferred_name = case_when(CASRN == "15356-70-4" ~ "Menthol",
                                    CASRN == "21416-87-5" ~ "Razoxane",
                                    CASRN == "56802-99-4" ~ "Tetrasodium;hypochlorite;phosphate",
                                    TRUE ~ coalesce(preferred_name.y, preferred_name.x))) %>% 
  unite(ICE, ToxVal, ToxRef, col = "ref", na.rm = TRUE, sep = ", ") %>% 
  mutate(Bioassay = "Bioassay_noMC") %>% 
  select(CASRN, DTXSID, preferred_name, Bioassay, ref) %>% unique()


write_csv(Bioassays, "outputs/NTP_toxval_toxref_bioassays.csv")



#### Genotoxicity ####
# Genotoxicity results compiled in 2_gentox_chems.R
gentox_chems <- read.csv("outputs/gentox_ccris_ecvam_ntp_echem_toxnet.csv") 


 
#### Steroidogens ####
# Chemicals that significantly increased estradiol (E2) or progesterone (P4) synthesis in EPA HT-H295R screens
#    Karmaus et al 2016 tested chemicals in a single high dose, DOI: 10.1093/toxsci/kfw002
#    Haggard et al 2018 tested chemicals in concentration response, DOI: 10.1093/toxsci/kfx274
#    Cardona and Rudel 2021 classified strength of E2/P4 induction from Haggard 2018 based on potency/efficacy, DOI: 10.1289/EHP8608 

# single high dose hit calls from Karmaus 2016 H295R
H295R_onedose <- read.csv("inputs/Karmaus_toxsci-15-0570-File009.csv") %>% 
  rename(CASRN = casn, E2P4_onedose_chem = chnm) %>% 
  select(-c(spid, chid, code, aeid, s2id:max_med, coff, resp_unit)) %>% 
  filter(CASRN != "NA") %>% 
  filter(str_detect(aenm, "_ESTRADIOL_up|_PROG_up")) %>% 
  unique() %>% 
  pivot_wider(names_from = aenm, values_from = hitc) %>% 
  rename(E2up_onedose = CEETOX_H295R_ESTRADIOL_up, P4up_onedose = CEETOX_H295R_PROG_up) %>% 
  # Indicate hormones and hormone synthesis substrates as NA, as these do not necessarily increase
  #    de novo synthesis of E2/P4, but rather reflect metabolism of the substrate added. 
  #    Since they were tested but we're not considering results, don't want to filter out
  mutate(E2up_onedose = case_when(str_detect(E2P4_onedose_chem, 
                                              "estosterone|Progesterone|17alpha|Estradiol|Equilin|Estriol|Estrone|[Aa]ndro") ~ "_NA",
                                   E2up_onedose == "1" ~ "positive",
                                   # diphenyl isophthalate 3/4 replicates positive --> assign positive
                                   E2P4_onedose_chem == "Diphenyl isophthalate" ~ "positive",
                                   # clorophene 1/6 replicates positive --> assign negative
                                   E2P4_onedose_chem == "Clorophene" ~ "negative",
                                   E2up_onedose == "0" ~ "negative",
                                   E2up_onedose == "NULL" ~ "no data", 
                                   TRUE ~ "check")) %>% 
  mutate(P4up_onedose = case_when(str_detect(E2P4_onedose_chem, 
                                              "estosterone|Progesterone|17alpha|Estradiol|Equilin|Estriol|Estrone|[Aa]ndro") ~ "_NA",
                                   P4up_onedose == "1" ~ "positive",
                                   # mancozeb 3/5 replicates positive --> assign positive
                                   E2P4_onedose_chem == "Mancozeb" ~ "positive",
                                   # PFOS 1/6 replicates positive --> assign negative
                                   E2P4_onedose_chem == "PFOS" ~ "negative",
                                   P4up_onedose == "0" ~ "negative", 
                                   P4up_onedose == "NULL" ~ "no data", 
                                   TRUE ~ "check")) %>% 
  unique()


# E2/P4 concentration-response results from Haggard 2018  
#    Includes all chemicals tested, with only E2/P4 results as reported in Cardona 2021
H295R_CR <- read_excel("inputs/H295R_CR_Cardona_EPsummary.xlsx") %>% 
  rename(E2P4_CR_chem = Chem_name, E2up_CR = E2_effect, P4up_CR = P4_effect) %>% 
  # As above, indicate hormones and hormone synthesis substrates as NA, as these do not necessarily 
  #    increase de novo synthesis of E2/P4, but rather reflect metabolism of the substrate added. 
  #    Since they were tested but we're not considering results, don't want to filter out
  mutate(E2up_CR = case_when(str_detect(E2P4_CR_chem, 
                                              "estosterone|Progesterone|17alpha|Estradiol|Equilin|Estriol|Estrone|[Aa]ndro") ~ "_NA", 
                             str_detect(E2up_CR, "higher") ~ "high",
                             str_detect(E2up_CR, "lower") ~ "low",
                             TRUE ~ E2up_CR)) %>% 
  mutate(P4up_CR = case_when(str_detect(E2P4_CR_chem, 
                                        "estosterone|Progesterone|17alpha|Estradiol|Equilin|Estriol|Estrone|[Aa]ndro") ~ "_NA", 
                             str_detect(P4up_CR, "higher") ~ "high",
                             str_detect(P4up_CR, "lower") ~ "low",
                             TRUE ~ P4up_CR)) %>% 
  unique()


# Merge single dose and CR H295R results
Hormonesynth <- full_join(H295R_onedose, H295R_CR, by = "CASRN") %>% 
  mutate(H295R_chem = coalesce(E2P4_onedose_chem, E2P4_CR_chem)) %>% 
  select(CASRN, H295R_chem, E2up_onedose, P4up_onedose, E2up_CR, P4up_CR) %>% 
  unique() %>% 
  mutate(E2Summary = case_when(E2up_onedose == "_NA" ~ "_NA",
                               (is.na(E2up_onedose) | str_detect(E2up_onedose, "negative|no")) & 
                                 (E2up_CR == "ns effect" | is.na(E2up_CR)) ~ "negative",
                               E2up_onedose == "positive" & E2up_CR == "ns effect" ~ "negative",
                               E2up_onedose == "positive" & is.na(E2up_CR) ~ "*E2",
                               E2up_CR == "borderline" ~ "*E2",
                               TRUE ~ "E2")) %>% 
  
  mutate(P4Summary = case_when(P4up_onedose == "_NA" ~ "_NA",
                               (is.na(P4up_onedose) | str_detect(P4up_onedose, "negative|no")) & 
                                 (P4up_CR == "ns effect" | is.na(P4up_CR)) ~ "negative",
                               P4up_onedose == "positive" & P4up_CR == "ns effect" ~ "negative",
                               P4up_onedose == "positive" & is.na(P4up_CR) ~ "*P4",
                               P4up_CR == "borderline" ~ "*P4",
                               TRUE ~ "P4")) %>% 
  
  mutate(HormoneSummary = case_when(E2up_onedose == "_NA" ~ "_NA",
                                    E2Summary == "E2" & P4Summary == "P4" ~ "E2, P4",
                                    E2Summary == "*E2" & P4Summary == "P4" ~ "*E2, P4",
                                    E2Summary == "E2" & P4Summary == "*P4" ~ "E2, *P4",
                                    E2Summary == "*E2" & P4Summary == "*P4" ~ "*E2, *P4",
                                    E2Summary == "negative" ~ P4Summary,
                                    P4Summary == "negative" ~ E2Summary,
                                    TRUE ~ "check")) %>% 
  select(-c(E2Summary, P4Summary)) %>% 
  unique()

write_csv(Hormonesynth, "outputs/H295R_hormonesynthesis_summary.csv")



#### ER active ####
# Computational integration of 18 in vitro assays for ER activity
#    Downloaded from Judson et al 2015 supplement, DOI: 10.1093/toxsci/kfv168

ERagonist <- read_excel("inputs/Judson_toxsci-15-0258-File002.xlsx") %>%
  # fix a broken CASRN
  mutate(CASRN = ifelse(Name == "1,3,5,7-Tetramethyl-1,3,5,7-tetravinylcyclotetrasiloxane", '2554-06-5', CASRN)) %>%
  mutate(ERactivity = case_when(AUC.Agonist >= 0.7 ~ "high_agonism",
                                AUC.Agonist >= 0.4 ~ "medium_agonism",
                                AUC.Agonist >= 0.1 ~ "low_agonism",
                                #the highest AUC.Antagonist is 0.686 - therefore no "high" antagonists
                                AUC.Antagonist >= 0.4 ~ "medium_antagonism",
                                AUC.Antagonist >= 0.1 ~ "low_antagonism",
                                AUC.Agonist >= 0.01 & AUC.Antagonist >= 0.01 ~ "borderline_mixed",
                                AUC.Agonist >= 0.01 ~ "borderline_agonism",
                                AUC.Antagonist >= 0.01 ~ "borderline_antagonism",
                                TRUE ~ "inactive")) %>%
  select(CASRN, Name, AUC.Agonist, AUC.Antagonist, ERactivity)

write_csv(ERagonist, "outputs/ERagonists.csv")



#### Combined lists ####

# Merge mammary carcinogens and complete results for hormone synthesis and ER agonism
# Add gentox results only for those chemicals (because gentox list is long 
#     and we only need info for MCs and endocrine-active chems)
# Clean up chem names and IDs 
# Label radiation as genotoxic

MCs_nonMCs_KCs <- full_join(MCList, Bioassays, by = "CASRN") %>% 
  full_join(Hormonesynth, by = "CASRN") %>%  
  full_join(ERagonist, by = "CASRN") %>% 
  left_join(gentox_chems, by = "CASRN") %>%
  left_join(chemids, by = "CASRN") %>%
  mutate(preferred_name = coalesce(preferred_name.y.y, preferred_name.x, preferred_name.y, H295R_chem, Name, preferred_name.x.x)) %>% 
  left_join(chemids, by = "preferred_name") %>% 
  mutate(CASRN = coalesce(CASRN.y, CASRN.x)) %>% 
  mutate(DTXSID = coalesce(DTXSID, DTXSID.y.y, DTXSID.x.x, DTXSID.x, DTXSID.y)) %>%
  arrange(CASRN) %>% 
  arrange(desc(MammaryTumorEvidence)) %>% 
  
  mutate(MammaryTumorEvidence = ifelse(is.na(MammaryTumorEvidence), Bioassay, MammaryTumorEvidence)) %>% 
  mutate(MammaryTumorRefs = ifelse(is.na(MammaryTumorRefs), ref, MammaryTumorRefs)) %>% 
  
  mutate(Genotoxicity = ifelse(str_detect(preferred_name, "radiation"), 'positive', Genotoxicity)) %>% 
  
  select(CASRN, DTXSID, preferred_name, MammaryTumorEvidence, MammaryTumorRefs, E2up_onedose:HormoneSummary, AUC.Agonist:ERactivity, Genotoxicity) %>%
  mutate(across(.cols = DTXSID:Genotoxicity, \(x) str_replace_na(x, replacement = "-"))) %>%
  
  # if not tested in H295R or ER activity, EDC = "-" for not tested
  # if tested negative for E2/P4 synthesis and ER agonism, EDC = "EDC-"
  # if E2/P4 steroidogenic only in single dose or borderline in concentration-response
  #     or borderline ER agonism, EDC = "EDC~" (lower confidence)
  # Progesterone and chemicals with low, medium, or high steroidogenesis or ER agonism, EDC = "EDC+"
  mutate(EDC = case_when(preferred_name == "Progesterone" ~ "EDC+",
                         str_detect(HormoneSummary, "-|NA") & (ERactivity == "-") ~ "-",
                         str_detect(HormoneSummary, "negative|-") & str_detect(ERactivity, "inactive|-|antag") ~ "EDC-",
                         (str_detect(HormoneSummary, "negative|-|NA") | HormoneSummary == "*E2" | 
                            HormoneSummary == "*P4" | HormoneSummary == "*E2, *P4") & 
                           str_detect(ERactivity, "inactive|-|borderline|antag") ~ "EDC~",
                         TRUE ~ "EDC+")) %>% 
  
  # top EDC scores based on strongest endocrine effect in H295R or ER agonism
  mutate(topEDCscore = case_when(
    str_detect(E2up_CR, "high") | str_detect(P4up_CR, "high") | str_detect(ERactivity, "high_ag") ~ "high",
    str_detect(E2up_CR, "med") | str_detect(P4up_CR, "med") | str_detect(ERactivity, "medium_ag") ~ "medium",
    str_detect(E2up_CR, "low") | str_detect(P4up_CR, "low") | str_detect(ERactivity, "low_ag") ~ "low",
    str_detect(E2up_CR, "border") | str_detect(P4up_CR, "border") | 
      str_detect(ERactivity, "borderline_ag") | str_detect(ERactivity, "borderline_mix") ~ "borderline",
    (EDC == "EDC~" | EDC == "EDC-") & (E2up_CR != "-" | ERactivity != "-") ~ "none",
    TRUE ~ "-")) %>%
  
  select(CASRN:ERactivity, EDC, topEDCscore, Genotoxicity) %>%
  unique()


# BC-relevant chemicals list: MCs and EDCs (Excel Table S1 in Kay et al 2023)
BCrelList <- MCs_nonMCs_KCs %>% 
  mutate(BCrel = ifelse(
    MammaryTumorEvidence == "MC" | str_detect(HormoneSummary, "E2|P4") |
      ERactivity %in% c("high_agonism", "medium_agonism", "low_agonism", "borderline_agonism", "borderline_mixed"),
    "relevant", "not")) %>%
  filter(BCrel != "not") %>%
  select(-BCrel) 
  
write_csv(BCrelList, "outputs/BCRelList.csv")



## Table with all genotoxic higher-confidence EDCs (Excel Table S3 in Kay et al 2023)
EDC_gentox <- filter(BCrelList, Genotoxicity == "positive" & EDC == "EDC+")  

write_csv(EDC_gentox, "outputs/EDC_gentox.csv")



## Comparing effects of MCs vs. putative non-MCs (Excel Table S5 in Kay et al 2023)
MC_and_Bioassay_effects <- MCs_nonMCs_KCs %>%
  filter(MammaryTumorEvidence != "-")

write_csv(MC_and_Bioassay_effects, "outputs/MC_and_Bioassay_effects.csv")


