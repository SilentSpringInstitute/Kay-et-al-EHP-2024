# AUTHOR: Jenny Kay
# PURPOSE: Compare breast cancer-relevant chemicals to 2016 MGDev list
# STARTED: 2021-05-19
# last update: 2023-07-24
# written in version: R version 4.1.0 (2021-05-18)

library(tidyverse)
library(readxl)
library(stringr)

# Assign the folder where the R script lives to working directory
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)


## glossary matching CASRNs to DTXSIDs and chem names
# downloaded version 2021r1 on Jan 19, 2022 from CompTox Dashboard 
# https://epa.figshare.com/articles/dataset/Chemistry_Dashboard_Data_DSSTox_Identifiers_Mapped_to_CAS_Numbers_and_Names/5588566
# no need to reload if running after one of the previous scripts
chemids <- read.csv("inputs/DSSTox_Identifiers_and_CASRN_2021r1.csv") %>%
  rename(CASRN = casrn, DTXSID = dtxsid, preferred_name = preferredName) %>% 
  select(CASRN, DTXSID, preferred_name)%>% 
  # some characters showed up weird for 1,2-benzenediol
  mutate(preferred_name = ifelse(CASRN == "120-80-9", "1,2-Benzenediol", preferred_name))


# BC-relevant chemicals created in 3_BCrelevant_chem_effects.R
BCrelList2 <- read.csv("outputs/BCRelList.csv") %>% 
  select(CASRN, MammaryTumorEvidence, MammaryTumorRefs) %>% 
  mutate(BCrelevant = "BCrelevant")  


# List of chemicals tested in bioassays created in 3_BCrelevant_chem_effects.R
Bioassays <- read.csv("outputs/NTP_toxval_toxref_bioassays.csv") 
  

# List of chemicals that affect mammary gland development published in Rudel 2011 DOI: 10.1289/ehp.1002864
#     Merge with CASRN/DTXSID glossary because this list doesn't have DTXSIDs
#     exclude TGFbeta because it's a cytokine, not a chemical
MGdevList <- read_excel("inputs/MGDevlist_chemsonly.xlsx") %>% 
  rename(CASRN = CAS_No) %>%
  inner_join(chemids, by = "CASRN") %>% 
  select(CASRN, DTXSID, preferred_name)


# Read in full hormone synth, ERagonist, and gentox results to match KCs of MG dev disruptors
gentox_chems <- read.csv("outputs/gentox_ccris_ecvam_ntp_echem_toxnet.csv")

Hormonesynth <- read.csv("outputs/H295R_hormonesynthesis_summary.csv")

ERagonist <- read.csv("outputs/ERagonists.csv")


# Get gentox, hormone synth, and ER activity of MG dev chemicals
MGdev_KCs <- left_join(MGdevList, Hormonesynth, by = "CASRN") %>% 
  left_join(ERagonist, by = "CASRN") %>% 
  left_join(gentox_chems, by = "CASRN") %>% 
  mutate(across(.cols = HormoneSummary:Genotoxicity, \(x) str_replace_na(x, replacement = "-"))) %>%
  
  # Same criteria for EDC and topEDCscore as in 3_BCrelevant_chem_effects.R
  mutate(EDC = case_when(str_detect(HormoneSummary, "-|NA") & ERactivity == "-" ~ "-",
                         str_detect(HormoneSummary, "negative|-") & str_detect(ERactivity, "inactive|-|antag") ~ "EDC-",
                         (str_detect(HormoneSummary, "negative|-|NA") | HormoneSummary == "*E2" | 
                            HormoneSummary == "*P4" | HormoneSummary == "*E2, *P4") & 
                           str_detect(ERactivity, "inactive|-|borderline") ~ "EDC~",
                         TRUE ~ "EDC+")) %>% 
  
  mutate(topEDCscore = case_when(str_detect(E2up_CR, "high") | str_detect(P4up_CR, "high") | str_detect(ERactivity, "high_ag") ~ "high",
                                 str_detect(E2up_CR, "med") | str_detect(P4up_CR, "med") | str_detect(ERactivity, "medium_ag") ~ "medium",
                                 str_detect(E2up_CR, "low") | str_detect(P4up_CR, "low") | str_detect(ERactivity, "low_ag") ~ "low",
                                 str_detect(E2up_CR, "border") | str_detect(P4up_CR, "border") | 
                                   str_detect(ERactivity, "borderline_ag") | str_detect(ERactivity, "borderline_mix") ~ "borderline",
                                 (EDC == "EDC~" | EDC == "EDC-") & (E2up_CR != "-" | ERactivity != "-") ~ "none",
                                 TRUE ~ "-")) %>%
  
  select(CASRN:preferred_name.x, E2up_onedose:HormoneSummary, ERactivity, EDC, topEDCscore, Genotoxicity)


BCrelMGDev_comp <- left_join(MGdev_KCs, Bioassays, by = "CASRN") %>% 
  mutate(MGDev = "MGDev") %>% 
  left_join(BCrelList2, by = "CASRN") %>% 
  mutate(MammaryTumorEvidence = coalesce(MammaryTumorEvidence, Bioassay)) %>% 
  mutate(MammaryTumorRefs = coalesce(MammaryTumorRefs, ref)) %>% 
  mutate(DTXSID = coalesce(DTXSID.x, DTXSID), 
         preferred_name = coalesce(preferred_name.x, preferred_name)) %>% 
  select(CASRN, DTXSID, preferred_name, MammaryTumorEvidence, MammaryTumorRefs, BCrelevant, MGDev, E2up_onedose:Genotoxicity) %>% 
  mutate(across(.cols = MammaryTumorEvidence:Genotoxicity, \(x) str_replace_na(x, replacement = "-"))) 
  

# Create output table for H295R, ER activity, and genotoxicity of mammary gland
#     development disruptors (Excel Table S4 in Kay et al 2023)
write_csv(BCrelMGDev_comp, "outputs/BCrel_MGdev_comparison.csv")

