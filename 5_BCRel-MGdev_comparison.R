# AUTHOR: Jenny Kay
# PURPOSE: Compare breast cancer-relevant chemicals to 2016 MGDev list
# STARTED: 2021-05-19
# written in version: R version 4.1.0 (2021-05-18)

# Last update: 2022-07-26

library(tidyverse)
library(readxl)


# Assign the folder where the R script lives to working directory
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)


# glossary matching CASRNs to DTXSIDs and chem names
# downloaded from CompTox Dashboard https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=616dd716e4b0a5ca8aeea68e&page=0
chemids <- read.csv("./inputs/DSSTox_Identifiers_and_CASRN_2021r1.csv") %>%
  rename(CASRN = casrn, DTXSID = dtxsid, preferred_name = preferredName) %>% 
  select(CASRN, DTXSID, preferred_name)%>% 
  # some characters showed up weird for 1,2-benzenediol
  mutate(preferred_name = ifelse(CASRN == "120-80-9", "1,2-Benzenediol", preferred_name))


# BC-relevant chemicals created in 3_BCrelevant_chem_effects.R
BCrelList2 <- read.csv("./outputs/BCRelList.csv") %>% 
  mutate(BCrelevant = "BCrelevant") 


# List of chemicals that affect mammary gland development published in Rudel 2011 DOI: 10.1289/ehp.1002864
#     Merge with CASRN/DTXSID glossary because this list doesn't have DTXSIDs
MGdevList <- read_excel("./inputs/MGDevlist_chemsonly.xlsx") %>% 
  rename(CASRN = CAS_No) %>%
  inner_join(chemids, by = "CASRN") %>% 
  select(CASRN, DTXSID, preferred_name)


# Read in full hormone synth, ERagonist, and gentox results to match w/ MG devs
gentox_chems <- read.csv("./outputs/gentox_ccris_ecvam_ntp_echem_toxnet.csv")

Hormonesynth <- read.csv("./outputs/H295R_hormonesynthesis_summary.csv")

ERagonist <- read.csv("./outputs/ERagonists.csv")


# Get gentox, hormone synth, and ER activity of MG dev chemicals
MGdev_KCs <- MGdevList %>% 
  left_join(Hormonesynth, by = "CASRN") %>% 
  left_join(ERagonist, by = "CASRN") %>% 
  left_join(gentox_chems, by = "CASRN") %>% 
  mutate(across(.cols = HormoneSummary:Genotoxicity, .fns = str_replace_na, replacement = "-")) %>%
  
  mutate(EDC = case_when(str_detect(HormoneSummary, "-|NA") & ERactivity == "-" ~ "-",
                         str_detect(HormoneSummary, "negative|-") & str_detect(ERactivity, "not|-|antag") ~ "EDC-",
                         (str_detect(HormoneSummary, "negative|-|NA") | HormoneSummary == "*E2" | 
                            HormoneSummary == "*P4" | HormoneSummary == "*E2, *P4") & 
                           str_detect(ERactivity, "not|-|weak") ~ "EDC~",
                         TRUE ~ "EDC+")) %>% 

  select(CASRN:preferred_name.x, HormoneSummary, ERactivity, EDC, Genotoxicity)


BCrelMGDev_comp <- MGdev_KCs %>% 
  mutate(MGDev = "MGDev") %>% 
  full_join(BCrelList2, by = "CASRN") %>% 
  mutate(preferred_name = coalesce(preferred_name.x, preferred_name)) %>% 
  mutate(DTXSID = coalesce(DTXSID.x, DTXSID)) %>% 
  mutate(HormoneSummary = coalesce(HormoneSummary.y, HormoneSummary.y)) %>% 
  mutate(ERactivity = coalesce(ERactivity.x, ERactivity.y)) %>% 
  mutate(EDC = coalesce(EDC.x, EDC.y)) %>%
  mutate(Genotoxicity = coalesce(Genotoxicity.x, Genotoxicity.y)) %>% 
  select(CASRN, DTXSID, preferred_name:MC_references, BCrelevant, MGDev, E2_onedose_up:P4_CR_up, HormoneSummary:Genotoxicity) %>% 
  mutate(across(.cols = MC:Genotoxicity, .fns = str_replace_na, replacement = "-")) %>% 
  filter(MGDev == "MGDev") 
    
  

write_csv(BCrelMGDev_comp, "./outputs/BCrel_MGdev_comparison.csv")

