# Compare breast cancer-relevant chemicals to 2016 MGDev list
# Updated 5/19/2021 by Jenny Kay
# Updated 5/25/21

library(tidyverse)
library(readxl)


# This assigns the folder where the R script lives to workingdir
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# This sets the working directory to workingdir
setwd(workingdir)

# This sets the working directory one directory back of workingdir
#setwd("..") 


# glossary matching CASRNs to DTXSIDs and chem names
# downloaded from CompTox Dashboard https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=616dd716e4b0a5ca8aeea68e&page=0
chemids <- read.csv("./inputs/DSSTox_Identifiers_and_CASRN_2021r1.csv") %>%
  rename(CASRN = casrn, DTXSID = dtxsid, preferred_name = preferredName) %>% 
  select(CASRN, DTXSID, preferred_name)%>% 
  # some characters showed up weird for 1,2-benzenediol
  mutate(preferred_name = ifelse(CASRN == "120-80-9", "1,2-Benzenediol", preferred_name))


# BC-relevant master list lives in the List_overlaps folder - current working directory
BCrelList <- read.csv("./outputs/BCRelList.csv") %>% 
  mutate(BCrelevant = "BCrelevant") 

# List of chemicals that affect mammary gland development published in Rudel 2011 DOI: 10.1289/ehp.1002864
#     Merge with CASRN/DTXSID glossary because this list doesn't have DTXSIDs
MGdevList <- read_excel("./inputs/MGDevlist_chemsonly.xlsx") %>% 
  rename(CASRN = CAS_No) %>%
  inner_join(chemids, by = "CASRN") %>% 
  select(CASRN, DTXSID, preferred_name)

# Read in full hormone synth, ERagonist, and gentox results to match w/ MG devs
gentox_chems <- read.csv("./outputs/gentox_ccris_ecvam_ntp_echem_toxnet.csv")

HormoneSummary <- read.csv("./outputs/H295R_hormonesynthesis_summary.csv")

ERagonist <- read.csv("./outputs/ERagonists.csv")


# H295R_summary <- read.csv("H295R_EPsummary_RAR.csv") %>% 
#   mutate(SteroidSummary = case_when((E2_onedose_up == "positive" | (E2_CR_up != "ns effect" & is.na(E2_CR_up) == FALSE)) &
#                                       (P4_onedose_up == "positive" | (P4_CR_up != "ns effect" & is.na(P4_CR_up) == FALSE)) ~ "E2P4up",
#                                     E2_onedose_up == "positive" | (E2_CR_up != "ns effect" & is.na(E2_CR_up) == FALSE) ~ "E2up",
#                                     P4_onedose_up == "positive" | (P4_CR_up != "ns effect" & is.na(P4_CR_up) == FALSE) ~ "P4up",
#                                     is.na(E2_onedose_up) == TRUE & is.na(E2_CR_up) == TRUE ~ "-",
#                                     TRUE ~ "negative"
#   )) %>% 
#   select(CASRN:chemname, SteroidSummary)

# ERagonist <- read.csv("./data/judson2015_eragonist_clean.csv") %>% 
#   mutate(ERactivity = case_when(AUC.Agonist > 0.1 ~ "agonist",
#                                 AUC.Agonist < 0.1 & AUC.Agonist > 0.01 ~ "weak_agonist",
#                                 TRUE ~ "not_agonist")) %>% 
#   select(CASRN, Name, ERactivity)


# Get KCs of MG dev chems
MGdev_KCs <- MGdevList %>% 
  left_join(HormoneSummary, by = "CASRN") %>% 
  left_join(ERagonist, by = "CASRN") %>% 
  left_join(gentox_chems, by = "CASRN") %>% 
  select(CASRN:preferred_name.x, HormoneSummary, ERactivity, Genotoxicity)




BCrelMGDev_comp <- MGdev_KCs %>% 
  mutate(MGDev = "MGDev") %>% 
  full_join(BCrelList, by = "CASRN") %>% 
  mutate(chem_name = coalesce(preferred_name.x, chem_name)) %>% 
  mutate(DTXSID = coalesce(DTXSID.x, DTXSID)) %>% 
  mutate(HormoneSummary = coalesce(HormoneSummary.y, HormoneSummary.y)) %>% 
  mutate(ERactivity = coalesce(ERactivity.x, ERactivity.y)) %>% 
  mutate(Genotoxicity = coalesce(Genotoxicity.x, Genotoxicity.y)) %>% 
  select(CASRN, DTXSID, chem_name:MC_references, BCrelevant, MGDev, HormoneSummary:Genotoxicity) %>% 
  mutate(BCrelevant = ifelse(is.na(BCrelevant)== TRUE, '-', BCrelevant)) %>% 
  mutate(MC = ifelse(is.na(MC)== TRUE, '-', MC)) %>% 
  mutate(MC_references = ifelse(is.na(MC_references)== TRUE, '-', MC_references)) %>% 
  mutate(MGDev = ifelse(is.na(MGDev)== TRUE, '-', MGDev)) %>% 
  mutate(HormoneSummary = ifelse(is.na(HormoneSummary)== TRUE, '-', HormoneSummary)) %>%
  mutate(ERactivity = ifelse(is.na(ERactivity)== TRUE, '-', ERactivity)) %>% 
  mutate(Genotoxicity = ifelse(is.na(Genotoxicity)== TRUE, '-', Genotoxicity)) %>% 
  unique()




MGdevonly <- BCrelMGDev_comp %>% 
  filter(MGDev == "MGDev") %>%
  mutate(EDC = ifelse(
    (HormoneSummary != "negative" & HormoneSummary != "-") |
      (ERactivity != "not_agonist" & ERactivity != "-"), "EDC", "-"
  )) #%>% 
  filter(EDC == "-")
  # filter(BCrelevant == "BCrelevant")
  # filter(MC == "MC")


#write.csv(MGdevonly, "./outputs/BCrel_MGdev_comparison.csv", row.names = FALSE)

