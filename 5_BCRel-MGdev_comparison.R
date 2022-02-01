# AUTHOR: Jenny Kay
# PURPOSE: Compare breast cancer-relevant chemicals to 2016 MGDev list
# STARTED: 2021-05-19
# written in version: R version 4.1.0 (2021-05-18)

# Updated 2/1/2022

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


# Get gentox, hormone synth, and ER activity of MG dev chemicals
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
  )) 
  

write.csv(MGdevonly, "./outputs/BCrel_MGdev_comparison.csv", row.names = FALSE)
