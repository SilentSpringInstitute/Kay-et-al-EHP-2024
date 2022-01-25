# AUTHOR: Jenny Kay
# PURPOSE: compiling gentox, hormone synth, and ER agonist results for 
#    mammary carcinogens and breast cancer-relevant chemicals
# STARTED: sometime in January 2021
# written in version: R version 4.1.0 (2021-05-18)


library(tidyverse)
library(readxl)

# This assigns the folder where the R script lives to workingdir
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# This sets the working directory to workingdir
setwd(workingdir)


options(stringsAsFactors = FALSE)


## glossary matching CASRNs to DTXSIDs and chem names
# downloaded from CompTox Dashboard https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=616dd716e4b0a5ca8aeea68e&page=0
chemids <- read.csv("./inputs/DSSTox_Identifiers_and_CASRN_2021r1.csv") %>%
  rename(CASRN = casrn, DTXSID = dtxsid, preferred_name = preferredName) %>% 
  select(CASRN, DTXSID, preferred_name)


#### Mammary carcinogens ####
#List of mammary carcinogens from in vivo studies compiled in 1_MCList_refs.R
MCList <- read.csv("./outputs/MCList_refs.csv") %>% 
  mutate(MC = "MC")



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
  subset(select = -c(spid, chid, code, aeid, s2id:max_med, coff, resp_unit)) %>% 
  filter(CASRN != "NA") %>% 
  filter(str_detect(aenm, "_ESTRADIOL_up|_PROG_up")) %>% 
  unique() %>% 
  pivot_wider(names_from = aenm, values_from = hitc) %>% 
  rename(E2_onedose_up = CEETOX_H295R_ESTRADIOL_up, P4_onedose_up = CEETOX_H295R_PROG_up) %>% 
  mutate(E2_onedose_up = case_when(E2_onedose_up == "1" ~ "positive",
                                   E2_onedose_up == "0" ~ "negative",
                                   E2_onedose_up == "NULL" ~ "no data")) %>% 
  mutate(P4_onedose_up = case_when(P4_onedose_up == "1" ~ "positive",
                                   P4_onedose_up == "0" ~ "negative",
                                   P4_onedose_up == "NULL" ~ "no data")) %>% 
  # Remove hormones and hormone synthesis substrates, as these do not necessarily increase
  #    de novo synthesis of E2/P4, but rather reflect metabolism of the substrate added
  filter(E2P4_onedose_chem != "Testosterone propionate") %>% 
  filter(E2P4_onedose_chem != "Progesterone") %>% 
  filter(E2P4_onedose_chem != "17alpha-Ethinylestradiol") %>% 
  filter(E2P4_onedose_chem != "Equilin") %>% 
  filter(E2P4_onedose_chem != "Estriol") %>% 
  filter(E2P4_onedose_chem != "Estrone") %>% 
  filter(E2P4_onedose_chem != "4-Androstene-3,17-dione") %>% 
  filter(E2P4_onedose_chem != "17alpha-Hydroxyprogesterone") %>% 
  filter(E2P4_onedose_chem != "Androsterone") %>% 
  filter(E2P4_onedose_chem != "Dehydroepiandrosterone") %>% 
  filter(E2P4_onedose_chem != "17-Methyltestosterone") %>% 
  filter(E2P4_onedose_chem != "5alpha-Dihydrotestosterone") %>% 
  filter(E2P4_onedose_chem != "17beta-Estradiol") %>% 
  filter(E2P4_onedose_chem != "17alpha-Estradiol") %>% 
  unique()

# E2/P4 concentration-response results from Haggard 2018  
#    Includes all chemicals tested, with only E2/P4 results as reported in Cardona 2021
H295R_CR <- read_excel("./inputs/H295R_CR_Cardona_EPsummary.xlsx") %>% 
  rename(E2P4_CR_chem = Chem_name, E2_CR_up = E2_effect, P4_CR_up = P4_effect) %>% 
  filter(E2P4_CR_chem != "Testosterone propionate") %>% 
  filter(E2P4_CR_chem != "Progesterone") %>% 
  filter(E2P4_CR_chem != "17alpha-Ethinylestradiol") %>% 
  filter(E2P4_CR_chem != "Equilin") %>% 
  filter(E2P4_CR_chem != "Estriol") %>% 
  filter(E2P4_CR_chem != "Estrone") %>% 
  filter(E2P4_CR_chem != "4-Androstene-3,17-dione") %>% 
  filter(E2P4_CR_chem != "17alpha-Hydroxyprogesterone") %>% 
  filter(E2P4_CR_chem != "Androsterone") %>% 
  filter(E2P4_CR_chem != "Dehydroepiandrosterone") %>% 
  filter(E2P4_CR_chem != "17-Methyltestosterone") %>% 
  filter(E2P4_CR_chem != "5alpha-Dihydrotestosterone") %>% 
  filter(E2P4_CR_chem != "17beta-Estradiol") %>% 
  filter(E2P4_CR_chem != "17alpha-Estradiol") %>% 
  unique()

# Merge single dose and CR H295R results
Hormonesynth <- full_join(H295R_onedose, H295R_CR, by = "CASRN") %>% 
  mutate(H295R_chem = coalesce(E2P4_onedose_chem, E2P4_CR_chem)) %>% 
  select(CASRN, H295R_chem, E2_onedose_up, P4_onedose_up, E2_CR_up, P4_CR_up) %>% 
  filter(CASRN != "NA") %>% 
  unique() %>% 
  mutate(HormoneSummary = case_when((E2_onedose_up == "positive" | (E2_CR_up != "ns effect" & is.na(E2_CR_up) == FALSE)) &
                                      (P4_onedose_up == "positive" | (P4_CR_up != "ns effect" & is.na(P4_CR_up) == FALSE)) ~ "E2P4up",
                                    E2_onedose_up == "positive" | (E2_CR_up != "ns effect" & is.na(E2_CR_up) == FALSE) ~ "E2up",
                                    P4_onedose_up == "positive" | (P4_CR_up != "ns effect" & is.na(P4_CR_up) == FALSE) ~ "P4up",
                                    is.na(E2_onedose_up) == TRUE & is.na(E2_CR_up) == TRUE ~ "-",
                                    TRUE ~ "negative")) 

#write.csv(Hormonesynth, "./outputs/H295R_hormonesynthesis_summary.csv", row.names = FALSE)



#### ER active ####
# Computational integration of in vitro assays for ER activity
#    Downloaded from Judson et al 2015 supplement, DOI: 10.1093/toxsci/kfv168
ERagonist <- read_excel("./inputs/Judson_toxsci-15-0258-File002.xlsx") %>% 
  # fix a broken CASRN
  mutate(CASRN = ifelse(Name == "1,3,5,7-Tetramethyl-1,3,5,7-tetravinylcyclotetrasiloxane", '2554-06-5', CASRN)) %>% 
  mutate(ERactivity = case_when(AUC.Agonist > 0.1 ~ "agonist",
                                AUC.Agonist < 0.1 & AUC.Agonist > 0.01 ~ "weak_agonist",
                                TRUE ~ "not_agonist")) %>% 
  select(CASRN, Name, ERactivity)

#write.csv(ERagonist, "./outputs/ERagonists.csv", row.names = FALSE)
    


#### Combined lists ####

# Merge mammary carcinogens and complete results for hormone synthesis and ER agonism
# Add gentox results only for those chemicals (because gentox list is long and we only need info for MCs and endocrine-active chems)
# Clean up chem names and IDs 
# filter out anything that's not "breast cancer-relevant" (i.e., MC, E2/P4 steroidogen, or ER agonist)
# Label radiation as genotoxic

BCrelList <- full_join(MCList, Hormonesynth, by = "CASRN") %>% 
  full_join(ERagonist, by = "CASRN") %>% 
  left_join(gentox_chems, by = "CASRN") %>%
  left_join(chemids, by = "CASRN") %>% 
  filter(CASRN != "NA") %>% 
  mutate(DTXSID = coalesce(DTXSID, DTXSID.x, DTXSID.y)) %>%
  mutate(chem_name = coalesce(preferred_name.y, chem_name, H295R_chem, Name, preferred_name.x)) %>% 
  select(CASRN, DTXSID, chem_name, MC, MC_references, E2_onedose_up:HormoneSummary, ERactivity, Genotoxicity) %>% 
  
  mutate(E2_onedose_up = ifelse(is.na(E2_onedose_up) == TRUE, '-', E2_onedose_up)) %>% 
  mutate(P4_onedose_up = ifelse(is.na(P4_onedose_up) == TRUE, '-', P4_onedose_up)) %>% 
  mutate(E2_CR_up = ifelse(is.na(E2_CR_up) == TRUE, '-', E2_CR_up)) %>% 
  mutate(P4_CR_up = ifelse(is.na(P4_CR_up) == TRUE, '-', P4_CR_up)) %>% 
  mutate(HormoneSummary = ifelse(is.na(HormoneSummary) == TRUE, '-', HormoneSummary)) %>%
  
  mutate(ERactivity = ifelse(is.na(ERactivity) == TRUE, '-', ERactivity)) %>%
  
  mutate(Genotoxicity = ifelse(grepl('radiation', chem_name, fixed = TRUE), 'positive', Genotoxicity)) %>% 
  mutate(Genotoxicity = ifelse(is.na(Genotoxicity) == TRUE, '-', Genotoxicity)) %>%
  
  mutate(BCrel = ifelse(is.na(MC) & 
                          (HormoneSummary == "-" | HormoneSummary == "negative") & 
                          (ERactivity == "-" | ERactivity == "not_agonist" ), 
                        "not relevant", "relevant")) %>% 
  filter(BCrel != "not relevant") %>% 
  subset(select = -c(BCrel)) %>% 
 
  mutate(EDC = ifelse((HormoneSummary != "-" & HormoneSummary != "negative") |
                        (ERactivity != "-" & ERactivity != "not_agonist"), "EDC", "-")) %>% 
  select(CASRN:chem_name, MC, MC_references, E2_onedose_up:P4_CR_up, HormoneSummary, ERactivity, EDC, Genotoxicity) %>% 
  unique()


#write.csv(BCrelList, "./outputs/BCRelList.csv", row.names = FALSE)





#### Endocrine-disrupting + Gentox highlights ####

## Table with all genotoxic EDCs and add a column to highlight the more "potent" ones
EDC_gentox <- BCrelList %>% 
  filter(EDC == "EDC") %>% 
  filter(Genotoxicity == "positive") %>% 
  mutate(Potent_EDC = ifelse(E2_CR_up == "medium" | E2_CR_up == "higher" | 
                              P4_CR_up == "medium" | P4_CR_up == "higher" |
                              ERactivity == "agonist", "Potent", "-")) %>% 
  select(CASRN:ERactivity, Potent_EDC, Genotoxicity)

#write.csv(EDC_gentox, "./outputs/EDC_gentox.csv", row.names = FALSE)


# May be of interest for some people to know which chemicals are steroidogenic, ER agonistic, and genotoxic
# Hormone_ER_and_gentox <- BCrelList %>% 
#   mutate(Hormone_ER = ifelse(HormoneSummary != "-" & HormoneSummary != "negative" &
#                                ERactivity != "-" & ERactivity != "not_agonist",
#                              "Both", "-")) %>% 
#   filter(Hormone_ER == "Both") %>% 
#   filter(Genotoxicity == "positive") %>% 
#   select(CASRN:ERactivity, Hormone_ER, Genotoxicity)
#   
# #write.csv(Hormone_ER_and_gentox, "./outputs/Hormone_ER_and_gentox.csv", row.names = FALSE)



