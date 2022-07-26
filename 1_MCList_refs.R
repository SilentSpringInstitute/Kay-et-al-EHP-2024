# AUTHOR: Jenny Kay
# PURPOSE: compiling references showing mammary tumor induction by chemicals in vivo
# last update: 2022-07-26
# written in version: R version 4.1.0 (2021-05-18)


library(tidyverse)
library(readxl)
library(stringr)

# Assign the folder where the R script lives to working directory
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)


options(stringsAsFactors = FALSE)


#create folder for code outputs. This folder will be used in each subsequent script
dir.create("./outputs")


## glossary matching CASRNs to DTXSIDs and chem names
# downloaded from CompTox Dashboard https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=616dd716e4b0a5ca8aeea68e&page=0
chemids <- read.csv("./inputs/DSSTox_Identifiers_and_CASRN_2021r1.csv") %>%
  rename(CASRN = casrn, DTXSID = dtxsid, preferred_name = preferredName) %>% 
  select(CASRN, DTXSID, preferred_name)%>% 
  # some characters showed up weird for 1,2-benzenediol
  mutate(preferred_name = ifelse(CASRN == "120-80-9", "1,2-Benzenediol", preferred_name))


#### IARC monographs ####
# Results from IARC monographs were gathered by searching pdfs for the term "mammary,"
#    then looking at results for tumors in experimental animals and including the chemical 
#    if there was at least one study showing significant induction of mammary tumors
# Note that ionizing radiation and 4'OH-PCB-61 induced mammary tumors but don't have CASRNs, 
#    so they were assigned X and Y, respectively

IARC <- read_excel("./inputs/IARCmonoMCchems.xlsx") %>% 
  select(CASRN, Chemname) %>% 
  # fix CASRN synonym w/ another chem on list
  mutate(CASRN = case_when(Chemname == "Trans-2-[(dimethylamino)methylimino]-5-[2-(5-nitro-2-furyl)vinyl]-1,3,4-oxadiazole"
                           ~ "55738-54-0", 
                           TRUE ~ CASRN)) %>% 
  left_join(chemids, by = "CASRN") %>% 
  mutate(Chemname = coalesce(preferred_name, Chemname)) %>% 
  mutate(IARC_result = "IARC") %>%
  select(CASRN, DTXSID, Chemname, IARC_result) 


#### 15th Report on Carcinogens ####
# ROC pdfs downloaded from https://ntp.niehs.nih.gov/whatwestudy/assessments/cancer/roc/index.html
# Results from the 15th ROC were gathered by searching pdfs for the term "mammary,"
#    then looking at results for tumors in experimental animals and including the chemical 
#    if there was at least one study showing significant induction of mammary tumors
ROC15 <- read_excel("./inputs/ROC15_ChemswithMC.xlsx") %>% 
  mutate(ROC15_result = ifelse(Call == "equivocal", "ROC15_equivocal", "ROC15")) %>% 
  select(CASRN, Chemname, ROC15_result)


#### NTP technical reports ####

#Downloaded results of cancer bioassays from NTP's Chemical Effects on Biological Systems database 
#     https://manticore.niehs.nih.gov/organsites on July 7 2022

NTP_CEBS <- read_tsv("./inputs/NTPCEBS_2022-07-07-site_data.tsv") %>% 
  rename(chemname = `Test Article Name`, NTP_result = `Organ Site Call`) %>%
  filter(Organ == "Mammary Gland") %>% 
  #combo of urethane and ethanol not a distinct exposure
  filter(CASRN != "URETHCOMB") %>% 
  # select only columns needed for constructing MC list
  select(CASRN, chemname, NTP_result) %>% 
  mutate(NTP_result = case_when(str_detect(NTP_result, 'Equiv') ~ "NTP_equivocal",
                                str_detect(NTP_result, 'ositive|lear|ome') ~ "NTP",
                                TRUE ~ "check")) %>% 
  pivot_wider(names_from = "NTP_result", values_from = "NTP_result") %>% 
  mutate(NTP_result = case_when(str_detect(NTP, 'NTP') ~ "NTP",
                                str_detect(NTP_equivocal, 'equiv') ~ "NTP_equivocal",
                                TRUE ~ "check")) %>% 
  select(-c(NTP_equivocal, NTP)) %>% 
  unique()

# Leucomalachite green, PeCDF, and amsonic acid were previously flagged as having 
#    legitimate mammary tumor induction dismissed in NTP bioassays in Rudel 2007, DOI: 10.1002/cncr.22653
#    Add to df as "NTP_dismissed"
NTP_dismissed <- data.frame(c("129-73-7", "57117-31-4", "7336-20-1"),
                            c("Leucomalachite green", "2,3,4,7,8-Pentachlorodibenzofuran", 
                              "4,4'-Diamino-2,2'-stilbenedisulfonic acid, disodium salt"), 
                            c("NTP_dismissed", "NTP_dismissed", "NTP_dismissed"))

colnames(NTP_dismissed) <- c("CASRN", "chemname", "NTP_result")
colnames <- NTP_dismissed

NTP <- rbind(NTP_CEBS, NTP_dismissed)



#### EPA IRIS reports ####
# chemicals that induce mammary tumors downloaded Aug 20, 2021 from 
#     https://iris.epa.gov/AdvancedSearch/ after searching "mammary"
EPA_IRIS <- read_excel("./inputs/EPA_IRIS_MCs.xlsx") %>% 
  select(CASRN, DTXSID, `Chemical Name`) %>% 
  mutate(EPA_IRIS_result = "EPA_IRIS")  


#### EPA OPP ####
# EPA Office of pesticide programs. Studies gathered from Reregistration 
#     Eligibility Decisions (REDs) and human health risk assessments
# Pesticides flagged for induction of mammary tumors as reported in Cardona and Rudel 2020
#     DOI 10.1016/j.mce.2020.110927
EPA_OPP <- read_excel("./inputs/EPA_OPP_MCs.xlsx") %>% 
  mutate(EPA_OPP_result = ifelse(EPA_RED_result == "positive", "EPA_OPP", paste0("EPA_OPP_", EPA_RED_result)))



#### ToxRefDB ####
# EPA's Toxicity Reference Database (ToxRefDB)
# Downloaded SQL database 6/23/22 from https://gaftp.epa.gov/comptox/High_Throughput_Screening_Data/Animal_Tox_Data/current
#     version dated 4/21/20

# From SQL database, pulled rodent cancer studies where there were treatment-related 
#     tumors induced in the mammary gland
ToxRef_MCs <- read.csv("./inputs/ToxRef_MCs.csv") %>% 
  select(casrn, preferred_name) %>% 
  rename(CASRN = casrn) %>% 
  mutate(Toxref_result = "ToxRefDB") %>% 
  unique()



#### ToxValDB ####
# EPA's Toxicity Value Database 
#    provided by Richard Judson April 20, 2022 by email (file available on github)
ToxVal_MCs <- read_excel("./inputs/toxval_cancer_details_with_references_dev_toxval_v9_2022-04-20.xlsx") %>% 
  filter(str_detect(common_name, "Rat|Mouse") | str_detect(species_original, "rat|mice")) %>% 
  filter(risk_assessment_class == "carcinogenicity" | risk_assessment_class == "chronic") %>% 
  filter(str_detect(critical_effect, "mammary")) %>% 
  rename(CASRN = casrn) %>% 
  select(CASRN) %>%
  mutate(ToxVal = "ToxValDB") %>% 
  unique()



#### CCRIS ####
# Downloaded NCI's Chemical Carcinogenesis Research Information System archive from 
#     https://www.nlm.nih.gov/databases/download/ccris.html
CCRIS <- read_excel("./inputs/ccris.xlsx", col_names = FALSE) 

# Full CCRIS download contains a junk row at the top - assign column names as second row
colnames(CCRIS) <- CCRIS[2,] 

CCRIS_mammary <- CCRIS[-c(1,2),] %>%   # Remove junk rows
  filter(grepl('[Mm][Aa][Mm][Mm][Aa][Rr][Yy]', `/DOC/cstu/tstlc`)) %>% # find chemicals with mammary tumor sites
  rename(CASRN = `/DOC/CASRegistryNumber`, CCRIS_result = `/DOC/cstu/rsltc`, 
         chemname = `/DOC/NameOfSubstance`) %>% 
  select(CASRN, chemname, CCRIS_result) %>% 
  # CASRN for ANTI-(+/-)-1,2,3,4-TETRAHYDROBENZO[C]PHENANTHRENE-3,4-DIOL-1,2-EPOXIDE not in original download
  mutate(CASRN = ifelse(chemname == 'ANTI-(+/-)-1,2,3,4-TETRAHYDROBENZO[C]PHENANTHRENE-3,4-DIOL-1,2-EPOXIDE', '75443-72-0', CASRN)) %>%  
  filter(CASRN != "1746-01-6") %>% # remove TCDD because mammary tumors were ns (it was there b/c other tumors were signif)
  filter(CASRN != "80-05-7") %>% # remove BPA because reference is for hyperplasia, not tumors
  filter(CASRN != "119-61-9") %>% # remove benzophenone because mammary tumors were decreased
  mutate(CCRIS_result = ifelse(grepl('EQUIVOCAL', CCRIS_result, fixed = TRUE), 'CCRIS_equivocal', 'CCRIS')) %>% #distinguish between positive and equivocal evidence
  pivot_wider(names_from = CCRIS_result, values_from = CCRIS_result) %>% 
  mutate(CCRIS_result = case_when(grepl('CCRIS', CCRIS, fixed = TRUE) ~ "CCRIS",
                                  CCRIS_equivocal != "NULL" ~ "CCRIS_equivocal", 
                                  TRUE ~ "check")) %>% 
  select(CASRN, chemname, CCRIS_result) %>% 
  unique()
  


#### Lhasa Carcinogenicity Database ####
# LCDB, gathered Aug 22, 2021 from their website https://carcdb.lhasalimited.org/
LCDB <- read_excel("./inputs/LCDB_Mammary_Carcinogens.xlsx") %>% 
  rename(CASRN = CAS_No, LCDB_Result = Result) %>% 
  # fix some CAS numbers for HCl salts - studies were duplicates
  mutate(CASRN = case_when(CASRN == "NoCAS-0867" ~ "76180-96-6", 
                           CASRN == "NoCAS-0868" ~ "105650-23-5", 
                           TRUE ~ CASRN)) %>% 
  mutate(LCDB_Result = case_when(LCDB_Result == "Positive" ~ "LCDB", 
                            LCDB_Result == "Equivocal" ~ "LCDB_equivocal",
                            TRUE ~ "check")) %>% 
  select(CASRN, name, LCDB_Result) %>% 
  unique()





####### Complete list of MCs from sources above #########

MCList <- full_join(IARC, ROC15, by = "CASRN") %>% 
  full_join(NTP, by = "CASRN") %>% 
  full_join(EPA_IRIS, by = "CASRN") %>% 
  full_join(EPA_OPP, by = "CASRN") %>% 
  full_join(ToxRef_MCs, by = "CASRN") %>% 
  full_join(ToxVal_MCs, by = "CASRN") %>% 
  full_join(CCRIS_mammary, by = "CASRN") %>% 
  full_join(LCDB, by = "CASRN") %>% 
  left_join(chemids, by = "CASRN") %>% 
  rename(DTXSID = DTXSID.y.y) %>% 
  select(-c(DTXSID.x, DTXSID.y, DTXSID.x.x)) %>% 
  # add in some chem names not included in chemids for some reason
  mutate(preferred_name = case_when(CASRN == "68162-13-0" ~ "Trans-7,12-Dimethylbenz(a)anthracene-3,4-dihydrodiol",
                            CASRN == "83349-67-1" ~ "Anti-1,2,3,10b-tetrahydrofluoranthene-2,3-diol 1,10b-oxide",
                            CASRN == "1010-61-3" ~ "4-Hydroxyaminoquinoline 1-oxide hydrochloride",
                            CASRN == "75443-72-0" ~ "Anti-3,4-Dihydroxy-1,2-epoxy-1,2,3,4-tetrahydrobenzo(c)phenanthrene", 
                            CASRN == "138857-19-9" ~ "Anti-4,5-dihydroxy-6,6a-epoxy-4,5,6,6a-tetrahydrobenzo[j]fluoranthene", 
                            CASRN == "153926-04-6" ~ "Anti-dibenzo[a,l]pyrene-11,12-dihydrodiol-13,14-epoxide", 
                            TRUE ~ preferred_name.y)) %>% 
  mutate(chem_name = coalesce(preferred_name, Chemname.x, `Chemical Name`)) %>% 
  # fix 2,4/26-dinitrotoluene identifiers
  mutate(CASRN = ifelse(chem_name == "2,4-/2,6-Dinitrotoluene mixture", "NOCAS_24069", CASRN)) %>% 
  mutate(DTXSID = ifelse(chem_name == "2,4-/2,6-Dinitrotoluene mixture", "DTXSID9024069", DTXSID)) %>% 
  mutate(MC = "MC") %>% 
  unite(IARC_result, ROC15_result, NTP_result, EPA_IRIS_result, EPA_OPP_result, 
        Toxref_result, ToxVal, CCRIS_result, LCDB_Result, 
        col = "MC_references", na.rm = TRUE, sep = ", ") %>% 
  select(CASRN, DTXSID, chem_name, MC, MC_references) %>% 
  unique() %>% 
  arrange(CASRN)


write_csv(MCList, "./outputs/MCList_refs.csv")





