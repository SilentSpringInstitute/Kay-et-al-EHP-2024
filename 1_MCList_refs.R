# AUTHOR: Jenny Kay
# PURPOSE: compiling references showing mammary tumor induction by chemicals in vivo
# last update: 2022-11-18
# written in version: R version 4.1.0 (2021-05-18)


library(tidyverse)
library(readxl)

# This assigns the folder where the R script lives to workingdir
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# This sets the working directory to workingdir
setwd(workingdir)


getwd()
options(stringsAsFactors = FALSE)



## glossary matching CASRNs to DTXSIDs and chem names
# downloaded from CompTox Dashboard https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=616dd716e4b0a5ca8aeea68e&page=0
chemids <- read.csv("./inputs/DSSTox_Identifiers_and_CASRN_2021r1.csv") %>%
  rename(CASRN = casrn, DTXSID = dtxsid, preferred_name = preferredName) %>% 
  select(CASRN, DTXSID, preferred_name)


#### IARC monographs ####
# Results from IARC monographs were gathered by searching pdfs for the term "mammary,"
#    then looking at results for tumors in experimental animals and including the chemical 
#    if there was at least one study showing significant induction of mammary tumors
# Note that ionizing radiation and 4'OH-PCB-61 induced mammary tumors but don't have CASRNs, 
#    so they were assigned X and Y, respectively

IARC <- read_excel("./inputs/IARCmonoMCchems.xlsx") %>% 
  select(CASRN, Chemname, DTXSID) %>% 
  # fix CASRN synonym w/ another chem on list
  mutate(CASRN = case_when(Chemname == "Trans-2-[(dimethylamino)methylimino]-5-[2-(5-nitro-2-furyl)vinyl]-1,3,4-oxadiazole"
                           ~ "55738-54-0", 
                           TRUE ~ CASRN)) %>% 
  left_join(chemids, by = "CASRN") %>% 
  mutate(DTXSID = coalesce(DTXSID.y, DTXSID.x)) %>% 
  mutate(Chemname = coalesce(preferred_name, Chemname)) %>% 
  mutate(IARC_result = "IARC") %>%
  select(CASRN, DTXSID, Chemname, IARC_result) 


#### 14th Report on Carcinogens ####
# Results from the 14th ROC were gathered by searching pdfs for the term "mammary,"
#    then looking at results for tumors in experimental animals and including the chemical 
#    if there was at least one study showing significant induction of mammary tumors
ROC14 <- read_excel("./inputs/ROC_ChemswithMC.xlsx") %>% 
  # remove things w/o DTXSIDs because they're groups (e.g., steroidal estrogens) 
  # or duplicates (e.g., procarbazine duplicate for procarbazine HCl)
  filter(is.na(DTXSID) == FALSE) %>% 
  mutate(ROC14_result = "ROC14") %>% 
  select(CASRN, DTXSID, Chemname, ROC14_result)


#### NTP technical reports ####

#Downloaded two files from NTP's Chemical Effects on Biological Systems database https://manticore.niehs.nih.gov/organsites
# The first contains chemicals with positive, clear, or some evidence
# The second is chemicals where NTP concluded equivocal or dismissed evidence, 
#    but we considered the underlying data to warrant inclusion for mammary tumorigenesis

NTP_pos <- read_excel("./inputs/CEBS_chems_mammarycancer.xlsx") %>% 
  mutate(result = "NTP") %>% 
  mutate(CASRN = ifelse(`Test Article Name` == "Phenesterin", "3546-10-9", CASRN)) %>% 
  select(CASRN, `Test Article Name`, result) %>% 
  unique()

NTP_equiv <- read_excel("./inputs/CEBS_chems_EquivOrDismissMC.xlsx") %>% 
  mutate(result = ifelse(grepl('quivocal', `Level of Evidence`, fixed = TRUE), "NTP_equivocal", "NTP_dismissed")) %>% 
  select(CASRN, `Test Article Name`, result) %>% 
  unique()

NTP <- full_join(NTP_pos, NTP_equiv, by = "CASRN") %>% 
  mutate(chemname = coalesce(`Test Article Name.x`, `Test Article Name.y`)) %>% 
  mutate(NTP_result = coalesce(result.x, result.y)) %>% 
  select(CASRN, chemname, NTP_result) %>% 
  unique()


#### EPA IRIS reports ####
# chemicals that induce mammary tumors downloaded from 
#     https://ntp.niehs.nih.gov/whatwestudy/assessments/cancer/roc/index.html
EPA_IRIS <- read_excel("./inputs/EPA_IRIS_MCs.xlsx") %>% 
  select(CASRN, DTXSID, `Chemical Name`) %>% 
  mutate(EPA_IRIS_result = "EPA_IRIS")  


#### EPA OPP ####
# EPA Office of pesticide programs. Studies gathered from Reregistration 
#     Eligibility Decisions (REDs) and human health risk assessments
# Pesticides flagged for induction of mammary tumors as reported in Cardona and Rudel 2020
#     DOI 10.1016/j.mce.2020.110927
EPA_OPP <- read_excel("./inputs/EPA_RED_MCs.xlsx") %>% 
  mutate(EPA_OPP_result = ifelse(EPA_RED_result == "positive", "EPA_OPP", paste0("EPA_OPP_", EPA_RED_result)))



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
# LCDB, gathered from their website https://carcdb.lhasalimited.org/
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

MCList_refs <- full_join(IARC, ROC14, by = "CASRN") %>% 
  full_join(NTP, by = "CASRN") %>% 
  full_join(EPA_IRIS, by = "CASRN") %>% 
  full_join(EPA_OPP, by = "CASRN") %>% 
  full_join(CCRIS_mammary, by = "CASRN") %>% 
  full_join(LCDB, by = "CASRN") %>% 
  left_join(chemids, by = "CASRN") %>% 
  #Manually add in some DTXSIDs and chem names not included in chemids for some reason
  mutate(DTXSID = case_when(CASRN == "2393-53-5" ~ "DTXSID00946752",
                            CASRN == "82617-23-0" ~ "DTXSID801002821", 
                            CASRN == "834-24-2" ~ "DTXSID001019381",
                            CASRN == "87625-62-5" ~ "DTXSID20892005", 
                            TRUE ~ DTXSID)) %>% 
  mutate(preferred_name = case_when(CASRN == "2393-53-5" ~ "17-Oxoestra-1(10),2,4-trien-3-yl benzoate",
                            CASRN == "82617-23-0" ~ "Oxiran-2-yl hydrogen carbonimidate", 
                            CASRN == "834-24-2" ~ "4-aminostilbene",
                            CASRN == "87625-62-5" ~ "Ptaquiloside", 
                            CASRN == "68162-13-0" ~ "Trans-7,12-Dimethylbenz(a)anthracene-3,4-dihydrodiol",
                            CASRN == "83349-67-1" ~ "Anti-1,2,3,10b-tetrahydrofluoranthene-2,3-diol 1,10b-oxide",
                            CASRN == "843-23-2" ~ "Trans-n-hydroxy-4-acetylaminostilbene",
                            CASRN == "1010-61-3" ~ "Trans-7,12-Dimethylbenz(a)anthracene-3,4-dihydrodiol",
                            CASRN == "118745-11-2" ~ "N-hydroxy-n-formyl-trans-4-aminostilbene",
                            CASRN == "118745-12-3" ~ "N-hydroxy-n-propionyl-trans-4-aminostilbene",
                            CASRN == "75443-72-0" ~ "Anti-3,4-Dihydroxy-1,2-epoxy-1,2,3,4-tetrahydrobenzo(c)phenanthrene", 
                            CASRN == "138857-19-9" ~ "Anti-4,5-dihydroxy-6,6a-epoxy-4,5,6,6a-tetrahydrobenzo[j]fluoranthene", 
                            CASRN == "138857-21-3" ~ "Anti-9,10-dihydroxy-11,12-epoxy-9,10,11,12-tetrahydrobenzo[j]fluoranthene", 
                            CASRN == "153926-04-6" ~ "Anti-dibenzo[a,l]pyrene-11,12-dihydrodiol-13,14-epoxide", 
                            TRUE ~ preferred_name)) %>% 
  mutate(DTXSID = coalesce(DTXSID, DTXSID.x, DTXSID.y, DTXSID.x.x, DTXSID.y.y)) %>% 
  subset(select = -c(DTXSID.x, DTXSID.y, DTXSID.x.x, DTXSID.y.y)) %>% 
  left_join(chemids, by = "DTXSID") %>% 
  mutate(CASRN = coalesce(CASRN.y, CASRN.x)) %>% 
  mutate(chem_name = coalesce(preferred_name.y, preferred_name.x, Chemname.x, 
                              Chemname.y, chemname.x, chemname.y, `Chemical Name`, Preferred_name, name)) %>% 
  unite(IARC_result, ROC14_result, NTP_result, EPA_IRIS_result, EPA_OPP_result, CCRIS_result, LCDB_Result, 
        col = "MC_references", na.rm = TRUE, sep = ", ") %>% 
  select(CASRN, DTXSID, chem_name, MC_references) %>% 
  unique() %>% 
  arrange(CASRN)


#write.csv(MCList_refs, "./outputs/MCList_refs.csv", row.names = FALSE)


