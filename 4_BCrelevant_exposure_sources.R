# AUTHOR: Jenny Kay
# PURPOSE: Identify chemical use categories and exposure data for BC-relevant chemicals 
#          from CPDat, ExpoCast, FDA, and EPA database
# STARTED: 2021-05-26
# written in version: R version 4.1.0 (2021-05-18)


library(tidyverse)
library(readxl)
library(stringr)

# Assign the folder where the R script lives to working directory
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)

options(stringsAsFactors = FALSE)
options(scipen=999)


## Combined MC+E/P+ER list of BC-relevant chemicals created in 3_BCrelevant_chem_effects.R
BCrelList <- read.csv("./outputs/BCRelList.csv") 

## glossary matching CASRNs to DTXSIDs and chem names
# downloaded from CompTox Dashboard https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=616dd716e4b0a5ca8aeea68e&page=0
chemids <- read.csv("./inputs/DSSTox_Identifiers_and_CASRN_2021r1.csv") %>%
  rename(CASRN = casrn, DTXSID = dtxsid, preferred_name = preferredName) %>% 
  select(CASRN, DTXSID, preferred_name) %>% 
  # some characters showed up weird for 1,2-benzenediol
  mutate(preferred_name = ifelse(CASRN == "120-80-9", "1,2-Benzenediol", preferred_name))

# needed to make chemids glossary all lowercase for matching to FDA drug database
lowercase_chemids <- chemids %>% 
  mutate(preferred_name = tolower(preferred_name))



#### CPDat #### 
# Data release Dec 16 2020, publication Isaacs 2020 DOI: 10.1038/s41370-019-0187-5
# database available at https://gaftp.epa.gov/COMPTOX/Sustainable_Chemistry_Data/Chemistry_Dashboard/CPDat/CPDat2020-12-16

# Exposure mapping performed with Alex Borrel's python script generateBoardExposure.py
#   Code available at https://github.com/SilentSpringInstitute/CPDatSSI
#   This code takes the many exposure identifiers used in CPDat and condenses down to
#      the categories pesticides, diet, consumer products, industrial use, pharmaceuticals, and environmental media
#   To create the file in "inputs" folder, make a .txt file of the CASRNs from 
#      BCRelList.csv and run in python script available on github

CPDat_exposures <- read.csv("./inputs/CPDat_BCrel_output.csv", sep = "\t") %>% 
  select(CASRN, class_combine) %>% 
  left_join(chemids, by = "CASRN") %>% 
  separate(col = class_combine, into = paste0("cp_cat", 1:6), sep = "\\+") %>% 
  pivot_longer(cols = c(cp_cat1:cp_cat6), names_to = "catno", values_to = "cp_source", values_drop_na = TRUE) %>% 
  subset(select = -c(catno)) %>% 
  pivot_wider(names_from = "cp_source", values_from = "cp_source") %>% 
  rename(Pesticide_cp = Pesticides, Environment_cp = Environmental, Industrial_cp = Industrial,
         Consumer_cp = `Consumer products`, Diet_cp = Diet, Pharma_cp = Pharmaceuticals) %>% 
  mutate(Pesticide_cp = ifelse(is.na(Pesticide_cp)==FALSE, "Pesticide_cp", NA)) %>% 
  mutate(Diet_cp = ifelse(is.na(Diet_cp)==FALSE, "Diet_cp", NA)) %>% 
  mutate(Consumer_cp = ifelse(is.na(Consumer_cp)==FALSE, "Consumer_cp", NA)) %>% 
  mutate(Industrial_cp = ifelse(is.na(Industrial_cp)==FALSE, "Industrial_cp", NA)) %>% 
  mutate(Environment_cp = ifelse(is.na(Environment_cp)==FALSE, "Environment_cp", NA)) %>% 
  mutate(Pharma_cp = ifelse(is.na(Pharma_cp)==FALSE, "Pharma_cp", NA)) %>% 
  subset(select = -c(`No data`))



#### HPV ####
# List of EPA's 2017 high production volume chemicals downloaded March 25, 2021 
#    from CompTox dashboard https://comptox.epa.gov/dashboard/chemical-lists
HPV <- read_excel("./inputs/HPVlist.xls") %>% 
  select(3,1,2) %>% 
  mutate(HPV = "HPV")



#### ExpoCast ####
# ExpoCast modeled exposure data in Ring 2019 Supplemental Table 1 DOI: 10.1021/acs.est.8b04056
#    Computational predictions of chemical uses/exposure sources, production volume, and median intake
ExpoCast <- read.table("./inputs/Ring_SupTable-all.chem.preds-2018-11-28.txt", header = TRUE, sep = ",") %>% 
  select(dsstox_substance_id, CAS, Substance_Name, Pred.Production.Volume, Pathway, 
         seem3, seem3.u95) %>% 
  rename(DTXSID = dsstox_substance_id, CASRN = CAS, chemname = Substance_Name, 
         PredProductVolume = Pred.Production.Volume, ExposureSource = Pathway, 
         medianintake = seem3, U95intake = seem3.u95) %>% 
  mutate(ExposureSource = ifelse(grepl('All Four', ExposureSource, fixed = TRUE), 
                                 'Ind., Cons., Pest., Diet.', ExposureSource)) %>% 
  separate(ExposureSource, into = paste0("source", 1:4), sep = ",") %>% 
  pivot_longer(cols = c(source1:source4)) %>% 
  rename(sources = value) %>% 
  filter(sources != "NA") %>% 
  mutate(sources = ifelse(grepl('Ind.', sources, fixed = TRUE), 'Industrial', sources)) %>% 
  mutate(sources = ifelse(grepl('Cons.', sources, fixed = TRUE), 'Consumer', sources)) %>% 
  mutate(sources = ifelse(grepl('Pest.', sources, fixed = TRUE), 'Pesticide', sources)) %>% 
  mutate(sources = ifelse(grepl('Diet.', sources, fixed = TRUE), 'Diet', sources)) %>% 
  subset(select = -c(name)) %>% 
  pivot_wider(names_from = sources, values_from = sources) %>%
  select(2,1, 3, 7:11, 4:6) %>%
  rename(Diet_exp = Diet, Consumer_exp = Consumer, Industrial_exp = Industrial, Pesticide_exp = Pesticide) %>%
  mutate(Diet_exp = ifelse(grepl('Diet', Diet_exp, fixed = TRUE), 'Diet', Diet_exp)) %>%
  mutate(Consumer_exp = ifelse(grepl('Consumer', Consumer_exp, fixed = TRUE), 'Consumer_exp', Consumer_exp))%>%
  mutate(Diet_exp = ifelse(grepl('Diet', Diet_exp, fixed = TRUE), 'Diet_exp', Diet_exp))%>%
  mutate(Pesticide_exp = ifelse(grepl('Pesticide', Pesticide_exp, fixed = TRUE), 'Pesticide_exp',Pesticide_exp))%>%
  mutate(Industrial_exp = ifelse(grepl('Industrial', Industrial_exp, fixed = TRUE), 'Industrial_exp', Industrial_exp)) %>%
  mutate(Unknown = ifelse(grepl('Unknown', Unknown, fixed = TRUE), 'Unknown_exp', Unknown)) %>%
  mutate(U95intake = case_when(U95intake >= 0.1 ~ "100 ug/kg/day or more", 
                               U95intake < 0.1 & U95intake >= 0.001 ~ "Between 1 to 100 ug/kg/day",
                               U95intake < 0.001 ~ "Less than 1 ug/kg/day")) 



#### FDA drugs ####
# Files from Drugs@FDA last updated Sep 21, 2021
# downloaded from https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files

# FDA drug product list
druglist <- read_tsv("./inputs/DrugProducts.txt") %>% 
  rename(Drug = ActiveIngredient)

# These docs say whether the drugs in the product list are Rx, OTC, discontinued, or tentatively approved
Marketingstatuskey <- read_tsv("./inputs/DrugMarketingStatus_Lookup.txt")
Marketingstatus <- read_tsv("./inputs/DrugMarketingStatus.txt")

Drugmarketing <- full_join(Marketingstatus, Marketingstatuskey, by = "MarketingStatusID") %>% 
  full_join(druglist, by = "ApplNo") %>% 
  unite("Drug", DrugName, Drug, sep = "; ") %>% 
  subset(select = -c(MarketingStatusID, ApplNo, ProductNo.x, ProductNo.y, Form, 
                     Strength, ReferenceDrug, ReferenceStandard)) %>% 
  unique()

## FDA drugs w/ marketing status
FDA <- Drugmarketing %>% 
  separate(col = Drug, into = paste0("Drug", 1:10), sep = "; ") %>% 
  pivot_longer(cols = c(Drug1:Drug10), names_to = "dr", values_to = "Drug", values_drop_na = TRUE) %>% 
  # Fix drug names so they'll match to the chemids glossary correctly
  mutate(preferred_name = tolower(Drug)) %>% 
  mutate(preferred_name = ifelse(grepl('ethinyl', preferred_name, fixed = TRUE), '17alpha-ethinylestradiol', preferred_name)) %>% 
  mutate(preferred_name = case_when((Drug == "ESTRADIOL" & MarketingStatusDescription == "Prescription" & dr == "Drug1") ~ "17alpha-estradiol",
                                    (Drug == "ESTRADIOL" & MarketingStatusDescription == "Prescription" & dr == "Drug2") ~ "17beta-estradiol", 
                                    TRUE ~ preferred_name)) %>% 
  mutate(preferred_name = ifelse(grepl('norlestrin', preferred_name, fixed = TRUE), 'norlestrin', preferred_name)) %>%
  mutate(preferred_name = ifelse(grepl('mitomycin', preferred_name, fixed = TRUE), 'mitomycin c', preferred_name)) %>% 
  mutate(preferred_name = ifelse(grepl('doxorubicin', preferred_name, fixed = TRUE), 'doxorubicin', preferred_name)) %>%
  mutate(preferred_name = ifelse(grepl('griseofulvin', preferred_name, fixed = TRUE), 'griseofulvin', preferred_name)) %>%
  mutate(preferred_name = ifelse(grepl('azacitidine', preferred_name, fixed = TRUE), '5-azacytidine', preferred_name)) %>%
  mutate(preferred_name = ifelse(grepl('indomethacin', preferred_name, fixed = TRUE), '1-(p-chlorobenzoyl)-5-methoxy-2-methyl-indole-3-acetic acid', preferred_name)) %>%
  mutate(preferred_name = ifelse(grepl('hydroxyprogesterone caproate', preferred_name, fixed = TRUE), '17-((1-oxohexyl)oxy)pregn-4-ene-3,20-dione', preferred_name)) %>%
  mutate(preferred_name = ifelse(grepl('disulfiram', preferred_name, fixed = TRUE), 'tetraethylthiuram disulfide', preferred_name)) %>%
  mutate(preferred_name = ifelse(grepl('phenylbutazone', preferred_name, fixed = TRUE), '4-butyl-1,2-diphenyl-3,5-pyrazolidinedione', preferred_name)) %>%
  subset(select = -c(dr)) %>% 
  inner_join(lowercase_chemids, by = "preferred_name") %>% 
  subset(select = -c(Drug)) %>% 
  select(3,4,2,1) %>% 
  unique() %>% 
  pivot_wider(names_from = MarketingStatusDescription, values_from = MarketingStatusDescription) %>% 
  mutate(FDADrug = case_when(Prescription == "Prescription" & `Over-the-counter` == "Over-the-counter" ~ "Pharma_FDA_Rx_OTC", 
                            Prescription == "Prescription" ~ "Pharma_FDA_Rx",
                            `Over-the-counter` == "Over-the-counter" ~ "Pharma_FDA_OTC",
                            `None (Tentative Approval)` == "None (Tentative Approval)" ~ "Pharma_FDA_tentative_approval",
                            Discontinued == "Discontinued" ~ "Pharma_FDA_discontinued")) %>% 
  select(1,2,3,8)



#### Substances added to food ####
## List from FDA contains everything that's added to foods in the US
# Downloaded from http://www.cfsanappsexternal.fda.gov/scripts/fdcc/?set=FoodSubstances
# Last updated 5/19/2021; Downloaded 9/28/2021
# I cleaned up the original download (FoodSubstances.csv) by deleting the top 
#     4 rows that describe the database, and all the columns except CASRN and 
#     substance name, since that's all that matters
# The full download FoodSubstances.csv is in the "inputs" folder as well for reference
EAFUS <- read.csv("./inputs/FoodSubstances_clean.csv") %>% 
  rename(CASRN = `CAS.Reg.No..or.other.ID.`) %>% 
  mutate(FoodSource = "Diet_FDA") %>% 
  mutate(CASRN = str_trim(CASRN))
  


#### EPA Pesticides ####
# downloaded lists of conventional chemical, antimicrobial, and biopesticide 
#   active ingredents from EPA website: https://ordspub.epa.gov/ords/pesticides/f?p=CHEMICALSEARCH:1
#   Downloaded 8/13/2021

## "conventional chemicals" used in pesticides from EPA 
pesticides <- read.csv("./inputs/EPA_conventionalpesticides.csv") %>% 
  rename(CASRN = CAS.Number) %>% 
  separate(col = CASRN, into = paste0("CASRN", 1:10), sep = "; ") %>% 
  pivot_longer(cols = c(CASRN1:CASRN10), names_to = "cas", values_to = "CASRN", values_drop_na = TRUE) %>% 
  select(CASRN, Chemical.Name, Latest.Process) %>% 
  mutate(Pesticide_EPA = "Pesticide_EPA")


### antimicrobials
antimicrobial <- read.csv("./inputs/EPA_antimicrobials.csv") %>% 
  rename(CASRN = CAS.Number) %>% 
  separate(col = CASRN, into = paste0("CASRN", 1:10), sep = "; ") %>% 
  pivot_longer(cols = c(CASRN1:CASRN10), names_to = "cas", values_to = "CASRN", values_drop_na = TRUE) %>% 
  select(CASRN, Chemical.Name, Latest.Process) %>% 
  mutate(Pesticide_EPA = "Pesticide_EPA")

### biopesticides
biopest <- read.csv("./inputs/EPA_biopesticides.csv") %>% 
  rename(CASRN = CAS.Number) %>% 
  separate(col = CASRN, into = paste0("CASRN", 1:10), sep = "; ") %>% 
  pivot_longer(cols = c(CASRN1:CASRN10), names_to = "cas", values_to = "CASRN", values_drop_na = TRUE) %>% 
  select(CASRN, Chemical.Name, Latest.Process) %>% 
  mutate(Pesticide_EPA = "Pesticide_EPA")

EPA_pesticides <- full_join(pesticides, antimicrobial, by = "CASRN") %>% 
  full_join(biopest, by = "CASRN") %>% 
  mutate(Chemical.Name = coalesce(Chemical.Name.x, Chemical.Name.y, Chemical.Name)) %>% 
  mutate(Pesticide_EPA = coalesce(Pesticide_EPA.x, Pesticide_EPA.y, Pesticide_EPA)) %>% 
  select(CASRN, Chemical.Name, Pesticide_EPA) %>% 
  unique()
  



####### Complete exposure sources #######

# Join, condense, and organize all exposure sources
ExposureSources <- full_join(CPDat_exposures, ExpoCast, by = "CASRN") %>% 
  left_join(chemids, by = "CASRN") %>% 
  full_join(HPV, by = "CASRN") %>% 
  mutate(DTXSID = coalesce(DTXSID.x, DTXSID.y, DTXSID.x.x, DTXSID.y.y)) %>% 
  mutate(chem_name = coalesce(preferred_name.x, preferred_name.y, PREFERRED_NAME)) %>% 
  select(CASRN, DTXSID, chem_name, Pesticide_cp:Pharma_cp, Pesticide_exp:Industrial_exp, HPV, U95intake) %>% 
  full_join(FDA, by = "CASRN") %>% 
  full_join(EAFUS, by = "CASRN") %>% 
  full_join(EPA_pesticides, by = "CASRN") %>% 
  mutate(chem_name = coalesce(chem_name, preferred_name, Substance, Chemical.Name)) %>% 
  mutate(DTXSID = coalesce (DTXSID.x, DTXSID.y)) %>% 
  select(CASRN, DTXSID, chem_name, Pesticide_cp:Industrial_exp, Pesticide_EPA, FDADrug, FoodSource, HPV, U95intake) %>% 
  
  mutate(Consumer = case_when(is.na(Consumer_cp) == FALSE & Consumer_exp == "Consumer_exp" ~ "Consumer_cp_exp",
                              TRUE ~ coalesce(Consumer_cp, Consumer_exp))) %>% 
  
  mutate(Diet = case_when(is.na(Diet_cp) == FALSE & Diet_exp == "Diet_exp" & FoodSource == "Diet_FDA" ~ "Diet_FDA_cp_exp",
                          is.na(Diet_cp) == FALSE & FoodSource == "Diet_FDA" & is.na(Diet_exp) == TRUE ~ "Diet_FDA_cp",
                          Diet_exp == "Diet_exp" & FoodSource == "Diet_FDA" & is.na(Diet_cp) == TRUE ~ "Diet_FDA_exp",
                          is.na(Diet_cp) == FALSE & Diet_exp == "Diet_exp" & is.na(FoodSource) == TRUE ~ "Diet_cp_exp",
                          TRUE ~ coalesce(FoodSource, Diet_cp, Diet_exp))) %>% 
  
  mutate(Pharma = coalesce(FDADrug, Pharma_cp)) %>% 
  
  mutate(Pesticide = case_when(Pesticide_EPA == "Pesticide_EPA" & is.na(Pesticide_cp) == FALSE & Pesticide_exp == "Pesticide_exp" ~ "Pesticide_EPA_cp_exp",
                               Pesticide_EPA == "Pesticide_EPA" & is.na(Pesticide_cp) == FALSE ~ "Pesticide_EPA_cp",
                               Pesticide_EPA == "Pesticide_EPA" & Pesticide_exp == "Pesticide_exp" ~ "Pesticide_EPA_exp",
                               is.na(Pesticide_cp) == FALSE & Pesticide_exp == "Pesticide_exp" ~ "Pesticide_cp_exp",
                               TRUE ~ coalesce(Pesticide_EPA, Pesticide_cp, Pesticide_exp))) %>% 
  
  mutate(Industrial = case_when(is.na(Industrial_cp) == FALSE & Industrial_exp == "Industrial_exp" ~ "Industrial_cp_exp",
                               TRUE ~ coalesce(Industrial_cp, Industrial_exp))) %>%
  
  rename(Environmental_media = Environment_cp) %>% 
  
  select(CASRN:chem_name, Consumer:Industrial, Environmental_media, HPV, U95intake)

write.csv(ExposureSources, "./outputs/ExposureSources.csv", row.names = FALSE)



#### Prop 65 ####
# California's Proposition 65 list of chemicals as of Dec 31, 2021 downloaded from
#    https://oehha.ca.gov/proposition-65/proposition-65-list
Prop65_orig <- read_excel("./inputs/p65chemicalslist.xlsx")
colnames(Prop65_orig) <- Prop65_orig[11,] 

Prop65_trim <- Prop65_orig[-c(1:11),] %>% # Remove rows describing the database and fix CASRN column name
  rename(CASRN = `CAS No.`, ToxType = `Type of Toxicity`) 

Prop65 <- Prop65_trim %>% 
  filter(str_detect(Chemical, 'Delisted') == FALSE) %>% 
  filter(str_detect(Chemical, "removal") == FALSE) %>% 
  # make CASRNs for chemicals without them listed, but CASRNs are available
  # Note that not all chemicals with CASRNs have been fixed, focused on those in the BC-relevant chemicals list
  mutate(CASRN = case_when(
    str_detect(Chemical, "Alcoholic") ~ "64-17-5",
    str_detect(Chemical, "Dinitrotoluene mixture") ~ "NOCAS_24069",
    str_detect(Chemical, "Hexachlorocyclohexane (technical grade)") ~ "608-73-1",
    str_detect(Chemical, "Methylhydrazine sulfate") ~ "302-15-8",
    str_detect(Chemical, "Mercury and mercury compounds") ~ "587-85-9", #diphenylmercury (II) is a E2/P4-up
    str_detect(Chemical, "Retinol") ~ "68-26-8",
    str_detect(Chemical, "Phenacetin") ~ "62-44-2",
    TRUE ~ CASRN)) %>% 
  select(CASRN, ToxType) %>%  #eliminated chemical name column for now because it's messy
  filter(str_detect(CASRN, "--") == FALSE) %>% 
  mutate(P65_Cancer = ifelse(str_detect(ToxType, 'cancer'), 'Cancer', NA)) %>% 
  mutate(P65_Dev = case_when(str_detect(ToxType, "female, male") ~ "Dev_F_M",
                             str_detect(CASRN, "75-21-8") ~ "Dev_F_M", # Ethylene oxide has two entries for M+F dev effects
                             str_detect(ToxType, "female") ~ "Dev_F",
                             str_detect(CASRN, "80-05-7") ~ "Dev_F", #BPA listed twice, but actual listing is for Dev_F
                             str_detect(ToxType, "male") ~ "Dev_M",
                             str_detect(ToxType, "developmental") ~ "Dev")) %>% 
  select(-c(ToxType)) 

P65_Cancer <- Prop65[,1:2] %>% 
  filter(is.na(P65_Cancer) == FALSE) %>% 
  unique()

P65_Dev <- Prop65[,c(1,3)] %>% 
  filter(is.na(P65_Dev) == FALSE) %>% 
  unique()

P65 <- full_join(P65_Cancer, P65_Dev, by = "CASRN") %>% 
  unite("Prop65", P65_Cancer:P65_Dev, sep = ", ", na.rm = TRUE) %>% 
  unique()





####### Complete list of BC-Relevant exposures, effects, sources and Prop 65 listing #######
BCrel_Effects_and_Sources <- left_join(BCrelList, ExposureSources, by = "CASRN") %>% 
  left_join(P65, by = "CASRN") %>% 
  mutate(DTXSID = coalesce(DTXSID.x, DTXSID.y)) %>% 
  mutate(chem_name = chem_name.x, chem_name.y) %>% 
  mutate(Diet = case_when(chem_name == "Ochratoxin A" | chem_name == "MeIQ" | 
                            chem_name == "MeIQx" | chem_name == "alpha-Ecdysone" | 
                            chem_name == "Ptaquiloside" | chem_name == "Zearalenone" | 
                            chem_name == "Trp-P-2" | chem_name == "Morin hydrate" |
                            chem_name == "Enterolactone" ~ "Diet_naturally_occurring",
                          TRUE ~ Diet)) %>% 
  select(CASRN, DTXSID, chem_name, MC, MC_references, E2_onedose_up:Genotoxicity, Consumer:Prop65) %>% 
  mutate(across(.cols = MC:Prop65, .fns = str_replace_na, replacement = "-")) %>% 
  unique()


write.csv(BCrel_Effects_and_Sources, "./outputs/BCrel_Effects_and_Sources.csv", row.names = FALSE)



####### Highlights #######

### Genotoxic EDCs that have high intake levels
#    EDC = any pos result in the H295R assay or ER activity
#    potent EDC = having medium or high effects in the H295R CR assay or ER agonist AUC > 0.1
BCRel_gentoxEDC_highexp <- BCrel_Effects_and_Sources %>% 
  filter(EDC == "EDC") %>% 
  mutate(EDC = ifelse(E2_CR_up == "medium" | E2_CR_up == "higher" | 
                              P4_CR_up == "medium" | P4_CR_up == "higher" |
                              ERactivity == "agonist", "EDC_Potent", "EDC")) %>% 
  filter(Genotoxicity == "positive") %>% 
  filter(U95intake == "100 ug/kg/day or more") 

write.csv(BCRel_gentoxEDC_highexp, "./outputs/BCRel_gentoxEDC_highexp.csv", row.names = FALSE)



### FDA drugs, w/ KC summaries
BCrelFDA <- BCrel_Effects_and_Sources %>% 
  select(CASRN:MC, HormoneSummary, ERactivity, Genotoxicity, Pharma) %>% 
  filter(Pharma != "-") %>%
  filter(Pharma != "Pharma_cp")

write.csv(BCrelFDA, "./outputs/BCrel_FDAdrugs.csv", row.names = FALSE)



### MCs not on Prop 65 list
MC_notP65 <- BCrel_Effects_and_Sources %>% 
  filter(grepl("Cancer", Prop65, fixed = TRUE) == FALSE) %>% 
  filter(MC == "MC") %>% 
  #remove ionizing radiation 
  filter(str_detect(chem_name, "Ionizing")==FALSE) %>% 
  # remove steroidal estrogens b/c they're Prop65 listed as a group
  filter(str_detect(chem_name, "Estradiol|Estriol|Norlestrin") == FALSE) %>% 
  # remove Aroclor 1254 and 4'OH-PCB-61 b/c Prop65 lists PCBs as a group
  filter(str_detect(chem_name, "Aroclor|4'OH") == FALSE) %>%
  subset(select = -c(E2_onedose_up:P4_CR_up))

write.csv(MC_notP65, "./outputs/MCs_not_Prop65_carcinogens.csv", row.names = FALSE)

