# AUTHOR: Jenny Kay
# PURPOSE: Identify chemical use categories and exposure data for BC-relevant chemicals 
#          from CPDat, ExpoCast, FDA, and EPA database
# STARTED: 2021-05-26
# Last update: 2022-07-26
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

CPDat_exposures <- read.csv("./inputs/BCRC_CPDAT.csv", sep = "\t") %>% 
  select(CASRN, class_combine) %>% 
  left_join(chemids, by = "CASRN") %>% 
  separate(col = class_combine, into = paste0("cp_cat", 1:6), sep = "\\+") %>% 
  pivot_longer(cols = c(cp_cat1:cp_cat6), names_to = "catno", values_to = "cp_source", values_drop_na = TRUE) %>% 
  select(-catno) %>% 
  pivot_wider(names_from = "cp_source", values_from = "cp_source") %>% 
  rename(Pesticide_cp = Pesticides, Environment_cp = Environmental, Industrial_cp = Industrial,
         Consumer_cp = `Consumer products`, Diet_cp = Diet, Pharma_cp = Pharmaceuticals) %>% 
  
  mutate(Pesticide_cp = ifelse(!is.na(Pesticide_cp), "CPDat", NA)) %>% 
  mutate(Diet_cp = ifelse(!is.na(Diet_cp), "CPDat", NA)) %>% 
  mutate(Consumer_cp = ifelse(!is.na(Consumer_cp), "CPDat", NA)) %>% 
  mutate(Industrial_cp = ifelse(!is.na(Industrial_cp), "CPDat", NA)) %>% 
  mutate(Environment_cp = ifelse(!is.na(Environment_cp), "CPDat", NA)) %>% 
  mutate(Pharma_cp = ifelse(!is.na(Pharma_cp), "CPDat", NA)) %>% 
  select(-`No data`)

write_csv(CPDat_exposures, "./outputs/CPDat_exposures.csv")



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
  select(CAS, dsstox_substance_id, Substance_Name, Pathway, seem3.u95) %>% 
  rename(DTXSID = dsstox_substance_id, CASRN = CAS, chemname = Substance_Name, 
         U95intake = seem3.u95) %>% 
  mutate(Pathway = ifelse(grepl('All Four', Pathway, fixed = TRUE), 
                                 'Ind., Cons., Pest., Diet', Pathway)) %>% 
  separate(Pathway, into = paste0("source", 1:4), sep = ",") %>% 
  pivot_longer(cols = c(source1:source4)) %>% 
  rename(sources = value) %>% 
  filter(sources != "NA" & sources != "Unknown") %>% 
  mutate(sources = case_when(grepl('Ind.', sources, fixed = TRUE) ~ "Industrial", 
                             grepl('Cons.', sources, fixed = TRUE) ~ "Consumer", 
                             grepl('Pest.', sources, fixed = TRUE) ~ "Pesticide", 
                             grepl('Diet', sources, fixed = TRUE) ~ "Diet", 
                             TRUE ~ sources)) %>% 
  select(-name) %>% 
  pivot_wider(names_from = sources, values_from = sources) %>%
  select(CASRN, DTXSID, chemname, Consumer, Diet, Pesticide, Industrial, U95intake) %>%
  rename(Diet_exp = Diet, Consumer_exp = Consumer, Industrial_exp = Industrial, Pesticide_exp = Pesticide) %>%
  
  mutate(Consumer_exp = ifelse(!is.na(Consumer_exp), 'ExpoCast', Consumer_exp)) %>%
  mutate(Diet_exp = ifelse(!is.na(Diet_exp), 'ExpoCast', Diet_exp))%>%
  mutate(Pesticide_exp = ifelse(!is.na(Pesticide_exp), 'ExpoCast',Pesticide_exp))%>%
  mutate(Industrial_exp = ifelse(!is.na(Industrial_exp), 'ExpoCast', Industrial_exp)) %>%

  mutate(U95intake = case_when(U95intake >= 0.1 ~ "100 ug/kg/day or more", 
                               U95intake < 0.1 & U95intake >= 0.001 ~ "1 to 100 ug/kg/day",
                               U95intake < 0.001 ~ "Less than 1 ug/kg/day")) 

write_csv(ExpoCast, "./outputs/ExpoCast.csv")



#### FDA drugs ####
# Files from Drugs@FDA last updated Sep 21, 2021
# downloaded from https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files

# FDA drug product list
druglist <- read_tsv("./inputs/DrugProducts.txt") %>% 
  select(ApplNo, ProductNo, DrugName, ActiveIngredient) %>% 
  unique()

# These docs say whether the drugs in the product list are Rx, OTC, discontinued, or tentatively approved
Marketingstatuskey <- read_tsv("./inputs/DrugMarketingStatus_Lookup.txt")
Marketingstatus <- read_tsv("./inputs/DrugMarketingStatus.txt")


## FDA drugs w/ marketing status
FDA <- full_join(Marketingstatus, Marketingstatuskey, by = "MarketingStatusID") %>% 
  full_join(druglist, by = "ApplNo") %>%  
  unique() %>% 
  unite("Drug", DrugName, ActiveIngredient, sep = "; ") %>%
  separate(col = Drug, into = paste0("Drug", 1:14), sep = ";") %>% 
  pivot_longer(cols = c(Drug1:Drug14), names_to = "dr", values_to = "Drug", values_drop_na = TRUE) %>% 
  mutate(Drug = str_trim(Drug)) %>% 
  # Fix drug names so they'll match to the chemids glossary correctly
  mutate(preferred_name = tolower(Drug)) %>% 
  mutate(preferred_name = case_when(str_detect(preferred_name, "ethinyl") ~ '17alpha-ethinylestradiol', 
                                    preferred_name == "estradiol" ~ "17beta-estradiol",
                                    str_detect(preferred_name, "norlestrin") ~ "norlestrin",
                                    str_detect(preferred_name, "mitomycin") ~ "mitomycin c",
                                    str_detect(preferred_name, "doxorubicin") ~ "doxorubicin",
                                    str_detect(preferred_name, "griseofulvin") ~ "griseofulvin",
                                    str_detect(preferred_name, "azacitidine") ~ "5-azacytidine",
                                    str_detect(preferred_name, "indomethacin") ~ "1-(p-chlorobenzoyl)-5-methoxy-2-methyl-indole-3-acetic acid",
                                    str_detect(preferred_name, "hydroxyprogesterone caproate") ~ "17-((1-oxohexyl)oxy)pregn-4-ene-3,20-dione",
                                    str_detect(preferred_name, "disulfiram") ~ "tetraethylthiuram disulfide",
                                    str_detect(preferred_name, "phenylbutazone") ~ "4-butyl-1,2-diphenyl-3,5-pyrazolidinedione",
                                    TRUE ~ preferred_name)) %>% 
  inner_join(lowercase_chemids, by = "preferred_name") %>% 
  select(CASRN, DTXSID, preferred_name, MarketingStatusDescription) %>% 
  unique() %>% 
  pivot_wider(names_from = MarketingStatusDescription, values_from = MarketingStatusDescription) %>% 
  mutate(FDADrug = case_when(Prescription == "Prescription" & `Over-the-counter` == "Over-the-counter" ~ "FDA_Rx_OTC", 
                            Prescription == "Prescription" ~ "FDA_Rx",
                            `Over-the-counter` == "Over-the-counter" ~ "FDA_OTC",
                            `None (Tentative Approval)` == "None (Tentative Approval)" ~ "FDA_tentative_approval",
                            Discontinued == "Discontinued" ~ "FDA_discontinued")) %>% 
  select(CASRN:preferred_name, FDADrug)

write_csv(FDA, "./outputs/FDAdrugs.csv")


#### Substances added to food ####
## List from FDA contains everything that's added to foods in the US
# Downloaded from http://www.cfsanappsexternal.fda.gov/scripts/fdcc/?set=FoodSubstances
# Last updated 5/17/2022; Downloaded 7/26/2022
# The original download (FoodSubstances.csv) is not readable by R, so I cleaned 
#     it up by deleting the top 4 rows that describe the database, and all the 
#     columns except CASRN and substance name, since that's all that matters
# The full download FoodSubstances.csv is in the "inputs" folder as well for reference

EAFUS <- read.csv("./inputs/FoodSubstances_clean.csv") %>% 
  rename(CASRN = `CAS.Reg.No..or.other.ID.`) %>% 
  mutate(FoodSource = "FDA") %>% 
  mutate(CASRN = str_trim(CASRN))
  


#### EPA Pesticides ####
# downloaded lists of conventional chemical, antimicrobial, and biopesticide 
#   active ingredents from EPA website: https://ordspub.epa.gov/ords/pesticides/f?p=CHEMICALSEARCH:1
#   (Filter by Pesticide Type -> select type -> Actions -> Download)
#   Downloaded 8/13/2021

## "conventional chemicals" used in pesticides from EPA 
pesticides <- read.csv("./inputs/EPA_conventionalpesticides.csv") %>% 
  rename(CASRN = CAS.Number) %>% 
  separate(col = CASRN, into = paste0("CASRN", 1:26), sep = "; ") %>% 
  pivot_longer(cols = c(CASRN1:CASRN26), names_to = "cas", values_to = "CASRN", values_drop_na = TRUE) %>% 
  select(CASRN, Chemical.Name, Latest.Process) %>% 
  mutate(Pesticide_EPA = "EPA")


### antimicrobials
antimicrobial <- read.csv("./inputs/EPA_antimicrobials.csv") %>% 
  rename(CASRN = CAS.Number) %>% 
  separate(col = CASRN, into = paste0("CASRN", 1:6), sep = "; ") %>% 
  pivot_longer(cols = c(CASRN1:CASRN6), names_to = "cas", values_to = "CASRN", values_drop_na = TRUE) %>% 
  select(CASRN, Chemical.Name, Latest.Process) %>% 
  mutate(Pesticide_EPA = "EPA")

### biopesticides
biopest <- read.csv("./inputs/EPA_biopesticides.csv") %>% 
  rename(CASRN = CAS.Number) %>% 
  separate(col = CASRN, into = paste0("CASRN", 1:6), sep = "; ") %>% 
  pivot_longer(cols = c(CASRN1:CASRN6), names_to = "cas", values_to = "CASRN", values_drop_na = TRUE) %>% 
  select(CASRN, Chemical.Name, Latest.Process) %>% 
  mutate(Pesticide_EPA = "EPA")

EPA_pesticides <- full_join(pesticides, antimicrobial, by = "CASRN") %>% 
  full_join(biopest, by = "CASRN") %>% 
  mutate(Chemical = coalesce(Chemical.Name.x, Chemical.Name.y, Chemical.Name)) %>% 
  mutate(Pesticide_EPA = coalesce(Pesticide_EPA.x, Pesticide_EPA.y, Pesticide_EPA)) %>% 
  select(CASRN, Chemical, Pesticide_EPA) %>% 
  filter(CASRN != "") %>% 
  unique()
  
write_csv(EPA_pesticides, "./outputs/EPA_pesticides.csv")



#### Water disinfection byproducts ####
# gleaned from Richardson 2007, DOI: 10.1016/j.mrrev.2007.09.001 because
#   there is no official or complete list of water DBPs
DBPs <- data.frame(c("Dibromochloroacetic acid", "Bromochloroacetic acid", "Bromodichloroacetic acid", 
                     "Trichloroacetic acid", "Trichloronitromethane", "Dichloromethane", 
                     "Acetaldehyde", "3-Chloro-4-(dichloromethyl)-5-hydroxy-2(5H)-furanone", 
                     "N-Nitrosodiethylamine", "N-Nitrosodiphenylamine")) %>% 
  mutate(DBP = "waterDBP")

colnames(DBPs) <- c("preferred_name", "waterDBP")



####### Complete exposure sources #######

# Join, condense, and organize all exposure sources
ExposureSources <- full_join(CPDat_exposures, ExpoCast, by = "CASRN") %>% 
  full_join(HPV, by = "CASRN") %>% 
  full_join(FDA, by = "CASRN") %>% 
  full_join(EAFUS, by = "CASRN") %>% 
  full_join(EPA_pesticides, by = "CASRN") %>% 
  left_join(chemids, by = "CASRN") %>%  
  full_join(DBPs, by = "preferred_name") %>% 
  select(CASRN, DTXSID, preferred_name, Industrial_cp:Pharma_cp, Consumer_exp:Industrial_exp,
         FDADrug, FoodSource, waterDBP, Pesticide_EPA, HPV, U95intake) %>%

  unite(Consumer, c(Consumer_cp, Consumer_exp), sep = ", ", na.rm = TRUE) %>% 
  mutate(Consumer = ifelse(Consumer == "", "-", Consumer)) %>% 
  
  unite(Diet, c(FoodSource, Diet_cp, Diet_exp, waterDBP), sep = ", ", na.rm = TRUE) %>% 
  mutate(Diet = case_when(str_detect(preferred_name, "Ochratoxin A|MeIQ|alpha-Ecdysone") | 
                         str_detect(preferred_name, "Ptaquiloside|Zearalenone|Trp-P-2") |
                         str_detect(preferred_name, "Morin hydrate|Enterolactone|Genistein") ~ "naturally_occurring", 
                         Diet == "" ~ "-",
                         TRUE ~ Diet)) %>% 
  
  mutate(Pharma = coalesce(FDADrug, Pharma_cp)) %>% 
  mutate(Pharma = ifelse(is.na(Pharma), "-", Pharma)) %>% 
  
  unite(Pesticide, c(Pesticide_EPA, Pesticide_cp, Pesticide_exp), sep = ", ", na.rm = TRUE) %>% 
  mutate(Pesticide = ifelse(Pesticide == "", "-", Pesticide)) %>% 
  
  unite(Industrial, c(Industrial_cp, Industrial_exp), sep = ", ", na.rm = TRUE) %>% 
  mutate(Industrial = ifelse(Industrial == "", "-", Industrial)) %>% 

  rename(Environmental_media = Environment_cp) %>% 
  mutate(Environmental_media = ifelse(is.na(Environmental_media), "-", Environmental_media)) %>% 
  select(CASRN:preferred_name, Consumer, Diet, Pharma, Pesticide, Industrial, Environmental_media, HPV, U95intake) %>% 
  mutate(across(.cols = HPV:U95intake, .fns = str_replace_na, replacement = "-"))
 
write_csv(ExposureSources, "./outputs/ExposureSources.csv")



#### Prop 65 ####
# California's Proposition 65 list of chemicals as of Feb 25, 2022 downloaded from
#    https://oehha.ca.gov/proposition-65/proposition-65-list on 7/26/2022
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

write_csv(P65, "./outputs/P65list.csv")



####### Complete list of BC-Relevant exposures, effects, sources and Prop 65 listing #######

BCrel_Effects_and_Sources <- left_join(BCrelList, ExposureSources, by = "CASRN") %>% 
  left_join(P65, by = "CASRN") %>% 
  rename(DTXSID = DTXSID.x, preferred_name = preferred_name.x) %>% 
  select(CASRN:Genotoxicity, Consumer:Prop65) %>% 
  mutate(across(.cols = Consumer:Prop65, .fns = str_replace_na, replacement = "-")) %>%  
  unique()


write_csv(BCrel_Effects_and_Sources, "./outputs/BCrel_Effects_and_Sources.csv")



####### Highlights #######

## Table with all genotoxic higher-confidence EDCs 
EDC_gentox <- BCrel_Effects_and_Sources %>% 
  filter(!str_detect(EDC, "-")) %>% 
  filter(Genotoxicity == "positive" & EDC == "EDC+")  


write_csv(EDC_gentox, "./outputs/EDC_gentox.csv")



### FDA drugs, w/ KC summaries
BCrelFDA <- BCrel_Effects_and_Sources %>% 
  select(CASRN:MC, HormoneSummary:EDC, Genotoxicity, Pharma) %>% 
  filter(str_detect(Pharma, "FDA")) 

write_csv(BCrelFDA, "./outputs/BCrel_FDAdrugs.csv")



### MCs not on Prop 65 list
MC_notP65 <- BCrel_Effects_and_Sources %>% 
  filter(!str_detect(Prop65, "Cancer")) %>% 
  filter(MC == "MC") %>% 
  #remove ionizing radiation 
  filter(!str_detect(preferred_name, "Ionizing")) %>% 
  # remove steroidal estrogens b/c they're Prop65 listed as a group
  filter(!str_detect(preferred_name, "Estradiol|Estriol|Norlestrin")) %>% 
  # remove Aroclor 1254 and 4'OH-PCB-61 b/c Prop65 lists PCBs as a group
  filter(!str_detect(preferred_name, "Aroclor|4'OH")) 

write_csv(MC_notP65, "./outputs/MCs_not_Prop65_carcinogens.csv")

