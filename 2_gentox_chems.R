# AUTHOR: Jenny Kay
# PURPOSE: Integrate gentox results from CCRIS, EURL ECVAM, NTP, eChemPortal, TOXNET. 
#           These were the sources used to construct the 2021 version of EPA's ToxValDB,
#           excluding COSMOS because COSMOS database has a ton of errors
# STARTED: 2021-07-01
# written in version: R version 4.1.0 (2021-05-18)

library(tidyverse)
library(readxl)

# This assigns the folder where the R script lives to workingdir
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path) ## when you type setup snippet, have to add $path after ...Context()
# This sets the working directory to workingdir
setwd(workingdir)

options(stringsAsFactors = FALSE)



# glossary matching CASRNs to DTXSIDs and chem names
# downloaded from CompTox Dashboard https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=616dd716e4b0a5ca8aeea68e&page=0
chemids <- read.csv("./inputs/DSSTox_Identifiers_and_CASRN_2021r1.csv") %>%
  rename(CASRN = casrn, DTXSID = dtxsid, preferred_name = preferredName) %>% 
  select(CASRN, DTXSID, preferred_name)%>% 
  # some characters showed up weird for 1,2-benzenediol
  mutate(preferred_name = ifelse(CASRN == "120-80-9", "1,2-Benzenediol", preferred_name))



##### CCRIS gentox #####
# Downloaded NCI's Chemical Carcinogenesis Research Information System archive from 
#     https://www.nlm.nih.gov/databases/download/ccris.html
ccris <- read_excel("./inputs/ccris.xlsx", col_names = FALSE)
len_ccris <- ncol(ccris) # you can't use name of function to name a variable

# Full CCRIS download contains a junk row at the top - assign column names as second row
colnames(ccris) <- ccris[2,] 

ccrisgentox <- ccris[-c(1,2),] %>% # Remove junk rows
  subset(select = -c(1, 3:59, 61, 62, 65:len_ccris)) %>% #pull results only
  select(1,4,3,2) %>%
  #subset(select = -c(1, 3:51, 65:len_ccris)) %>%  #pull results with references
  rename(CASRN = `/DOC/CASRegistryNumber`, chemname = `/DOC/NameOfSubstance`, 
         test = `/DOC/mstu/tsstm`, result = `/DOC/mstu/rsltm`) %>%
  filter(test != 'NA') %>% 
  mutate(result = tolower(result)) %>% 
  mutate(result = ifelse(grepl('positive', result, fixed = FALSE), 'positive', result)) %>% 
  mutate(result = ifelse(grepl('negative', result, fixed = FALSE), 'negative', result)) %>%
  mutate(result = ifelse(grepl('inconclusive', result, fixed = FALSE), 'inconclusive', result)) %>% 
  mutate(result = ifelse(grepl('equivocal', result, fixed = FALSE), 'equivocal', result)) %>% 
  unique() %>% 
  subset(select = -c(test)) %>% 
  unique() %>% 
  pivot_wider(names_from = result, values_from = result) %>% 
  subset(select = -c(5, 6, 9:11)) %>% 
  unite(result, c(positive:equivocal), sep=",", na.rm = TRUE) %>% 
  mutate(cgentox = ifelse(grepl('positive', result, fixed = FALSE), 'positive', result)) %>%
  left_join(chemids, by = "CASRN") %>%
  mutate(preferred_name = coalesce(preferred_name, chemname)) %>%
  select(CASRN, DTXSID, preferred_name, cgentox)





##### ECVAM #####
# ECVAM "positive Ames results" database, Reference Corvi 2018
#     Download from http://data.europa.eu/89h/jrc-eurl-ecvam-genotoxicity-carcinogenicity-ames
#     Due to structure of excel file with filtered and grouped columns, had to remove first row
#     Full download also included in "inputs" for reference, but not used in code

ECVAM_amespos <- read_excel("./inputs/ECVAM_Ames_positives_DB_row1removed.xls") %>% 
  rename(CASRN = `CAS No. cleaned`, invitropos = `invitro+`, invitroneg = `invitro-`, 
         invivopos = `invivo+`, invivoneg = `invivo-`) %>% 
  # some chems had acid/base/color indicated in parentheses - cleaned to just CASRN
  separate(CASRN, into = c("CASRN", "parenthetical"), sep = "[(]") %>% 
  select(CASRN, Chemical, invitropos, invitroneg, invivopos, invivoneg) %>% 
  #this chem missing CASRN
  mutate(CASRN = ifelse(Chemical == "2-Nitro-3-methylimidazo[4,5-f]quinoline [AKA nitro-IQ]", "114451-08-0", CASRN)) %>% 
  mutate(ECVAMpos = case_when(invitropos > 0 | invivopos > 0 ~ "pos", 
                              invitroneg > 0 | invivoneg > 0 ~ "neg",
                              TRUE ~ "neither")) %>% 
  # Remove GSK pharmaceuticals w/o CASRN
  filter(CASRN != "n.a.")

# ECVAM "negative Ames results" database, reference Madia 2020
#     Download from supplemental table at DOI: 10.1016/j.mrgentox.2020.503199
#     Due to structure of excel file with grouped columns, had to remove first row to read in
#     Full download also included in "inputs" for reference, but not used in code
ECVAM_amesneg <- read_excel("./inputs/ECVAM_AmesNegDB_row1removed.xls") %>% 
  rename(CASRN = `CAS No.`, Amesoverall = `AMES Overall`, invitromut = `in vitro MCGM Overall`, 
         invitroMN = `in vitro MN Overall`, invitroCA = `in vitro CA  Overall`, 
         invivoMN = `in vivo MN Overall`, invivoCA = `in vivo CA Overall`, 
         TGR = `TGR Overall`, UDS = `in vivo UDS Overall`, DNAdamage = `in vivo DNA damage Overall`) %>% 
  # some chems had CASRNs for parents and salts together separated by "/" - clean to just parent
  separate(CASRN, into = c("CASRN", "salt"), sep = "[/]") %>% 
  select(CASRN, Chemical, Amesoverall, invitromut, invitroMN, invitroCA, invivoMN, invivoCA, TGR, UDS, DNAdamage) %>% 
  unite("ECVAMnegmerge", Amesoverall:DNAdamage, sep = ";", na.rm = TRUE) %>%
  mutate(ECVAMneg = case_when(grepl('+', ECVAMnegmerge, fixed = TRUE) ~ "pos",
                              grepl('-', ECVAMnegmerge, fixed = TRUE) ~ "neg",
                              TRUE ~ "check")) %>% 
  # Remove apicidin derivatives SD-0203 and SD-2007; Apicidin and both of these are recorded as positive
  filter(CASRN != "N")

ECVAMoverall <- ECVAM_amespos %>% 
  full_join(ECVAM_amesneg, by = "CASRN") %>% 
  left_join(chemids, by = "CASRN") %>% 
  mutate(ECVAMgentox = case_when(ECVAMpos == "pos" | ECVAMneg == "pos" ~ "positive",
                                 ECVAMpos == "neg" | ECVAMneg == "neg" ~ "negative",
                                 TRUE ~ "-")) %>% 
  select(CASRN, DTXSID, preferred_name, ECVAMgentox) 
  


##### NTP #####
# NTP CEBS gentox results available at DOI: 10.22427/NTP-DATA-022-00002-0002-000-8 
#     and https://cebs.niehs.nih.gov/datasets/search/trf
NTP_gentox <- read_excel("./inputs/NTP_Gentox_andother_Findings_Data_2020-03-05.xlsx") %>% 
  select(CASRN, `Chemical Name`, `Bacterial Mutagenicity Conclusion`, 
         `Male Rat Micronucleus Conclusion`:`Female Mouse Comet Assay Conclusion`) %>%  
  left_join(chemids, by = "CASRN") %>% 
  mutate(preferred_name = coalesce(preferred_name, `Chemical Name`)) %>% 
  rename(amesresult = `Bacterial Mutagenicity Conclusion`, 
         femalemousecomet = `Female Mouse Comet Assay Conclusion`) %>% 
  unite("ntpgentox", amesresult:femalemousecomet, sep = ";", na.rm = TRUE) %>% 
  mutate(ntpgentox = case_when(grepl('ositive', ntpgentox, fixed = TRUE) ~ "positive",
                                 grepl('egative', ntpgentox, fixed = TRUE) ~ "negative",
                                 TRUE ~ "-")) %>% 
  select(CASRN, DTXSID, preferred_name, ntpgentox)



##### eChemPortal #####
# eChemPortal genotoxicity database provided by Richard Judson at US EPA ftp 
#     (https://gaftp.epa.gov/Comptox/Staff/rjudson/datasets/genetox/)
echemportal <- read_excel("./inputs/eChemPortalAPI_GeneticToxicityVivo_FinalRecords.xlsx") %>% 
  rename(CASRN = Number) %>% 
  filter(`Number Type` == "CAS Number") %>% # remove things with only IUPAC names
  mutate(Genotoxicity = case_when(grepl('quivocal', Genotoxicity, fixed = TRUE) | 
                                    grepl('mbiguous', Genotoxicity, fixed = TRUE) |
                                    grepl('mbiguous', Genotoxicity, fixed = TRUE) ~ "-",
                                  
                                  grepl('ositive', Genotoxicity, fixed = TRUE) ~ "positive",
                                  grepl('ignificant increase', Genotoxicity, fixed = TRUE) ~ "positive",
                                  
                                  grepl('egative', Genotoxicity, fixed = TRUE) ~ "negative",
                                  TRUE ~ "-")) %>% 
  filter(Genotoxicity != "-") %>% 
  select(CASRN, Name, Genotoxicity) %>% 
  unique() %>% 
  pivot_wider(names_from = Genotoxicity, values_from = Genotoxicity) %>% 
  mutate(echemgentox = coalesce(positive, negative)) 




###### GENE-TOX #####
# NLM TOXNET's GENE-TOX database files
#    Downloaded at https://www.ncbi.nlm.nih.gov/pcsubstance?term=%22Genetic%20Toxicology%20Data%20Bank%20(GENE-TOX)%22%5BSourceName%5D%20AND%20hasnohold%5Bfilt%5D
toxnet_gentox_IDs <- read_excel("./inputs/NLM_TOXNET_GENETOX_Substance.xlsx") # key of substance IDs, given as "NLM_TOXNET_GENETOX_n"
toxnet_gentox_results <- read_tsv("./inputs/NLM_TOXNET_GENTOX_results.txt", # results for each chem ID "NLM_TOXNET_GENETOX_n"
                                  col_names = c("IDs", "results"), quote = "") %>% 
  filter(IDs != "END") 

v_asta = NULL
v_resa = NULL
SOURCE_NAME_SID = NULL
flag_resa = 0
flag_asta = 0

for (i in seq(1:dim(toxnet_gentox_results)[1])){
  value_test = toxnet_gentox_results[i, 1]
  if(grepl('NLM_TOXNET_GENETOX', value_test, fixed = TRUE)){
    SOURCE_NAME_SID = append(SOURCE_NAME_SID, value_test)
    if(length(SOURCE_NAME_SID) != 1){
      if(flag_resa == 1){
        v_resa = append(v_resa, resa)
      }else{
        v_resa = append(v_resa, "NA")
      }
      if(flag_asta == 1){
        v_asta = append(v_asta, asta)
      }else{
        v_asta = append(v_asta, "NA")
      }
      flag_resa = 0
      flag_asta = 0
    }
  }
  
  if(toxnet_gentox_results[i, 1] == "asta"){
    asta = toxnet_gentox_results[i,2]
    flag_asta = 1
  }
  
  if(toxnet_gentox_results[i, 1] == "resa"){
    resa = toxnet_gentox_results[i,2]
    flag_resa = 1
  }
}

## asta and resa not working

v_asta = append(v_asta, asta)
v_resa = append(v_resa, resa)


SOURCE_NAME_SID <- unlist(SOURCE_NAME_SID)
v_asta <- unlist(v_asta)
v_resa <- unlist(v_resa)

toxnet_gentox <- cbind(SOURCE_NAME_SID, v_asta) %>% 
  cbind(v_resa) %>% 
  as.data.frame() %>%
  left_join(toxnet_gentox_IDs, by = "SOURCE_NAME_SID") %>% 
  filter(CASRN != "NOCAS") %>% 
  left_join(chemids, by = "CASRN") %>% 
  filter(v_asta != "Sperm morphology") %>% 
  filter(v_asta != "In vivo carcinogenicity studies") %>% 
  filter(v_asta != "NA") %>% 
  rename(tngentox = v_resa) %>% 
  select(CASRN, DTXSID, preferred_name, tngentox)  %>% 
  unique() %>% 
  pivot_wider(names_from = tngentox, values_from = tngentox) %>% 
  unite("tngentox", Positive, Negative, `Positive without activation, negative with activation`, sep = ";", na.rm = TRUE) %>% 
  mutate(tngentox = case_when(grepl('Positive', tngentox, fixed = TRUE) ~ "positive",
                              grepl('Negative', tngentox, fixed = TRUE) ~ "negative",
                              TRUE ~ "-")) %>% 
  select(CASRN:tngentox) 
  




##### Combine data from input sources #####

gentox_ccris_ecvam_ntp_echem_toxnet <- ECVAMoverall %>% 
  full_join(ccrisgentox, by = "CASRN") %>% 
  full_join(NTP_gentox, by = "CASRN") %>% 
  full_join(echemportal, by = "CASRN") %>% 
  full_join(toxnet_gentox, by = "CASRN") %>% 
  unite("Genotoxicity", c(ECVAMgentox, cgentox, ntpgentox, echemgentox, tngentox), sep = ";", na.rm = TRUE) %>% 
  mutate(Genotoxicity = case_when(grepl('positive', Genotoxicity, fixed = TRUE) ~ "positive",
                                  grepl('negative', Genotoxicity, fixed = TRUE) ~ "negative",
                                  TRUE ~ "-")) %>% 
  mutate(DTXSID = coalesce(DTXSID.x, DTXSID.y, DTXSID.x.x, DTXSID.y.y)) %>% 
  mutate(preferred_name = coalesce(preferred_name.x, preferred_name.y, preferred_name.x.x, preferred_name.y.y)) %>% 
  select(CASRN, DTXSID, preferred_name, Genotoxicity) %>% 
  unique()


#write.csv(gentox_ccris_ecvam_ntp_echem_toxnet, "./outputs/gentox_ccris_ecvam_ntp_echem_toxnet.csv", row.names = FALSE)

