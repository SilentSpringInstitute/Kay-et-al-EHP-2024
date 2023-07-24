# AUTHOR: Jenny Kay
# PURPOSE: Integrate gentox results from CCRIS, EURL ECVAM, NTP, eChemPortal, & GENE-TOX databases 
# STARTED: 2021-07-01
# last update: 2023-07-24
# written in version: R version 4.1.0 (2021-05-18)

library(tidyverse)
library(readxl)
library(stringr)

# Assign the folder where the R script lives to working directory
workingdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(workingdir)

options(stringsAsFactors = FALSE)



# glossary matching CASRNs to DTXSIDs and chem names (this will already be in 
#     your environment if following from 1_MCList_refs.R)
# downloaded from CompTox Dashboard https://clowder.edap-cluster.com/datasets/61147fefe4b0856fdc65639b?space=6112f2bee4b01a90a3fa7689#folderId=616dd716e4b0a5ca8aeea68e&page=0
chemids <- read.csv("inputs/DSSTox_Identifiers_and_CASRN_2021r1.csv") %>%
  rename(CASRN = casrn, DTXSID = dtxsid, preferred_name = preferredName) %>% 
  select(CASRN, DTXSID, preferred_name)%>% 
  # some characters showed up weird for 1,2-benzenediol
  mutate(preferred_name = ifelse(CASRN == "120-80-9", "1,2-Benzenediol", preferred_name))



##### CCRIS gentox #####
# Downloaded NCI's Chemical Carcinogenesis Research Information System archive from 
#     https://www.nlm.nih.gov/databases/download/ccris.html
# If you're running immediately after 1_MCList_refs.R, no need to reload CCRIS database, 
#     skip to creation of ccrisgentox dataframe
CCRIS <- read_excel("inputs/ccris.xlsx", col_names = FALSE)
len_ccris <- ncol(CCRIS)

# Full CCRIS download contains a junk row at the top - assign column names as second row
colnames(CCRIS) <- CCRIS[2,] 

ccrisgentox <- CCRIS[-c(1,2),] %>% # Remove junk rows
  rename(CASRN = `/DOC/CASRegistryNumber`, chemname = `/DOC/NameOfSubstance`, 
         test = `/DOC/mstu/tsstm`, result = `/DOC/mstu/rsltm`) %>%
  select(CASRN, chemname, test, result) %>% 
  # fix a CASRN
  mutate(CASRN = ifelse(
    chemname == "ANTI-(+/-)-1,2,3,4-TETRAHYDROBENZO[C]PHENANTHRENE-3,4-DIOL-1,2-EPOXIDE", 
    "82510-57-4", CASRN)) %>% 
  # only want tested chems; also there's a chem without a CASRN that can't be matched, so removing it
  filter(test != 'NA' & CASRN != "$null") %>% 
  mutate(result = tolower(result)) %>% 
  mutate(result = case_when(str_detect(result, "positive|breaks|elevated") ~ "positive",
                            str_detect(result, "negative") ~ "negative",
                            str_detect(result, "inconclusive|equivocal") ~ "inconclusive",
                            # remaining results are NA and selenite reducing micronuclei 
                            #    from MMC or cyclophosphamide and therefore not 
                            #    gentox tests of the chemical
                            TRUE ~ "-")) %>% 
  subset(select = -c(test)) %>% 
  unique() %>% 
  filter(result != "-") %>% 
  pivot_wider(names_from = result, values_from = result) %>% 
  mutate(cgentox = coalesce(positive, negative, inconclusive)) %>% 
  select(CASRN, chemname, cgentox) %>% 
  left_join(chemids, by = "CASRN") %>%
  mutate(preferred_name = coalesce(preferred_name, chemname)) %>% 
  select(CASRN, DTXSID, preferred_name, cgentox) %>% unique()



##### ECVAM #####
# ECVAM "positive Ames results" database, Reference Corvi 2018
#     Download from http://data.europa.eu/89h/jrc-eurl-ecvam-genotoxicity-carcinogenicity-ames
#     R can't read excel file with filtered and grouped columns, so had to 
#     remove first row. Full download also included in "inputs" for reference, but not used in code
ECVAM_amespos <- read_excel("inputs/ECVAM_Ames_positives_DB_row1removed.xls") %>% 
  rename(CASRN = `CAS No. cleaned`, invitropos = `invitro+`, invitroneg = `invitro-`, 
         invivopos = `invivo+`, invivoneg = `invivo-`) %>% 
  # some chems had acid/base/color indicated in parentheses - cleaned to just CASRN
  separate(CASRN, into = c("CASRN", "parenthetical"), sep = "[(]") %>% 
  select(CASRN, Chemical, invitropos, invitroneg, invivopos, invivoneg, 
         `Ames Overall`, `in vitro MLA Overall`, `in vitro MN Overall`, `in vitro CA Overall`,
         `in vivo MN Overall`, `in vivo CA Overall`, `in vivo UDS Overall`, `transgenic Overall`,
         `in vivo DNA damage Overall`) %>% 
  # two chems have incorrect CASRNs
  mutate(CASRN = case_when(
    Chemical == "2-Nitro-3-methylimidazo[4,5-f]quinoline [AKA nitro-IQ]" ~ "114451-08-0", 
    Chemical == "Aloe emodin" ~ "481-72-1",
    TRUE ~ CASRN)) %>% 
  mutate(ECVAMpos = case_when(`Ames Overall` == "+" | invitropos > 0 | invivopos > 0 ~ "pos", 
                              invitroneg > 0 | invivoneg > 0 ~ "neg",
                              TRUE ~ "inconclusive")) %>% 
  # Remove GSK pharmaceuticals and non-specific chemicals (diesel exhaust and tobacco protein) w/o CASRN
  filter(CASRN != "n.a.") %>% 
  select(CASRN, Chemical, ECVAMpos) 

# ECVAM "negative Ames results" database, reference Madia 2020
#     Download from supplemental table at DOI: 10.1016/j.mrgentox.2020.503199
#     Due to structure of excel file with grouped columns, had to remove first row to read in
#     Full download also included in "inputs" for reference, but not used in code
ECVAM_amesneg <- read_excel("inputs/ECVAM_AmesNegDB_row1removed.xls") %>% 
  rename(CASRN = `CAS No.`, Amesoverall = `AMES Overall`, invitromut = `in vitro MCGM Overall`, 
         invitroMN = `in vitro MN Overall`, invitroCA = `in vitro CA  Overall`, 
         invivoMN = `in vivo MN Overall`, invivoCA = `in vivo CA Overall`, 
         TGR = `TGR Overall`, UDS = `in vivo UDS Overall`, DNAdamage = `in vivo DNA damage Overall`) %>% 
  # some chems had CASRNs for parents and salts together separated by "/" - clean to just parent
  separate(CASRN, into = c("CASRN", "salt"), sep = "[/]") %>% 
  select(CASRN, Chemical, Amesoverall, invitromut, invitroMN, invitroCA, invivoMN, 
         invivoCA, TGR, UDS, DNAdamage) %>% 
  unite("ECVAMnegmerge", Amesoverall:DNAdamage, sep = ";", na.rm = TRUE) %>%
  mutate(ECVAMneg = case_when(grepl('+', ECVAMnegmerge, fixed = TRUE) ~ "pos",
                              grepl('-', ECVAMnegmerge, fixed = TRUE) ~ "neg",
                              TRUE ~ "-")) %>% 
  # Remove apicidin derivatives SD-0203 and SD-2007; Apicidin and both of these are recorded as positive
  filter(CASRN != "N")

# Join Ames pos and Ames neg databases
ECVAMoverall <- full_join(ECVAM_amespos, ECVAM_amesneg, by = "CASRN") %>%
  left_join(chemids, by = "CASRN") %>%
  mutate(preferred_name = coalesce(preferred_name, Chemical.x, Chemical.y)) %>%
  mutate(ECVAMgentox = case_when(ECVAMpos == "pos" | ECVAMneg == "pos" ~ "positive",
                                 ECVAMpos == "neg" | ECVAMneg == "neg" ~ "negative",
                                 TRUE ~ "inconclusive")) %>% 
  select(CASRN, DTXSID, preferred_name, ECVAMgentox) %>% unique()
  


##### NTP #####
# NTP CEBS gentox results available at DOI: 10.22427/NTP-DATA-022-00002-0002-000-8 
#     and https://cebs.niehs.nih.gov/datasets/search/trf
#     version updated February 2022 (most recent as of July 24, 2023)

NTP_gentox <- read.csv("inputs/20220927-NTPgentox.csv") %>% 
  select(CASRN, DTXSID, Chemical.Name, `Bacterial.Mutagenicity.Conclusion`:`Female.Mouse.Comet.Assay.Conclusion`) %>%  
  #remove URL from CASRN entries
  separate(CASRN, into = c("blank", "CASRN"), sep = "[ (]") %>%
  
  # many identifiers are weird, so use chemids glossary to get standardized CASRN and DTXSIDs
  left_join(chemids, by = "CASRN") %>% 
  # dioctyl phthalate is incorrectly given di(2-ethylhexyl) phthalate's DTXSID, so fix that, then collapse DTXSIDs
  mutate(DTXSID = ifelse(CASRN == "117-84-0", "DTXSID1021956", coalesce(DTXSID.x, DTXSID.y))) %>% 
  left_join(chemids, by = "DTXSID") %>% 
  rename(CASRN = CASRN.y, preferred_name = preferred_name.y) %>% 
  
  unite("ntpgentox", `Bacterial.Mutagenicity.Conclusion`:`Female.Mouse.Comet.Assay.Conclusion`, sep = ";", na.rm = TRUE) %>% 
  mutate(ntpgentox = case_when(grepl('ositive', ntpgentox, fixed = TRUE) ~ "positive",
                               #black cohosh and extracts thereof have multiple entries but all with same CASRN/DTXSID.
                               #    the straight-up "black cohosh" entry is positive, so assign that CASRN pos result
                               CASRN == "84776-26-1" ~ "positive",
                               grepl('egative', ntpgentox, fixed = TRUE) ~ "negative",
                               grepl('quivoc', ntpgentox, fixed = TRUE) ~ "inconclusive",
                               # Other entries had cancer data but not gentox
                               TRUE ~ "-")) %>% 
  # some mixtures, mold, and cell phone radiation don't have CASRNs - remove 
  filter(ntpgentox != "-" & !is.na(CASRN)) %>% 
  select(CASRN, DTXSID, preferred_name, ntpgentox) %>% 
  unique()




##### eChemPortal #####
# eChemPortal genotoxicity database provided by Richard Judson at US EPA ftp 
#     (https://gaftp.epa.gov/Comptox/Staff/rjudson/datasets/genetox/)
#     latest in vivo and in vitro versions, dated and downloaded 7/6/2021
echemportal_vivo <- read_excel("inputs/eChemPortalAPI_GeneticToxicityVivo_FinalRecords.xlsx") %>% 
  # only include studies considered reliable without (cat 1) or with (cat 2) restrictions
  filter(str_detect(Reliability, "1|2")) %>% 
  rename(CASRN = Number) %>% 
  filter(`Number Type` == "CAS Number") %>% # remove things without CASRNs
  left_join(chemids, by = "CASRN") %>% 
  select(CASRN, Genotoxicity) %>%

  mutate(Genotoxicity = case_when(
    str_detect(Genotoxicity, "egative") |
      # four entries are for negative results but mention effects in the positive control,
      #   deleting so everything else with the word "positive" is correctly ID'ed as positive
      str_detect(Genotoxicity, "ositive control") ~ "negative",
    
    str_detect(Genotoxicity, "ambiguous") |
      str_detect(Genotoxicity, "quivocal") ~ "inconclusive",
    
    str_detect(Genotoxicity, "ositive") |
      str_detect(Genotoxicity, "ignificant increase") ~ "positive",
    
    str_detect(Genotoxicity, "other") ~ "inconclusive",
    # all other entries for Genotoxicity are no data or not determined
    TRUE ~ "-")) %>% 
  filter(Genotoxicity != "-") %>% 
  pivot_wider(names_from = Genotoxicity, values_from = Genotoxicity) %>% 
  unite("echemvivo", positive, negative, inconclusive) %>% 
  # any positive result = positive; negative or inconclusive = negative; inconclusive only = inconclusive
  mutate(echemvivo = case_when(str_detect(echemvivo, "positive") ~ "positive",
                                 str_detect(echemvivo, "negative") ~ "negative",
                                 str_detect(echemvivo, "inconclusive") ~ "inconclusive",
                                 TRUE ~ "-")) %>% 
  filter(echemvivo != "-") %>% 
  unique()


echemportal_vitro <- read_excel("inputs/eChemPortalAPI_GeneticToxicityVitro_FinalRecords.xlsx") %>% 
  # only include studies considered reliable without (cat 1) or with (cat 2) restrictions
  filter(str_detect(Reliability, "1|2")) %>% 
  rename(CASRN = Number) %>% 
  filter(`Number Type` == "CAS Number") %>% # remove things without CASRNs
  left_join(chemids, by = "CASRN") %>% 
  mutate(preferred_name = coalesce(preferred_name, Name)) %>% 
  select(CASRN, Genotoxicity) %>% 

  # terms identified by iteration to classify any positive result as positive, 
  #   anything uncertain as inconclusive, anything negative-only as negative
  #   I have lots of terms because many results descriptions are long and complicated
  mutate(Genotoxicity = case_when(
    str_detect(Genotoxicity, "^[pP]ositive|positive struct|: pos") |
      str_detect(Genotoxicity, "[pP]ositive [wW]") ~ "positive", 
    
    Genotoxicity == "negative" | Genotoxicity == "other: No" |
      str_detect(Genotoxicity, "^[nN]egative|[Nn]o positive|not mut|main mut") ~ "negative",
    
    #entries without valid gentox data (not determined, not complete, false pos/neg)
    str_detect(Genotoxicity, "no data|other: see|applicable|not req") |
      # gentox not determined or studies not complete
      str_detect(Genotoxicity, "[nN]ot det|considered valid|[Pp]relim|on going") |
      str_detect(Genotoxicity, "in progress|tbd|SNIF|: TEA|DMF|[fF]alse") |
      Genotoxicity == "other:" ~ "-",
    
    # any entry containing "conclusive" is inconclusive or no conclusive statement
    str_detect(Genotoxicity, "mbiguous|quivocal|conclusive|no clear") |
      str_detect(Genotoxicity, "cytotox") ~ "inconclusive",
    
    str_detect(Genotoxicity, "ositive for") ~ "positive",
    
    #^[nN]egative doesn't work b/c sometimes followed by pos
    str_detect(Genotoxicity, "no increase|inactive") |
      str_detect(Genotoxicity, "non clast|Not clast") |
      str_detect(Genotoxicity, "no trans|no relev|No geno")|
      str_detect(Genotoxicity, "not obs|reduction") |
      str_detect(Genotoxicity, "no sig") ~ "negative",
    
    str_detect(Genotoxicity, "borderline|relevance") ~ "inconclusive",
    
    str_detect(Genotoxicity, "ositive|lastogen|yes|mutagenic") |
      str_detect(Genotoxicity, "small|margin|[wW]eak|Slight|only") |
      str_detect(Genotoxicity, "elevate|transform|increase|Based on") ~ "positive",
    
    str_detect(Genotoxicity, "other: [nN]|change|neg") ~ "negative",
    
    # all other entries for Genotoxicity are no data or not determined
    TRUE ~ "-")) %>% 
  filter(Genotoxicity != "-") %>% 
  pivot_wider(names_from = Genotoxicity, values_from = Genotoxicity) %>% 
  unite("echemvitro", positive, negative, inconclusive, na.rm = TRUE) %>% 
  mutate(echemvitro = case_when(str_detect(echemvitro, "positive") ~ "positive",
                                 str_detect(echemvitro, "negative") ~ "negative",
                                 str_detect(echemvitro, "inconclusive") ~ "inconclusive",
                                 TRUE ~ "-")) %>% 
  filter(echemvitro != "-") %>% 
  unique()


# Merge in vivo and in vivo gentox results from eChemPortal
echemportal <- full_join(echemportal_vivo, echemportal_vitro, by = "CASRN") %>% 
  unite("echemgentox", echemvivo, echemvitro) %>% 
  mutate(echemgentox = case_when(str_detect(echemgentox, "positive") ~ "positive",
                                str_detect(echemgentox, "negative") ~ "negative",
                                str_detect(echemgentox, "inconclusive") ~ "inconclusive",
                                TRUE ~ "-")) %>% 
  unique()
  



##### TOXNET GENE-TOX ####
# NLM TOXNET's GENE-TOX database files, downloaded 7/6/2021 at
#    https://www.ncbi.nlm.nih.gov/pcsubstance?term=%22Genetic%20Toxicology%20Data%20Bank%20(GENE-TOX)%22%5BSourceName%5D%20AND%20hasnohold%5Bfilt%5D

# key of substance IDs, given as "NLM_TOXNET_GENETOX_n"
toxnet_gentox_IDs <- read_excel("inputs/NLM_TOXNET_GENETOX_Substance.xlsx") 
# results for each chem ID "NLM_TOXNET_GENETOX_n"
toxnet_gentox_results <- read_tsv("./inputs/NLM_TOXNET_GENTOX_results.tsv", 
                                  col_names = c("IDs", "results"), quote = "") %>% 
  filter(IDs != "END" & IDs != "GENA") 

# asta/resa = evaluation pre-1980
v_asta = NULL
v_resa = NULL
# astb/resb = evaluation post-1980
v_astb = NULL
v_resb = NULL
SOURCE_NAME_SID = NULL
flag_resa = 0
flag_asta = 0
flag_resb = 0
flag_astb = 0

# First do evals pre-1980
for (i in seq(1:dim(toxnet_gentox_results)[1])){
  value_test = toxnet_gentox_results[i, 1]
  if(grepl('NLM_TOXNET_GENETOX', value_test, fixed = TRUE)
  ){SOURCE_NAME_SID = append(SOURCE_NAME_SID, value_test)
  if(length(SOURCE_NAME_SID) != 1){
    if(flag_resa == 1){v_resa = append(v_resa, resa)}
    else{v_resa = append(v_resa, "NA")}
    if(flag_asta == 1){v_asta = append(v_asta, asta)}
    else{v_asta = append(v_asta, "NA")}
    flag_resa = 0
    flag_asta = 0}}
  
  if(toxnet_gentox_results[i, 1] == "asta"){
    asta = toxnet_gentox_results[i,2]
    flag_asta = 1}
  
  if(toxnet_gentox_results[i, 1] == "resa"){
    resa = toxnet_gentox_results[i,2]
    flag_resa = 1}
}
v_asta = append(v_asta, asta)
v_resa = append(v_resa, resa)

# next do evals post-1980
for (i in seq(1:dim(toxnet_gentox_results)[1])){
  value_test = toxnet_gentox_results[i, 1]
  if(grepl('NLM_TOXNET_GENETOX', value_test, fixed = TRUE)
  ){SOURCE_NAME_SID = append(SOURCE_NAME_SID, value_test)
  if(length(SOURCE_NAME_SID) != 1){
    if(flag_resb == 1){v_resb = append(v_resb, resb)}
    else{v_resb = append(v_resb, "NA")}
    if(flag_astb == 1){v_astb = append(v_astb, astb)}
    else{v_astb = append(v_astb, "NA")}
    flag_resb = 0
    flag_astb = 0}}
  
  if(toxnet_gentox_results[i, 1] == "astb"){
    astb = toxnet_gentox_results[i,2]
    flag_astb = 1}
  
  if(toxnet_gentox_results[i, 1] == "resb"){
    resb = toxnet_gentox_results[i,2]
    flag_resb = 1}
}
v_astb = append(v_astb, astb)
v_resb = append(v_resb, resb)


SOURCE_NAME_SID <- unlist(SOURCE_NAME_SID)
v_asta <- unlist(v_asta)
v_resa <- unlist(v_resa)
v_astb <- unlist(v_astb)
v_resb <- unlist(v_resb)

# Now convert pre-1980 evals into a meaningful dataframe
toxnet_gentox_a <- cbind(SOURCE_NAME_SID, v_asta) %>% 
  cbind(v_resa) %>% 
  as.data.frame() %>%
  left_join(toxnet_gentox_IDs, by = "SOURCE_NAME_SID") %>% 
  filter(CASRN != "NOCAS") %>% 
  left_join(chemids, by = "CASRN") %>% 
  mutate(preferred_name = coalesce(preferred_name, NAME)) %>% 
  filter(v_asta != "Sperm morphology" & 
           v_asta != "In vivo carcinogenicity studies" &
           v_asta != "NA") %>% 
  rename(tngentoxa = v_resa) %>% 
  select(CASRN, DTXSID, preferred_name, tngentoxa)  %>% 
  unique() %>% 
  pivot_wider(names_from = tngentoxa, values_from = tngentoxa) %>% 
  unite("tngentoxa", Positive:`"Positive without activation, negative with activation"`, sep = ";", na.rm = TRUE) %>% 
  mutate(tngentoxa = case_when(str_detect(tngentoxa, "Positive") ~ "positive",
                               str_detect(tngentoxa, "Negative") ~ "negative",
                              # all remaining entries are "No conclusion"
                              TRUE ~ "inconclusive")) %>% 
  select(-`NA`) %>% unique()

#make dataframe of post-1980 evals
toxnet_gentox_b <- cbind(SOURCE_NAME_SID, v_astb) %>% 
  cbind(v_resb) %>% 
  as.data.frame() %>%
  left_join(toxnet_gentox_IDs, by = "SOURCE_NAME_SID") %>% 
  filter(CASRN != "NOCAS") %>% 
  left_join(chemids, by = "CASRN") %>% 
  mutate(preferred_name = coalesce(preferred_name, NAME)) %>% 
  filter(v_astb != "Sperm morphology" & 
           v_astb != "In vivo carcinogenicity studies" &
           v_astb != "NA") %>% 
  rename(tngentoxb = v_resb) %>% 
  select(CASRN, DTXSID, preferred_name, tngentoxb)  %>% 
  unique() %>% 
  pivot_wider(names_from = tngentoxb, values_from = tngentoxb) %>% 
  unite("tngentoxb", `No conclusion`:`"Positive without activation, negative with activation"`, sep = ";", na.rm = TRUE) %>% 
  mutate(tngentoxb = case_when(str_detect(tngentoxb, "Positive") ~ "positive",
                               str_detect(tngentoxb, "Negative") ~ "negative",
                               # all remaining entries are "No conclusion"
                               TRUE ~ "inconclusive")) %>% 
  unique() 

# complete toxnet genotoxicity results
toxnet_gentox <- full_join(toxnet_gentox_a, toxnet_gentox_b, by = "CASRN") %>% 
  mutate(tngentox = case_when(tngentoxa == "positive" | tngentoxb == "positive" ~ "positive", 
                              tngentoxa == "negative" | tngentoxb == "negative" ~ "negative",
                              TRUE ~ coalesce(tngentoxa, tngentoxb)),
         DTXSID = coalesce(DTXSID.x, DTXSID.y),
         preferred_name = coalesce(preferred_name.x, preferred_name.y)) %>% 
  select(CASRN, DTXSID, preferred_name, tngentox)



##### Combine data from input sources #####

gentox_ccris_ecvam_ntp_echem_toxnet <- full_join(ccrisgentox, ECVAMoverall, by = "CASRN") %>% 
  full_join(NTP_gentox, by = "CASRN") %>% 
  full_join(echemportal, by = "CASRN") %>% 
  full_join(toxnet_gentox, by = "CASRN") %>% 
  left_join(chemids, by = "CASRN") %>% 
  mutate(DTXSID = coalesce(DTXSID.x, DTXSID.y, DTXSID.x.x, DTXSID.y.y),
         preferred_name = coalesce(preferred_name.x, preferred_name.y, preferred_name.x.x, preferred_name.y.y)) %>% 
  unite("Genotoxicity", c(ECVAMgentox, cgentox, ntpgentox, echemgentox, tngentox), sep = ";", na.rm = TRUE) %>% 
  mutate(Genotoxicity = case_when(grepl('positive', Genotoxicity, fixed = TRUE) ~ "positive",
                                  grepl('negative', Genotoxicity, fixed = TRUE) ~ "negative",
                                  # all remaining entries are inconclusive
                                  TRUE ~ Genotoxicity)) %>% 
  select(CASRN, DTXSID, preferred_name, Genotoxicity) %>% 
  unique()


write_csv(gentox_ccris_ecvam_ntp_echem_toxnet, "outputs/gentox_ccris_ecvam_ntp_echem_toxnet.csv")

