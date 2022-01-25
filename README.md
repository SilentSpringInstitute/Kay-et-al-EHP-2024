# Kay2022
## Breast cancer-relevant chemicals paper

### Source code 

R version 4.1.0 (2021-05-18)

### Libraries
- tidyverse
- readxl
- stringr

### Input files
Input files should be put in a folder called "inputs" in the same directory as the scripts. Also create a folder in the same directory called "outputs" 

- DSSTox_Identifiers_and_CASRN_2021r1.csv: DSSTox DTXSID identifiers mapped to CAS Numbers and Names, downloaded from EPA CompTox Chemicals Dashboard: https://comptox.epa.gov/dashboard/downloads (last update Oct 18, 2021)
- IARCmonoMCchems.xlsx: chemicals with studies showing induction of mammary tumors recorded in IARC monographs through Vol. 128. Data compiled by authors by searching monograph pdfs and previous records published in Rudel 2007 https://pubmed.ncbi.nlm.nih.gov/17503434/ (data file available in github)
- ROC15_ChemswithMC.xlsx: chemicals with studies showing induction of mammary tumors recorded in the 15th Report on Carcinogens. Data compiled manually by authors by searching pdfs (data file available in github)
- NTPCEBS_2022-01-21-site_data.tsv: Results of NTP cancer bioassays downloaded from https://cebs.niehs.nih.gov/organsites/ (data file available in github)
- EPA_IRIS_MCs.xlsx: EPA IRIS list of chemicals that induce mammary tumors in rodents downloaded from https://iris.epa.gov/AdvancedSearch/ after searching "mammary"
- EPA_OPP_MCs.xlsx: Pesticides flagged for having induced mammary tumors in cancer bioassays from Cardona and Rudel 2020 (data compiled by authors using paper results https://pubmed.ncbi.nlm.nih.gov/32645345/) (data file available in github)
- ccris.xlsx: Chemical Carcinogenesis Research Information System archive last updated 2018, downloaded from https://www.nlm.nih.gov/databases/download/ccris.html
- LCDB_Mammary_Carcinogens.xlsx: chemicals that induce mammary tumors recorded in the Lhasa Carcinogenicity Database, recorded manually from searching https://carcdb.lhasalimited.org/ (data file available in github)
- ECVAM_Ames_positives_DB_row1removed.xls: Genotoxicity database from EURL ECVAM, downloaded from http://data.europa.eu/89h/jrc-eurl-ecvam-genotoxicity-carcinogenicity-ames and edited to remove the first row so it can be read into R correctly
- ECVAM_AmesNegDB_row1removed.xls: Additional genotoxicity database from EURL ECVAM, downloaded from supplemental information in Madia et al 2020 https://pubmed.ncbi.nlm.nih.gov/32660827/ and edited to remove the first row so it can be read into R correctly
- NTP_Gentox_andother_Findings_Data_2020-03-05.xlsx: NTP genotoxicity results downloaded from https://doi.org/10.22427/NTP-DATA-022-00002-0002-000-8 available at https://cebs.niehs.nih.gov/datasets/search/trf
- eChemPortalAPI_GeneticToxicityVivo_FinalRecords.xlsx: OECD eChemPortal genotoxicity database provided by Richard Judson at https://gaftp.epa.gov/Comptox/Staff/rjudson/datasets/genetox/
- NLM_TOXNET_GENETOX_Substance.xlsx: Substances in NLM TOXNET's GENE-TOX database substances, downloaded from https://www.ncbi.nlm.nih.gov/pcsubstance?term=%22Genetic%20Toxicology%20Data%20Bank%20(GENE-TOX)%22%5BSourceName%5D%20AND%20hasnohold%5Bfilt%5D
- NLM_TOXNET_GENTOX_results.txt: Genotoxicity results in NLM TOXNET's GENE-TOX database, downloaded from https://www.ncbi.nlm.nih.gov/pcsubstance?term=%22Genetic%20Toxicology%20Data%20Bank%20(GENE-TOX)%22%5BSourceName%5D%20AND%20hasnohold%5Bfilt%5D
- Karmaus_toxsci-15-0570-File009.csv: single-dose H295R results from Karmaus et al 2016, downloaded from https://doi.org/10.1093/toxsci/kfw002
- H295R_CR_Cardona_EPsummary.xlsx: Concentration-response H295R results as reported in Cardona and Rudel 2021 (https://pubmed.ncbi.nlm.nih.gov/34287026/) compiled by the authors (data file available in github)
- Judson_toxsci-15-0258-File002.xlsx: Results from computational integration of in vitro assays for ER activity reported in Judson et al 2015, downloaded from supplemental information https://doi.org/10.1093/toxsci/kfv168
- CPDat_BCrel_output.csv: EPA CPDat exposure data for BC-relevant chemicals. EPA data from Isaacs 2020 downloaded from https://doi.org/10.1038/s41370-019-0187-5 and exposure mapping executed with Alex Borrel's python script generateBoardExposure.py (Code available at https://github.com/SilentSpringInstitute/CPDatSSI, requires BC-relevant chemicals list created in script 3)
- HPVlist.xls: EPA's 2017 high production volume chemicals downloaded March 25, 2021 from CompTox dashboard https://comptox.epa.gov/dashboard/chemical-lists
- Ring_SupTable-all.chem.preds-2018-11-28.txt: ExpoCast modeled exposure data in Ring et al 2019 supplemental table 1 in https://doi.org/10.1021/acs.est.8b04056
- DrugProducts.txt: list of drugs in Drugs@FDA database downloaded from https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files
- DrugMarketingStatu.txt: marketing status of drug products downloaded from Drugs@FDA https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files
- DrugMarketingStatus_Lookup.txt: lookup key for marketing status downloaded from Drugs@FDA https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files
- FoodSubstances_clean.csv: FDA list of Substances Added to Food, downloaded from http://www.cfsanappsexternal.fda.gov/scripts/fdcc/?set=FoodSubstances, with top 4 rows and everything but CASRNs and chemical names removed to read into R correctly
- EPA_conventionalpesticides.csv: EPA list of active ingredients in conventional pesticides, downloaded from https://ordspub.epa.gov/ords/pesticides/f?p=CHEMICALSEARCH:1
- EPA_antimicrobials.csv: EPA list of active ingredients in antimicrobial pesticides, downloaded from https://ordspub.epa.gov/ords/pesticides/f?p=CHEMICALSEARCH:1
- EPA_biopesticides.csv: EPA list of active ingredients in biopesticides, downloaded from https://ordspub.epa.gov/ords/pesticides/f?p=CHEMICALSEARCH:1
- p65chemicalslist.xlsx: California's Proposition 65 list of chemicals as of Dec 31, 2021 downloaded from https://oehha.ca.gov/proposition-65/proposition-65-list
- MGDevlist_chemsonly.xlsx: List of chemicals that affect mammary gland development published in Rudel 2011 https://doi.org/10.1289/ehp.1002864, data compiled by the authors and available in github folder

### Scripts
Scripts should be run in order:
- 1_MCList_refs.R: compile list of rodent MCs
- 2_gentox_chems.R: compile chemical genotoxicity databases
- 3_BCrelevant_chem_effects.R: compile results for E2/P4 synthesis and ER agonism assays, construct list of BC-relevant chemicals 
- 4_BCrelevant_exposure_sources.R: compile exposure sources for BC-relevant chemicals
- 5_BCRel-MGdev_comparison.R: compare lists of BC-relevant chemicals and mammary gland developmental toxicants and their genotoxic/endocrine-disrupting properties
