# A new list of potential breast carcinogens identified using mechanistic screening and traditional animal testing
## Authors
Jennifer E. Kay1, Julia G. Brody1, Megan Schwarzman2,3,4, Ruthann A. Rudel1

1Silent Spring Institute, Newton, MA; 
2University of California, Berkeley, School of Public Health;
3University of California, San Francisco, Family and Community Medicine;
4Berkeley Center for Green Chemistry Center for Occupational and Environmental Health

Corresponding author: Ruthann Rudel, Silent Spring Institute, Newton, MA 02460, USA. rudel@silentspring.org


### Programming language 

- R version 4.1.0 (2021-05-18)

### Libraries
- tidyverse
- readxl
- stringr
- ggplot2
- ggmosaic
- openxlsx

### Input files
Input files should be put in a folder called "./inputs/" in the same directory as the scripts. 


<b>Input file to download</b>
- DSSTox_Identifiers_and_CASRN_2021r1.csv: DSSTox DTXSID identifiers mapped to CAS Numbers and Names, downloaded from EPA CompTox Chemicals Dashboard: https://clowder.edap-cluster.com/files/616dd943e4b0a5ca8aeea69d?dataset=61147fefe4b0856fdc65639b&space=6112f2bee4b01a90a3fa7689&folder=638a569de4b04f6bb1489bb2

<b>Description of files included in the "inputs" folder (in order of appearance in code)</b>
- IARCmonoMCchems.xlsx: chemicals with studies showing induction of mammary tumors recorded in IARC monographs through Vol. 131. Data compiled by authors by searching monograph pdfs and previous records published in Rudel et al 2007 https://pubmed.ncbi.nlm.nih.gov/17503434/ 
- ROC15_ChemswithMC.xlsx: chemicals with studies showing induction of mammary tumors recorded in the 15th Report on Carcinogens. 15th ROC pdfs available at: https://ntp.niehs.nih.gov/whatwestudy/assessments/cancer/roc/index.html Data compiled manually by authors by searching pdfs 
- NTPCEBS_2022-07-07-site_data.tsv: Results of NTP cancer bioassays downloaded from https://cebs.niehs.nih.gov/organsites/ 
- EPA_IRIS_MCs.xlsx: EPA IRIS list of chemicals that induce mammary tumors in rodents downloaded from https://iris.epa.gov/AdvancedSearch/ after searching "mammary" 
- EPA_OPP_MCs.xlsx: Pesticides flagged for having induced mammary tumors in cancer bioassays from Cardona and Rudel 2020 (data compiled by authors using paper results https://pubmed.ncbi.nlm.nih.gov/32645345/) 
- ToxRef_MCs.csv: EPA Toxicity Reference Database v2.0, chemicals tested a mouse or rat carcinogenicity assay, with treatment-related mammary tumors in positive direction. Downloaded most recent version (dated Apr 21, 2020) Full database available at: https://gaftp.epa.gov/comptox/High_Throughput_Screening_Data/Animal_Tox_Data/current
- toxval_cancer_details_with_references_dev_toxval_v9_2022-04-20.xlsx: EPA Toxicity Value DB (ToxValDB), file provided by Richard Judson by email Apr 20, 2022
- ccris.xlsx: Chemical Carcinogenesis Research Information System archive last updated 2018, downloaded from https://www.nlm.nih.gov/databases/download/ccris.html 
- LCDB_Mammary_Carcinogens.xlsx: chemicals that induce mammary tumors recorded in the Lhasa Carcinogenicity Database, https://carcdb.lhasalimited.org/
- Rudel2007.xlsx: original rodent mammary carcinogens list from Rudel et al, Cancer, 2007, DOI: 10.1002/cncr.22653
- ECVAM_Ames_positives_DB_row1removed.xls: Genotoxicity database from EURL ECVAM, downloaded from http://data.europa.eu/89h/jrc-eurl-ecvam-genotoxicity-carcinogenicity-ames and edited to remove the first row so it can be read into R correctly (original and row-1-removed data files available)
- ECVAM_AmesNegDB_row1removed.xls: Additional genotoxicity database from EURL ECVAM, downloaded from supplemental information in Madia et al 2020 https://pubmed.ncbi.nlm.nih.gov/32660827/ and edited to remove the first row so it can be read into R correctly (original and row-1-removed data files available)
- 20220927-NTPgentox.csv: NTP genotoxicity results downloaded from https://doi.org/10.22427/NTP-DATA-022-00002-0002-000-8 available at https://cebs.niehs.nih.gov/datasets/search/trf
- eChemPortalAPI_GeneticToxicityVivo_FinalRecords.xlsx: OECD eChemPortal in vivo genotoxicity database provided by Richard Judson at https://gaftp.epa.gov/Comptox/Staff/rjudson/datasets/genetox/ 
- eChemPortalAPI_GeneticToxicityVitro_FinalRecords.xlsx: OECD eChemPortal in vitro genotoxicity database provided by Richard Judson at https://gaftp.epa.gov/Comptox/Staff/rjudson/datasets/genetox/ 
- NLM_TOXNET_GENETOX_Substance.xlsx: Substances in National Library of Medicine (NLM) TOXNET's GENE-TOX database substances, downloaded from https://www.ncbi.nlm.nih.gov/pcsubstance?term=%22Genetic%20Toxicology%20Data%20Bank%20(GENE-TOX)%22%5BSourceName%5D%20AND%20hasnohold%5Bfilt%5D 
- NLM_TOXNET_GENTOX_results.txt: Genotoxicity results in NLM TOXNET's GENE-TOX database, downloaded from https://www.ncbi.nlm.nih.gov/pcsubstance?term=%22Genetic%20Toxicology%20Data%20Bank%20(GENE-TOX)%22%5BSourceName%5D%20AND%20hasnohold%5Bfilt%5D 
- NTP_Cancer_Bioassay_Chemicals.xlsx: List of chemicals tested in NTP bioassays, downloaded May 2 2023. Available by selecting list of "NTP Cancer Bioassay Chemicals" at https://ice.ntp.niehs.nih.gov/Search
- ToxRef_MG_tested04062023.csv: chemicals tested a mouse or rat carcinogenicity assay where mammary gland was assessed for gross or microscopic pathology. Downloaded most recent version (dated Apr 21, 2020) Full database available at: https://gaftp.epa.gov/comptox/High_Throughput_Screening_Data/Animal_Tox_Data/current
- Karmaus_toxsci-15-0570-File009.csv: single-dose H295R results from Karmaus et al 2016, downloaded from https://doi.org/10.1093/toxsci/kfw002 
- H295R_CR_Cardona_EPsummary.xlsx: Concentration-response H295R results as reported in Cardona and Rudel 2021 (https://pubmed.ncbi.nlm.nih.gov/34287026/) compiled by the authors 
- Judson_toxsci-15-0258-File002.xlsx: Results from computational integration of in vitro assays for ER activity reported in Judson et al 2015, downloaded from supplemental information https://doi.org/10.1093/toxsci/kfv168  
- MGDevlist_chemsonly.xlsx: List of chemicals that affect mammary gland development published in Rudel 2011 https://doi.org/10.1289/ehp.1002864, data compiled by the authors 

### Scripts
The folder "./outputs/" will be created in the first script, MCList_refs
Scripts should be run in order:
- 1_MCList_refs.R: compile list of rodent mammary carcinogens (MCs)
- 2_gentox_chems.R: compile chemical genotoxicity databases
- 3_BCrelevant_chem_effects.R: compile results of rodent cancer bioassays without mammary tumors (putative non-MCs), E2/P4 synthesis, and ER agonism assays; construct list of breast cancer-relevant chemicals with Key Characteristic (steroidogenic, ER agonistic, and genotoxic) activities; construct list with KCs of MCs and putative non-MCs
- 4_BCRel-MGdev_comparison.R: compare lists of BC-relevant chemicals and mammary gland developmental toxicants and their genotoxic/endocrine-disrupting properties
- 5_BCRC_tables_figures.R: create Excel file with supplemental tables created in R, and figures 2 and 3 for the paper  
 
