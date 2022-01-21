# Kay2022
## Breast cancer-relevant chemicals paper

### Source code 

R version 4.1.0 (2021-05-18)

### Libraries
tidyverse 

readxl 

stringr


**Input files**
- DSSTox_Identifiers_and_CASRN_2021r1.csv: DSSTox DTXSID identifiers mapped to CAS Numbers and Names, downloaded from EPA CompTox Chemicals Dashboard: https://comptox.epa.gov/dashboard/downloads (last update Oct 18, 2021)
- IARCmonoMCchems.xlsx: chemicals with studies showing induction of mammary tumors recorded in IARC monographs through Vol. 128. Data compiled by authors by searching monograph pdfs and previous records published in Rudel 2007 https://pubmed.ncbi.nlm.nih.gov/17503434/
- ROC_ChemswithMC.xlsx: chemicals with studies showing induction of mammary tumors recorded in the 14th Report on Carcinogens. Data compiled manually by authors by searching pdfs
- **Jenny, make NTP CEBS one input file** https://cebs.niehs.nih.gov/organsites/
- EPA_IRIS_MCs.xlsx: EPA IRIS list of chemicals that induce mammary tumors in rodents downloaded from https://iris.epa.gov/AdvancedSearch/ after searching "mammary"
- EPA_OPP_MCs.xlsx: Pesticides flagged for having induced mammary tumors in cancer bioassays from Cardona and Rudel 2020 (data compiled by authors using paper results https://pubmed.ncbi.nlm.nih.gov/32645345/)
- 
