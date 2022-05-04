# TTD_parse

> Get information on drugs, targets and indications in Therapeutic Target Database (TTD) from downloaded files.
> Based on TTD v8.1.01 (2021.11.08) and UniprotKB release 2021_04 (2021-11-17).

## Script Description

- Target_Drug.R is used to obtain all the mapping relationship of all drugs and targets information in Therapeutic Target Database (TTD) from files downloaded in TTD and UniprotKB.
- TTD_TDI.R is used to process the mapping relationship of Targets_Indications and Drugs_Indications, and finally obtain target_drug_indication form of TTD.
- The required files can be obtained from the following URLs:
  - http://db.idrblab.net/web/full-data-download
  - https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/
  - (Optional) https://ftp.ncbi.nlm.nih.gov/pubchem/Compound/CURRENT-Full/XML/

## Tips: 
Due to the line break problem and unreadable nature of the original files, some pre-process must be done before running these scripts. Detailed introductions can be found in the comments at the beginning of these scripts.
