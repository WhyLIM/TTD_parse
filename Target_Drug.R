 # @Author: Min Li
 # @Email: mli.bio@outlook.com
 # @Last Modified by: Min Li
 # @Timestamp for Last Modification: 2022-02-14 21:49:03
 # @Description: This R file is used to obtain all the mapping relationship of all drugs and targets information 
 # in Therapeutic Target Database (TTD) from files downloaded in TTD and UniprotKB.
 # Downloaded Files: From TTD: (http://db.idrblab.net/web/full-data-download), Version 8.1.01 (2021.11.08)
 #                   P1-01-TTD_target_download.txt
 #                   P1-07-Drug-TargetMapping.xlsx
 #                   P1-09-Target_compound_activity.txt
 #                   P2-01-TTD_uniprot_all.txt
 #                   P3-07-Approved_smi_inchi.txt
 #
 #                   From UniprotKB: (https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/), (2021-11-17)
 #                   HUMAN_9606_idmapping_selected.tab
 # 
 #                   (Optional) For further research: (https://ftp.ncbi.nlm.nih.gov/pubchem/Compound/CURRENT-Full/XML/)
 #                   These XML files could be used to map information of CIDs offered by "P1-09-Target_compound_activity.txt".
 #                   These files are too large (each >= 1.0G) and my PC is not up to the task.
 #
 # Before using this file, you need to comment out the previous description information line in the txt files with "#". 
 # And due to the line break problem and unreadable nature of the original files, txt&xlsx files needs to be converted to csv files first. 

library(tidyverse)
library(clusterProfiler)
library(org.Hs.eg.db)
library(do)
setwd("D:/Study/Project/Graduation/1-2_deal_TTD")

rm(list = ls())

# The drug-target mapping relationship is obtained, and targets without corresponding drugs are excluded.
target_all <- read.delim("P1-01-TTD_target_download.csv", header = F, row.names = NULL, sep = ",", 
                         check.names = F, stringsAsFactors = F, comment.char = "#", na.strings = c("NA", ""))
# For each target, not all the information is available, so for convenience, I extract lines I need.
# Note: Some targets don't have genenames...set to NA
targetid_row <- which(target_all[, 2] == "TARGETID")
genename_row <- which(target_all[, 2] == "GENENAME")
druginfo_row <- which(target_all[, 2] == "DRUGINFO")

target_all_filter <- target_all[c(targetid_row, genename_row, druginfo_row), ]
target_all_filter_order <- target_all_filter[order(as.numeric(row.names(target_all_filter))), ]

# Number of lines of information for each target.
ntgid <- c()
targetid_filter_row <- which(target_all_filter_order[, 2] == "TARGETID")
for (i in 2:length(targetid_filter_row)) {
  ntgid <- append(ntgid, targetid_filter_row[i] - targetid_filter_row[i-1])
}
ntgid_last <- nrow(target_all_filter_order) - targetid_filter_row[length(targetid_filter_row)] + 1
ntgid <- append(ntgid, ntgid_last)

target_locate <- 0
targetid_genename <- c()
drugid <- c()
drugname <- c()
for (n in ntgid) {
  target_locate = target_locate + 1
  head_temp <- targetid_filter_row[target_locate]
  tail_temp <- head_temp + n - 1
  data_temp <- target_all_filter_order[head_temp:tail_temp, ]
  if (n < 2 || (n == 2 && data_temp[2, 2] == "GENENAME")) {
    next
  }
  else if (n >= 2 && data_temp[2, 2] != "GENENAME") {
    data_temp <- rbind(data_temp[1, ], c(data_temp[1, 1], "GENENAME", NA, NA, NA), data_temp[2:n, ])
    n <- n + 1
  }
  targetid_genename <- append(targetid_genename, rep(paste(data_temp[1, 3], data_temp[2, 3], sep = ",,"), n-2))
  for (j in 3:n) {
    drugid <- append(drugid, data_temp[j, 3])
    drugname <- append(drugname, data_temp[j, 4])
  }
}
drug <- paste(drugid, drugname, sep = ",,")

target_drug_str <- paste(targetid_genename, drug, sep = ",,")
target_drug <- as.data.frame(target_drug_str, StringsAsFactors = F) %>% 
  tidyr::separate(col = target_drug_str, into = c("TargetID", "GeneName", "DrugID", "DrugName"), sep = ",,")
# paste operation above makes NA a str, reassign to NA.
target_drug[which(target_drug$GeneName == "NA"), 2] <- NA

# Supplements of target to drug mapping with mode of action from the literature.
ref_target <- read.csv("P1-07-Drug-TargetMapping.csv", header = T, stringsAsFactors = F)[, 1:2]
ref_target <- merge(ref_target, dplyr::distinct(target_drug[, 1:2]), by = "TargetID", all.x = T) %>% 
  merge(dplyr::distinct(target_drug[, 3:4]), by = "DrugID", all.x = T) %>% 
  dplyr::select("TargetID", "GeneName", "DrugID", "DrugName")

# Supplements of target to compound mapping with activity data.
compound <- read.csv("P1-09-Target_compound_activity.csv", header = T, stringsAsFactors = F)[, 1:2]
names(compound) <- c("TargetID", "DrugID")
compound <- merge(compound, dplyr::distinct(target_drug[, 1:2]), by = "TargetID", all.x = T) %>% 
  merge(dplyr::distinct(target_drug[, 3:4]), by = "DrugID", all.x = T) %>% 
  dplyr::select("TargetID", "GeneName", "DrugID", "DrugName")

# Mapping relationship between target and UniProtKB-ID.
uniid <- read.delim("P2-01-TTD_uniprot_all.csv", header = F, row.names = NULL, sep = ",", 
                    check.names = F, stringsAsFactors = F, comment.char = "#", na.strings = c("NA", ""))
uni_targetid_row <- which(uniid[, 2] == "TARGETID")
uni_uniproid_row <- which(uniid[, 2] == "UNIPROID")
# Since targets in this file must have a corresponding id, no other processing is required.
# Note: UniprotIDs extracted from TTD may have forms like "CUL4A_HUMAN/CUL4B_HUMAN-DDB1_HUMAN-CRBN_HUMAN", handle after merge.
target_uni <- data.frame(TargetID = uniid[uni_targetid_row, 3], UniprotID = uniid[uni_uniproid_row, 3]) %>% 
  tidyr::separate_rows(UniprotID, sep = "; ") %>% 
  tidyr::separate_rows(UniprotID, sep = "/") %>% 
  tidyr::separate_rows(UniprotID, sep = "-")

# test <- data.frame(UniprotID = "CUL4A_HUMAN/CUL4B_HUMAN-DDB1_HUMAN-CRBN_HUMAN", 
#                    GeneName = "CUL4A/CUL4B-DDB1-CRBN")
# test <- data.frame(UniprotID = "CUL4A_HUMAN", 
#                    GeneName = "CUL4A/CUL4B-DDB1-CRBN")
# test1 <- tidyr::separate_rows(test, UniprotID, sep = "/")
# tidyr::separate_rows(test1, UniprotID, sep = "-")
# test %>% tidyr::separate_rows(UniprotID, sep = "/") %>% 
#   tidyr::separate_rows(UniprotID, sep = "-")
# test %>% tidyr::separate_rows(c("UniprotID", "GeneName"), sep = "/") %>% 
#   tidyr::separate_rows(c("UniprotID", "GeneName"), sep = "-")
# test %>% tidyr::separate_rows(UniprotID, sep = "/") %>% 
#   tidyr::separate_rows(UniprotID, sep = "-") %>% 
#   tidyr::separate_rows(GeneName, sep = "/") %>% 
#   tidyr::separate_rows(GeneName, sep = "-")


# All column names
# unimap_herder <- c("UniProtKB-AC", "UniProtKB-ID", "GeneID (EntrezGene)", "RefSeq", "GI", "PDB", 
#                    "GO", "UniRef100", "UniRef90", "UniRef50", "UniParc", "PIR", "NCBI-taxon", 
#                    "MIM", "UniGene", "PubMed", "EMBL", "EMBL-CDS", "Ensembl", "Ensembl_TRS", 
#                    "Ensembl_PRO", "Additional PubMed")
unimap <- read.delim("HUMAN_9606_idmapping_selected.tab", sep = "\t", header = F)[, 1:2]
names(unimap) <- c("UniprotAC", "UniprotID")

TTD_Target <- rbind(target_drug, ref_target, compound)
TTD_Target <- TTD_Target[-which(duplicated(TTD_Target)), ] %>% 
  merge(target_uni, by = "TargetID", all.x = T)

# Some GeneNames have string "-", causing error when running "tidyr::separate_rows(..., c("UniprotID", "GeneName"), sep = "-")"
multigene <- str_count(TTD_Target$GeneName, "-")
multiuniid <- str_count(TTD_Target$UniprotID, "-")
multi <- which((multigene != multiuniid) & (multigene > 0) & (multiuniid > 0))
special_treat <- TTD_Target[multi, ]

# GeneName(s) with "-" inside, which need manual search.
pattern <- c("MT-CYB:MTCYB")
pattern_restore <- c("MTCYB:MT-CYB")
special_treat$GeneName <- do::Replace(data = special_treat$GeneName, pattern = pattern)
special_treat <- tidyr::separate_rows(special_treat, c("UniprotID", "GeneName"), sep = "-")
special_treat$GeneName <- do::Replace(data = special_treat$GeneName, pattern = pattern_restore)

# Handle very special UniproIDs, such as "POLG_DEN1W (281-775)"
TTD_Target$UniprotID <- gsub(" \\(.*?\\)", "", TTD_Target$UniprotID)
TTD_Target <- TTD_Target[-multi, ] %>%
  tidyr::separate_rows(DrugName, sep = "; ") %>% 
  tidyr::separate_rows(c("UniprotID", "GeneName"), sep = "; ") %>% 
  tidyr::separate_rows(c("UniprotID", "GeneName"), sep = "/") %>% 
  tidyr::separate_rows(c("UniprotID", "GeneName"), sep = "-") %>% 
  rbind(special_treat)

# Individual delimiters are not standardized, missing a space.
TTD_Target$GeneName <- gsub(";", "", TTD_Target$GeneName)
TTD_Target <- merge(TTD_Target, unimap, by = "UniprotID", all.x = T)

mapfail <- which(is.na(TTD_Target$GeneName) & !is.na(TTD_Target$UniprotAC))
uniid_convert <- TTD_Target$UniprotAC[mapfail]
convert <- bitr(uniid_convert, 
                fromType = "UNIPROT", 
                toType = "SYMBOL", 
                OrgDb = "org.Hs.eg.db")
names(convert) <- c("UniprotAC", "GeneName")

# Reverse Operation of tidyr::separate_rows().
dprow <- which(duplicated(convert$UniprotAC))
dprow_all <- which(duplicated(convert$UniprotAC) | duplicated(convert$UniprotAC, fromLast = T))
dprow_first <- which((duplicated(convert$UniprotAC) | duplicated(convert$UniprotAC, fromLast = T)) & 
                     duplicated(convert$UniprotAC, fromLast = T))

paste_list <- list()
paste_str <- c()
for (fr in dprow_first) {
  for (ar in dprow_all) {
    if (ar == fr) {
      paste_list <- append(paste_list, paste_str)
      paste_str <- convert$GeneName[ar]
    } else {
      paste_str <- paste(paste_str, convert$GeneName[ar], sep = "; ")
      if (ar == dprow_all[length(dprow_all)]) {
        paste_list <- append(paste_list, paste_str)
      }
    }
  }
}
convert$GeneName[dprow_first] <- paste_list
convert <- convert[-dprow, ]

for (uni in mapfail) {
  mfrow <- which(convert$UniprotAC == TTD_Target$UniprotAC[uni])
  if (length(mfrow) != 0) {
    TTD_Target$GeneName[uni] <- convert$GeneName[mfrow]
  }
}
TTD_Target <- tidyr::separate_rows(TTD_Target, GeneName, sep = "; ")

# Obtain Drug InChI.
# Why InChI?: The expression methods of SMILES in DrugBank and TTD are inconsistent.
# PS: Wrong label in this file offered by TTD: reverse order between "CASMILE" and "DRUINCHI".
inchi <- read.delim("P3-07-Approved_smi_inchi.csv", header = F, row.names = NULL, sep = ",", 
                    check.names = F, stringsAsFactors = F, comment.char = "#", na.strings = c("NA", ""))
inchi_ttddruid_row <- which(inchi[, 2] == "TTDDRUID")
inchi_druginch_row <- which(inchi[, 2] == "CASMILES")
# Since drugs in this file must have one corresponding InChI, no other processing is required.
drug_inchi <- data.frame(DrugID = inchi[inchi_ttddruid_row, 3], InChI = inchi[inchi_druginch_row, 3])
TTD_Target <- merge(TTD_Target, drug_inchi, by = "DrugID", all.x = T) %>% 
  dplyr::select(c("TargetID", "UniprotID", "UniprotAC", "GeneName", "DrugID", "DrugName", "InChI"))
TTD_Target$Source <- "TTD"

write.csv(TTD_Target, file = "TTD_TD.csv", row.names = F)

