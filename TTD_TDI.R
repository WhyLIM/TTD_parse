 # @Author: Min Li
 # @Email: mli.bio@outlook.com
 # @Last Modified by: Min Li
 # @Timestamp for Last Modification: 2022-02-13 23:06:27
 # @Description: This R file is used to process the mapping relationship of Targets_Indications 
 # and Drugs_Indications, and finally obtain target_drug_indication form of TTD.
 # Downloaded File(s): From TTD: (http://db.idrblab.net/web/full-data-download), Version 8.1.01 (2021.11.08)
 #                     P1-05-Drug_disease.txt
 #                    *P1-06-Target_disease.txt
 #                    *: Both files were handled the same way, ultimately I didn't use P1-06.
 #
 # Before using this file, you need to comment out the previous description information line in the txt files with "#". 
 # And due to the line break problem and unreadable nature of the original files, the txt files needs to be converted to csv files first. 

library(tidyverse)
setwd("D:/Study/Project/Graduation/1-2_deal_TTD")

rm(list = ls())

drug_indi <- read.delim("P1-05-Drug_disease.csv", header = F, row.names = NULL, 
                       sep = ",", check.names = F, stringsAsFactors = F, 
                       comment.char = "#", na.strings = c("NA", ""))[, -3:-5]
# Note: Each drug does not necessarily have a corresponding name, but there must be corresponding indication(s).
dgindi_ttddruid_row <- which(drug_indi[, 1] == "TTDDRUID")
dgindi_indicati_row <- which(drug_indi[, 1] == "INDICATI")

drug_indi_filter <- drug_indi[c(dgindi_ttddruid_row, dgindi_indicati_row), ] %>% .[order(as.numeric(row.names(.))), ]

ndgid <- c()
dgindi_filter_row <- which(drug_indi_filter[, 1] == "TTDDRUID")
for (i in 2:length(dgindi_filter_row)) {
  ndgid <- append(ndgid, dgindi_filter_row[i] - dgindi_filter_row[i-1])
}
ndgid_last <- nrow(drug_indi_filter) - dgindi_filter_row[length(dgindi_filter_row)] + 1
ndgid <- append(ndgid, ndgid_last)

drugindi_list <- c()
drug_locate <- 0
for (nd in ndgid) {
  drug_locate <- drug_locate + 1
  head_temp <- dgindi_filter_row[drug_locate]
  tail_temp <- head_temp + nd - 1
  indicati_each <- paste(drug_indi_filter[(head_temp+1):tail_temp, 2], collapse = "; ")
  drugindi_list <- append(drugindi_list, indicati_each)
}
Drug_Indi <- data.frame(DrugID = drug_indi_filter[dgindi_filter_row, 2], Indication = drugindi_list) %>% 
  tidyr::separate_rows(Indication, sep = "; ")

write.csv(Drug_Indi, "Drug_Indication.csv", row.names = F)


target_drug <- read.delim("TTD_TD.csv", header = T, sep = ",", stringsAsFactors = F, na.strings = "NA")
TTD_TDI <- target_drug %>% 
  merge(Drug_Indi, by = "DrugID", all.x = T) %>% 
  dplyr::select(c(2, 3, 4, 5, 1, 6, 7, 9, 8))

write.csv(TTD_TDI, "TTD_TDI.csv", row.names = F)

# # Target_Indication can also be treated in the same way.
# target_indi <- read.delim("P1-06-Target_disease.csv", header = F, row.names = NULL, sep = ",", 
#                          check.names = F, stringsAsFactors = F, comment.char = "#", na.strings = c("NA", ""))
# # Note: Each target does not necessarily have a corresponding name, but there must be corresponding indication(s).
# tgindi_targetid_row <- which(target_indi[, 2] == "TARGETID")
# tgindi_indicati_row <- which(target_indi[, 2] == "INDICATI")

# target_indi_filter <- target_indi[c(tgindi_targetid_row, tgindi_indicati_row), ] %>% 
#   .[order(as.numeric(row.names(.))), ] %>% tidyr::unite("V3", c(4, 3), sep = " ", remove = T)
# target_indi_filter$V3 <- gsub("NA ", "", target_indi_filter$V3)

# ntgid <- c()
# tgindi_filter_row <- which(target_indi_filter[, 2] == "TARGETID")
# for (i in 2:length(tgindi_filter_row)) {
#   ntgid <- append(ntgid, tgindi_filter_row[i] - tgindi_filter_row[i-1])
# }
# ntgid_last <- nrow(target_indi_filter) - tgindi_filter_row[length(tgindi_filter_row)] + 1
# ntgid <- append(ntgid, ntgid_last)

# targindi_list <- c()
# target_locate <- 0
# for (nt in ntgid) {
#   target_locate <- target_locate + 1
#   head_temp <- tgindi_filter_row[target_locate]
#   tail_temp <- head_temp + nt - 1
#   indicati_each <- paste(target_indi_filter[(head_temp+1):tail_temp, 3], collapse = "; ")
#   targindi_list <- append(targindi_list, indicati_each)
# }
# Target_Indi <- data.frame(targetID = target_indi_filter[tgindi_filter_row, 3], Indication = targindi_list) %>% 
#   tidyr::separate_rows(Indication, sep = "; ")

# write.csv(Target_Indi, "Target_Indication.csv", row.names = F)


