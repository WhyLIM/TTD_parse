 # @Author: Min Li
 # @Email: mli.bio@outlook.com
 # @Last Modified by: Min Li
 # @Timestamp for Last Modification: 2021-07-27 00:19:12
 # @Description: This R code file is used to obtain the final target_drug_indication form of TTD

library(tidyverse)
setwd("D:/Study/Project/Graduation/deal_TTD")

rm(list = ls())
target_drug <- read.delim("Target_Drug.csv", header = T, sep = ",", check.names = F, stringsAsFactors = F)
drug_ind <- read.delim("Drug_Indication.csv", header = T, sep = ",", check.names = F, stringsAsFactors = F)

drug_ind$TargetID <- NA
drug_ind$GeneName <- NA
drug_ind_cycle <- 0
for (drug in drug_ind$DrugID) {
  drug_ind_cycle <- drug_ind_cycle + 1
  rowid <- which(target_drug$DrugID == drug)
  if (length(rowid) != 0) {
    drug_ind$TargetID[drug_ind_cycle] <- target_drug$TargetID[rowid]
    drug_ind$GeneName[drug_ind_cycle] <- target_drug$GeneName[rowid]
  }
}

TTD_TDI <- select(drug_ind, c(4, 5, 1, 2, 3))
write.csv(TTD_TDI, file = "TTD_Target_Drug_Indication.csv", row.names = F)
