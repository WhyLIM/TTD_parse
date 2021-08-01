 # @Author: Min Li
 # @Email: mli.bio@outlook.com
 # @Last Modified by: Min Li
 # @Timestamp for Last Modification: 2021-07-27 00:18:43
 # @Description: This R code file is used to obtain the corresponding relationship 
 #  between the target and the drug from the downloaded file P1-01-TTD_target_download.txt.

library(tidyverse)
setwd("D:/Study/Project/Graduation/deal_TTD")

rm(list = ls())
# You need to comment out the previous description information line in the txt file with "#"
# This P1-01-TTD_target_download.txt file needs to be converted to csv file first because of the line break problem
target_all <- read.delim("P1-01-TTD_target_download.csv", header = F, row.names = NULL, sep = ",", 
                         check.names = F, stringsAsFactors = F, comment.char = "#", na.strings = c("NA", ""))

# For each target, not all the information is available, so for convenience, 
# I extract the TARGETID, GENENAME, and DRUGINFO lines that I need
targetid_row <- which(target_all[, 2] == "TARGETID")
genename_row <- which(target_all[, 2] == "GENENAME")
druginfo_row <- which(target_all[, 2] == "DRUGINFO")

target_all_filter <- target_all[c(targetid_row, genename_row, druginfo_row), ]
target_all_filter_order <- target_all_filter[order(as.numeric(row.names(target_all_filter))), ]

ntgid <- c()
targetid_filter_row <- which(target_all_filter_order[, 2] == "TARGETID")
genename_filter_row <- which(target_all_filter_order[, 2] == "GENENAME")
druginfo_filter_row <- which(target_all_filter_order[, 2] == "DRUGINFO")
for (i in 2:length(targetid_filter_row)) {
  ntgid <- append(ntgid, targetid_filter_row[i] - targetid_filter_row[i - 1])
}
ntgid_last <- nrow(target_all_filter_order) - targetid_filter_row[length(targetid_filter_row)] + 1
ntgid <- append(ntgid, ntgid_last)

target_locate <- 0
targetid_genename <- c()
drugid <- c()
drugname <- c()
for (n in ntgid) {
  if (n > 2) {
    target_locate = target_locate + 1
    targetid_genename <- append(targetid_genename, 
                                rep(paste(target_all_filter_order[targetid_filter_row[target_locate], 3], 
                                          target_all_filter_order[targetid_filter_row[target_locate]+1, 3], 
                                          sep = ",,"), n-2))
    
    for (j in 3:n) {
      drugid <- append(drugid, target_all_filter_order[targetid_filter_row[target_locate]+j-1, 3])
      drugname <- append(drugname, target_all_filter_order[targetid_filter_row[target_locate]+j-1, 4])
    }
  }
  else {
    target_locate = target_locate + 1
  }
}
drug <- paste(drugid, drugname, sep = ",,")

target_drug_str <- paste(targetid_genename, drug, sep = ",,")
target_drug <- as.data.frame(target_drug_str, StringsAsFactors = FALSE)

target_drug <- separate(data = target_drug, col = target_drug_str, 
                        into = c("TargetID", "GeneName", "DrugID", "DrugName"), sep = ",,")
write.csv(target_drug, file = "Target_Drug.csv", row.names = F)

