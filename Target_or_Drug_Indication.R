 # @Author: Min Li
 # @Email: mli.bio@outlook.com
 # @Last Modified by: Min Li
 # @Timestamp for Last Modification: 2021-07-27 00:18:58
 # @Description: This R code file is used to process the mapping relationship 
 #  between targets and indications or drugs and indications.

library(tidyverse)
setwd("D:/Study/Project/Graduation/deal_TTD")

rm(list = ls())
# You need to comment out the previous description information line in the txt file with "#"
target_dis <- read.delim("P1-06-Target_disease.txt", header = F, row.names = NULL, sep = "\t", 
                         check.names = F, stringsAsFactors = F, comment.char = "#", na.strings=c("NA", ""))

indication_row <- which(target_dis[, 2] == "INDICATI")
target_dis[indication_row, 3] <- target_dis[indication_row, 4]
target_dis_nogroup <- target_dis[, -ncol(target_dis)]

# This step is actually not necessary...
target_dis_nona <- na.omit(target_dis_nogroup)


# # Drug_Indication can also be treated in the same way
# target_dis_nona <- read.delim("P1-05-Drug_disease.txt", header = F, row.names = NULL, sep = "\t",
#                          check.names = F, stringsAsFactors = F, comment.char = "#", na.strings=c("NA", ""))
# ntgid <- c()
# targetid_row <- which(target_dis_nona[, 2] == "TTDDRUID")


# For Target_Indication
ntgid <- c()
targetid_row <- which(target_dis_nona[, 2] == "TARGETID")

for (i in 2:length(targetid_row)) {
  ntgid <- append(ntgid, targetid_row[i] - targetid_row[i - 1])
}
ntgid_last <- nrow(target_dis_nona) - targetid_row[length(targetid_row)] + 1
ntgid <- append(ntgid, ntgid_last)

target_locate <- 0
targetid <- c()
indication <- c()
for (n in ntgid) {
  target_locate = target_locate + 1
  targetid <- append(targetid, rep(paste(target_dis_nona[targetid_row[target_locate], 3], 
                        target_dis_nona[targetid_row[target_locate]+1, 3], 
                        sep = ",,"), n-2))
  for (j in 3:n) {
    indication <- append(indication, target_dis_nona[targetid_row[target_locate]+j-1, 3])
  }
}

target_ind_str <- paste(targetid, indication, sep = ",,")
target_ind <- as.data.frame(target_ind_str, StringsAsFactors = FALSE)
# For Target_Indication
target_ind <- separate(data = target_ind, col = target_ind_str, 
                       into = c("TargetID", "TargetName", "Indication"), sep = ",,")
write.csv(target_ind, file = "Target_Indication.csv", row.names = F)


# # For Drug_Indication
# target_ind <- separate(data = target_ind, col = target_ind_str, 
#                        into = c("DrugID", "DrugName", "Indication"), sep = ",,")
# write.csv(target_ind, file = "Drug_Indication.csv", row.names = F)
