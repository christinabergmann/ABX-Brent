# Author: Christina Bergmann (chbergma 'at' gmail)
# Last modified: 06.07.2016
# Description: Read in ABX-generated csv files (shared with this script) and create all necessary columns
# Analyzing between-/within-speaker discriminability of sound contrasts in the Brent corpus (pre-selected portion based on forced alignments, see paper for details)

# load libraries
library(plyr)


#The acoustic encoding is MEL, change here to to other encodings when available, eg to MFCC (all lowercase)
acoustic = "mel"
#Which distance measure is used?
distance = "avg"



#To be sure that there are no unnecessary comparisons between consonants and vowels, I separated the two. Also faster.

withinfile_vowels = paste("Data/Brent_Clean_withinSpeaker_vowels_", acoustic, "_", distance,".csv", sep="")
betweenfile_vowels = paste("Data/Brent_Clean_acrossSpeaker_vowels_", acoustic, "_", distance,".csv", sep="")

withinfile_cons = paste("Data/Brent_Clean_withinSpeaker_cons_", acoustic, "_", distance,".csv", sep="")
betweenfile_cons = paste("Data/Brent_Clean_acrossSpeaker_cons_", acoustic, "_", distance,".csv", sep="")


read.table(betweenfile_vowels,sep="\t",header=T)->rawdata_between_temp
read.table(betweenfile_cons,sep="\t",header=T)->rawdata_between
rawdata_between = rbind(rawdata_between, rawdata_between_temp)


read.table(withinfile_vowels,sep="\t",header=T)->rawdata_within_temp
read.table(withinfile_cons,sep="\t",header=T)->rawdata_within
rawdata_within = rbind(rawdata_within, rawdata_within_temp)

remove(rawdata_within_temp, rawdata_between_temp)


rawdata_between$contrasts <- as.factor(ifelse((as.character(rawdata_between$phone_1)<as.character(rawdata_between$phone_2)), paste(rawdata_between$phone_1, rawdata_between$phone_2, sep = "-"), paste(rawdata_between$phone_2, rawdata_between$phone_1, sep = "-")))
rawdata_between$speakerpairs <- as.factor(ifelse((as.character(rawdata_between$talker_1)<as.character(rawdata_between$talker_2)), paste(rawdata_between$talker_1, rawdata_between$talker_2, sep = "-"), paste(rawdata_between$talker_2, rawdata_between$talker_1, sep = "-")))
names(rawdata_between)[names(rawdata_between)=="by"] <- "context"

rawdata_within$contrasts <- as.factor(ifelse((as.character(rawdata_within$phone_1)<as.character(rawdata_within$phone_2)), paste(rawdata_within$phone_1, rawdata_within$phone_2, sep = "-"), paste(rawdata_within$phone_2, rawdata_within$phone_1, sep = "-")))
rawdata_within$speaker <- as.factor(gsub(")", "", (gsub("^.*?, ","", (gsub("^.*?, ","", as.character(rawdata_within$by)))))))
rawdata_within$context <- as.factor(gsub("[(), ]", "", (gsub("\\d","", as.character(rawdata_within$by)))))
rawdata_within <- subset(rawdata_within, select = -c(by))

remove("acoustic", "betweenfile_cons", "betweenfile_vowels", "distance", "withinfile_cons", "withinfile_vowels")
