# Author: Alejandrina Cristia (alecristia 'at' gmail), largely from Christina Bergmann's code
# Last modified: 25.12.2017
# Description: Calculate model to decide whether between speaker leads to worse scores than within

source("scripts/Readin.R")

library(dplyr)

#For sampling later we need to add this column
rawdata_within$speakerpairs = paste(rawdata_within$speaker, rawdata_within$speaker, sep="-")

#Average over contexts to have a mean score  per contrast-speakerpair
between_percontrastspeaker = rawdata_between %>%
  group_by(speakerpairs, contrasts) %>%
  summarise(mean_score_temp = mean(score)) %>%
  ungroup() 

within_percontrastspeaker = rawdata_within %>%
  group_by(speakerpairs, contrasts) %>%
  summarise(mean_score_temp = mean(score)) %>%
  ungroup() 

both=rbind(between_percontrastspeaker,within_percontrastspeaker)
both$sp_type=ifelse(substr(both$speakerpairs,1,2)==substr(both$speakerpairs,4,5),"w","b")
both$c_type<-"C"
both$c_type[grep("[aeiou]",both$contrasts)]<-"V"
both=data.frame(both)
#The goal is to assess the size of the effect for sp_type (between versus within) versus sound contrast

whole=lm(mean_score_temp~contrasts+speakerpairs,data=both)
contrast_only=lm(mean_score_temp~contrasts,data=both)
speakers_only=lm(mean_score_temp~speakerpairs,data=both)
anova(whole,contrast_only) # F 7.442
anova(whole,speakers_only) # F 114.2
summary(contrast_only) #contrast ID explains 43% of the variance
summary(speakers_only) #speaker ID explains 4% of the variance  - 10x less !!
#and notice that this includes both individual variability and that related to within vs across
summary(whole) #the model with both explains 48% of the variance 

write.table(both,"Supplementary/both_brent.txt",row.names=F,sep="\t")
