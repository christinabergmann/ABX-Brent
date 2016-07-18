

speakers = levels(rawdata_within$speaker)

#Build empty data structures to store results for within and between speaker data.
within_res <- matrix(NA, 0, length(levels(rawdata_within$contrasts)))
colnames(within_res)<-levels(rawdata_within$contrasts)
between_res <- matrix(NA, 0, length(levels(rawdata_between$contrasts)))
colnames(between_res)<-levels(rawdata_between$contrasts)

set.seed(111)
for (i in 1:1000){
  #Use half the speakers, other numbers are also possible. 
  speakersample <- sample(speakers)[1:(length(speakers)/2)]
  #Only use data which comes from the subsetted speakers
  within_subset <- rawdata_within[rawdata_within$speaker %in% speakersample, ]
  between_subset_1 <- rawdata_between[rawdata_between$child_1 %in% speakersample, ]
  between_subset <- between_subset_1[between_subset_1$child_2 %in% speakersample, ]
  #remove dummy variable
  rm(between_subset_1)
  #compute means and copy over column names
  within_means <- t(as.matrix(ddply(within_subset, .(contrasts), summarize, score=mean(score))$score))
  colnames(within_means)<-levels(as.factor(within_subset$contrasts))
  between_means <- t(as.matrix(ddply(between_subset, .(contrasts), summarize, score=mean(score))$score))
  colnames(between_means)<-levels(as.factor(between_subset$contrasts))
  #Add result to the datastructure
  within_res = rbind(within_res, within_means)
  between_res = rbind(between_res, between_means)
}
p=.95
quants=c((1-p)/2,.5,1-(1-p)/2) # these are the quantiles for the CI plus the median
within_quants=NULL # collect the p quantile stats over each dimension of the result matrices
between_quants=NULL

for (j in 1:length(levels(as.factor(rawdata_within$contrasts)))) {
  within_quants<- cbind(within_quants,c(mean=mean(within_res[,j]),quantile(within_res[,j],quants)))
  between_quants<- cbind(between_quants,c(mean=mean(between_res[,j]),quantile(between_res[,j],quants)))
}
dimnames(within_quants)[[2]]=dimnames(within_res)[[2]]
dimnames(between_quants)[[2]]=dimnames(within_res)[[2]]



##### Confidence intervals for difference scores ####


speakers = levels(rawdata_within$speaker)

res <- matrix(NA, 0, length(levels(rawdata_within$contrasts)))
colnames(res)<-levels(rawdata_within$contrasts)


set.seed(111)
for (i in 1:1000){
  speakersample <- sample(speakers)[1:(length(speakers)/2)]
  within_subset <- rawdata_within[rawdata_within$speaker %in% speakersample, ]
  between_subset_1 <- rawdata_between[rawdata_between$child_1 %in% speakersample, ]
  between_subset <- between_subset_1[between_subset_1$child_2 %in% speakersample, ]
  rm(between_subset_1)
  within_means <- t(as.matrix(ddply(within_subset, .(contrasts), summarize, score=mean(score))$score))
  colnames(within_means)<-levels(as.factor(within_subset$contrasts))
  between_means <- t(as.matrix(ddply(between_subset, .(contrasts), summarize, score=mean(score))$score))
  colnames(between_means)<-levels(as.factor(between_subset$contrasts))
  diffscores = within_means-between_means
  res = rbind(res, diffscores)
}

p=.95
quants=c((1-p)/2,.5,1-(1-p)/2) # these are the quantiles for the CI plus the median
diff_quants=NULL # collect the p quantile stats over each dimension of the result matrix


for (j in 1:length(levels(as.factor(rawdata_within$contrasts)))) {
  diff_quants<- cbind(diff_quants,c(mean=mean(res[,j]),quantile(res[,j],quants)))
}
dimnames(diff_quants)[[2]]=dimnames(res)[[2]]






