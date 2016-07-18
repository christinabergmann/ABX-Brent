# Author: Christina Bergmann (chbergma 'at' gmail)
# Last modified: 07.07.2016
# Description: Analyze data
# Analyzing between-/within-speaker discriminability of sound contrasts in the Brent corpus

library(dplyr)

#For sampling later we need to add this column
rawdata_within$speakerpairs = paste(rawdata_within$speaker, rawdata_within$speaker, sep="-")

#pipeline, as agreed on by Bergmann, Cristia, Dupoux in June 2016
#Cf https://docs.google.com/document/d/1x0lpC0sgho8k9h0gE3gi0VwWgcZ2RyTd1uM0nRBhjXE/edit

#### Effect per condition (within and across speaker) ####
#Average over contexts → average over contrasts  → (one number per speaker-speaker) → 
# average all s-s in within and, separately, all s-s in between → (average effect) 

#Step 1, have one mean score per contrast-speaker(pair)
between_percontrastspeaker = rawdata_between %>%
  group_by(speakerpairs, contrasts) %>%
  summarise(mean_score_temp = mean(score)) %>%
  ungroup() 

within_contrast = rawdata_within%>%
  group_by(contrasts) %>%
  summarise(sum_n = sum(n)) %>%
  ungroup() 

within_percontrastspeaker = rawdata_within %>%
  group_by(speakerpairs, contrasts) %>%
  summarise(mean_score_temp = mean(score)) %>%
  ungroup() 

#Step 2: Have one number per speaker(pair)
between = between_percontrastspeaker %>%
  group_by(speakerpairs) %>%
  summarise(mean_score = mean(mean_score_temp)) %>%
  ungroup()



within = within_percontrastspeaker %>%
  group_by(speakerpairs) %>%
  summarise(mean_score = mean(mean_score_temp)) %>%
  ungroup()

#Step 3: Final score
between_score = mean(between$mean_score)

within_score = mean(within$mean_score)


#### Difference score ####
#→ do within minus between → (difference score)

difference_score = within_score-between_score


#### Create a null distribution: Random Sampling per speakerpair (re-assigininh within and across speaker) ####
#Then take matrix and shuffle the within & between labels 10,000 → 
#average all s-s in within and, separately, all s-s in between → (average effect) → do within minus between → (difference score)



sampling_data = merge(between, within, all = TRUE)

n_loops = 10000

n_within = length(within$speakerpairs)
n_between = length(between$speakerpairs)

within_results = matrix(NA, nrow = n_loops, ncol = 1)
between_results = matrix(NA, nrow = n_loops, ncol = 1)
diff_results = matrix(NA, nrow = n_loops, ncol = 1)

set.seed(111)
for(loop in 1:n_loops){
  within_sampling_rows = sample(nrow(sampling_data), n_within)
  sample_within = sampling_data[within_sampling_rows, ]
  sample_between = sampling_data[-within_sampling_rows, ]
  
  #Step 3: Final score, directly record
  between_results[loop] = mean(sample_between$mean_score)
  
  within_results[loop] = mean(sample_within$mean_score)
  
  #→ do within minus between → (difference score)
  diff_results[loop] = mean(sample_within$mean_score) - mean(sample_between$mean_score)
}

#### Goal: Get CI for each contrast ####
# 1. Average over contexts → 2. average over s-s-w and, separately, s-s-b per contrast → 3. do the difference per contrast → 4. average effect per contrast 
#shuffle s-s-w and s-s-b labels after step 1 and redo 2-4 10k times 

#We already averaged over contexts and can just use: between_percontrastspeaker, within_percontrastspeaker


#We want to randomly subsample separately for the within and across speaker condition. 

n_loops_contrast = 1000

n_contrasts = length(levels(rawdata_within$contrasts))

within_results_contrast = matrix(NA, nrow = n_loops_contrast, ncol = n_contrasts)
between_results_contrast = matrix(NA, nrow = n_loops_contrast, ncol = n_contrasts)
diff_results_contrast = matrix(NA, nrow = n_loops_contrast, ncol = n_contrasts)

set.seed(111)
for(loop in 1:n_loops_contrast){
  #Todo: reshuffle label per contrast. 
  
}



#within_ci = quantile(within_results, probs = c(.05, .95))
#between_ci = quantile(between_results, probs = c(.05, .95))

#diff_ci = quantile(diff_results, probs = c(.05, .95))
