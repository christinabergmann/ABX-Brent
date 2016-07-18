# Author: Christina Bergmann (chbergma 'at' gmail)
# Last modified: 13.06.2016
# Description: This script prepares item files based on posteriors from forced alignments

library(dplyr)

alignments = read.csv("Supplementary/alignement.csv", header = FALSE)
segments_data = read.csv("Supplementary/segments.csv", header = FALSE)


names(alignments) <-  c("segment", "phone_onset", "phone_offset", "posterior", "phone", "word")
names(segments_data) <- c("segment", "file", "segmentonset", "segmentoffset")

alignments$talker = substring(alignments$segment, 1, 2)
alignments$phone = as.character(alignments$phone)


data = left_join(alignments, segments_data, by = "segment") %>%
  mutate(onset = phone_onset+segmentonset) %>%
  mutate(offset = phone_offset+segmentonset) %>%
  #Many thanks to Page Piccinini (page.piccinini 'at' gmail) for this code snippet which greatly improved efficiency
  # To make sure it's all within file
  group_by(file) %>%
  # To make sure they are in the right order
  arrange(onset) %>%
  mutate(next_sound = lead(phone)) %>%
  mutate(previous_sound = lag(phone)) %>%
  ungroup()

#the pedestrian way, which takes forever

# for(line in 1:nrow(alignments)){
#   if(line > 1){ 
#     if(alignments$file[line]==alignments$file[line-1]){
#       alignments$previous_sound[line] = as.character(alignments$phone[line-1])
#     }
#     else{alignments$previous_sound[line] = "_"}
#   }
#   else{alignments$previous_sound[line] = "_"}
#   if(line < length(alignments$file) & alignments$file[line]==alignments$file[line+1]){
#     alignments$next_sound[line] = as.character(alignments$phone[line+1])
#   }
#   else{alignments$next_sound[line] = "_"} 
# }

alignments = as.data.frame(data)
alignments$previous_sound=ifelse(is.na(alignments$previous_sound), "_", alignments$previous_sound)
alignments$next_sound=ifelse(is.na(alignments$next_sound), "_", alignments$next_sound)


alignments$context = paste(alignments$previous_sound, alignments$next_sound, sep="-")



#Now we set different thresholds and create item files accordingly
threshold = 1

thresholded = subset(alignments, posterior>=threshold)

#for simplicity split into vowels and consonant files
vowels = c("aa", "ae", "ah", "ao", "aw", "ax", "ay", "eh", "er", "ey", "ih", "iy", "ow", "oy", "uh", "uw")


brent.vowels = droplevels(subset(thresholded, phone %in% vowels))
brent.cons = droplevels(subset(thresholded, !(phone %in% vowels)))

#Make the data structure fit the file format for easier write out later. 
brent.vowels =  brent.vowels[c("file", "onset", "offset", "phone", "context", "talker")]
brent.cons =  brent.cons[c("file", "onset", "offset", "phone", "context", "talker")]


vowfile = paste("Brent_vowels_", as.character(threshold),".item", sep="")
consfile = paste("Brent_cons_", as.character(threshold),".item", sep="")
file.create(vowfile)
file.create(consfile)
write("#file onset offset #phone context talker", file = vowfile)
write("#file onset offset #phone context talker", file = consfile)



#Next we want to remove those elements where a context does not appear in all talkers per phone

talkers = levels(as.factor(thresholded$talker))
contexts = levels(as.factor(thresholded$context))


phones = levels(as.factor(brent.cons$phone))

#This is a very roundabout way and I am sure in dplyr there are better solutions.
for(phone in phones[phones != "SIL"]){
  for(context in contexts){
    subset = brent.cons[brent.cons$phone==phone & brent.cons$context==context, ]
    if(length(unique(subset$talker))==length(talkers)){
      write.table(subset, append = TRUE, sep = " ", col.names= FALSE, row.names = FALSE, quote = FALSE, file = consfile)
    }
  }
}
phones = levels(as.factor(brent.vowels$phone))

for(phone in phones){
  for(context in contexts){
    subset = brent.vowels[brent.vowels$phone==phone & brent.vowels$context==context, ]
    if(length(unique(subset$talker))==length(talkers)){
      write.table(subset, append = TRUE, sep = " ", col.names= FALSE, row.names = FALSE, quote = FALSE, file = vowfile)
    }
  }
}

