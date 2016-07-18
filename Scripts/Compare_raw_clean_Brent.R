


withinfile_cons = paste("Data/Brent_Clean_withinSpeaker_cons_", acoustic, "_", distance,".csv", sep="")
betweenfile_cons = paste("Data/Brent_Clean_acrossSpeaker_cons_", acoustic, "_", distance,".csv", sep="")

read.table(betweenfile_cons,sep="\t",header=T)->rawdata_between
read.table(withinfile_cons,sep="\t",header=T)->rawdata_within

rawdata_between$contrasts <- as.factor(ifelse((as.character(rawdata_between$phone_1)<as.character(rawdata_between$phone_2)), paste(rawdata_between$phone_1, rawdata_between$phone_2, sep = "-"), paste(rawdata_between$phone_2, rawdata_between$phone_1, sep = "-")))
rawdata_between$speakerpairs <- as.factor(ifelse((as.character(rawdata_between$talker_1)<as.character(rawdata_between$talker_2)), paste(rawdata_between$talker_1, rawdata_between$talker_2, sep = "-"), paste(rawdata_between$talker_2, rawdata_between$talker_1, sep = "-")))
names(rawdata_between)[names(rawdata_between)=="by"] <- "context"

rawdata_within$contrasts <- as.factor(ifelse((as.character(rawdata_within$phone_1)<as.character(rawdata_within$phone_2)), paste(rawdata_within$phone_1, rawdata_within$phone_2, sep = "-"), paste(rawdata_within$phone_2, rawdata_within$phone_1, sep = "-")))
rawdata_within$speaker <- as.factor(gsub(")", "", (gsub("^.*?, ","", (gsub("^.*?, ","", as.character(rawdata_within$by)))))))
rawdata_within$context <- as.factor(gsub("[(), ]", "", (gsub("\\d","", as.character(rawdata_within$by)))))
rawdata_within <- subset(rawdata_within, select = -c(by))


withinfile_cons = paste("../Brent/Data/Brent_withinSpeaker_cons_", acoustic, "_", distance,"_t.csv", sep="")
betweenfile_cons = paste("../Brent/Data/Brent_acrossSpeaker_cons_", acoustic, "_", distance,"_t.csv", sep="")


read.table(betweenfile_cons,sep="\t",header=T)->rawdata_between_old
read.table(withinfile_cons,sep="\t",header=T)->rawdata_within_old

rawdata_between_old$contrasts <- as.factor(ifelse((as.character(rawdata_between_old$sound_1)<as.character(rawdata_between_old$sound_2)), paste(rawdata_between_old$sound_1, rawdata_between_old$sound_2, sep = "-"), paste(rawdata_between_old$sound_2, rawdata_between_old$sound_1, sep = "-")))
rawdata_between_old$speakerpairs <- as.factor(ifelse((as.character(rawdata_between_old$child_1)<as.character(rawdata_between_old$child_2)), paste(rawdata_between_old$child_1, rawdata_between_old$child_2, sep = "-"), paste(rawdata_between_old$child_2, rawdata_between_old$child_1, sep = "-")))

rawdata_within_old$contrasts <- as.factor(ifelse((as.character(rawdata_within_old$sound_1)<as.character(rawdata_within_old$sound_2)), paste(rawdata_within_old$sound_1, rawdata_within_old$sound_2, sep = "-"), paste(rawdata_within_old$sound_2, rawdata_within_old$sound_1, sep = "-")))
rawdata_within_old$speaker <- as.factor(gsub(")", "", (gsub("^.*?, ","", (gsub("^.*?, ","", as.character(rawdata_within_old$by)))))))


rawdata_within_old$context <- as.factor(gsub("\\(| ", "", (gsub(".{5}$","", as.character(rawdata_within_old$by)))))
rawdata_between_old$context <-as.factor(gsub("\\(|\\)| ", "", as.character(rawdata_between_old$by)))




par(pty="s")
plot(c(.45,1.), c(.45, 1.), type="n",xlab="Within-Speaker ABX Score",ylab="Across-Speaker ABX Score",main = "ABX scores", xlim=c(0.45,.99),ylim=c(0.45,.99), frame.plot = FALSE)
axis(side = 1)

for (thiscontrast in levels(rawdata_within$contrasts)) {
  between_score = rawdata_between$score[rawdata_between$contrasts == thiscontrast]
  within_score = rawdata_within$score[rawdata_within$contrasts == thiscontrast]
  points(mean(within_score), mean(between_score), pch = 1, cex =1)
}

for (thiscontrast in levels(rawdata_within_old$contrasts)) {
  between_score = rawdata_between_old$score[rawdata_between_old$contrasts == thiscontrast]
  within_score = rawdata_within_old$score[rawdata_within_old$contrasts == thiscontrast]
  points(mean(within_score), mean(between_score), pch = 4, cex =.5, col = "blue")
}



lines(c(0.45,1.),c(0.45,1.),col="red",lty=3)


