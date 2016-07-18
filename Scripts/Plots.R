
#### Banana Plot: Within vs between scores #### 

par(pty="s")
plot(c(.45,1.), c(.45, 1.), type="n",xlab="Within-Speaker ABX Score",ylab="Across-Speaker ABX Score",main = "ABX scores", xlim=c(0.45,.99),ylim=c(0.45,.99), frame.plot = FALSE)
axis(side = 1)



for (thiscontrast in levels(rawdata_within$contrasts)) {
  between_score_plot = rawdata_between$score[rawdata_between$contrasts == thiscontrast]
  within_score_plot = rawdata_within$score[rawdata_within$contrasts == thiscontrast]
  points(mean(within_score_plot), mean(between_score_plot), pch = 1, cex =1)
}


lines(c(0.45,1.),c(0.45,1.),col="red",lty=3)

#### Histogram of Null distribution of difference scores vs actual score ####

hist(diff_results, xlim =c(-.055, .055))
abline(v=difference_score, col = "red")




pdf(paste("Barplot_vowelContrasts_", acoustic,".pdf", sep = ""),width=200,height=7)
b<-barplot(diff_quants[1,], ylim=c(.0, .5), ylab = "ABX Difference Score", cex.names=1.2)
bd=.15 #  a hack to print the CIs as error bars
x=c(t(cbind(b,b,NaN)))
y=c(t(cbind(diff_quants[2, ],diff_quants[4,],NaN)))
x=c(t(cbind(b,b,b-bd,b+bd,NaN,b-bd,b+bd,NaN)))
y=c(t(cbind(diff_quants[2, ],diff_quants[4,],diff_quants[4,],diff_quants[4,],NaN,diff_quants[2, ],diff_quants[2, ],NaN)))
lines(x,y,col="#303030") 
dev.off()

