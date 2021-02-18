#######################################################################
#Name of code file: human_coding_analysis.R

#Data In:
  #human_coded_data.csv
#Data Out:
  #Figure 2
######################################################################


#Load Packages
library(readr)
library(ggplot2)

#Set Working Directory
setwd("..")

#Read in Data 
data<-read_csv("data/human_coded_data.csv")

##########
#Figure 2#
##########

#make table
political<-as.data.frame(table(data$directed))
#relabel
political$Var1<-gsub("another_democratic_politican", "other democrat ", political$Var1)
political$Var1<-gsub("another_republican_politican", "other republican", political$Var1)
political$Var1<-gsub("_", " ", political$Var1)
political$Var1<-gsub("trumps", "trump's ", political$Var1)
#remove "none"
political <-political[ which(political$Var1!='none'),]
political$Prop<-political$Freq/sum(political$Freq)
political <- political[order(-political$Prop),] 
barplot(political$Prop, names.arg=political$Var1, main="Hatespeech Tweets Directed at Political Actors by Type", ylim=c(0,.5))

pdf("figures/figure_2.pdf", height=7, width=20)
barplot(political$Prop, names.arg=political$Var1, ylim=c(0,.5))
dev.off()





