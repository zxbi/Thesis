#######################################################################
#Name of code file: dictionary_analysis.R

#Data In: 
  #dictionary_data_clinton
  #dictionary_data_trump
  #dictionary_data-random
  #dictionary_data_clinton_agg
  #dictionary_data_trump_agg
  #dictionary_data_random_agg

#Data Out: 
  #Loess Plots, Descriptive Statistics & Interrupted Time Series Analysis (Plots & Regression Tables)
      #Figures 3-6, A1-A37 
      #Tables A1-A24
######################################################################

#Load Packages
library(readr)
library(texreg)
library(ggplot2)
library(nlme)
library(plyr)
library(dplyr)
library(psych)
library(stringi)
library(xtable)
library(scales)

#Set Working Directory
setwd("..")

#Read in Clinton data and create list of data frames 

folder <- "data/dictionary_data_clinton/"  
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read_csv(paste(folder, file_list[i], sep=''))
  )}

#Make list of dataframes 
clinton_list <- Filter(function(x) is(x, "data.frame"), mget(ls()))
#Remove individual data frames 
rm(list = ls()[grep("*.csv", ls())])

#Read in Trump data and create list of data frames

folder <- "data/dictionary_data_trump/"  
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read_csv(paste(folder, file_list[i], sep=''))
  )}

#Make list of dataframes 
trump_list <- Filter(function(x) is(x, "data.frame"), mget(ls()))
#Remove individual data frames 
rm(list = ls()[grep("*.csv", ls())])

#Read in Random Sample data and create a list of data frames

folder <- "data/dictionary_data_random/"  
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read_csv(paste(folder, file_list[i], sep=''))
  )}

#Make list of dataframes 
random_list <- Filter(function(x) is(x, "data.frame"), mget(ls()))
#Remove individual data frames 
rm(list = ls()[grep("*.csv", ls())])


#Read in Aggregated Data
clinton_data_agg<-read_csv("data/dictionary_data_clinton_agg.csv")
trump_data_agg<-read_csv("data/dictionary_data_trump_agg.csv")
random_data_agg<-read_csv("data/dictionary_data_random_agg.csv")

#Aggregate Data for Monthly Plots 

clinton_data_agg$month<-as.Date(cut(clinton_data_agg$date, breaks="month"))
clinton_data_agg_month<-aggregate(. ~ month, clinton_data_agg, sum)
clinton_data_agg_month$data_name<-rep("Clinton",25)
clinton_data_agg_month$month_norm<-clinton_data_agg_month$freq_hate/clinton_data_agg_month$hillary_tpd
clinton_data_agg_month$month_norm_upd<-clinton_data_agg_month$freq_upd_hate/clinton_data_agg_month$hillary_upd

trump_data_agg$month<-as.Date(cut(trump_data_agg$date, breaks="month"))
trump_data_agg_month<-aggregate(. ~ month, trump_data_agg, sum)
trump_data_agg_month$data_name<-rep("Trump",25)
trump_data_agg_month$month_norm<-trump_data_agg_month$freq_hate/trump_data_agg_month$trump_tpd
trump_data_agg_month$month_norm_upd<-trump_data_agg_month$freq_upd_hate/trump_data_agg_month$trump_upd

random_data_agg$month<-as.Date(cut(random_data_agg$date, breaks="month"))
random_data_agg_month<-aggregate(. ~ month, random_data_agg, sum)
random_data_agg_month$data_name<-rep("Random Sample",25)
random_data_agg_month$month_norm<-random_data_agg_month$freq_hate/random_data_agg_month$tweet_count
random_data_agg_month$month_norm_upd<-random_data_agg_month$freq_upd_hate/random_data_agg_month$unique_users

month_data<-rbind.fill(clinton_data_agg_month, trump_data_agg_month, random_data_agg_month)

#Same for White Nationalist Only
clinton_list$white_nationalist_clinton.csv$month<-as.Date(cut(clinton_list$white_nationalist_clinton.csv$date, breaks="month"))
clinton_white_nationalist_month<-aggregate(. ~ month, clinton_list$white_nationalist_clinton.csv, sum)
clinton_white_nationalist_month$data_name<-rep("clinton",25)
clinton_white_nationalist_month$month_norm<-clinton_white_nationalist_month$freq_hate/clinton_white_nationalist_month$hillary_tpd
clinton_white_nationalist_month$month_norm_upd<-clinton_white_nationalist_month$freq_upd_hate/clinton_white_nationalist_month$hillary_upd

trump_list$white_nationalist_trump.csv$month<-as.Date(cut(trump_list$white_nationalist_trump.csv$date, breaks="month"))
trump_white_nationalist_month<-aggregate(. ~ month, trump_list$white_nationalist_trump.csv, sum)
trump_white_nationalist_month$data_name<-rep("trump",25)
trump_white_nationalist_month$month_norm<-trump_white_nationalist_month$freq_hate/trump_white_nationalist_month$trump_tpd
trump_white_nationalist_month$month_norm_upd<-trump_white_nationalist_month$freq_upd_hate/trump_white_nationalist_month$trump_upd

random_list$white_nationalist_random.csv$month<-as.Date(cut(random_list$white_nationalist_random.csv$date, breaks="month"))
random_white_nationalist_month<-aggregate(. ~ month, random_list$white_nationalist_random.csv, sum)
random_white_nationalist_month$data_name<-rep("random",25)
random_white_nationalist_month$month_norm<-random_white_nationalist_month$freq_hate/random_white_nationalist_month$tweet_count
random_white_nationalist_month$month_norm_upd<-random_white_nationalist_month$freq_upd_hate/random_white_nationalist_month$unique_users

month_data_wn<-rbind.fill(clinton_white_nationalist_month, trump_white_nationalist_month, random_white_nationalist_month)

#Combine data for descriptive statistics
clinton_list_combined<-bind_rows(clinton_list, .id="group")
trump_list_combined<-bind_rows(trump_list, .id="group")
random_list_combined<-bind_rows(random_list, .id="group")


#######################
#FIGURES WITH RAW DATA#
########################


# Create titles/colors for disaggregated plots 
titlenames<- c(" Anti-Asian Language (Tweets)", " Anti-Black Language (Tweets)", " Anti-Immigrant Language (Tweets)", " Anti-Latino Language (Tweets)", " Anti-Muslim Language (Tweets)", " Anti-Semitic Language (Tweets)", " Homophobic Language (Tweets)", " Misogynistic Language (Tweets)", "  White Nationalist Language (Tweets)")
titlenames_classified<- c(" Anti-Asian Language (Classified Tweets)", " Anti-Black Language (Classified Tweets)", " Anti-Immigrant Language (Classified Tweets)", " Anti-Latino Language (Classified Tweets)", " Anti-Muslim Language (Classified Tweets)", " Anti-Semitic Language (Classified Tweets)", " Homophobic Language (Classified Tweets)", " Misogynistic Language (Classified Tweets)", "  White Nationalist Language (Classified Tweets)")
titlenames_unique<- c(" Anti-Asian Language (Unique Users)", " Anti-Black Language (Unique Users)", " Anti-Immigrant Language (Unique Users)", " Anti-Latino Language (Unique Users)", " Anti-Muslim Language (Unique Users)", " Anti-Semitic Language (Unique Users)", " Homophobic Language (Unique Users)", " Misogynistic Language (Unique Users)", "  White Nationalist Language (Unique Users)")
titlenames_rt<-c(" Anti-Asian Language (Tweets/RTs)", " Anti-Black Language (Tweets/RTs)", " Anti-Immigrant Language (Tweets/RTs)", " Anti-Latino Language (Tweets/RTs)", " Anti-Muslim Language (Tweets/RTs)", " Anti-Semitic Language (Tweets/RTs)", " Homophobic Language (Tweets/RTs)", " Misogynistic Language (Tweets/RTs)", "  White Nationalist Language (Tweets/RTs)")
color<-c("cyan", "red", "orange", "maroon", "seagreen", "blue", "purple", "magenta", "navy")

##########
#Figure 3#
##########

#Plot
options(scipen=999)
pdf("figures/figure_3.pdf", height=7, width=11)
g <- ggplot(month_data, aes(month, month_norm, fill=data_name))
g + geom_bar(stat="identity", position="dodge", width = 20) + xlab("Date") + ylab("Proportion of Tweets")+
  #labs(title="Monthly Proportion of Classified Tweets Containing Hatespeech")+
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=0, vjust=0.6, size=13), axis.text.y = element_text(size=13), axis.title.x=element_text(size=15), axis.title.y=element_text(size=15)) + 
  scale_x_date(labels=date_format("%m-%y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-01"), by="4 months")) + scale_fill_manual(values=c("blue", "red", "black"), name="Data Set", breaks=c("Clinton", "Trump", "Random Sample"), labels=c("Clinton", "Trump", "Random Sample"))
dev.off()


##########
#Figure 4#
##########

#Define Plot Function 

plot_fun= function(x, col, title){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$date, x$norm_hate, type ="n", yaxt='n', xaxt='n', ann=FALSE)
  axis(side=2, las=1, font.lab=2)
  lines(x$date, x$norm_hate, col=col, yaxt='n', ann=FALSE, lwd=2)
  title(main=title)
  title(ylab = "Daily Proportion of Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(from=min(x$date), to=max(x$date), by='month')
  lbl <- strftime(ticks, '%b %d %y')
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  election_day<-which(x$date =="2016-11-08")
  abline(v=as.numeric(x$date[election_day]), lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=as.numeric(x$date[election_day]), cex=.85)
  
}

#Plot Trump Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_4.pdf", height=7, width=11)
mapply(plot_fun, x = trump_list, col = color, title=titlenames_classified)
dev.off()


###########
#Figure 5a#
###########

pdf("figures/figure_5a.pdf", height=7, width=11)
ggplot(trump_data_agg, aes(date, norm_hate, group=election)) +  geom_line() + geom_smooth(method="loess", se=TRUE, colour="red", fill="red")+  geom_vline(xintercept =as.numeric(as.Date("2016-11-08")), linetype=2) + labs(y="Daily Proportion of Tweets", x = "Date")+ annotate("text", label="Election Day", x=as.Date("2016-11-08", origin = '2015-06-15'), y = Inf, vjust = 2, hjust=1, size=6) + scale_x_date(labels=date_format("%m-%Y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-15"), by="4 months")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text.y = element_text(size = rel(1.5)), axis.text.x = element_text( size = rel(1.5)), axis.line.x = element_line(color="black", size = .5), axis.line.y = element_line(color="black", size = .5), axis.title = element_text(size=16))
dev.off()

###########
#Figure 5b#
###########

pdf("figures/figure_5b.pdf", height=7, width=11)
ggplot(trump_list$white_nationalist_trump.csv, aes(date, norm_hate, group=election)) +  geom_line() + geom_smooth(method="loess", se=TRUE, colour="red", fill="red")+  geom_vline(xintercept =as.numeric(as.Date("2016-11-08")), linetype=2) + labs(y="Daily Proportion of Tweets", x = "Date")+ annotate("text", label="Election Day", x=as.Date("2016-11-08", origin = '2015-06-15'), y = Inf, vjust = 2, hjust=1, size=6) + scale_x_date(labels=date_format("%m-%Y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-15"), by="4 months")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text.y = element_text(size = rel(1.5)), axis.text.x = element_text( size = rel(1.5)), axis.line.x = element_line(color="black", size = .5), axis.line.y = element_line(color="black", size = .5), axis.title = element_text(size=16))
dev.off()

###########
#Figure 5c#
###########

pdf("figures/figure_5c.pdf", height=7, width=11)
ggplot(clinton_data_agg, aes(date, norm_hate, group=election)) +  geom_line() + geom_smooth(method="loess", se=TRUE, colour="blue", fill="blue")+  geom_vline(xintercept =as.numeric(as.Date("2016-11-08")), linetype=2) + labs(y="Daily Proportion of Tweets", x = "Date")+ annotate("text", label="Election Day", x=as.Date("2016-11-08", origin = '2015-06-15'), y = Inf, vjust = 2, hjust=1, size=6) + scale_x_date(labels=date_format("%m-%Y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-15"), by="4 months")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text.y = element_text(size = rel(1.5)), axis.text.x = element_text( size = rel(1.5)), axis.line.x = element_line(color="black", size = .5), axis.line.y = element_line(color="black", size = .5), axis.title = element_text(size=16))
dev.off()

###########
#Figure 5d#
###########

pdf("figures/figure_5d.pdf", height=7, width=11)
ggplot(clinton_list$white_nationalist_clinton.csv, aes(date, norm_hate, group=election)) +  geom_line() + geom_smooth(method="loess", se=TRUE, colour="blue", fill="blue")+  geom_vline(xintercept =as.numeric(as.Date("2016-11-08")), linetype=2) + labs(y="Daily Proportion of Tweets", x = "Date")+ annotate("text", label="Election Day", x=as.Date("2016-11-08", origin = '2015-06-15'), y = Inf, vjust = 2, hjust=1, size=6) + scale_x_date(labels=date_format("%m-%Y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-15"), by="4 months")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text.y = element_text(size = rel(1.5)), axis.text.x = element_text( size = rel(1.5)), axis.line.x = element_line(color="black", size = .5), axis.line.y = element_line(color="black", size = .5), axis.title = element_text(size=16))
dev.off()

###########
#Figure 5e#
###########

pdf("figures/figure_5e.pdf", height=7, width=11)
ggplot(random_data_agg, aes(date, norm_hate, group=election)) +  geom_line() + geom_smooth(method="loess", se=TRUE, colour="black", fill="black")+  geom_vline(xintercept =as.numeric(as.Date("2016-11-08")), linetype=2) + labs(y="Daily Proportion of Tweets", x = "Date")+ annotate("text", label="Election Day", x=as.Date("2016-11-08", origin = '2015-06-15'), y = Inf, vjust = 2, hjust=1, size=6) + scale_x_date(labels=date_format("%m-%Y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-15"), by="4 months")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text.y = element_text(size = rel(1.5)), axis.text.x = element_text( size = rel(1.5)), axis.line.x = element_line(color="black", size = .5), axis.line.y = element_line(color="black", size = .5), axis.title = element_text(size=16))
dev.off()


###########
#Figure 5f#
###########

pdf("figures/figure_5f.pdf", height=7, width=11)
ggplot(random_list$white_nationalist_random.csv, aes(date, norm_hate, group=election)) +  geom_line() + geom_smooth(method="loess", se=TRUE, colour="black", fill="black")+  geom_vline(xintercept =as.numeric(as.Date("2016-11-08")), linetype=2) + labs(y="Daily Proportion of Tweets", x = "Date")+ annotate("text", label="Election Day", x=as.Date("2016-11-08", origin = '2015-06-15'), y = Inf, vjust = 2, hjust=1, size=6) + scale_x_date(labels=date_format("%m-%Y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-15"), by="4 months")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text.y = element_text(size = rel(1.5)), axis.text.x = element_text( size = rel(1.5)), axis.line.x = element_line(color="black", size = .5), axis.line.y = element_line(color="black", size = .5), axis.title = element_text(size=16))
dev.off()

###########
#Figure 6a#
###########

pdf("figures/figure_6a.pdf", height=7, width=11)
ggplot(clinton_data_agg, aes(date, norm_upd_hate, group=election)) +  geom_line() + geom_smooth(method="loess", se=TRUE, colour="blue", fill="blue")+  geom_vline(xintercept =as.numeric(as.Date("2016-11-08")), linetype=2) + labs(y="Daily Proportion of Tweets", x = "Date")+ annotate("text", label="Election Day", x=as.Date("2016-11-08", origin = '2015-06-15'), y = Inf, vjust = 2, hjust=1, size=6) + scale_x_date(labels=date_format("%m-%Y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-15"), by="4 months")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text.y = element_text(size = rel(1.5)), axis.text.x = element_text( size = rel(1.5)), axis.line.x = element_line(color="black", size = .5), axis.line.y = element_line(color="black", size = .5), axis.title = element_text(size=16))
dev.off()

###########
#Figure 6b#
###########

pdf("figures/figure_6b.pdf", height=7, width=11)
ggplot(clinton_list$white_nationalist_clinton.csv, aes(date, norm_upd_hate, group=election)) +  geom_line() + geom_smooth(method="loess", se=TRUE, colour="blue", fill="blue")+  geom_vline(xintercept =as.numeric(as.Date("2016-11-08")), linetype=2) + labs(y="Daily Proportion of Tweets", x = "Date")+ annotate("text", label="Election Day", x=as.Date("2016-11-08", origin = '2015-06-15'), y = Inf, vjust = 2, hjust=1, size=6) + scale_x_date(labels=date_format("%m-%Y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-15"), by="4 months")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text.y = element_text(size = rel(1.5)), axis.text.x = element_text( size = rel(1.5)), axis.line.x = element_line(color="black", size = .5), axis.line.y = element_line(color="black", size = .5), axis.title = element_text(size=16))
dev.off()


####################
#Figures A1, A3, A5#
####################

#Define Plot Function (Proporiton of Classified Tweets)

plot_fun= function(x, col, title){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$date, x$norm_hate, type ="n", yaxt='n', xaxt='n', ann=FALSE)
  axis(side=2, las=1, font.lab=2)
  lines(x$date, x$norm_hate, col=col, yaxt='n', ann=FALSE, lwd=2)
  title(main=title)
  title(ylab = "Daily Proportion of Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(from=min(x$date), to=max(x$date), by='month')
  lbl <- strftime(ticks, '%b %d %y')
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  election_day<-which(x$date =="2016-11-08")
  abline(v=as.numeric(x$date[election_day]), lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=as.numeric(x$date[election_day]), cex=.85)
  
}

#Plot Clinton Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A1.pdf", height=7, width=11)
mapply(plot_fun, x = clinton_list, col = color, title=titlenames_classified)
dev.off()

#Plot Trump Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A3.pdf", height=7, width=11)
mapply(plot_fun, x = trump_list, col = color, title=titlenames_classified)
dev.off()

#Plot Random Sample Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A5.pdf", height=7, width=11)
mapply(plot_fun, x = random_list, col = color, title=titlenames_classified)
dev.off()


####################
#Figures A2, A4, A6#
####################

#Define Plot Function (Proporiton of Unique Users Producing Classified Tweets)

plot_fun= function(x, col, title){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$date, x$norm_upd_hate, type ="n", yaxt='n', xaxt='n', ann=FALSE)
  axis(side=2, las=1, font.lab=2)
  lines(x$date, x$norm_upd_hate, col=col, yaxt='n', ann=FALSE, lwd=2)
  title(main=title)
  title(ylab = "Daily Proportion of Unique Users", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(from=min(x$date), to=max(x$date), by='month')
  lbl <- strftime(ticks, '%b %d %y')
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  election_day<-which(x$date =="2016-11-08")
  abline(v=as.numeric(x$date[election_day]), lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=as.numeric(x$date[election_day]), cex=.85)
  
}

#Plot Clinton Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A2.pdf", height=7, width=11)
mapply(plot_fun, x = clinton_list, col = color, title=titlenames_unique)
dev.off()

#Plot Trump Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A4.pdf", height=7, width=11)
mapply(plot_fun, x = trump_list, col = color, title=titlenames_unique)
dev.off()

#Plot Random Sample Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A6.pdf", height=7, width=11)
mapply(plot_fun, x = random_list, col = color, title=titlenames_unique)
dev.off()


##############
#Figure A7-A9#
##############

#Define Plot Function (Proporiton of Classified Tweets vs. Classified Retweets)

plot_fun= function(x, col, title){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$date, x$norm_hate, type ="n", yaxt='n', xaxt='n', ann=FALSE)
  axis(side=2, las=1, font.lab=2)
  lines(x$date, x$norm_hate, col=col, yaxt='n', ann=FALSE, lwd=2)
  lines(x$date, x$norm_rt_hate, col="honeydew4", yaxt='n', ann=FALSE, lwd=1.5)
  title(main=title)
  title(ylab = "Daily Proportion of Classified Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(from=min(x$date), to=max(x$date), by='month')
  lbl <- strftime(ticks, '%b %d %y')
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  election_day<-which(x$date =="2016-11-08")
  abline(v=as.numeric(x$date[election_day]), lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=as.numeric(x$date[election_day]), cex=.85)
  legend("top", legend=c("Tweets", "Retweets"), col=c(col,"honeydew4"), lw=c(2,2), bty="n")
}

#Plot Clinton Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A7.pdf", height=7, width=11)
mapply(plot_fun, x = clinton_list, col = color, title=titlenames_rt)
dev.off()

#Plot Trump Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A8.pdf", height=7, width=11)
mapply(plot_fun, x = trump_list, col = color, title=titlenames_rt)
dev.off()

#Plot Random Sample Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A9.pdf", height=7, width=11)
mapply(plot_fun, x = random_list, col = color, title=titlenames_rt)
dev.off()

################
#Figure A10-A12#
################

#Define Plot Function (Volume of Classified Tweets)

plot_fun= function(x, col, title){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$date, x$freq_hate, type ="n", yaxt='n', xaxt='n', ann=FALSE)
  axis(side=2, las=1, font.lab=2)
  lines(x$date, x$freq_hate, col=col, yaxt='n', ann=FALSE, lwd=2)
  title(main=title)
  title(ylab = "Daily Volume of Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(from=min(x$date), to=max(x$date), by='month')
  lbl <- strftime(ticks, '%b %d %y')
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  election_day<-which(x$date =="2016-11-08")
  abline(v=as.numeric(x$date[election_day]), lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=as.numeric(x$date[election_day]), cex=.85)
  
}

#Plot Clinton Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A10.pdf", height=7, width=11)
mapply(plot_fun, x = clinton_list, col = color, title=titlenames)
dev.off()

#Plot Trump Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A11.pdf", height=7, width=11)
mapply(plot_fun, x = trump_list, col = color, title=titlenames)
dev.off()

#Plot Random Sample Disaggregated Plots 
options(scipen=999)
pdf("figures/figure_A12.pdf", height=7, width=11)
mapply(plot_fun, x = random_list, col = color, title=titlenames)
dev.off()

###########
#Figure 13#
###########

pdf("figures/figure_A13.pdf", height=7, width=11)
g <- ggplot(month_data, aes(month, freq_hate, fill=data_name))
g + geom_bar(stat="identity", position="dodge",width = 20) + xlab("Date") + ylab("Tweets per Month")+
  #labs(title="Monthly Volume of Classified Tweets Containing Hatespeech")+
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=0, vjust=0.6, size=13), axis.text.y = element_text(size=13), axis.title.x=element_text(size=15), axis.title.y=element_text(size=15)) + 
  scale_x_date(labels=date_format("%m-%y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-01"), by="4 months")) + scale_fill_manual(values=c("blue", "red", "black"), name="Data Set", breaks=c("Clinton", "Trump", "Random Sample"), labels=c("Clinton", "Trump", "Random Sample"))
dev.off()

###########
#Figure 14#
###########
pdf("figures/figure_A14.pdf", height=7, width=11)
g <- ggplot(month_data_wn, aes(month, freq_hate, fill=data_name))
g + geom_bar(stat="identity", position="dodge",width = 20) + xlab("Date") + ylab("Tweets per Month")+
  #labs(title="Monthly Volume of Classified White Nationalist Tweets")+
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=0, vjust=0.6, size=13), axis.text.y = element_text(size=13), axis.title.x=element_text(size=15), axis.title.y=element_text(size=15)) + 
  scale_x_date(labels=date_format("%m-%y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-01"), by="4 months")) + scale_fill_manual(values=c("blue", "red", "black"), name="Data Set", breaks=c("clinton", "trump", "random"), labels=c("Clinton", "Trump", "Random Sample"))
dev.off()

###########
#Figure 15#
###########
pdf("figures/figure_A15.pdf", height=7, width=11)
g <- ggplot(month_data_wn, aes(month, month_norm, fill=data_name))
g + geom_bar(stat="identity", position="dodge",width = 20) + xlab("Date") + ylab("Proportion of Tweets per Month")+
  #labs(title="Monthly Proportion of Classified Tweets Containing White Nationalist Tweets")+
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=0, vjust=0.6, size=13), axis.text.y = element_text(size=13), axis.title.x=element_text(size=15), axis.title.y=element_text(size=15)) + 
  scale_x_date(labels=date_format("%m-%y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-01"), by="4 months")) + scale_fill_manual(values=c("blue", "red", "black"), name="Data Set", breaks=c("clinton", "trump", "random"), labels=c("Clinton", "Trump", "Random Sample"))
dev.off()


###########
#Figure 16#
###########

pdf("figures/figure_A16.pdf", height=7, width=11)
g <- ggplot(month_data, aes(month, freq_upd_hate, fill=data_name))
g + geom_bar(stat="identity", position="dodge",width = 20) + xlab("Date") + ylab("Unique Users per Month")+
  #labs(title="Monthly Volume of Unique Users Tweeting Hatespeech")+
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=0, vjust=0.6, size=13), axis.text.y = element_text(size=13), axis.title.x=element_text(size=15), axis.title.y=element_text(size=15)) + 
  scale_x_date(labels=date_format("%m-%y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-01"), by="4 months")) + scale_fill_manual(values=c("blue", "red", "black"), name="Data Set", breaks=c("Clinton", "Trump", "Random Sample"), labels=c("Clinton", "Trump", "Random Sample"))
dev.off()

###########
#Figure 17#
###########

pdf("figures/figure_A17.pdf", height=7, width=11)
g <- ggplot(month_data, aes(month, month_norm_upd , fill=data_name))
g + geom_bar(stat="identity", position="dodge",width = 20) + xlab("Date") + ylab("Proportion Unique Users per Month")+
  #labs(title="Monthly Proportion of Unique Users Tweeting Hatespeech")+
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=0, vjust=0.6, size=13), axis.text.y = element_text(size=13), axis.title.x=element_text(size=15), axis.title.y=element_text(size=15)) + 
  scale_x_date(labels=date_format("%m-%y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-01"), by="4 months")) + scale_fill_manual(values=c("blue", "red", "black"), name="Data Set", breaks=c("Clinton", "Trump", "Random Sample"), labels=c("Clinton", "Trump", "Random Sample"))
dev.off()


###########
#Figure 18#
###########

pdf("figures/figure_A18.pdf", height=7, width=11)
g <- ggplot(month_data_wn, aes(month, freq_upd_hate, fill=data_name))
g + geom_bar(stat="identity", position="dodge",width = 20) + xlab("Date") + ylab("Unique Users per Month")+
  #labs(title="Monthly Volume of Unique Users Tweeting White Nationalist Language")+
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=0, vjust=0.6, size=13), axis.text.y = element_text(size=13), axis.title.x=element_text(size=15), axis.title.y=element_text(size=15)) + 
  scale_x_date(labels=date_format("%m-%y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-01"), by="4 months")) + scale_fill_manual(values=c("blue", "red", "black"), name="Data Set", breaks=c("clinton", "trump", "random"), labels=c("Clinton", "Trump", "Random Sample"))
dev.off()

###########
#Figure 19#
###########

pdf("figures/figure_A19.pdf", height=7, width=11)
g <- ggplot(month_data_wn, aes(month, month_norm_upd , fill=data_name))
g + geom_bar(stat="identity", position="dodge",width = 20) + xlab("Date") + ylab("Prop. Unique Users per Month")+
  #labs(title="Monthly Proportion of Unique Users Tweeting White Nationalist Language")+
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle=0, vjust=0.6, size=13), axis.text.y = element_text(size=13), axis.title.x=element_text(size=15), axis.title.y=element_text(size=15)) + 
  scale_x_date(labels=date_format("%m-%y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-01"), by="4 months")) + scale_fill_manual(values=c("blue", "red", "black"), name="Data Set", breaks=c("clinton", "trump", "random"), labels=c("Clinton", "Trump", "Random Sample"))
dev.off()

##################
#REGRESSION PLOTS#
##################

############
#Figure A20#
############

#AR1 Model (interrupted time series analysis)

ar1<-lapply(clinton_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})

# Generate predicted values based on the models in order to create a plot
predicted_values<-lapply(ar1, function(x){
  predict(x)
})

#Plot Function
plot_fun= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_hate,cex=0.5, pch=16)
  lines(predicted_values[1:507],  col=col, lwd=2.5)
  lines(predicted_values[508:730], x=508:730,col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Classified Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A20.pdf", height=7, width=11)
mapply(plot_fun, x = clinton_list, col = color, title=titlenames_classified, predicted_values=predicted_values)
dev.off()

############
#Figure A21#
############

#AR1 Model (interrupted time series analysis with quadratic term)

ar1_q<-lapply(clinton_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
# Generate predicted values based on the models in order to create a plot

predicted_values_q<-lapply(ar1_q, function(x){
  predict(x)
})

#Plot Function

plot_fun_q= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_hate,cex=0.5, pch=16)
  lines(predicted_values_q[1:507],  col=col, lwd=2.5)
  lines(predicted_values_q[508:730],x=508:730, col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Classified Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=min(x$date), to=as.Date(max(x$date)), by=30)
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A21.pdf", height=7, width=11)
mapply(plot_fun, x = clinton_list, col = color, title=titlenames_classified, predicted_values=predicted_values_q)
dev.off()

############
#Figure A22#
############

#AR1 Model (interrupted time series analysis)

ar1<-lapply(clinton_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})

# Generate predicted values based on the models in order to create a plot
predicted_values<-lapply(ar1, function(x){
  predict(x)
})

#Plot Function
plot_fun= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_upd_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_upd_hate,cex=0.5, pch=16)
  lines(predicted_values[1:507],  col=col, lwd=2.5)
  lines(predicted_values[508:730],x=508:730, col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Unique Users", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A22.pdf", height=7, width=11)
mapply(plot_fun, x = clinton_list, col = color, title=titlenames_unique, predicted_values=predicted_values)
dev.off()

############
#Figure A23#
############

#AR1 Model (interrupted time series analysis with quadratic term)

ar1_q<-lapply(clinton_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
# Generate predicted values based on the models in order to create a plot

predicted_values_q<-lapply(ar1_q, function(x){
  predict(x)
})

#Plot Function

plot_fun_q= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_upd_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_upd_hate,cex=0.5, pch=16)
  lines(predicted_values_q[1:507],  col=col, lwd=2.5)
  lines(predicted_values_q[508:730],x=508:730, col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Unique Users Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=min(x$date), to=as.Date(max(x$date)), by=30)
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A23.pdf", height=7, width=11)
mapply(plot_fun, x = clinton_list, col = color, title=titlenames_unique, predicted_values=predicted_values_q)
dev.off()


############
#Figure A24#
############

ar1<-gls(norm_hate ~ date_num + election + slope_change, data=clinton_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=clinton_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))

predicted_values<-predict(ar1)
predicted_values_q<-predict(ar1_q)

#Top Plot
pdf("figures/figure_A24_top.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(clinton_data_agg$norm_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(clinton_data_agg$norm_hate,cex=0.5, pch=16)
lines(predicted_values[1:507],  col="black", lwd=2.5)
lines(predicted_values[508:730],x=508:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Classified Hatespeech Tweets (ITSA)")
title(ylab = "Daily Proportion of Classified Hatespeech Tweets", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()


pdf("figures/figure_A24_bottom.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(clinton_data_agg$norm_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(clinton_data_agg$norm_hate,cex=0.5, pch=16)
lines(predicted_values_q[1:507],  col="black", lwd=2.5)
lines(predicted_values_q[508:730],x=508:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Classified Hate Speech Tweets (ITSA)")
title(ylab = "Daily Proportion of Classified Hate Speech Tweets", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()



############
#Figure A25#
############

ar1<-gls(norm_upd_hate ~ date_num + election + slope_change, data=clinton_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_upd_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=clinton_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))

predicted_values<-predict(ar1)
predicted_values_q<-predict(ar1_q)

#Top Plot
pdf("figures/figure_A25_top.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(clinton_data_agg$norm_upd_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(clinton_data_agg$norm_upd_hate,cex=0.5, pch=16)
lines(predicted_values[1:507],  col="black", lwd=2.5)
lines(predicted_values[508:730],x=508:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Unique Users Tweeting Hatespeech (ITSA)")
title(ylab = "Daily Proportion of Unique Users", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()


pdf("figures/figure_A25_bottom.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(clinton_data_agg$norm_upd_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(clinton_data_agg$norm_upd_hate,cex=0.5, pch=16)
lines(predicted_values_q[1:507],  col="black", lwd=2.5)
lines(predicted_values_q[508:730],x=508:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Unique Users Tweeting Hatespeech (ITSA)")
title(ylab = "Daily Proportion of Unique Users", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()


############
#Figure A26#
############

#AR1 Model (interrupted time series analysis)

ar1<-lapply(trump_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})

# Generate predicted values based on the models in order to create a plot
predicted_values<-lapply(ar1, function(x){
  predict(x)
})

#Plot Function
plot_fun= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_hate,cex=0.5, pch=16)
  lines(predicted_values[1:507],  col=col, lwd=2.5)
  lines(predicted_values[508:730],x=508:730, col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Classified Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A26.pdf", height=7, width=11)
mapply(plot_fun, x = trump_list, col = color, title=titlenames_classified, predicted_values=predicted_values)
dev.off()

############
#Figure A27#
############

#AR1 Model (interrupted time series analysis with quadratic term)

ar1_q<-lapply(trump_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
# Generate predicted values based on the models in order to create a plot

predicted_values_q<-lapply(ar1_q, function(x){
  predict(x)
})

#Plot Function

plot_fun_q= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_hate,cex=0.5, pch=16)
  lines(predicted_values_q[1:507],  col=col, lwd=2.5)
  lines(predicted_values_q[508:730],x=508:730, col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Classified Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=min(x$date), to=as.Date(max(x$date)), by=30)
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A27.pdf", height=7, width=11)
mapply(plot_fun, x = trump_list, col = color, title=titlenames_classified, predicted_values=predicted_values_q)
dev.off()

############
#Figure A28#
############

#AR1 Model (interrupted time series analysis)

ar1<-lapply(trump_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})

# Generate predicted values based on the models in order to create a plot
predicted_values<-lapply(ar1, function(x){
  predict(x)
})

#Plot Function
plot_fun= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_upd_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_upd_hate,cex=0.5, pch=16)
  lines(predicted_values[1:507],  col=col, lwd=2.5)
  lines(predicted_values[508:730],x=508:730, col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Unique Users", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A28.pdf", height=7, width=11)
mapply(plot_fun, x = trump_list, col = color, title=titlenames_unique, predicted_values=predicted_values)
dev.off()

############
#Figure A29#
############

#AR1 Model (interrupted time series analysis with quadratic term)

ar1_q<-lapply(trump_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
# Generate predicted values based on the models in order to create a plot

predicted_values_q<-lapply(ar1_q, function(x){
  predict(x)
})

#Plot Function

plot_fun_q= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_upd_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_upd_hate,cex=0.5, pch=16)
  lines(predicted_values_q[1:507],  col=col, lwd=2.5)
  lines(predicted_values_q[508:730],x=508:730, col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Unique Users Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=min(x$date), to=as.Date(max(x$date)), by=30)
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A29.pdf", height=7, width=11)
mapply(plot_fun, x = trump_list, col = color, title=titlenames_unique, predicted_values=predicted_values_q)
dev.off()


############
#Figure A30#
############

ar1<-gls(norm_hate ~ date_num + election + slope_change, data=trump_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=trump_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))

predicted_values<-predict(ar1)
predicted_values_q<-predict(ar1_q)

#Top Plot
pdf("figures/figure_A30_top.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(trump_data_agg$norm_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(trump_data_agg$norm_hate,cex=0.5, pch=16)
lines(predicted_values[1:507],  col="black", lwd=2.5)
lines(predicted_values[508:730],x=508:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Classified Hatespeech Tweets (ITSA)")
title(ylab = "Daily Proportion of Classified Hatespeech Tweets", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()


pdf("figures/figure_A30_bottom.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(trump_data_agg$norm_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(trump_data_agg$norm_hate,cex=0.5, pch=16)
lines(predicted_values_q[1:507],  col="black", lwd=2.5)
lines(predicted_values_q[508:730],x=508:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Classified Hate Speech Tweets (ITSA)")
title(ylab = "Daily Proportion of Classified Hate Speech Tweets", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()



############
#Figure A31#
############

ar1<-gls(norm_upd_hate ~ date_num + election + slope_change, data=trump_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_upd_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=trump_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))

predicted_values<-predict(ar1)
predicted_values_q<-predict(ar1_q)

#Top Plot
pdf("figures/figure_A31_top.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(trump_data_agg$norm_upd_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(trump_data_agg$norm_upd_hate,cex=0.5, pch=16)
lines(predicted_values[1:507],  col="black", lwd=2.5)
lines(predicted_values[508:730],x=508:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Unique Users Tweeting Hatespeech (ITSA)")
title(ylab = "Daily Proportion of Unique Users", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()


pdf("figures/figure_A31_bottom.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(trump_data_agg$norm_upd_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(trump_data_agg$norm_upd_hate,cex=0.5, pch=16)
lines(predicted_values_q[1:507],  col="black", lwd=2.5)
lines(predicted_values_q[508:730],x=508:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Unique Users Tweeting Hatespeech (ITSA)")
title(ylab = "Daily Proportion of Unique Users", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()


############
#Figure A32#
############

#AR1 Model (interrupted time series analysis)

ar1<-lapply(random_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})

# Generate predicted values based on the models in order to create a plot
predicted_values<-lapply(ar1, function(x){
  predict(x)
})

#Plot Function
plot_fun= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_hate,cex=0.5, pch=16)
  lines(predicted_values[1:507],  col=col, lwd=2.5)
  lines(predicted_values[508:730],x=508:730, col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Classified Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A32.pdf", height=7, width=11)
mapply(plot_fun, x = random_list, col = color, title=titlenames_classified, predicted_values=predicted_values)
dev.off()

############
#Figure A33#
############

#AR1 Model (interrupted time series analysis with quadratic term)

ar1_q<-lapply(random_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
# Generate predicted values based on the models in order to create a plot

predicted_values_q<-lapply(ar1_q, function(x){
  predict(x)
})

#Plot Function

plot_fun_q= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_hate,cex=0.5, pch=16)
  lines(predicted_values_q[1:507],  col=col, lwd=2.5)
  lines(predicted_values_q[508:730],x=508:730, col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Classified Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=min(x$date), to=as.Date(max(x$date)), by=30)
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A33.pdf", height=7, width=11)
mapply(plot_fun, x = random_list, col = color, title=titlenames_classified, predicted_values=predicted_values_q)
dev.off()

############
#Figure A34#
############

#AR1 Model (interrupted time series analysis)

ar1<-lapply(random_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})

# Generate predicted values based on the models in order to create a plot
predicted_values<-lapply(ar1, function(x){
  predict(x)
})

#Plot Function
plot_fun= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_upd_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_upd_hate,cex=0.5, pch=16)
  lines(predicted_values[1:507],  col=col, lwd=2.5)
  lines(predicted_values[508:730],x=508:730, col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Unique Users", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A34.pdf", height=7, width=11)
mapply(plot_fun, x = random_list, col = color, title=titlenames_unique, predicted_values=predicted_values)
dev.off()

###########
#Figure A35#
###########

#AR1 Model (interrupted time series analysis with quadratic term)

ar1_q<-lapply(random_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
# Generate predicted values based on the models in order to create a plot

predicted_values_q<-lapply(ar1_q, function(x){
  predict(x)
})

#Plot Function

plot_fun_q= function(x, col, title, predicted_values){
  par(mar = c(7,7,7,5) + 0.1)
  plot.new()
  plot(x$norm_upd_hate,type="n", yaxt="n", ylab="", xaxt="n", xlab="")
  points(x$norm_upd_hate,cex=0.5, pch=16)
  lines(predicted_values_q[1:507],  col=col, lwd=2.5)
  lines(predicted_values_q[508:730],x=508:730, col=col, lwd=2.5)
  axis(side=2, las=1, font.lab=2)
  title(main=title)
  title(ylab = "Daily Proportion of Unique Users Tweets", line=4.5)
  title(xlab="Date", cex.lab = 1, line=6)
  ticks <- seq(0, 730, by=30)
  lbl <- seq(from=min(x$date), to=as.Date(max(x$date)), by=30)
  axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
  abline(v=508, lty="dashed", col='black', lwd=2)
  mtext("Election Day", side=3,  at=508, cex=.85)
  
}

#Plot 
pdf("figures/figure_A35.pdf", height=7, width=11)
mapply(plot_fun, x = random_list, col = color, title=titlenames_unique, predicted_values=predicted_values_q)
dev.off()


############
#Figure A36#
############

ar1<-gls(norm_hate ~ date_num + election + slope_change, data=random_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=random_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))

predicted_values<-predict(ar1)
predicted_values_q<-predict(ar1_q)

#Top Plot
pdf("figures/figure_A36_top.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(random_data_agg$norm_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(random_data_agg$norm_hate,cex=0.5, pch=16)
lines(predicted_values[1:507],  col="black", lwd=2.5)
lines(predicted_values[508:730],x=508:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Classified Hatespeech Tweets (ITSA)")
title(ylab = "Daily Proportion of Classified Hatespeech Tweets", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()


pdf("figures/figure_A36_bottom.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(random_data_agg$norm_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(random_data_agg$norm_hate,cex=0.5, pch=16)
lines(predicted_values_q[1:507],  col="black", lwd=2.5)
lines(predicted_values_q[508:730],x=508:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Classified Hate Speech Tweets (ITSA)")
title(ylab = "Daily Proportion of Classified Hate Speech Tweets", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()



############
#Figure A37#
############

ar1<-gls(norm_upd_hate ~ date_num + election + slope_change, data=random_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_upd_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=random_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))

predicted_values<-predict(ar1)
predicted_values_q<-predict(ar1_q)

#Top Plot
pdf("figures/figure_A37_top.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(random_data_agg$norm_upd_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(random_data_agg$norm_upd_hate,cex=0.5, pch=16)
lines(predicted_values[1:507],  col="black", lwd=2.5)
lines(predicted_values[508:730],x=508:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Unique Users Tweeting Hatespeech (ITSA)")
title(ylab = "Daily Proportion of Unique Users", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()

pdf("figures/figure_A37_bottom.pdf", height=7, width=11)
plot.new()
par(mar = c(7,7,7,5) + 0.1)
plot(random_data_agg$norm_upd_hate, type="n", yaxt="n", ylab="", xaxt="n", xlab="")
points(random_data_agg$norm_upd_hate,cex=0.5, pch=16)
lines(predicted_values_q[1:507],  col="black", lwd=2.5)
lines(predicted_values_q[509:730],x=509:730, col="black", lwd=2.5)
axis(side=2, las=1, font.lab=2)
title(main="Effect of 2016 Election on Proportion of Unique Users Tweeting Hatespeech (ITSA)")
title(ylab = "Daily Proportion of Unique Users", line=4.5)
title(xlab="Date", cex.lab = 1, line=6)
ticks <- seq(0, 730, by=30)
lbl <- seq(from=as.Date("2015/6/17"), to=as.Date("2017/6/15"), by="30 days")
axis(side=1, at=ticks, labels=lbl, las=2, cex=.75, font.lab=2)
abline(v=508, lty="dashed", col='black', lwd=2)
mtext("Election Day", side=3,  at=508, cex=.85)
dev.off()



########################
#DESCRIPTIVE STATISTICS#
#########################

##############
#Tables A1-A3#
##############

#Clinton (A1)

vars<-c("hillary_tpd", "hillary_upd", "freq", "freq_hate", "freq_upd_hate", "freq_rt_hate")
des_stat<-clinton_list_combined[vars]
des_stat<-describeBy(des_stat, group=clinton_list_combined$group, mat=TRUE)
des_stat<-des_stat[c("group1", "mean", "median", "sd", "min", "max", "n")]
des_stat$group1<-gsub("_|.csv", " ", des_stat$group1)
des_stat$group1<-stri_trans_totitle(des_stat$group1)
des_stat<-des_stat[-(c(2:9, 11:18)),]
rownames(des_stat)<-gsub("freq_", "", rownames(des_stat))
rownames(des_stat)<-gsub("freq|tpd", "Tweets Per Day", rownames(des_stat))
rownames(des_stat)<-gsub("upd", "Unique Users Per Day", rownames(des_stat))
rownames(des_stat)<-gsub("_hate", " (Classified) ", rownames(des_stat))
rownames(des_stat)<-gsub("_", " ", rownames(des_stat))
rownames(des_stat)<-gsub("hillary", "Clinton", rownames(des_stat))
rownames(des_stat)<-gsub("rt", "Retweets", rownames(des_stat))
rownames(des_stat)<-gsub("hate", "Tweets Per Day (Classified)", rownames(des_stat))
rownames(des_stat)<-gsub("norm", "Normalized", rownames(des_stat))
latex_table_clinton<-xtable(des_stat, size=scriptsize)
print(latex_table_clinton)

#Trump (A2)

vars<-c("trump_tpd", "trump_upd", "freq", "freq_hate", "freq_upd_hate", "freq_rt_hate")
des_stat<-trump_list_combined[vars]
des_stat<-describeBy(des_stat, group=trump_list_combined$group, mat=TRUE)
des_stat<-des_stat[c("group1", "mean", "median", "sd", "min", "max", "n")]
des_stat$group1<-gsub("_|.csv", " ", des_stat$group1)
des_stat$group1<-stri_trans_totitle(des_stat$group1)
des_stat<-des_stat[-(c(2:9, 11:18)),]
rownames(des_stat)<-gsub("freq_", "", rownames(des_stat))
rownames(des_stat)<-gsub("freq|tpd", "Tweets Per Day", rownames(des_stat))
rownames(des_stat)<-gsub("upd", "Unique Users Per Day", rownames(des_stat))
rownames(des_stat)<-gsub("_hate", " (Classified) ", rownames(des_stat))
rownames(des_stat)<-gsub("_", " ", rownames(des_stat))
rownames(des_stat)<-gsub("trump", "Trump", rownames(des_stat))
rownames(des_stat)<-gsub("rt", "Retweets", rownames(des_stat))
rownames(des_stat)<-gsub("hate", "Tweets Per Day (Classified)", rownames(des_stat))
rownames(des_stat)<-gsub("norm", "Normalized", rownames(des_stat))
latex_table_trump<-xtable(des_stat, size=scriptsize)
print(latex_table_trump)

#Random Sample (A3)

vars<-c("tweet_count", "unique_users", "freq", "freq_hate", "freq_upd_hate", "freq_rt_hate")
des_stat<-random_list_combined[vars]
des_stat<-describeBy(des_stat, group=random_list_combined$group, mat=TRUE)
des_stat<-des_stat[c("group1", "mean", "median", "sd", "min", "max", "n")]
des_stat$group1<-gsub("_|.csv", " ", des_stat$group1)
des_stat$group1<-stri_trans_totitle(des_stat$group1)
des_stat<-des_stat[-(c(2:9, 11:18)),]
rownames(des_stat)<-gsub("freq_", "", rownames(des_stat))
rownames(des_stat)<-gsub("freq|tpd", "Tweets Per Day", rownames(des_stat))
rownames(des_stat)<-gsub("upd", "Unique Users Per Day", rownames(des_stat))
rownames(des_stat)<-gsub("_hate", " (Classified) ", rownames(des_stat))
rownames(des_stat)<-gsub("_", " ", rownames(des_stat))
rownames(des_stat)<-gsub("random", "random", rownames(des_stat))
rownames(des_stat)<-gsub("rt", "Retweets", rownames(des_stat))
rownames(des_stat)<-gsub("hate", "Tweets Per Day (Classified)", rownames(des_stat))
rownames(des_stat)<-gsub("norm", "Normalized", rownames(des_stat))
latex_table_random<-xtable(des_stat, size=scriptsize)
print(latex_table_random)

###################
#Top Dates (A4-A6)#
###################


#Clinton (A4)
top_dates_combined_clinton<-head(clinton_data_agg[order(-clinton_data_agg$norm_hate),], 10)
top_dates_combined_clinton<-top_dates_combined_clinton[c("date", "norm_hate")]
top_dates_combined_clinton$date<-as.character(top_dates_combined_clinton$date)
latex_top_dates_clinton<-xtable(top_dates_combined_clinton, row.names=F, digits=4)
print(latex_top_dates_clinton, include.rownames=FALSE)

#Trump (A5)
top_dates_combined_trump<-head(trump_data_agg[order(-trump_data_agg$norm_hate),], 10)
top_dates_combined_trump<-top_dates_combined_trump[c("date", "norm_hate")]
top_dates_combined_trump$date<-as.character(top_dates_combined_trump$date)
latex_top_dates_trump<-xtable(top_dates_combined_trump, row.names=F, digits=4)
print(latex_top_dates_trump, include.rownames=FALSE)

#Random (A6)
top_dates_combined_random<-head(random_data_agg[order(-random_data_agg$norm_hate),], 10)
top_dates_combined_random<-top_dates_combined_random[c("date", "norm_hate")]
top_dates_combined_random$date<-as.character(top_dates_combined_random$date)
latex_top_dates_random<-xtable(top_dates_combined_random, row.names=F, digits=4)
print(latex_top_dates_random, include.rownames=FALSE)


####################
#Regression Tables#
####################

###########
#Table A7#
###########

#run model on each disaggregated datasets
ar1<-lapply(clinton_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})
#extract coefficients for table 
ar1<-lapply(ar1, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex
texreg(c(ar1$anti_asian_clinton.csv, ar1$anti_black_clinton.csv, ar1$anti_immigrant_clinton.csv, ar1$anti_latino_clinton.csv, ar1$anti_muslim_clinton.csv, ar1$anti_semitic_clinton.csv, ar1$homophobic_clinton.csv, ar1$misogynistic_clinton.csv, ar1$white_nationalist_clinton.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Tweets Containing Hatespeech (Clinton Data)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)


###########
#Table A8#
###########

#run model on each disaggregated datasets
ar1_q<-lapply(clinton_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
#extract coefficients for table 
ar1_q<-lapply(ar1_q, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex 
texreg(c(ar1_q$anti_asian_clinton.csv, ar1_q$anti_black_clinton.csv, ar1_q$anti_immigrant_clinton.csv, ar1_q$anti_latino_clinton.csv, ar1_q$anti_muslim_clinton.csv, ar1_q$anti_semitic_clinton.csv, ar1_q$homophobic_clinton.csv, ar1_q$misogynistic_clinton.csv, ar1_q$white_nationalist_clinton.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change", "Pre-Elelction Trend$^2$", "Election Slope Change$^2$"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Tweets Containing Hate Speech (Clinton Dataset)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)

###########
#Table A9#
###########

#run models on aggregated dataset
ar1<-gls(norm_hate ~ date_num + election + slope_change, data=clinton_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=clinton_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
#extract coefficients for table 
ar1<-extract(ar1, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
ar1_q<-extract(ar1_q, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#print table for latex 
texreg(c(ar1, ar1_q), digits=9, custom.coef.names = c("Baseline", "Pre-Election Trend",  "Election Level Change", "Election Slope Change", "Pre-Election Trend^2", "Election Slope Change^2"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Tweets Containing Hatespeech (Clinton Data)", stars = c(0.001,0.01, 0.05, 0.1), custom.gof.names = NULL, custom.coef.map = NULL)

###########
#Table A10#
###########

#run model on each disaggregated datasets
ar1<-lapply(clinton_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})
#extract coefficients for table 
ar1<-lapply(ar1, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex
texreg(c(ar1$anti_asian_clinton.csv, ar1$anti_black_clinton.csv, ar1$anti_immigrant_clinton.csv, ar1$anti_latino_clinton.csv, ar1$anti_muslim_clinton.csv, ar1$anti_semitic_clinton.csv, ar1$homophobic_clinton.csv, ar1$misogynistic_clinton.csv, ar1$white_nationalist_clinton.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Unique Users Tweeting Hatespeech (Clinton Data)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)


###########
#Table A11#
###########

#run model on each disaggregated datasets
ar1_q<-lapply(clinton_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
#extract coefficients for table 
ar1_q<-lapply(ar1_q, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex 
texreg(c(ar1_q$anti_asian_clinton.csv, ar1_q$anti_black_clinton.csv, ar1_q$anti_immigrant_clinton.csv, ar1_q$anti_latino_clinton.csv, ar1_q$anti_muslim_clinton.csv, ar1_q$anti_semitic_clinton.csv, ar1_q$homophobic_clinton.csv, ar1_q$misogynistic_clinton.csv, ar1_q$white_nationalist_clinton.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change", "Pre-Elelction Trend$^2$", "Election Slope Change$^2$"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Unique Users Tweeting Hate Speech (Clinton Dataset)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)

###########
#Table A12#
###########

#run models on aggregated dataset
ar1<-gls(norm_upd_hate ~ date_num + election + slope_change, data=clinton_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_upd_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=clinton_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
#extract coefficients for table 
ar1<-extract(ar1, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
ar1_q<-extract(ar1_q, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#print table for latex 
texreg(c(ar1, ar1_q), digits=9, custom.coef.names = c("Baseline", "Pre-Election Trend",  "Election Level Change", "Election Slope Change", "Pre-Election Trend^2", "Election Slope Change^2"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Unique Users Tweeting Hatespeech (Clinton Data)", stars = c(0.001,0.01, 0.05, 0.1), custom.gof.names = NULL, custom.coef.map = NULL)


###########
#Table A13#
###########

#run model on each disaggregated datasets
ar1<-lapply(trump_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})
#extract coefficients for table 
ar1<-lapply(ar1, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex
texreg(c(ar1$anti_asian_trump.csv, ar1$anti_black_trump.csv, ar1$anti_immigrant_trump.csv, ar1$anti_latino_trump.csv, ar1$anti_muslim_trump.csv, ar1$anti_semitic_trump.csv, ar1$homophobic_trump.csv, ar1$misogynistic_trump.csv, ar1$white_nationalist_trump.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Tweets Containing Hatespeech (Trump Data)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)


###########
#Table A14#
###########

#run model on each disaggregated datasets
ar1_q<-lapply(trump_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
#extract coefficients for table 
ar1_q<-lapply(ar1_q, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex 
texreg(c(ar1_q$anti_asian_trump.csv, ar1_q$anti_black_trump.csv, ar1_q$anti_immigrant_trump.csv, ar1_q$anti_latino_trump.csv, ar1_q$anti_muslim_trump.csv, ar1_q$anti_semitic_trump.csv, ar1_q$homophobic_trump.csv, ar1_q$misogynistic_trump.csv, ar1_q$white_nationalist_trump.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change", "Pre-Elelction Trend$^2$", "Election Slope Change$^2$"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Tweets Containing Hate Speech (Trump Dataset)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)

###########
#Table A15#
###########

#run models on aggregated dataset
ar1<-gls(norm_hate ~ date_num + election + slope_change, data=trump_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=trump_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
#extract coefficients for table 
ar1<-extract(ar1, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
ar1_q<-extract(ar1_q, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#print table for latex 
texreg(c(ar1, ar1_q), digits=9, custom.coef.names = c("Baseline", "Pre-Election Trend",  "Election Level Change", "Election Slope Change", "Pre-Election Trend^2", "Election Slope Change^2"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Tweets Containing Hatespeech (Trump Data)", stars = c(0.001,0.01, 0.05, 0.1), custom.gof.names = NULL, custom.coef.map = NULL)

###########
#Table A16#
###########

#run model on each disaggregated datasets
ar1<-lapply(trump_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})
#extract coefficients for table 
ar1<-lapply(ar1, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex
texreg(c(ar1$anti_asian_trump.csv, ar1$anti_black_trump.csv, ar1$anti_immigrant_trump.csv, ar1$anti_latino_trump.csv, ar1$anti_muslim_trump.csv, ar1$anti_semitic_trump.csv, ar1$homophobic_trump.csv, ar1$misogynistic_trump.csv, ar1$white_nationalist_trump.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Unique Users Tweeting Hatespeech (Trump Data)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)


###########
#Table A17#
###########

#run model on each disaggregated datasets
ar1_q<-lapply(trump_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
#extract coefficients for table 
ar1_q<-lapply(ar1_q, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex 
texreg(c(ar1_q$anti_asian_trump.csv, ar1_q$anti_black_trump.csv, ar1_q$anti_immigrant_trump.csv, ar1_q$anti_latino_trump.csv, ar1_q$anti_muslim_trump.csv, ar1_q$anti_semitic_trump.csv, ar1_q$homophobic_trump.csv, ar1_q$misogynistic_trump.csv, ar1_q$white_nationalist_trump.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change", "Pre-Elelction Trend$^2$", "Election Slope Change$^2$"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Unique Users Tweeting Hate Speech (Trump Dataset)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)

###########
#Table A18#
###########

#run models on aggregated dataset
ar1<-gls(norm_upd_hate ~ date_num + election + slope_change, data=trump_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_upd_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=trump_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
#extract coefficients for table 
ar1<-extract(ar1, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
ar1_q<-extract(ar1_q, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#print table for latex 
texreg(c(ar1, ar1_q), digits=9, custom.coef.names = c("Baseline", "Pre-Election Trend",  "Election Level Change", "Election Slope Change", "Pre-Election Trend^2", "Election Slope Change^2"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Unique Users Tweeting Hatespeech (Trump Data)", stars = c(0.001,0.01, 0.05, 0.1), custom.gof.names = NULL, custom.coef.map = NULL)


###########
#Table A19#
###########

#run model on each disaggregated datasets
ar1<-lapply(random_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})
#extract coefficients for table 
ar1<-lapply(ar1, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex
texreg(c(ar1$anti_asian_random.csv, ar1$anti_black_random.csv, ar1$anti_immigrant_random.csv, ar1$anti_latino_random.csv, ar1$anti_muslim_random.csv, ar1$anti_semitic_random.csv, ar1$homophobic_random.csv, ar1$misogynistic_random.csv, ar1$white_nationalist_random.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Tweets Containing Hatespeech (Random Sample Data)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)


###########
#Table A20#
###########

#run model on each disaggregated datasets
ar1_q<-lapply(random_list, function(x){
  gls(norm_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
#extract coefficients for table 
ar1_q<-lapply(ar1_q, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex 
texreg(c(ar1_q$anti_asian_random.csv, ar1_q$anti_black_random.csv, ar1_q$anti_immigrant_random.csv, ar1_q$anti_latino_random.csv, ar1_q$anti_muslim_random.csv, ar1_q$anti_semitic_random.csv, ar1_q$homophobic_random.csv, ar1_q$misogynistic_random.csv, ar1_q$white_nationalist_random.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change", "Pre-Elelction Trend$^2$", "Election Slope Change$^2$"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Tweets Containing Hate Speech (Random Sample Dataset)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)

###########
#Table A21#
###########

#run models on aggregated dataset
ar1<-gls(norm_hate ~ date_num + election + slope_change, data=random_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=random_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
#extract coefficients for table 
ar1<-extract(ar1, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
ar1_q<-extract(ar1_q, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#print table for latex 
texreg(c(ar1, ar1_q), digits=9, custom.coef.names = c("Baseline", "Pre-Election Trend",  "Election Level Change", "Election Slope Change", "Pre-Election Trend^2", "Election Slope Change^2"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Tweets Containing Hatespeech (Random Sample Data)", stars = c(0.001,0.01, 0.05, 0.1), custom.gof.names = NULL, custom.coef.map = NULL)

###########
#Table A22#
###########

#run model on each disaggregated datasets
ar1<-lapply(random_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))})
#extract coefficients for table 
ar1<-lapply(ar1, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex
texreg(c(ar1$anti_asian_random.csv, ar1$anti_black_random.csv, ar1$anti_immigrant_random.csv, ar1$anti_latino_random.csv, ar1$anti_muslim_random.csv, ar1$anti_semitic_random.csv, ar1$homophobic_random.csv, ar1$misogynistic_random.csv, ar1$white_nationalist_random.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Unique Users Tweeting Hatespeech (Random Sample Data)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)


###########
#Table A23#
###########

#run model on each disaggregated datasets
ar1_q<-lapply(random_list, function(x){
  gls(norm_upd_hate ~ date_num + election + slope_change + date_num2 + slope_change2, data=x, na.action = na.omit, correlation = corAR1(form = ~ 1))
})
#extract coefficients for table 
ar1_q<-lapply(ar1_q, function(x){
  extract(x, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
})
#print table for latex 
texreg(c(ar1_q$anti_asian_random.csv, ar1_q$anti_black_random.csv, ar1_q$anti_immigrant_random.csv, ar1_q$anti_latino_random.csv, ar1_q$anti_muslim_random.csv, ar1_q$anti_semitic_random.csv, ar1_q$homophobic_random.csv, ar1_q$misogynistic_random.csv, ar1_q$white_nationalist_random.csv), digits=12, ci.force=FALSE, custom.model.names=c("Anti-Asian", "Anti-Black", "Anti-Immigrant", "Anti-Latino", "Anti-Muslim", "Anti-Semitic", "Homophobic", "Misogynistic", "White Nationalist"), custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change", "Pre-Elelction Trend$^2$", "Election Slope Change$^2$"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Unique Users Tweeting Hate Speech (Random Sample Dataset)", stars = c(0.001,0.01, 0.05), custom.gof.names = NULL, custom.coef.map = NULL)

###########
#Table A24#
###########

#run models on aggregated dataset
ar1<-gls(norm_upd_hate ~ date_num + election + slope_change, data=random_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
ar1_q<-gls(norm_upd_hate ~ date_num  + election + slope_change + date_num2 + slope_change2, data=random_data_agg, na.action = na.omit, correlation = corAR1(form = ~ 1))
#extract coefficients for table 
ar1<-extract(ar1, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
ar1_q<-extract(ar1_q, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#print table for latex 
texreg(c(ar1, ar1_q), digits=9, custom.coef.names = c("Baseline", "Pre-Election Trend",  "Election Level Change", "Election Slope Change", "Pre-Election Trend^2", "Election Slope Change^2"), caption.above=TRUE, caption="Effect of Election on Daily Proportion of Unique Users Tweeting Hatespeech (Random Sample Data)", stars = c(0.001,0.01, 0.05, 0.1), custom.gof.names = NULL, custom.coef.map = NULL)





