#######################################################################
#Name of code file: non_dictionary_analysis.R

#Data In:non_dictionary_data.csv

#Data Out: Loess Plots & Interrupted Time Series Analysis Tables
#Figures 7a, 7b, 7c
#Tables A28-A30
######################################################################

#Load Packages
library(readr)
library(texreg)
library(ggplot2)
library(nlme)
library(scales)

#Set Working Directory
setwd("..")

#Read in Data 
data<-read_csv("data/non_dictionary_data.csv")

##########
#Figure 7#
##########

pdf("figures/figure_7a.pdf", height=7, width=11)
ggplot(data, aes(day, clinton, group=election)) +  geom_line() + geom_smooth(method="loess", se=TRUE, colour="blue", fill="blue")+  geom_vline(xintercept =as.numeric(as.Date("2016-11-08")), linetype=2) + labs(y="Daily Probability Tweets are Classified as Alt-Right", x = "Date")+ annotate("text", label="Election Day", x=as.Date("2016-11-08", origin = '2015-06-15'), y = Inf, vjust = 2, hjust=1, size=6) + scale_x_date(labels=date_format("%m-%Y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-15"), by="4 months")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text.y = element_text(size = rel(1.2)), axis.text.x = element_text( size = rel(1.2)), axis.line.x = element_line(color="black", size = .5), axis.line.y = element_line(color="black", size = .5), axis.title = element_text(size=16))+scale_y_continuous(limits=c(0, max(data$clinton)))
dev.off()


pdf("figures/figure_7b.pdf", height=7, width=11)
ggplot(data, aes(day, trump, group=election)) +  geom_line() + geom_smooth(method="loess", se=TRUE, colour="red", fill="red")+  geom_vline(xintercept =as.numeric(as.Date("2016-11-08")), linetype=2) + labs(y="Daily Probability Tweets are Classified as Alt-Right", x = "Date")+ annotate("text", label="Election Day", x=as.Date("2016-11-08", origin = '2015-06-15'), y = Inf, vjust = 2, hjust=1, size=6) + scale_x_date(labels=date_format("%m-%Y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-15"), by="4 months")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text.y = element_text(size = rel(1.2)), axis.text.x = element_text( size = rel(1.2)), axis.line.x = element_line(color="black", size = .5), axis.line.y = element_line(color="black", size = .5), axis.title = element_text(size=16))+scale_y_continuous(limits=c(0, max(data$trump)))
dev.off()

pdf("figures/figure_7c.pdf", height=7, width=11)
ggplot(na.omit(data), aes(day, random, group=election)) +  geom_line() + geom_smooth(method="loess", se=TRUE, colour="black", fill="black")+  geom_vline(xintercept =as.numeric(as.Date("2016-11-08")), linetype=2) + labs(y="Daily Probability Tweets are Classified as Alt-Right", x = "Date")+ annotate("text", label="Election Day", x=as.Date("2016-11-08", origin = '2015-06-15'), y = Inf, vjust = 2, hjust=1, size=6) + scale_x_date(labels=date_format("%m-%Y"), breaks=seq(as.Date("2015-06-01"), as.Date("2017-06-15"), by="4 months")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text.y = element_text(size = rel(1.2)), axis.text.x = element_text( size = rel(1.2)), axis.line.x = element_line(color="black", size = .5), axis.line.y = element_line(color="black", size = .5), axis.title = element_text(size=16))+scale_y_continuous(limits=c(0, max(na.omit(data$random))))
dev.off()

###########
#Table A28#
###########

ar1<-gls(clinton ~ date_num + election + slope_change, data=data, correlation = corAR1(form = ~ 1))
ar1_total<-extract(ar1, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#quadratic model
ar1_q<-gls(clinton ~ date_num +date_num2 + election + slope_change + slope_change2, data=data, correlation = corAR1(form = ~ 1))
ar1_total_q<-extract(ar1_q, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#output for latex
texreg(c(ar1_total, ar1_total_q), digits=8, ci.force=FALSE, custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change", "Pre-Election Trend ^2", "Election Slope Change ^2"), caption.above=TRUE, caption="Effect of Election on Alt-Right Reddit Similarity (Clinton Data)", stars = c(0.001,0.01, 0.05, 0.1), custom.gof.names = NULL, custom.coef.map = NULL)

###########
#Table A29#
###########

#linear model
ar1<-gls(trump ~ date_num + election + slope_change, data=data, correlation = corAR1(form = ~ 1))
ar1_total<-extract(ar1, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#quadratic model
ar1_q<-gls(trump ~ date_num +date_num2 + election + slope_change + slope_change2, data=data, correlation = corAR1(form = ~ 1))
ar1_total_q<-extract(ar1_q, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#output for latex
texreg(c(ar1_total, ar1_total_q), digits=8, ci.force=FALSE, custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change", "Pre-Election Trend ^2", "Election Slope Change ^2"), caption.above=TRUE, caption="Effect of Election on Alt-Right Reddit Similarity (Trump Data)", stars = c(0.001,0.01, 0.05, 0.1), custom.gof.names = NULL, custom.coef.map = NULL)

###########
#Table A30#
###########

#linear model
ar1<-gls(random ~ date_num + election + slope_change, data=na.omit(data), correlation = corAR1(form = ~ 1))
ar1_total<-extract(ar1, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#quadratic model
ar1_q<-gls(random ~ date_num +date_num2 + election + slope_change + slope_change2, data=na.omit(data), correlation = corAR1(form = ~ 1))
ar1_total_q<-extract(ar1_q, include.aic = TRUE, include.bic = TRUE, include.loglik = TRUE,  include.nobs = TRUE)
#output for latex
texreg(c(ar1_total, ar1_total_q), digits=8, ci.force=FALSE, custom.coef.names = c("Baseline", "Pre-Election Trend", "Election Level Change", "Election Slope Change", "Pre-Election Trend ^2", "Election Slope Change ^2"), caption.above=TRUE, caption="Effect of Election on Alt-Right Reddit Similarity (Random Sample Data)", stars = c(0.001,0.01, 0.05, 0.1), custom.gof.names = NULL, custom.coef.map = NULL)





