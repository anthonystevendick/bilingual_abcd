#Updated 4/16/19 by Anthony Dick

rm(list=ls())
setwd("<path to data")
list.files()

####################
####################
## Load libraries ##
####################
####################

library(ggplot2)
library(gamm4)
library(MASS)
library(car)
library(plyr)
library(TOSTER)
library(forcats)
library(QuantPsyc)
library(data.table)
library(reshape2)
library(psych)

options(max.print=10000)

###################################
###################################
## Load and manipulate ABCD data ##
###################################
###################################
###

###################################
##Data sets and variables used

#This analysis begins after merging of data, recovery of categorical variables,
#imputation of missing core demographic variables, and recoding of core demographic variables. The
#scripts to conduct this preliminary analysis are available at https://github.com/ABCD-STUDY/analysis-nda17.
#Data from the ABCD Youth Acculturation Survey were unmodified, except as described below.

#Variables used in this study:
#
#completedData$src_subject_id
#completedData$age
#completedData$female
#completedData$household.income
#completedData$high.educ
#completedData$married
#completedData$race.ethnicity
#nda17$demo_origin_v2
#nda17$demo_prnt_16
#nda17$demo_biofather_v2
#nda17$demo_biomother_v2
#nda17$nihtbx_reading_uncorrected
#nda17$nihtbx_picture_uncorrected
#nda17$nihtbx_list_uncorrected
#nda17$nihtbx_pattern_uncorrected
#nda17$nihtbx_picvocab_uncorrected
#nda17$nihtbx_flanker_uncorrected
#nda17$nihtbx_cardsort_uncorrected
#nda17$beh_sst_ssrt_mean_total
#nda17$abcd_site
#nda17$rel_family_id
#nda17$accult_q1_y
#nda17$accult_q2_y
#nda17$accult_q3_dropdwn_y
#nda17$accult_q3_other_y
#nda17$accult_q4_y
#nda17$accult_q5_y

#Data about bilingualism are taken from the Youth Acculturation Survey. The original coding is below:
#accult_q1_y: "How well do you speak English?" 1, Poor | 2, Fair | 3, Good | 4, Excellent
#accult_q2_y: "Besides English, do you speak or understand another language or dialect?"
#accult_q3_other_y and accult_q3_dropdown_y: "What other language or dialect do you speak or understand (besides English)?"
#accult_q4_y: "What language do you speak with most of your friends?" 1, (<em>Other language</em>) all the time | 2, (<em>Other language</em>) most of the time | 3, (<em>Other language</em>) and English equally | 4, English most of the time | 5, English all the time
#accult_q5_y: "What language do you speak with most of your family?" 1, (<em>Other language</em>) all the time | 2, (<em>Other language</em>) most of the time | 5, (<em>Other language</em>) and English equally | 4, English most of the time | 5, English all the time

nda17 =  readRDS("nda17.Rds")
attach(nda17)
completedData =  readRDS("completedData.Rds")

#recode some variables to reflect the original data scale, to deal with NAs, and to recode answers that are clear coding mistakes

accult_q1_y <- factor(ifelse(nda17$accult_q1_y == "", NA, ifelse(nda17$accult_q1_y == "Poor", 0, ifelse(nda17$accult_q1_y == "Fair", 1,ifelse(nda17$accult_q1_y == "Good", 2, ifelse(nda17$accult_q1_y == "Excellent", 3, NA))))))
accult_q2_y <- factor(ifelse(nda17$accult_q2_y == "", NA, ifelse(nda17$accult_q2_y == "no", 0, ifelse(nda17$accult_q2_y == "yes", 1,ifelse(nda17$accult_q2_y == "decline to answer", NA, ifelse(nda17$accult_q2_y == "refuse to answer", NA, NA))))))
accult_q4_y <- factor(ifelse(nda17$accult_q4_y == "", NA, ifelse(nda17$accult_q4_y == "(Other language) all the time", 1, ifelse(nda17$accult_q4_y == "(Other language) most of the time", 2,ifelse(nda17$accult_q4_y == "(Other language) and English equally", 3, ifelse(nda17$accult_q4_y == "English most of the time", 4, ifelse(nda17$accult_q4_y == "English all the time", 5, NA)))))))
accult_q5_y <- factor(ifelse(nda17$accult_q5_y == "", NA, ifelse(nda17$accult_q5_y == "(Other language) all the time", 1, ifelse(nda17$accult_q5_y == "(Other language) most of the time", 2,ifelse(nda17$accult_q5_y == "(Other language) and English equally", 3, ifelse(nda17$accult_q5_y == "English most of the time", 4, ifelse(nda17$accult_q5_y == "English all the time", 5, NA)))))))
accult_q2_y[which(accult_q3_other_y=="Pig Latin")]<-"0"
accult_q2_y[which(accult_q3_other_y=="A little bit")]<-"0"
accult_q2_y[which(accult_q3_other_y=="British")]<-"0"
accult_q2_y[which(accult_q3_other_y=="English")]<-"0"
SSRTr<-(beh_sst_ssrt_mean_total) * -1 #Stop-Signal reaction time, recode: recode/reverse score SSRT (make lower SSRT mean poorer performance)

#Missing Data Summary

sum(is.na(nda17$age))
sum(is.na(nda17$female))
sum(is.na(nda17$household.income))
sum(is.na(nda17$high.educ))
sum(is.na(nda17$married))
sum(is.na(nda17$race.ethnicity))
sum(is.na(nda17$nihtbx_reading_uncorrected))
sum(is.na(nda17$nihtbx_picture_uncorrected))
sum(is.na(nda17$nihtbx_list_uncorrected))
sum(is.na(nda17$nihtbx_pattern_uncorrected))
sum(is.na(nda17$nihtbx_picvocab_uncorrected))
sum(is.na(nda17$nihtbx_flanker_uncorrected))
sum(is.na(nda17$nihtbx_cardsort_uncorrected))
sum(is.na(nda17$beh_sst_ssrt_mean_total))
sum(is.na(nda17$abcd_site))
sum(is.na(nda17$rel_family_id))

#predictor independent variables (i.e., the ultimate outcome variables of interest)

predictor_iv = c(which(names(nda17)=="nihtbx_picvocab_uncorrected"),which(names(nda17)=="nihtbx_flanker_uncorrected"),which(names(nda17)=="nihtbx_cardsort_uncorrected"), which(names(nda17)=="beh_sst_ssrt_mean_total"))
for(j in 1:length(predictor_iv)) nda17[,predictor_iv[j]] = scale(as.numeric(nda17[,predictor_iv[j]])) # standardize the DV to get standardized betas

#outcome variables, demographic variables with non-zero missing cases

outcome_dv = c(which(names(nda17)=="race.ethnicity"), which(names(nda17)=="high.educ"), which(names(nda17)=="household.income"))

pause = function()
{
    if (interactive())
    {
        invisible(readline(prompt = "Press <Enter> to continue..."))
    }
    else
    {
        cat("Press <Enter> to continue...")
        invisible(readLines(file("stdin"), 1))
    }
}
for(i in 1:length(outcome_dv)){
print(i)
	for(j in 1:length(predictor_iv)){
	print(j)
		pred<-paste(names(nda17)[predictor_iv[j]],"-->",names(nda17)[outcome_dv[i]])
		print(pred)
		form = paste("as.numeric(","is.na(",names(nda17)[outcome_dv[i]],")",")","~" ,names(nda17)[predictor_iv[j]], sep = "")
		print(form)
		log.mod<-glm(form, family = binomial, data = nda17)
    print(summary(log.mod))
	}
}

###End Missing Data Summary############
#######################################

#create a smaller data frame that is easier to manage

abcd_subset<-data.frame(completedData$src_subject_id, as.numeric(nda17$nihtbx_reading_uncorrected), as.numeric(nda17$nihtbx_picture_uncorrected), as.numeric(nda17$nihtbx_list_uncorrected),
as.numeric(nda17$nihtbx_pattern_uncorrected), as.numeric(nda17$nihtbx_cryst_uncorrected), as.numeric(nda17$nihtbx_fluidcomp_uncorrected), completedData$age, completedData$female,
completedData$race.ethnicity, completedData$high.educ, completedData$married, completedData$household.income, as.numeric(nda17$nihtbx_picvocab_uncorrected), as.numeric(nda17$nihtbx_flanker_uncorrected),
as.numeric(nda17$nihtbx_cardsort_uncorrected), as.numeric(nda17$beh_sst_ssrt_mean_total), SSRTr, nda17$abcd_site, nda17$rel_family_id, accult_q1_y, accult_q2_y, nda17$accult_q3_dropdwn_y, nda17$accult_q3_other_y, accult_q4_y, accult_q5_y)

colnames(abcd_subset)<-c("src_subject_id", "nihtbx_reading_uncorrected", "nihtbx_picture_uncorrected", "nihtbx_list_uncorrected", "nihtbx_pattern_uncorrected",
"nihtbx_cryst_uncorrected", "nihtbx_fluidcomp_uncorrected", "age", "female", "race.ethnicity", "high.educ", "married", "household.income",
"nihtbx_picvocab_uncorrected", "nihtbx_flanker_uncorrected", "nihtbx_cardsort_uncorrected", "beh_sst_ssrt_mean_total", "SSRTr", "abcd_site","rel_family_id","accult_q1_y","accult_q2_y",
"accult_q3_dropdwn_y","accult_q3_other_y","accult_q4_y","accult_q5_y")

#saveRDS(abcd_subset, "abcd_subset_7_8_18.Rds") # save this dataframe for future use
#abcd_subset<-readRDS("abcd_subset_7_8_18.Rds") # load the dataframe

#summarize data

names(abcd_subset)
dim(abcd_subset)
attach(abcd_subset)
Hmisc::describe(abcd_subset)

count(accult_q1_y)
count(accult_q2_y)
count(accult_q3_other_y)
count(accult_q3_dropdwn_y)
count(accult_q4_y)
count(accult_q5_y)
count(nda17$demo_origin_v2)
count(nda17$demo_prnt_16)
count(nda17$demo_biofather_v2) # 814  0.1799293
count(nda17$demo_biomother_v2) # 540  0.1193634

#recode the accult_q2_y variable into a binary "Bilingual Status", 0 = not bilingual; 1 = bilingual

bilingual_status <- accult_q2_y
sum(is.na(bilingual_status))

#dimension a 'bilingual degree' variable, where 1 = participant said they were bilingual, and they speak the other language with friends all the time, most of the time,
#or equally, OR they speak the other language with family all the time, most of the time, or equally.

bilingual_degree <- ifelse(bilingual_status == 0, 0, ifelse(bilingual_status == 1 & (as.numeric(accult_q4_y) <= 3 | as.numeric(accult_q5_y) <= 3), 1, NA))

count(bilingual_degree) #check the data
sum(is.na(bilingual_degree))

#dimension a continuous 'bilingual use' variable, and reverse-score so that if participants speak the other language with friends all the time, most of the time...,
#they will receive high scores on this measure (range 0-8, with 8 indicating a high-degree of other language use)

bilingual_use<-10-(as.numeric(abcd_subset$accult_q4_y)+as.numeric(abcd_subset$accult_q5_y))
sum(is.na(bilingual_use))

count(bilingual_use) # check the data
for.hist.use <- melt(bilingual_use)
tiff("figure_use.tiff", units = 'in', width = 12, height = 10, res = 300, compression = "lzw")
par(mar = c(5, 5, 8, 8), xpd=FALSE) #set figure boundaries
ggplot(for.hist.use,aes(x = for.hist.use$value)) + theme_bw() + geom_histogram(
    col = "grey", bins = 17,
    fill="light blue") +
    labs(x="Score", y="Frequency") +
    ggtitle("Frequency of Scores for Bilingual Use Variable") +
  	theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20), axis.text = element_text(size = 14, colour = "black"))
dev.off()

#compute new IQ measure

fluidIQ<-scale((nihtbx_picture_uncorrected+nihtbx_list_uncorrected+nihtbx_pattern_uncorrected)/3)
hist(fluidIQ)
sum(is.na(fluidIQ))

###################################################
###################################################
##Select Variables for OLS and Multilevel Models###
###################################################
###################################################

abcd_subset<-cbind.data.frame(abcd_subset, fluidIQ, bilingual_status, bilingual_degree, bilingual_use) # create a new data frame
#abcd_subset.noimp<-cbind.data.frame(abcd_subset, fluidIQ, bilingual_status, bilingual_degree, bilingual_use) # create a new data frame

#Descriptive Statistics by bilingual grouping
stat_vocab<-ddply(abcd_subset,~bilingual_status,summarise,mean=mean(nihtbx_picvocab_uncorrected, na.rm = TRUE),sd=sd(nihtbx_picvocab_uncorrected, na.rm = TRUE))
stat_flanker<-ddply(abcd_subset,~bilingual_status,summarise,mean=mean(nihtbx_flanker_uncorrected, na.rm = TRUE),sd=sd(nihtbx_flanker_uncorrected, na.rm = TRUE))
stat_card<-ddply(abcd_subset,~bilingual_status,summarise,mean=mean(nihtbx_cardsort_uncorrected, na.rm = TRUE),sd=sd(nihtbx_cardsort_uncorrected, na.rm = TRUE))
stat_SST<-ddply(abcd_subset,~bilingual_status,summarise,mean=mean(SSRTr, na.rm = TRUE),sd=sd(SSRTr, na.rm = TRUE))

degree_vocab<-ddply(abcd_subset,~bilingual_degree,summarise,mean=mean(nihtbx_picvocab_uncorrected, na.rm = TRUE),sd=sd(nihtbx_picvocab_uncorrected, na.rm = TRUE))
degree_flanker<-ddply(abcd_subset,~bilingual_degree,summarise,mean=mean(nihtbx_flanker_uncorrected, na.rm = TRUE),sd=sd(nihtbx_flanker_uncorrected, na.rm = TRUE))
degree_card<-ddply(abcd_subset,~bilingual_degree,summarise,mean=mean(nihtbx_cardsort_uncorrected, na.rm = TRUE),sd=sd(nihtbx_cardsort_uncorrected, na.rm = TRUE))
degree_SST<-ddply(abcd_subset,~bilingual_degree,summarise,mean=mean(SSRTr, na.rm = TRUE),sd=sd(SSRTr, na.rm = TRUE))

use_vocab<-ddply(abcd_subset,~bilingual_use,summarise,mean=mean(nihtbx_picvocab_uncorrected, na.rm = TRUE),sd=sd(nihtbx_picvocab_uncorrected, na.rm = TRUE))
use_flanker<-ddply(abcd_subset,~bilingual_use,summarise,mean=mean(nihtbx_flanker_uncorrected, na.rm = TRUE),sd=sd(nihtbx_flanker_uncorrected, na.rm = TRUE))
use_card<-ddply(abcd_subset,~bilingual_use,summarise,mean=mean(nihtbx_cardsort_uncorrected, na.rm = TRUE),sd=sd(nihtbx_cardsort_uncorrected, na.rm = TRUE))
use_SST<-ddply(abcd_subset,~bilingual_use,summarise,mean=mean(SSRTr, na.rm = TRUE),sd=sd(SSRTr, na.rm = TRUE))

col1<-c(round(stat_vocab$mean[1], 1), round(stat_flanker$mean[1], 1), round(stat_card$mean[1], 1), round(stat_SST$mean[1], 1),round(degree_vocab$mean[1], 1), round(degree_flanker$mean[1], 1), round(degree_card$mean[1], 1), round(degree_SST$mean[1], 1))
col2<-c(round(stat_vocab$sd[1], 1), round(stat_flanker$sd[1], 1), round(stat_card$sd[1], 1), round(stat_SST$sd[1], 1),round(degree_vocab$sd[1], 1), round(degree_flanker$sd[1], 1), round(degree_card$sd[1], 1), round(degree_SST$sd[1], 1))
col3<-c(round(stat_vocab$mean[2], 1), round(stat_flanker$mean[2], 1), round(stat_card$mean[2], 1), round(stat_SST$mean[2], 1),round(degree_vocab$mean[2], 1), round(degree_flanker$mean[2], 1), round(degree_card$mean[2], 1), round(degree_SST$mean[2], 1))
col4<-c(round(stat_vocab$sd[2], 1), round(stat_flanker$sd[2], 1), round(stat_card$sd[2], 1), round(stat_SST$sd[2], 1),round(degree_vocab$sd[2], 1), round(degree_flanker$sd[2], 1), round(degree_card$sd[2], 1), round(degree_SST$sd[2], 1))
descr.m<-data.frame(col1,col2,col3,col4)
descr.use.m<-data.frame(use_vocab, use_flanker, use_card, use_SST)

colnames(descr.m)<-c("Mean Monolingual","Standard Deviation Monolingual", "Mean Bilingual", "Standard Deviation Bilingual")
write.table(descr.m, file = "descriptives.csv", sep = ",")


###### Select covariates for model  ######

##Vocabulary Only##

ind_cov = c(which(names(abcd_subset)=="nihtbx_picvocab_uncorrected"))
names(abcd_subset)[ind_cov]
summary(abcd_subset[,ind_cov])

###### Select demographic covariates for model  ######

ind_cov = c(which(names(abcd_subset)=="age"), which(names(abcd_subset)=="female"), which(names(abcd_subset)=="race.ethnicity"), which(names(abcd_subset)=="high.educ"), which(names(abcd_subset)=="married"), which(names(abcd_subset)=="household.income"))
names(abcd_subset)[ind_cov]
summary(abcd_subset[,ind_cov])

###### Select covariates for model, including IQ  ######

ind_cov = c(which(names(abcd_subset)=="nihtbx_reading_uncorrected"), which(names(abcd_subset)=="fluidIQ"), which(names(abcd_subset)=="age"),which(names(abcd_subset)=="female"),which(names(abcd_subset)=="race.thnicity"), which(names(abcd_subset)=="high.educ"),which(names(abcd_subset)=="married"),which(names(abcd_subset)=="household.income"))

## Select covariates for model, including Picture Vocabulary and IQ

ind_cov = c(which(names(abcd_subset)=="nihtbx_reading_uncorrected"), which(names(abcd_subset)=="fluidIQ"), which(names(abcd_subset)=="nihtbx_picvocab_uncorrected"), which(names(abcd_subset)=="age"),which(names(abcd_subset)=="female"),which(names(abcd_subset)=="race.thnicity"), which(names(abcd_subset)=="high.educ"),which(names(abcd_subset)=="married"),which(names(abcd_subset)=="household.income"))

## Select nesting variables (doesn't change)

ind_nest = c(which(names(abcd_subset)=="abcd_site"),which(names(abcd_subset)=="rel_family_id"));names(abcd_subset)[ind_nest]

###### Select DVs  ######

ind_dv = c(which(names(abcd_subset)=="nihtbx_picvocab_uncorrected"),which(names(abcd_subset)=="nihtbx_flanker_uncorrected"),which(names(abcd_subset)=="nihtbx_cardsort_uncorrected"), which(names(abcd_subset)=="SSRTr"))

#ind_dv = c(which(names(abcd_subset)=="nihtbx_flanker_uncorrected"),which(names(abcd_subset)=="nihtbx_cardsort_uncorrected"), which(names(abcd_subset)=="SSRTr"))

abcd_subset_scale<-abcd_subset
for(j in 1:length(ind_dv)) abcd_subset_scale[,ind_dv[j]] = scale(as.numeric(abcd_subset_scale[,ind_dv[j]])) # standardize the DV to get standardized betas

names(abcd_subset)[ind_dv]
boxplot(abcd_subset[ind_dv])
summary(abcd_subset[ind_dv])

###### Select IVs  ######

ind_iv = c(which(names(abcd_subset)=="bilingual_status"), which(names(abcd_subset)=="bilingual_degree"), which(names(abcd_subset)=="bilingual_use"))

for(k in 1:length(ind_iv)) abcd_subset_scale[,ind_iv[k]] = scale(as.numeric(abcd_subset_scale[,ind_iv[k]])) # standardize the IV to get standardized betas

names(abcd_subset)[ind_iv]
summary(abcd_subset[,ind_iv])


####################################
####################################
## Run OLS and Multilevel Models  ##
####################################
####################################

#run once with original units, once with standardized units

pause = function()
{
    if (interactive())
    {
        invisible(readline(prompt = "Press <Enter> to continue..."))
    }
    else
    {
        cat("Press <Enter> to continue...")
        invisible(readLines(file("stdin"), 1))
    }
}
m <- matrix(nrow=42, ncol=15) # set the output matrix size to hold the results
mat_row = 34 # to fill out the m matrix, start at row 1, row 10, row 22, row 34  when running with different covariates
for(i in 1:length(ind_dv)){
print(i)
	for(j in 1:length(ind_iv)){
	print(j)
		pred<-paste(names(abcd_subset)[ind_iv[j]],"-->",names(abcd_subset)[ind_dv[i]])
		print(pred)
		form = paste(names(abcd_subset)[ind_dv[i]],"~" ,names(abcd_subset)[ind_iv[j]])
		form.simple = formula(form)
			for(k in 1:length(ind_cov)){
				form = paste(form,"+",names(abcd_subset)[ind_cov[k]])
					}
		print(form)
		form = formula(form)
		ran = paste("~(1|",names(abcd_subset)[ind_nest[1]])
			for(k in 2:length(ind_nest)){
				ran = paste(ran,"/",names(abcd_subset)[ind_nest[k]])
					}
		ran = paste(ran,")")
		ran = formula(ran)
			mod0 = gamm4(formula = form.simple, random = ran, data = abcd_subset)
			print(summary(mod0$gam))
			mod1 = gamm4(formula = form.simple, random = ran, data = abcd_subset_scale)
			#print(summary(mod1$gam))
			#pause()
			gamm0 = gamm4(formula = form, random = ran, data = abcd_subset)
			#pause()
			print(summary(gamm0$gam))
			gamm1 = gamm4(formula = form, random = ran, data = abcd_subset_scale)
			#pause()
			print(summary(gamm1$gam))
			output<-c(pred,mod0$gam$df.residual, summary(mod0$gam)$p.coeff[2],summary(mod0$gam)$se[2],summary(mod1$gam)$p.coeff[2],summary(mod1$gam)$se[2], summary(mod0$gam)$p.t[2],summary(mod0$gam)$p.pv[2], gamm0$gam$df.residual, summary(gamm0$gam)$p.coeff[2],summary(gamm0$gam)$se[2], summary(gamm1$gam)$p.coeff[2], summary(gamm1$gam)$se[2], summary(gamm0$gam)$p.t[2],summary(gamm0$gam)$p.pv[2])
			m[mat_row,]<-output # place the output into the matrix that was dimensioned above
			mat_row = mat_row + 1 # step to the next matrix row
	}
}
colnames(m)<-c("prediction","mod0_df","nocov_b","nocov_se","nocov_beta","nocov_se_beta","nocov_tval","nocov_pval","gamm_df", "gamm_b","gamm_se","gamm_beta","gamm_se_beta","gamm_tval","gamm_pval")
print(m)
write.table(m,file="m_bvals_11_27_18.txt")
#m<-read.csv(file="m_bvals_11_15_18.csv")

################################################
################################################
## Two-One-Sided Tests of Equivalence (TOSTs) ##
################################################
################################################

## function below uses regression slopes not raw data

# b1 is the regression coefficient for group 1
# b2 is the regression coefficient for group 2
# se1 is the standard error of the regression coefficient for group 1
# se2 is the standard error of the regression coefficient for group 2
# equiv_int is the equivalence interval

equivbs<-function(b1,b2,se1,se2,equiv_int,alpha=.05){
	p.value<-pnorm((abs(b1-b2)-equiv_int)/(sqrt(se1^2+se2^2)))-pnorm((-abs(b1-b2)-equiv_int)/(sqrt(se1^2+se2^2)))
	upper<-(b1-b2)+qnorm(alpha)*(sqrt(se1^2+se2^2))
	lower<-(b1-b2)-qnorm(alpha)*(sqrt(se1^2+se2^2))
		if (lower<upper) {
			lower2<-lower
			upper2<-upper
			}
		if(lower>upper){
			lower2<-upper
			upper2<-lower
			}
	CI<-c(lower2,upper2)
	ifelse (p.value<=alpha, decision<-"The null hypothesis that the difference between the groups' regression coefficients falls outside of the equivalence interval can be rejected.The groups' regression coefficients can be considered equivalent", decision<-"The null hypothesis that the difference between the groups' regression coefficients falls outside of the equivalence interval cannot be rejected.The groups' regression coefficients can NOT be considered equivalent.")
	out<-list( p.value, CI,decision)
	names(out)<-c("P value", "CI", "Decision")
print(out)
}


y<-rev(c(NA, NA, as.numeric(m[10,4]),
as.numeric(m[11,4]),
as.numeric(m[12,4]),
NA,
as.numeric(m[13,4]),
as.numeric(m[14,4]),
as.numeric(m[15,4]),
NA,
as.numeric(m[16,4]),
as.numeric(m[17,4]),
as.numeric(m[18,4]),
NA,
as.numeric(m[19,4]),
as.numeric(m[20,4]),
as.numeric(m[21,4]),
NA, NA, NA,
as.numeric(m[1,10]),
as.numeric(m[2,10]),
as.numeric(m[3,10]),
NA,
as.numeric(m[4,10]),
as.numeric(m[5,10]),
as.numeric(m[6,10]),
NA,
as.numeric(m[7,10]),
as.numeric(m[8,10]),
as.numeric(m[9,10]),
NA, NA, NA,
as.numeric(m[10,10]),
as.numeric(m[11,10]),
as.numeric(m[12,10]),
NA,
as.numeric(m[13,10]),
as.numeric(m[14,10]),
as.numeric(m[15,10]),
NA,
as.numeric(m[16,10]),
as.numeric(m[17,10]),
as.numeric(m[18,10]),
NA,
as.numeric(m[19,10]),
as.numeric(m[20,10]),
as.numeric(m[21,10]),
NA, NA, NA,
as.numeric(m[22,10]),
as.numeric(m[23,10]),
as.numeric(m[24,10]),
NA,
as.numeric(m[25,10]),
as.numeric(m[26,10]),
as.numeric(m[27,10]),
NA,
as.numeric(m[28,10]),
as.numeric(m[29,10]),
as.numeric(m[30,10]),
NA,
as.numeric(m[31,10]),
as.numeric(m[32,10]),
as.numeric(m[33,10]),
NA, NA, NA,
as.numeric(m[34,10]),
as.numeric(m[35,10]),
as.numeric(m[36,10]),
NA,
as.numeric(m[37,10]),
as.numeric(m[38,10]),
as.numeric(m[39,10]),
NA,
as.numeric(m[40,10]),
as.numeric(m[41,10]),
as.numeric(m[42,10])))


ylo<-rev(c(NA, NA, equivbs(as.numeric(m[10,4]),0,as.numeric(m[10,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[11,4]),0,as.numeric(m[11,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[12,4]),0,as.numeric(m[12,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[13,4]),0,as.numeric(m[13,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[14,4]),0,as.numeric(m[14,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[15,4]),0,as.numeric(m[15,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[16,4]),0,as.numeric(m[16,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[17,4]),0,as.numeric(m[17,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[18,4]),0,as.numeric(m[18,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[19,4]),0,as.numeric(m[19,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[20,4]),0,as.numeric(m[20,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[21,4]),0,as.numeric(m[21,5]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA, NA, NA,
equivbs(as.numeric(m[1,10]),0,as.numeric(m[1,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[2,10]),0,as.numeric(m[2,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[3,10]),0,as.numeric(m[3,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[4,10]),0,as.numeric(m[4,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[5,10]),0,as.numeric(m[5,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[6,10]),0,as.numeric(m[6,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[7,10]),0,as.numeric(m[7,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[8,10]),0,as.numeric(m[8,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[9,10]),0,as.numeric(m[9,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA, NA, NA,
equivbs(as.numeric(m[10,10]),0,as.numeric(m[10,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[11,10]),0,as.numeric(m[11,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[12,10]),0,as.numeric(m[12,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[13,10]),0,as.numeric(m[13,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[14,10]),0,as.numeric(m[14,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[15,10]),0,as.numeric(m[15,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[16,10]),0,as.numeric(m[16,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[17,10]),0,as.numeric(m[17,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[18,10]),0,as.numeric(m[18,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[19,10]),0,as.numeric(m[19,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[20,10]),0,as.numeric(m[20,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[21,10]),0,as.numeric(m[21,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA, NA, NA,
equivbs(as.numeric(m[22,10]),0,as.numeric(m[22,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[23,10]),0,as.numeric(m[23,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[24,10]),0,as.numeric(m[24,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[25,10]),0,as.numeric(m[25,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[26,10]),0,as.numeric(m[26,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[27,10]),0,as.numeric(m[27,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[28,10]),0,as.numeric(m[28,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[29,10]),0,as.numeric(m[29,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[30,10]),0,as.numeric(m[30,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[31,10]),0,as.numeric(m[31,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[32,10]),0,as.numeric(m[32,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[33,10]),0,as.numeric(m[33,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA, NA, NA,
equivbs(as.numeric(m[34,10]),0,as.numeric(m[34,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[35,10]),0,as.numeric(m[35,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[36,10]),0,as.numeric(m[36,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[37,10]),0,as.numeric(m[37,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[38,10]),0,as.numeric(m[38,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[39,10]),0,as.numeric(m[39,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
NA,
equivbs(as.numeric(m[40,10]),0,as.numeric(m[40,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[41,10]),0,as.numeric(m[41,11]),0,c(-.1, .1),alpha = .05)$`CI`[1],
equivbs(as.numeric(m[42,10]),0,as.numeric(m[42,11]),0,c(-.1, .1),alpha = .05)$`CI`[1]))


yhi<-rev(c(NA, NA, equivbs(as.numeric(m[10,4]),0,as.numeric(m[10,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[11,4]),0,as.numeric(m[11,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[12,4]),0,as.numeric(m[12,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[13,4]),0,as.numeric(m[13,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[14,4]),0,as.numeric(m[14,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[15,4]),0,as.numeric(m[15,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[16,4]),0,as.numeric(m[16,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[17,4]),0,as.numeric(m[17,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[18,4]),0,as.numeric(m[18,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[19,4]),0,as.numeric(m[19,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[20,4]),0,as.numeric(m[20,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[21,4]),0,as.numeric(m[21,5]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA, NA, NA,
equivbs(as.numeric(m[1,10]),0,as.numeric(m[1,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[2,10]),0,as.numeric(m[2,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[3,10]),0,as.numeric(m[3,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[4,10]),0,as.numeric(m[4,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[5,10]),0,as.numeric(m[5,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[6,10]),0,as.numeric(m[6,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[7,10]),0,as.numeric(m[7,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[8,10]),0,as.numeric(m[8,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[9,10]),0,as.numeric(m[9,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA, NA, NA,
equivbs(as.numeric(m[10,10]),0,as.numeric(m[10,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[11,10]),0,as.numeric(m[11,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[12,10]),0,as.numeric(m[12,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[13,10]),0,as.numeric(m[13,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[14,10]),0,as.numeric(m[14,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[15,10]),0,as.numeric(m[15,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[16,10]),0,as.numeric(m[16,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[17,10]),0,as.numeric(m[17,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[18,10]),0,as.numeric(m[18,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[19,10]),0,as.numeric(m[19,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[20,10]),0,as.numeric(m[20,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[21,10]),0,as.numeric(m[21,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA, NA, NA,
equivbs(as.numeric(m[22,10]),0,as.numeric(m[22,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[23,10]),0,as.numeric(m[23,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[24,10]),0,as.numeric(m[24,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[25,10]),0,as.numeric(m[25,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[26,10]),0,as.numeric(m[26,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[27,10]),0,as.numeric(m[27,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[28,10]),0,as.numeric(m[28,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[29,10]),0,as.numeric(m[29,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[30,10]),0,as.numeric(m[30,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[31,10]),0,as.numeric(m[31,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[32,10]),0,as.numeric(m[32,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[33,10]),0,as.numeric(m[33,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA, NA, NA,
equivbs(as.numeric(m[34,10]),0,as.numeric(m[34,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[35,10]),0,as.numeric(m[35,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[36,10]),0,as.numeric(m[36,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[37,10]),0,as.numeric(m[37,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[38,10]),0,as.numeric(m[38,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[39,10]),0,as.numeric(m[39,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
NA,
equivbs(as.numeric(m[40,10]),0,as.numeric(m[40,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[41,10]),0,as.numeric(m[41,11]),0,c(-.1, .1),alpha = .05)$`CI`[2],
equivbs(as.numeric(m[42,10]),0,as.numeric(m[42,11]),0,c(-.1, .1),alpha = .05)$`CI`[2]))

x<-as_factor(c("Bilingual Use -> SSRTa", "Bilingual Degree -> SSRTa", "Bilingual Status -> SSRTa", "",
"Bilingual Use -> Card Sorta", "Bilingual Degree -> Card Sorta", "Bilingual Status -> Card Sorta", " ",
"Bilingual Use -> Flankera", "Bilingual Degree -> Flankera","Bilingual Status -> Flankera","  ",
"GAMM Model (w covariates 1-9)", "   ",
"Bilingual Use -> SSRTb", "Bilingual Degree -> SSRTb", "Bilingual Status -> SSRTb", "    ",
"Bilingual Use -> Card Sortb", "Bilingual Degree -> Card Sortb", "Bilingual Status -> Card Sortb", "     ",
"Bilingual Use -> Flankerb", "Bilingual Degree -> Flankerb", "Bilingual Status -> Flankerb", "      ",
"Bilingual Use -> Vocabularyb","Bilingual Degree -> Vocabularyb", "Bilingual Status -> Vocabularyb", "       ",
"GAMM Model (w covariates 1-8)","        ",
"Bilingual Use -> SSRTc", "Bilingual Degree -> SSRTc", "Bilingual Status -> SSRTc", "         ",
"Bilingual Use -> Card Sortc", "Bilingual Degree -> Card Sortc","Bilingual Status -> Card Sortc", "          ",
"Bilingual Use -> Flankerc", "Bilingual Degree -> Flankerc", "Bilingual Status -> Flankerc", "            ",
"Bilingual Use -> Vocabularyc", "Bilingual Degree -> Vocabularyc","Bilingual Status -> Vocabularyc","              ",
"GAMM Model (w covariates 1-6)","               ",
"Bilingual Use -> SSRTd", "Bilingual Degree -> SSRTd","Bilingual Status -> SSRTd", "                ",
"Bilingual Use -> Card Sortd", "Bilingual Degree -> Card Sortd","Bilingual Status -> Card Sortd", "                 ",
"Bilingual Use -> Flankerd", "Bilingual Degree -> Flankerd", "Bilingual Status -> Flankerd","                  ",
"GAMM Model (w Covariate 9 only)", "                   ",
"Bilingual Use -> SSRTe", "Bilingual Degree -> SSRTe","Bilingual Status -> SSRTe", "                    ",
"Bilingual Use -> Card Sorte", "Bilingual Degree -> Card Sorte","Bilingual Status -> Card Sorte", "                     ",
"Bilingual Use -> Flankere", "Bilingual Degree -> Flankere", "Bilingual Status -> Flankere","                      ",
"Bilingual Use -> Vocabularye", "Bilingual Degree -> Vocabularye", "Bilingual Status -> Vocabularye", "                       ",
"OLS Model"))

d<-data.frame(x, y, ylo, yhi)

credplot.gg <- function(d){
 # d is a data frame with 4 columns
 # d$x gives variable names
 # d$y gives center point
 # d$ylo gives lower limits
 # d$yhi gives upper limits
 require(ggplot2)
 theme_set(theme_light(base_size = 22))
 p <- ggplot(d, aes(x=x, y=y, ymin=ylo, ymax=yhi))+
 geom_pointrange()+
 geom_point(shape=21, fill = "light blue", col = "grey", size=5, stroke = 1) + geom_hline(yintercept = 0, linetype=1) +
 geom_hline(yintercept = -.1, size = 1, linetype=2) +
 geom_hline(yintercept = .1, size = 1, linetype=2) +
 coord_flip()+scale_y_continuous(breaks=seq(-4,4,.1)) +
 ylab('Effect Size') +
 scale_x_discrete('Predictor -> Outcome', waiver(), labels = c("Bilingual Use -> SSRT", "Bilingual Degree -> SSRT", "Bilingual Status -> SSRT", "",
 "Bilingual Use -> Card Sort", "Bilingual Degree -> Card Sort", "Bilingual Status -> Card Sort", " ",
 "Bilingual Use -> Flanker", "Bilingual Degree -> Flanker","Bilingual Status -> Flanker","  ",
 "GAMM Model (w covariates 1-9)", "   ",
 "Bilingual Use -> SSRT", "Bilingual Degree -> SSRT", "Bilingual Status -> SSRT", "    ",
 "Bilingual Use -> Card Sort", "Bilingual Degree -> Card Sort", "Bilingual Status -> Card Sort", "     ",
 "Bilingual Use -> Flanker", "Bilingual Degree -> Flanker", "Bilingual Status -> Flanker", "      ",
 "Bilingual Use -> Vocabulary","Bilingual Degree -> Vocabulary", "Bilingual Status -> Vocabulary", "       ",
 "GAMM Model (w covariates 1-8)","        ",
 "Bilingual Use -> SSRT", "Bilingual Degree -> SSRT", "Bilingual Status -> SSRT", "         ",
 "Bilingual Use -> Card Sort", "Bilingual Degree -> Card Sort","Bilingual Status -> Card Sort", "          ",
 "Bilingual Use -> Flanker", "Bilingual Degree -> Flanker", "Bilingual Status -> Flanker", "            ",
 "Bilingual Use -> Vocabulary", "Bilingual Degree -> Vocabulary","Bilingual Status -> Vocabulary","              ",
 "GAMM Model (w covariates 1-6)","               ",
 "Bilingual Use -> SSRT", "Bilingual Degree -> SSRT","Bilingual Status -> SSRT", "                ",
 "Bilingual Use -> Card Sort", "Bilingual Degree -> Card Sort","Bilingual Status -> Card Sort", "                 ",
 "Bilingual Use -> Flanker", "Bilingual Degree -> Flanker", "Bilingual Status -> Flanker","                  ",
 "GAMM Model (w Covariate 9 only)", "                   ",
 "Bilingual Use -> SSRT", "Bilingual Degree -> SSRT","Bilingual Status -> SSRT", "                    ",
 "Bilingual Use -> Card Sort", "Bilingual Degree -> Card Sort","Bilingual Status -> Card Sort", "                     ",
 "Bilingual Use -> Flanker", "Bilingual Degree -> Flanker", "Bilingual Status -> Flanker","                      ",
 "Bilingual Use -> Vocabulary", "Bilingual Degree -> Vocabulary", "Bilingual Status -> Vocabulary", "                       ",
 "OLS Model")) +
 ggtitle(expression(paste("Effect Sizes (", italic(beta), ") and Confidence Intervals Plotted Against the Interval of Equivalence"))) +
 theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          axis.line = element_line(size = 1, linetype = "solid", colour = "black"),
          axis.text = element_text(size = 20, colour = "black"), plot.title = element_text(hjust = -.5),
          )
 return(p)
}

tiff("figure1_newestestest.tiff", units = 'in', width = 20, height = 20, res = 200, compression = "lzw")
credplot.gg(d)
dev.off()

########################################
########################################
## Bootstrapping at Small Samples	  ##
########################################
########################################
abcd_subset<-cbind.data.frame(abcd_subset, random_data=rnorm(4524, mean=0, sd=1))
ind_iv = c(which(names(abcd_subset)=="bilingual_status"), which(names(abcd_subset)=="bilingual_degree"), which(names(abcd_subset)=="bilingual_use"))
ind_dv = c(which(names(abcd_subset)=="nihtbx_picvocab_uncorrected"),which(names(abcd_subset)=="nihtbx_flanker_uncorrected"),which(names(abcd_subset)=="nihtbx_cardsort_uncorrected"), which(names(abcd_subset)=="SSRTr"), which(names(abcd_subset)=="random_data"))

samp.n = 30
mat_row = 1
mat_col = 1
nboot = 5000
m.colnames<-matrix(nrow=1,ncol = length(ind_dv)*length(ind_iv))
mp <- matrix(nrow=nboot, ncol = length(ind_dv)*length(ind_iv)) # set the output matrix size to hold the results

for(b in 1:nboot){
	print(b)
	abcd_subset.samp<-rbind(abcd_subset[sample(which(abcd_subset$bilingual_status=='0'), samp.n), ], abcd_subset[sample(which(abcd_subset$bilingual_status=='1'), samp.n), ])
		for(i in 1:length(ind_dv)){
			print(i)
				for(j in 1:length(ind_iv)){
					print(j)
					prediction<-paste(names(abcd_subset)[ind_iv[j]],"-->",names(abcd_subset)[ind_dv[i]])
					print(prediction)
					form = paste(names(abcd_subset)[ind_dv[i]],"~" ,names(abcd_subset)[ind_iv[j]])
					form.simple = formula(form)
					mod<-lm(form.simple, data = abcd_subset.samp)
					print(summary(mod))
					p.value<-summary(mod)$coefficients[8]
					output.p<-p.value
					mp[mat_row,mat_col]<-output.p # place the output into the matrix that was dimensioned above
					m.colnames[,mat_col]<-prediction
					mat_col = mat_col + 1 # step to the next matrix column
					}
		} # go to the next step in the loop
		mat_col = 1
		mat_row = mat_row + 1 # step to the next matrix column
	colnames(mp)<-c("Bilingual Status --> Vocabulary", "Bilingual Degree --> Vocabulary", "Bilingual Use --> Vocabulary", "Bilingual Status --> Flanker",
  "Bilingual Degree --> Flanker", "Bilingual Use --> Flanker", "Bilingual Status --> Card Sort", "Bilingual Degree --> Card Sort",
  "Bilingual Use --> Card Sort", "Bilingual Status --> SSRT", "Bilingual Degree --> SSRT", "Bilingual Use --> SSRT", "Bilingual Status --> Random Data",
  "Bilingual Degree --> Random Data", "Bilingual Use --> Random Data")
	#print(mp)
	#print(mdf)
}

for.hist <- melt(mp)
for.hist<-for.hist[which(for.hist$value < .05),]

tiff("figure_hist.tiff", units = 'in', width = 12, height = 10, res = 300, compression = "lzw")
#par(mar = c(5, 5, 8, 8), xpd=FALSE) #set figure boundaries
ggplot(for.hist,aes(x = for.hist$value)) +
    facet_wrap(~Var2,scales = "free_x", nrow = 5, ncol = 3) + theme_bw() + geom_histogram(
    col = "grey", bins = 20,
    fill="light blue") +
    labs(x=expression(paste(italic("p "), "Value")), y="Frequency") +
    ggtitle(expression(paste("Histograms for ", italic("p "), "values < .05 for ", italic("n "), "= 30, out of 5000 Bootstrap Replicates"))) +
  	theme(plot.title = element_text(hjust = 0.5), text = element_text(size=20), axis.text = element_text(size = 14, colour = "black"))
dev.off()


#################

