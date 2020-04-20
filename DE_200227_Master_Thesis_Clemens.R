# Packages
#install.packages("haven")
#install.packages("stargazer")
#install.packages("descr")
#install.packages("ggplot2")
#install.packages("questionr")
#install.packages("tidyverse")
#install.packages("margins")
#install.packages("sjPlot")

#Libraries
library(haven)
library(descr)
library(stargazer)
library(ggplot2)
library(questionr)
library(tidyverse)
library(margins) 
library(sjPlot)

#Set Working Directory
#setwd("~/OneDrive/Master Thesis/Data")

#Set Working Directory
setwd("C:/Users/varno/Documents/Master of Public Policy/Masterarbeit/Daten")

##################################################################################################################################
###GERMANY_Read in data & Select sample for DE AES 2012###########################################################################
AES2012_DE <- read_dta("DE_AES_2012_ZA5354_Personen_v1-0-0.dta")
SUBSET01a_DE_2012<-subset(AES2012_DE)

#filter: Occupational status: Delete those that are still in primary education to keep the ones that could potentially participate in a FURTHER adult education activity.
attributes(SUBSET01a_DE_2012$F006)
table(SUBSET01a_DE_2012$F006)
SUBSET01_DE_2012 <- subset(SUBSET01a_DE_2012, SUBSET01a_DE_2012$F006==1 | SUBSET01a_DE_2012$F006==2 | SUBSET01a_DE_2012$F006==3 | SUBSET01a_DE_2012$F006==4 | SUBSET01a_DE_2012$F006==8 | SUBSET01a_DE_2012$F006==9 | SUBSET01a_DE_2012$F006==10 | SUBSET01a_DE_2012$F006==11 | SUBSET01a_DE_2012$F006==99)
table(SUBSET01_DE_2012$F006)

#new variable: Participation in non-formal adult education (NFE)
attributes(SUBSET01_DE_2012$F120)
table(SUBSET01_DE_2012$F120)
SUBSET01_DE_2012$NFE_FED <- -1
SUBSET01_DE_2012$NFE_FED[SUBSET01_DE_2012$F120==1] <- "1-yes" #yes
SUBSET01_DE_2012$NFE_FED[SUBSET01_DE_2012$F120==2] <- "0-no" #no
SUBSET01_DE_2012$NFE_FED <-factor(SUBSET01_DE_2012$NFE_FED)
table(SUBSET01_DE_2012$NFE_FED)
freq(SUBSET01_DE_2012$NFE_FED)
#no: 3206 (49.8%), yes: 3227 (50.2%)

#new variable: GENDER (sex)
attributes(SUBSET01_DE_2012$F001)
table(SUBSET01_DE_2012$F001)
SUBSET01_DE_2012$GENDER <- -1
SUBSET01_DE_2012$GENDER[SUBSET01_DE_2012$F001==1] <-"Male"
SUBSET01_DE_2012$GENDER[SUBSET01_DE_2012$F001==2] <-"Female"
SUBSET01_DE_2012$GENDER <-factor(SUBSET01_DE_2012$GENDER)
table(SUBSET01_DE_2012$GENDER)
freq(SUBSET01_DE_2012$GENDER)
#Female: 3677 (51.8%), male: 3422 (48.2%)

#new variable: AGE (alter)
attributes(SUBSET01_DE_2012$alter)
table(SUBSET01_DE_2012$alter)
freq(SUBSET01_DE_2012$alter)
sum(is.na(SUBSET01_DE_2012$alter))
#relabel
SUBSET01_DE_2012$AGE <- "NA"
SUBSET01_DE_2012$AGE[SUBSET01_DE_2012$alter>17 & SUBSET01_DE_2012$alter<35] <-"1-young"
SUBSET01_DE_2012$AGE[SUBSET01_DE_2012$alter>35 & SUBSET01_DE_2012$alter<53] <-"2-medium"
SUBSET01_DE_2012$AGE[SUBSET01_DE_2012$alter>54] <-"3-old"
table(SUBSET01_DE_2012$AGE)
#delete the ones with missing values (n=468).
SUBSET01a_DE_2012 <-subset(SUBSET01_DE_2012, AGE!="NA")
SUBSET01a_DE_2012$AGE <- droplevels(SUBSET01a_DE_2012$AGE)
SUBSET01a_DE_2012$AGE <-factor(SUBSET01a_DE_2012$AGE)
table(SUBSET01a_DE_2012$AGE)
freq(SUBSET01a_DE_2012$AGE)
#young: 2146 (32.4%), middle: 2935 (44.3%), old: 1546 (23.3%)

#new variable: migrant background (MIGR)
attributes(SUBSET01a_DE_2012$migra)
table(SUBSET01a_DE_2012$migra)
#take the ones with NA out: 1 case
SUBSET02_DE_2012 <- subset(SUBSET01a_DE_2012, SUBSET01a_DE_2012$migra<8)
SUBSET02_DE_2012$MIGR <- -1
SUBSET02_DE_2012$MIGR[SUBSET02_DE_2012$migra==1] <- "DE" #no migrant background
SUBSET02_DE_2012$MIGR[SUBSET02_DE_2012$migra==2 | SUBSET02_DE_2012$migra==3] <- "non-DE" #migrant background
SUBSET02_DE_2012$MIGR <-factor(SUBSET02_DE_2012$MIGR)
table(SUBSET02_DE_2012$MIGR)
freq(SUBSET02_DE_2012$MIGR)
#non-migrant: 5894 (89%), migrant: 732 (11%)

#new variable: highest primary education degree (schulab3) (EDUC)
attributes(SUBSET02_DE_2012$schulab3)
table(SUBSET02_DE_2012$schulab3)
#exclude the ones who are still pupils and those that said NA
SUBSET03_DE_2012 <- subset(SUBSET02_DE_2012, SUBSET02_DE_2012$schulab3<4)
SUBSET03_DE_2012$EDUC <- -1
SUBSET03_DE_2012$EDUC[SUBSET03_DE_2012$schulab3==0 | SUBSET03_DE_2012$schulab3==1] <- "3_low" #niedriger Schulabschluss
SUBSET03_DE_2012$EDUC[SUBSET03_DE_2012$schulab3==2] <- "2_middle" #mittlerer Schulabschluss
SUBSET03_DE_2012$EDUC[SUBSET03_DE_2012$schulab3==3] <- "1_high" #hoher Schulabschluss
attributes(SUBSET03_DE_2012$EDUC)
table(SUBSET03_DE_2012$EDUC)
freq(SUBSET03_DE_2012$EDUC)
#low:1920 (29.3%), medium: 2353 (36%), high: 2272 (34.7%)

#new variable: part-time/full-time work (TIME) (F006)
attributes(SUBSET03_DE_2012$F006)
table(SUBSET03_DE_2012$F006)
freq(SUBSET03_DE_2012$F006)
SUBSET03_DE_2012$TIME <- -1
SUBSET03_DE_2012$TIME[SUBSET03_DE_2012$F006==1] <- "1-FT" #full-time
SUBSET03_DE_2012$TIME[SUBSET03_DE_2012$F006==2] <- "2-PT" #part-time
SUBSET03_DE_2012$TIME[SUBSET03_DE_2012$F006==3 | SUBSET03_DE_2012$F006==4 | SUBSET03_DE_2012$F006==5 | SUBSET03_DE_2012$F006==6 | SUBSET03_DE_2012$F006==7 | SUBSET03_DE_2012$F006==8 | SUBSET03_DE_2012$F006==9 | SUBSET03_DE_2012$F006==10 | SUBSET03_DE_2012$F006==11 | SUBSET03_DE_2012$F006==99] <- "3-OT" #others
attributes(SUBSET03_DE_2012$TIME)
table(SUBSET03_DE_2012$TIME)
freq(SUBSET03_DE_2012$TIME)
#FT: 3116 (47.6%), PT: 1155 (17.6%), OT: 1679 (34.7%)

#new variable: establishment size (ORG)
#0-49 small // 1+2+3, 50-249 medium//4, 250->1000 large//5+7, others//-1, -2
attributes(SUBSET03_DE_2012$x019)
table(SUBSET03_DE_2012$x019)
SUBSET03_DE_2012$ORG <- "4-NA"
SUBSET03_DE_2012$ORG[SUBSET03_DE_2012$x019==1 | SUBSET03_DE_2012$x019==2 | SUBSET03_DE_2012$x019==3] <-"1-small"
SUBSET03_DE_2012$ORG[SUBSET03_DE_2012$x019==4] <-"2-medium" 
SUBSET03_DE_2012$ORG[SUBSET03_DE_2012$x019==5 | SUBSET03_DE_2012$x019==6] <-"3-large"
SUBSET03_DE_2012$ORG[SUBSET03_DE_2012$x019==8 | SUBSET03_DE_2012$x019==9] <-"4-NA"
SUBSET03_DE_2012$ORG <-factor(SUBSET03_DE_2012$ORG)
table(SUBSET03_DE_2012$ORG)
freq(SUBSET03_DE_2012$ORG)
#small: 1365 (20.9%); medium: 588 (9%); large: 1857 (28.4%), NA: 2735 (41.8%)

#new variable: employment status (EMPL)
attributes(SUBSET03_DE_2012$stelk)
table(SUBSET03_DE_2012$stelk)
#exclude the ones that are NA (n = 75)
SUBSET04_DE_2012 <- subset(SUBSET03_DE_2012, SUBSET03_DE_2012$stelk<5 | SUBSET03_DE_2012$stelk>5)
SUBSET04_DE_2012$EMPL <- "NA"
SUBSET04_DE_2012$EMPL[SUBSET04_DE_2012$stelk==6] <-"3_not-empl" #unemployed or other inactive on the labor market
SUBSET04_DE_2012$EMPL[SUBSET04_DE_2012$stelk==4] <-"2_self-empl" #self-employed
SUBSET04_DE_2012$EMPL[SUBSET04_DE_2012$stelk==1 | SUBSET04_DE_2012$stelk==2 | SUBSET04_DE_2012$stelk==3] <-"1_employee" #employees and civil servants
SUBSET04_DE_2012$EMPL <-factor(SUBSET04_DE_2012$EMPL)
table(SUBSET04_DE_2012$EMPL)
freq(SUBSET04_DE_2012$EMPL)
#employees: 3779; self-employed: 547; not-employed: 1586

#skill level (Occupational Group / ISCO08 / international standard classification of occupations) (SKILL)
attributes(SUBSET04_DE_2012$isco08_1)
table(SUBSET04_DE_2012$isco08_1)
#relabel into five categories: Managers and professionals, Technicians. associate professionals, Clerks. service and sales workers, Skilled manual workers, Elementary occupations 
SUBSET04_DE_2012$SKILL <- "NA"
SUBSET04_DE_2012$SKILL[SUBSET04_DE_2012$isco08_1==1 | SUBSET04_DE_2012$isco08_1==2] <-"1-Managers" #legislators, senior officials, managers and professionals
SUBSET04_DE_2012$SKILL[SUBSET04_DE_2012$isco08_1==3] <-"2-Technicians" #technicians and associate professionals
SUBSET04_DE_2012$SKILL[SUBSET04_DE_2012$isco08_1==4 | SUBSET04_DE_2012$isco08_1==5 | SUBSET04_DE_2012$isco08_1==6 | SUBSET04_DE_2012$isco08_1==7 | SUBSET04_DE_2012$isco08_1==8] <-"3-Clerks_Workers" #clerical support workers, service and sales workers; skilled manual workers
SUBSET04_DE_2012$SKILL[SUBSET04_DE_2012$isco08_1==9] <-"4-Elementary" #elementary occupations
SUBSET04_DE_2012$SKILL[SUBSET04_DE_2012$isco08_1==-1 | SUBSET04_DE_2012$isco08_1==0] <-"NA" #no answer and not applicable
table(SUBSET04_DE_2012$SKILL)
freq(SUBSET04_DE_2012$SKILL)
#Managers: 908 (14%); Technicians: 1037 (16%); Clerks_Workers: 2061 (31.9%); Elementary: 402 (6.2%); NA: 2062 (31.9%)
#the ones with NA are unemployed.

#Sample statistics
table(SUBSET04_DE_2012$NFE_FED)
freq(SUBSET04_DE_2012$NFE_FED)
table1<-table(SUBSET04_DE_2012$GENDER, SUBSET04_DE_2012$NFE_FED)
table2<-table(SUBSET04_DE_2012$AGE, SUBSET04_DE_2012$NFE_FED)
table3<-table(SUBSET04_DE_2012$MIGR, SUBSET04_DE_2012$NFE_FED)
table4<-table(SUBSET04_DE_2012$EDUC, SUBSET04_DE_2012$NFE_FED)
table5<-table(SUBSET04_DE_2012$EMPL, SUBSET04_DE_2012$NFE_FED)
table6<-table(SUBSET04_DE_2012$SKILL, SUBSET04_DE_2012$NFE_FED)
table7<-table(SUBSET04_DE_2012$TIME, SUBSET04_DE_2012$NFE_FED)
round(prop.table(table1,2), digits=2)
round(prop.table(table2,2), digits=2)
round(prop.table(table3,2), digits=2)
round(prop.table(table4,2), digits=2)
round(prop.table(table5,2), digits=2)
round(prop.table(table6,2), digits=2)
round(prop.table(table7,2), digits=2)

#new variable: barriers to prevent participation in NFE (BARRIER) // Most important barrier
attributes(SUBSET04_DE_2012$F123)
table(SUBSET04_DE_2012$F123) 
freq(SUBSET04_DE_2012$F123)

SUBSET04_DE_2012$BARRIER <- "NA"
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==3] <-"prerequisits" #does not fulfill the prerequisits
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==4] <-"costs" #too expensive
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==5] <-"lack_support" #lack of employer support
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==6] <-"time_conflict" #education cannot be combined with work time or other duties
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==7] <-"family" #no time due to family obligations
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==8] <-"distance" #no suitable educational offer in reachable distance
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==9] <-"offer" #there is no suitable offer available. 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==10] <- "prior_exp" #bad prior learning experience
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==11 | SUBSET04_DE_2012$F123==12] <-"age_health" #health-reasons or due to age constraints
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==13 | SUBSET04_DE_2012$F123==16] <-"personal" #other personal reasons
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==14] <- "counselling" #lack of counseling
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==15] <- "computer" #no access to computer or internet
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==98 | SUBSET04_DE_2012$F123==1 | SUBSET04_DE_2012$F123==2 | SUBSET04_DE_2012$F123==99] <- "NA" #no reason provided by respondent
SUBSET04_DE_2012$BARRIER <-factor(SUBSET04_DE_2012$BARRIER)

SUBSET04a_DE_2012 <-subset(SUBSET04_DE_2012, BARRIER!= "NA")
SUBSET04a_DE_2012$BARRIER <- droplevels(SUBSET04a_DE_2012$BARRIER)
table(SUBSET04a_DE_2012$BARRIER)
freq(SUBSET04a_DE_2012$BARRIER)

table(SUBSET04a_DE_2012$BARRIER)
freq(SUBSET04a_DE_2012$BARRIER)

#combine barriers to prevent participation in NFE (BARRIER_COMB) // BINARY VARIABLE 
SUBSET05_DE_2012 <- subset(SUBSET04a_DE_2012)
SUBSET05_DE_2012$BARRIER2 <- -1
SUBSET05_DE_2012$BARRIER2[SUBSET05_DE_2012$F123==3 | SUBSET05_DE_2012$F123==15 | SUBSET05_DE_2012$F123==7 | SUBSET05_DE_2012$F123==12 | SUBSET05_DE_2012$F123==11 | SUBSET05_DE_2012$F123==10 | SUBSET05_DE_2012$F123==13 | SUBSET05_DE_2012$F123==16] <- "1_ind_level" #BARRIERS AT PERSONAL/INDIVIDUAL-LEVEl 
SUBSET05_DE_2012$BARRIER2[SUBSET05_DE_2012$F123==9 | SUBSET05_DE_2012$F123==4 | SUBSET05_DE_2012$F123==8 | SUBSET05_DE_2012$F123==14 | SUBSET05_DE_2012$F123==5 | SUBSET05_DE_2012$F123==6] <- "0_inst_level" #BARRIERS AT EDUCATIONAL INSTITUTION-LEVEL
SUBSET05_DE_2012$BARRIER2[SUBSET05_DE_2012$F123==99 | SUBSET05_DE_2012$F123==98 | SUBSET05_DE_2012$F123==1 | SUBSET05_DE_2012$F123==2] <- "N_A" #no answer or no reason
SUBSET05_DE_2012$BARRIER2 <-factor(SUBSET05_DE_2012$BARRIER2)

table(SUBSET05_DE_2012$BARRIER2)
freq(SUBSET05_DE_2012$BARRIER2)

SUBSET06_DE_2012 <-subset(SUBSET05_DE_2012, BARRIER2!= "N_A")
SUBSET07_DE_2012 <-subset(SUBSET06_DE_2012, BARRIER2!= -1)
SUBSET07_DE_2012$BARRIER2 <- droplevels(SUBSET07_DE_2012$BARRIER2)
table(SUBSET07_DE_2012$BARRIER2)
freq(SUBSET07_DE_2012$BARRIER2)

#Code into 4 BARRIERS

SUBSET07_DE_2012$BARRIER4 <- -1
SUBSET07_DE_2012$BARRIER4[SUBSET07_DE_2012$F123==3 | SUBSET07_DE_2012$F123==11 | SUBSET07_DE_2012$F123==12 | SUBSET07_DE_2012$F123==15 | SUBSET07_DE_2012$F123==7] <-"socio_econ_dem" #BARRIERS AT PERSONAL/INDIVIDUAL-LEVEL: Socio-economic / demographic 
SUBSET07_DE_2012$BARRIER4[SUBSET07_DE_2012$F123==10 | SUBSET07_DE_2012$F123==13 | SUBSET07_DE_2012$F123==16] <-"psych" #BARRIERS AT PERSONAL/INDIVIDUAL-LEVEL: psychological 
SUBSET07_DE_2012$BARRIER4[SUBSET07_DE_2012$F123==8 | SUBSET07_DE_2012$F123==9 | SUBSET07_DE_2012$F123==4] <-"direct" #BARRIERS AT EDUCATIONAL INSTITUTION-LEVEL: directly related to educational offer / autonomous decision-making by educational institution
SUBSET07_DE_2012$BARRIER4[SUBSET07_DE_2012$F123==14 | SUBSET07_DE_2012$F123==6 | SUBSET07_DE_2012$F123==5 | SUBSET07_DE_2012$F123==5] <-"indirect" #BARRIERS AT EDUCATIONAL INSTITUTION-LEVEL:	wider circumstances / non-autonomous / indirect 
SUBSET07_DE_2012$BARRIER4[SUBSET07_DE_2012$F123==99 | SUBSET07_DE_2012$F123==98] <- "N_A" #no answer or no reason
#SUBSET07_DE_2012$BARRIER[SUBSET07_DE_2012$F123==99 | SUBSET07_DE_2012$F123==98] <-"NA" #not applicable and no answer
SUBSET07_DE_2012$BARRIER4 <-factor(SUBSET07_DE_2012$BARRIER4)

table(SUBSET07_DE_2012$BARRIER4)
freq(SUBSET07_DE_2012$BARRIER4)

SUBSET08_DE_2012 <-subset(SUBSET07_DE_2012, BARRIER4!= "N_A")
SUBSET08_DE_2012$BARRIER4 <- droplevels(SUBSET08_DE_2012$BARRIER4)
table(SUBSET08_DE_2012$BARRIER4)
freq(SUBSET08_DE_2012$BARRIER4)


#Sample statistic
table8<-table(SUBSET07_DE_2012$BARRIER2, SUBSET07_DE_2012$NFE_FED)
round(prop.table(table8,2), digits=2)

#########Descriptive Statistics#####################
#DESCRIPTIVE STATISTICS
#Cross-Table // previous level of education and Adult education, 2016
crosstab(SUBSET04_DE_2012$NFE_FED, SUBSET04_DE_2012$SKILL, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET04_DE_2012$NFE_FED, SUBSET04_DE_2012$GENDER, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET04_DE_2012$NFE_FED, SUBSET04_DE_2012$AGE, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET04_DE_2012$NFE_FED, SUBSET04_DE_2012$MIGR, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET04_DE_2012$NFE_FED, SUBSET04_DE_2012$EDUC, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET04_DE_2012$NFE_FED, SUBSET04_DE_2012$EMPL, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET04_DE_2012$NFE_FED, SUBSET04_DE_2012$TIME, plot=FALSE,prop.c = TRUE)

#Chi2 Test // previous level of education and Adult education
chisq.test(SUBSET04_DE_2012$NFE_FED, SUBSET04_DE_2012$SKILL, correct= F,)

#Graph
# Calculate a cross table (only absolute values)
#TABLE1 <- table(SUBSET04$DV, SUBSET04$DEGREE)
# Use the cross table (TABLE1) to calculate the column percent
#TABLE2 <- prop.table(TABLE1,2)
# Use the calculated column percent (TABLE2) and insert it into a data frame (TABLE3)
#TABLE3 <- as.data.frame(TABLE2)
#ggplot(TABLE3, aes(fill=Var1, x=Var2, Var3, y=Freq)) + geom_bar(stat="identity") + ggtitle("Prior educational attainment and participation in adult education in Germany") + ylim(0,1) + ylab("%")+ xlab("") + scale_fill_manual(name = "Participated in adult education?", labels = c("No", "Yes"), values = c("blue1", "skyblue1"))



#########Logistic Regression_BEFORE COMBINING YEARS#####################
#######HYPOTHESIS 1####################################
#######Socio-demographics###################################
OUTPUT02 <-glm(NFE_FED~ GENDER+AGE+MIGR+EDUC, data=SUBSET04_DE_2012,family=binomial())
OR.vector02 <-exp(coef(OUTPUT02))
CI.vector02 <-exp(confint(OUTPUT02))
p.values02 <-list(summary(OUTPUT02)$coefficients[,4])

stargazer(OUTPUT02,
          coef= list(OR.vector02), ci = T,
          ci.custom= list(CI.vector02),
          single.row= F,
          type = "text",
          p=c(p.values02))

#######Employment/skill-related variable###################################
OUTPUT03 <-glm(NFE_FED~ EMPL+SKILL+ORG+TIME, data=SUBSET04_DE_2012,family=binomial())
OR.vector03 <-exp(coef(OUTPUT03))
CI.vector03 <-exp(confint(OUTPUT03))
p.values03 <-list(summary(OUTPUT03)$coefficients[,4])

stargazer(OUTPUT03,
          coef= list(OR.vector03), ci = T,
          ci.custom= list(CI.vector03),
          single.row= F,
          type = "text",
          p=c(p.values03))

#########FINAL MODEL################################
OUTPUT04 <-glm(NFE_FED~ GENDER+AGE+MIGR+EDUC+EMPL+SKILL+ORG+TIME, data=SUBSET04_DE_2012,family=binomial())
OR.vector04 <-exp(coef(OUTPUT04))
CI.vector04 <-exp(confint(OUTPUT04))
p.values04 <-list(summary(OUTPUT04)$coefficients[,4])

stargazer(OUTPUT04,
          coef= list(OR.vector04), ci = T,
          ci.custom= list(CI.vector04),
          single.row= F,
          type = "text",
          p=c(p.values04))

####################################################################
#######Hypothesis 2################################################
####STEPWISEMODELING##############################################
#######MODEL1#####################################################
OUTPUT05 <-glm(BARRIER~ GENDER, data=SUBSET07_DE_2012,family=binomial())
OR.vector05 <-exp(coef(OUTPUT05))
CI.vector05 <-exp(confint(OUTPUT05))
p.values05 <-list(summary(OUTPUT05)$coefficients[,4])

stargazer(OUTPUT05,
          coef= list(OR.vector05), ci = T,
          ci.custom= list(CI.vector05),
          single.row= F,
          type = "text",
          p=c(p.values05))

#######MODEL2: Socio-demographics###################################
OUTPUT06 <-glm(BARRIER~ GENDER+AGE+MIGR+EDUC, data=SUBSET07_DE_2012,family=binomial())
OR.vector06 <-exp(coef(OUTPUT06))
CI.vector06 <-exp(confint(OUTPUT06))
p.values06 <-list(summary(OUTPUT06)$coefficients[,4])

stargazer(OUTPUT06,
          coef= list(OR.vector06), ci = T,
          ci.custom= list(CI.vector06),
          single.row= F,
          type = "text",
          p=c(p.values06))

#######MODEL3: Employment/skill-related variable###################################
OUTPUT07 <-glm(BARRIER~ EMPL+SKILL+ORG+TIME, data=SUBSET07_DE_2012,family=binomial())
OR.vector07 <-exp(coef(OUTPUT07))
CI.vector07 <-exp(confint(OUTPUT07))
p.values07 <-list(summary(OUTPUT07)$coefficients[,4])

stargazer(OUTPUT07,
          coef= list(OR.vector07), ci = T,
          ci.custom= list(CI.vector07),
          single.row= F,
          type = "text",
          p=c(p.values07))

#######MODEL4: ALL IVs#########################################
OUTPUT08 <-glm(BARRIER~ GENDER+AGE+MIGR+EDUC+EMPL+SKILL+ORG+TIME, data=SUBSET07_DE_2012,family=binomial())
OR.vector08 <-exp(coef(OUTPUT08))
CI.vector08 <-exp(confint(OUTPUT08))
p.values08 <-list(summary(OUTPUT08)$coefficients[,4])

stargazer(OUTPUT08,
          coef= list(OR.vector08), ci = T,
          ci.custom= list(CI.vector08),
          single.row= F,
          type = "text",
          p=c(p.values08))

#new variable: reasons to participate in NFE (REASON)

#Incentives to participate in NFE & FED 
attributes(SUBSET04_DE_2012$F09901_1)
table(SUBSET04_DE_2012$F09901_1)
freq(SUBSET04_DE_2012$F09901_1)

attributes(SUBSET04_DE_2012$F09902_1)
table(SUBSET04_DE_2012$F09902_1)
freq(SUBSET04_DE_2012$F09902_1)

attributes(SUBSET04_DE_2012$F09903_1)
table(SUBSET07_DE_2012$F09903_1)
freq(SUBSET07_DE_2012$F09903_1)





##################################################################################################################################
###GERMANY_Read in data & Select sample for DE AES 2016###########################################################################
AES2016_DE <- read_dta("DE_AES_2016_ZA6887_Personen_v1-0-0.dta")
SUBSET01a_DE_2016<-subset(AES2016_DE)

#filter: Occupational status: Delete those that are still in primary education to keep the ones that could potentially participate in a FURTHER adult education activity.
attributes(SUBSET01a_DE_2016$F006)
table(SUBSET01a_DE_2016$F006)
SUBSET01_DE_2016 <- subset(SUBSET01a_DE_2016, SUBSET01a_DE_2016$F006==1 | SUBSET01a_DE_2016$F006==2 | SUBSET01a_DE_2016$F006==3 | SUBSET01a_DE_2016$F006==4 | SUBSET01a_DE_2016$F006==8 | SUBSET01a_DE_2016$F006==9 | SUBSET01a_DE_2016$F006==10 | SUBSET01a_DE_2016$F006==11 | SUBSET01a_DE_2016$F006==99)
table(SUBSET01_DE_2016$F006)

#new variable: Participation in non-formal adult education (NFE)
attributes(SUBSET01_DE_2016$F120)
table(SUBSET01_DE_2016$F120)
SUBSET01_DE_2016$NFE_FED <- -1
SUBSET01_DE_2016$NFE_FED[SUBSET01_DE_2016$F120==1] <- "1-yes" #yes
SUBSET01_DE_2016$NFE_FED[SUBSET01_DE_2016$F120==2] <- "0-no" #no
SUBSET01_DE_2016$NFE_FED <-factor(SUBSET01_DE_2016$NFE_FED)
table(SUBSET01_DE_2016$NFE_FED)
freq(SUBSET01_DE_2016$NFE_FED)
#no: 3798 (54%), yes: 3231 (46%)

#new variable: Participation in non-formal adult education (NFE)
attributes(SUBSET01_DE_2016$nfe12mo)
table(SUBSET01_DE_2016$nfe12mo)
SUBSET01_DE_2016$NFE <- -1
SUBSET01_DE_2016$NFE[SUBSET01_DE_2016$nfe12mo==1] <- "1-yes" #yes
SUBSET01_DE_2016$NFE[SUBSET01_DE_2016$nfe12mo==2] <- "0-no" #no
SUBSET01_DE_2016$NFE <-factor(SUBSET01_DE_2016$NFE)
table(SUBSET01_DE_2016$NFE)
freq(SUBSET01_DE_2016$NFE)
#no: 4343 (56%), yes: 3407 (44%)

#new variable: GENDER (sex)
attributes(SUBSET01_DE_2016$F001)
table(SUBSET01_DE_2016$F001)
SUBSET01_DE_2016$GENDER <- -1
SUBSET01_DE_2016$GENDER[SUBSET01_DE_2016$F001==1] <-"Male"
SUBSET01_DE_2016$GENDER[SUBSET01_DE_2016$F001==2] <-"Female"
SUBSET01_DE_2016$GENDER <-factor(SUBSET01_DE_2016$GENDER)
table(SUBSET01_DE_2016$GENDER)
freq(SUBSET01_DE_2016$GENDER)
#Female: 4169 (53.8%), male: 3581 (46.2%)

#new variable: AGE (alter)
attributes(SUBSET01_DE_2016$alter)
table(SUBSET01_DE_2016$alter)
sum(is.na(SUBSET01_DE_2016$alter))
#relabel
SUBSET01_DE_2016$AGE <- "NA"
SUBSET01_DE_2016$AGE[SUBSET01_DE_2016$alter>17 & SUBSET01_DE_2016$alter<35] <-"1-young"
SUBSET01_DE_2016$AGE[SUBSET01_DE_2016$alter>35 & SUBSET01_DE_2016$alter<53] <-"2-medium"
SUBSET01_DE_2016$AGE[SUBSET01_DE_2016$alter>54] <-"3-old"
#delete the ones with missing values (n=500).
SUBSET01a_DE_2016 <-subset(SUBSET01_DE_2016, AGE!="NA")
SUBSET01a_DE_2016$AGE <- droplevels(SUBSET01a_DE_2016$AGE)
SUBSET01a_DE_2016$AGE <-factor(SUBSET01a_DE_2016$AGE)
table(SUBSET01a_DE_2016$AGE)
freq(SUBSET01a_DE_2016$AGE)
#young: 2301 (31.7%), middle: 2465 (34%), old: 2484 (34.3%)

#new variable: migrant background (MIGR)
attributes(SUBSET01a_DE_2016$migra)
table(SUBSET01a_DE_2016$migra)
#take the ones with NA out: 3 cases
SUBSET02_DE_2016 <- subset(SUBSET01a_DE_2016, SUBSET01a_DE_2016$migra<8)
SUBSET02_DE_2016$MIGR <- -1
SUBSET02_DE_2016$MIGR[SUBSET02_DE_2016$migra==1] <- "DE" #no migrant background
SUBSET02_DE_2016$MIGR[SUBSET02_DE_2016$migra==2 | SUBSET02_DE_2016$migra==3] <- "non-DE" #migrant background
SUBSET02_DE_2016$MIGR <-factor(SUBSET02_DE_2016$MIGR)
table(SUBSET02_DE_2016$MIGR)
freq(SUBSET02_DE_2016$MIGR)
#non-migrant: 6305 (87%), migrant: 942 (13%)

#new variable: highest primary education degree (schulab3) (EDUC)
attributes(SUBSET02_DE_2016$schulab3)
table(SUBSET02_DE_2016$schulab3)
#exclude the ones who are still pupils and those that said NA (n=60)
SUBSET02a_DE_2016 <- subset(SUBSET02_DE_2016, SUBSET02_DE_2016$schulab3<4)
SUBSET02a_DE_2016$EDUC <- -1
SUBSET02a_DE_2016$EDUC[SUBSET02a_DE_2016$schulab3==0 | SUBSET02a_DE_2016$schulab3==1] <- "3_low" #niedriger Schulabschluss
SUBSET02a_DE_2016$EDUC[SUBSET02a_DE_2016$schulab3==2] <- "2_middle" #mittlerer Schulabschluss
SUBSET02a_DE_2016$EDUC[SUBSET02a_DE_2016$schulab3==3] <- "1_high" #hoher Schulabschluss
attributes(SUBSET02a_DE_2016$EDUC)
table(SUBSET02a_DE_2016$EDUC)
freq(SUBSET02a_DE_2016$EDUC)
#low:2136 (29.7%), medium: 2510 (34.9%), high: 2541 (35.4%)

#new variable: part-time/full-time work (TIME)  (F006)
attributes(SUBSET02a_DE_2016$F006)
table(SUBSET02a_DE_2016$F006)
freq(SUBSET02a_DE_2016$F006)
SUBSET02a_DE_2016$TIME <- "NA"
SUBSET02a_DE_2016$TIME[SUBSET02a_DE_2016$F006==1] <- "1-FT" #full-time
SUBSET02a_DE_2016$TIME[SUBSET02a_DE_2016$F006==2] <- "2-PT" #part-time
SUBSET02a_DE_2016$TIME[SUBSET02a_DE_2016$F006==3 | SUBSET02a_DE_2016$F006==4 | SUBSET02a_DE_2016$F006==5 | SUBSET02a_DE_2016$F006==6 | SUBSET02a_DE_2016$F006==7 | SUBSET02a_DE_2016$F006==8 | SUBSET02a_DE_2016$F006==9 | SUBSET02a_DE_2016$F006==10 | SUBSET02a_DE_2016$F006==11 | SUBSET02a_DE_2016$F006==99 | SUBSET02a_DE_2016$F006==12] <- "3-OT" #others
attributes(SUBSET02a_DE_2016$TIME)
table(SUBSET02a_DE_2016$TIME)
freq(SUBSET02a_DE_2016$TIME)
#FT: 3042 (42.3%), PT: 1145 (15.9%), OT: 3000 (41.7%)

#new variable: establishment size (ORG)
#0-49 small // 1+2+3, 50-249 medium//4, 250->1000 large//5+7, others//-1, -2
attributes(SUBSET02a_DE_2016$x019)
table(SUBSET02a_DE_2016$x019)
SUBSET02a_DE_2016$ORG <- "NA"
SUBSET02a_DE_2016$ORG[SUBSET02a_DE_2016$x019==1 | SUBSET02a_DE_2016$x019==2 | SUBSET02a_DE_2016$x019==3] <-"1-small"
SUBSET02a_DE_2016$ORG[SUBSET02a_DE_2016$x019==4] <-"2-medium" 
SUBSET02a_DE_2016$ORG[SUBSET02a_DE_2016$x019==5 | SUBSET02a_DE_2016$x019==6] <-"3-large"
SUBSET02a_DE_2016$ORG[SUBSET02a_DE_2016$x019==8 | SUBSET02a_DE_2016$x019==9] <-"NA"
SUBSET02a_DE_2016$ORG <-factor(SUBSET02a_DE_2016$ORG)
table(SUBSET02a_DE_2016$ORG)
freq(SUBSET02a_DE_2016$ORG)
#small: 1352 (18.8%); medium: 575 (8%); large: 1862 (25.9%), NA: 3398 (47.3%)

#new variable: employment status (EMPL)
attributes(SUBSET02a_DE_2016$stelk)
table(SUBSET02a_DE_2016$stelk)
#exclude the ones that are NA (n = 26)
SUBSET03_DE_2016 <- subset(SUBSET02a_DE_2016, SUBSET02a_DE_2016$stelk<5 | SUBSET02a_DE_2016$stelk>5)
SUBSET03_DE_2016$EMPL <- "NA"
SUBSET03_DE_2016$EMPL[SUBSET03_DE_2016$stelk==6] <-"3_not-empl" #unemployed or other inactive on the labor market
SUBSET03_DE_2016$EMPL[SUBSET03_DE_2016$stelk==4] <-"2_self-empl" #self-employed
SUBSET03_DE_2016$EMPL[SUBSET03_DE_2016$stelk==1 | SUBSET03_DE_2016$stelk==2 | SUBSET03_DE_2016$stelk==3] <-"1_employee" #employees and civil servants
SUBSET03_DE_2016$EMPL <-factor(SUBSET03_DE_2016$EMPL)
table(SUBSET03_DE_2016$EMPL)
freq(SUBSET03_DE_2016$EMPL)
#employees: 4126 (57.6%); self-employed: 443 (6.2%); not-employed: 2592 (36.2%)

#skill level (Occupational Group / ISCO08 / international standard classification of occupations) (SKILL)
attributes(SUBSET03_DE_2016$isco08_1)
table(SUBSET03_DE_2016$isco08_1)
#relabel into five categories: Managers and professionals, Technicians. associate professionals, Clerks. service and sales workers, Skilled manual workers, Elementary occupations 
SUBSET03_DE_2016$SKILL <- "NA"
SUBSET03_DE_2016$SKILL[SUBSET03_DE_2016$isco08_1==1 | SUBSET03_DE_2016$isco08_1==2] <-"1-Managers" #legislators, senior officials, managers and professionals
SUBSET03_DE_2016$SKILL[SUBSET03_DE_2016$isco08_1==3] <-"2-Technicians" #technicians and associate professionals
SUBSET03_DE_2016$SKILL[SUBSET03_DE_2016$isco08_1==4 | SUBSET03_DE_2016$isco08_1==5 | SUBSET03_DE_2016$isco08_1==6 | SUBSET03_DE_2016$isco08_1==7 | SUBSET03_DE_2016$isco08_1==8] <-"3-Clerks_Workers" #clerical support workers, service and sales workers; skilled manual workers
SUBSET03_DE_2016$SKILL[SUBSET03_DE_2016$isco08_1==9] <-"4-Elementary" #elementary occupations
SUBSET03_DE_2016$SKILL[SUBSET03_DE_2016$isco08_1==-1 | SUBSET03_DE_2016$isco08_1==0] <-"NA" #no answer and not applicable
table(SUBSET03_DE_2016$SKILL)
freq(SUBSET03_DE_2016$SKILL)
#Managers: 895 (12.5%); Technicians: 1066 (14.9%); Clerks_Workers: 1953 (27.3%); Elementary: 436 (6.1%); NA: 2811 (39.3%)
#include the ones with NA. 

# Sample statistics
table1<-table(SUBSET03_DE_2016$GENDER, SUBSET03_DE_2016$NFE_FED)
table2<-table(SUBSET03_DE_2016$AGE, SUBSET03_DE_2016$NFE_FED)
table3<-table(SUBSET03_DE_2016$MIGR, SUBSET03_DE_2016$NFE_FED)
table4<-table(SUBSET03_DE_2016$EDUC, SUBSET03_DE_2016$NFE_FED)
table5<-table(SUBSET03_DE_2016$EMPL, SUBSET03_DE_2016$NFE_FED)
table6<-table(SUBSET03_DE_2016$SKILL, SUBSET03_DE_2016$NFE_FED)
table7<-table(SUBSET03_DE_2016$TIME, SUBSET03_DE_2016$NFE_FED)
round(prop.table(table1,2), digits=2)
round(prop.table(table2,2), digits=2)
round(prop.table(table3,2), digits=2)
round(prop.table(table4,2), digits=2)
round(prop.table(table5,2), digits=2)
round(prop.table(table6,2), digits=2)
round(prop.table(table7,2), digits=2)

##########################################
#new variable: barriers to prevent participation in NFE (BARRIER)
attributes(SUBSET03_DE_2016$F123)
table(SUBSET03_DE_2016$F123)
freq(SUBSET03_DE_2016$F123)

SUBSET03_DE_2016$BARRIER <- -1
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==1] <-"prerequisits" #does not fulfill the prerequisits
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==2] <-"costs" #too expensive
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==3 | SUBSET03_DE_2016$F123==4] <-"lack_support" #lack of employer/public service support
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==5] <-"time_conflict" #education cannot be combined with work time or other duties
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==6] <-"family" #no time due to family obligations
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==7] <-"distance" #no suitable educational offer in reachable distance
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==8] <-"offer" #there is no suitable offer available. 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==9] <- "prior_exp" #bad prior learning experience
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==10 | SUBSET03_DE_2016$F123==11] <-"age_health" #health-reasons
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==12 | SUBSET03_DE_2016$F123==15 | SUBSET03_DE_2016$F123==17] <-"personal" #other personal reasons
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==13] <- "counselling" #lack of counseling
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==14] <- "computer" #no access to computer or internet
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==99 | SUBSET03_DE_2016$F123==98] <-"NA" #not applicable and no answer
SUBSET03_DE_2016$BARRIER <-factor(SUBSET03_DE_2016$BARRIER)

table(SUBSET03_DE_2016$BARRIER)
freq(SUBSET03_DE_2016$BARRIER)

SUBSET03a_DE_2016 <-subset(SUBSET03_DE_2016, BARRIER!= "NA")
SUBSET03b_DE_2016 <-subset(SUBSET03a_DE_2016, BARRIER!= -1)
SUBSET03b_DE_2016$BARRIER <- droplevels(SUBSET03b_DE_2016$BARRIER)
table(SUBSET03b_DE_2016$BARRIER)
freq(SUBSET03b_DE_2016$BARRIER)

##########################################
#combine barriers to prevent participation in NFE (BARRIER_COMB)
#new variable: barriers to prevent participation in NFE (BARRIER)
SUBSET04_DE_2016 <- subset(SUBSET03b_DE_2016)
attributes(SUBSET04_DE_2016$F123)
table(SUBSET04_DE_2016$F123)
freq(SUBSET04_DE_2016$F123)

SUBSET04_DE_2016$BARRIER4 <- "NA"
SUBSET04_DE_2016$BARRIER4[SUBSET04_DE_2016$F123==1 | SUBSET04_DE_2016$F123==6 | SUBSET04_DE_2016$F123==14 | SUBSET04_DE_2016$F123==11 | SUBSET04_DE_2016$F123==10] <-"socio_econ_dem" #BARRIERS AT PERSONAL/INDIVIDUAL-LEVEL: Socio-economic / demographic 
SUBSET04_DE_2016$BARRIER4[SUBSET04_DE_2016$F123==9 | SUBSET04_DE_2016$F123==12 | SUBSET04_DE_2016$F123==15 | SUBSET04_DE_2016$F123==17] <-"psych" #BARRIERS AT PERSONAL/INDIVIDUAL-LEVEL: psychological 
SUBSET04_DE_2016$BARRIER4[SUBSET04_DE_2016$F123==8 | SUBSET04_DE_2016$F123==2 | SUBSET04_DE_2016$F123==7] <-"direct" #BARRIERS AT EDUCATIONAL INSTITUTION-LEVEL: directly related to educational offer / autonomous decision-making by educational institution
SUBSET04_DE_2016$BARRIER4[SUBSET04_DE_2016$F123==13 | SUBSET04_DE_2016$F123==3 | SUBSET04_DE_2016$F123==4 | SUBSET04_DE_2016$F123==5] <-"indirect" #BARRIERS AT EDUCATIONAL INSTITUTION-LEVEL:	wider circumstances / non-autonomous / indirect 
SUBSET04_DE_2016$BARRIER4[SUBSET04_DE_2016$F123==99 | SUBSET04_DE_2016$F123==98] <- "N_A" #no answer or no reason
#SUBSET04_DE_2016$BARRIER[SUBSET04_DE_2016$F123==99 | SUBSET04_DE_2016$F123==98] <-"NA" #not applicable and no answer
SUBSET04_DE_2016$BARRIER4 <-factor(SUBSET04_DE_2016$BARRIER4)

table(SUBSET04_DE_2016$BARRIER4)
freq(SUBSET04_DE_2016$BARRIER4)

SUBSET05_DE_2016 <-subset(SUBSET04_DE_2016, BARRIER4!= "NA")
SUBSET05_DE_2016$BARRIER4 <- droplevels(SUBSET05_DE_2016$BARRIER4)
table(SUBSET05_DE_2016$BARRIER4)
freq(SUBSET05_DE_2016$BARRIER4)

#combine barriers to prevent participation in NFE (BARRIER_COMB) // BINARY VARIABLE 
#new variable: barriers to prevent participation in NFE (BARRIER)
SUBSET06_DE_2016 <- subset(SUBSET05_DE_2016)
attributes(SUBSET06_DE_2016$F123)
table(SUBSET06_DE_2016$F123)
freq(SUBSET06_DE_2016$F123)

SUBSET06_DE_2016$BARRIER2 <- -1
SUBSET06_DE_2016$BARRIER2[SUBSET06_DE_2016$F123==1 | SUBSET06_DE_2016$F123==6 | SUBSET06_DE_2016$F123==14 | SUBSET06_DE_2016$F123==11 | SUBSET06_DE_2016$F123==10 | SUBSET06_DE_2016$F123==9 | SUBSET06_DE_2016$F123==12 | SUBSET06_DE_2016$F123==15 | SUBSET06_DE_2016$F123==17] <- "1_ind_level" #BARRIERS AT PERSONAL/INDIVIDUAL-LEVEl 
SUBSET06_DE_2016$BARRIER2[SUBSET06_DE_2016$F123==8 | SUBSET06_DE_2016$F123==2 | SUBSET06_DE_2016$F123==7 | SUBSET06_DE_2016$F123==13 | SUBSET06_DE_2016$F123==3 | SUBSET06_DE_2016$F123==4 | SUBSET06_DE_2016$F123==5] <- "0_inst_level" #BARRIERS AT EDUCATIONAL INSTITUTION-LEVEL
SUBSET06_DE_2016$BARRIER2[SUBSET06_DE_2016$F123==99 | SUBSET06_DE_2016$F123==98] <- "N_A" #no answer or no reason
SUBSET06_DE_2016$BARRIER2 <-factor(SUBSET06_DE_2016$BARRIER2)

table(SUBSET06_DE_2016$BARRIER2)
freq(SUBSET06_DE_2016$BARRIER2)

SUBSET07_DE_2016 <-subset(SUBSET06_DE_2016, BARRIER2!= "N_A")
SUBSET07_DE_2016$BARRIER2 <- droplevels(SUBSET06_DE_2016$BARRIER2)
table(SUBSET07_DE_2016$BARRIER2)
freq(SUBSET07_DE_2016$BARRIER2)

####RE-CODE TO BINARY 0/1##################################
#new variable: barriers to prevent participation in NFE (BARRIER)
SUBSET08_DE_2016 <- subset(SUBSET07_DE_2016)
attributes(SUBSET08_DE_2016$F123)
table(SUBSET08_DE_2016$F123)
freq(SUBSET08_DE_2016$F123)

SUBSET08_DE_2016$BARRIER <- -1
SUBSET08_DE_2016$BARRIER[SUBSET08_DE_2016$F123==1 | SUBSET08_DE_2016$F123==6 | SUBSET08_DE_2016$F123==14 | SUBSET08_DE_2016$F123==11 | SUBSET08_DE_2016$F123==10 | SUBSET08_DE_2016$F123==9 | SUBSET08_DE_2016$F123==12 | SUBSET08_DE_2016$F123==15 | SUBSET08_DE_2016$F123==17] <- "1_IND_LEVEL" #BARRIERS AT PERSONAL/INDIVIDUAL-LEVEl 
SUBSET08_DE_2016$BARRIER[SUBSET08_DE_2016$F123==8 | SUBSET08_DE_2016$F123==2 | SUBSET08_DE_2016$F123==7 | SUBSET08_DE_2016$F123==13 | SUBSET08_DE_2016$F123==3 | SUBSET08_DE_2016$F123==4 | SUBSET08_DE_2016$F123==5] <- "0_INST_LEVEL" #BARRIERS AT EDUCATIONAL INSTITUTION-LEVEL
#SUBSET08_DE_2016$BARRIER[SUBSET08_DE_2016$F123==99 | SUBSET08_DE_2016$F123==98] <- "N_A" #no answer or no reason
SUBSET08_DE_2016$BARRIER <-factor(SUBSET08_DE_2016$BARRIER)
table(SUBSET08_DE_2016$BARRIER)
freq(SUBSET08_DE_2016$BARRIER)

##################Sample statistic##################################
table8<-table(SUBSET08_DE_2016$BARRIER, SUBSET08_DE_2016$NFE_FED)
round(prop.table(table8,2), digits=2)


#########Descriptive Statistics#####################
####################################################
#Cross-Table // previous level of education and Adult education, 2016
crosstab(SUBSET03_DE_2016$NFE_FED, SUBSET03_DE_2016$SKILL, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET03_DE_2016$NFE_FED, SUBSET03_DE_2016$GENDER, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET03_DE_2016$NFE_FED, SUBSET03_DE_2016$AGE, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET03_DE_2016$NFE_FED, SUBSET03_DE_2016$MIGR, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET03_DE_2016$NFE_FED, SUBSET03_DE_2016$EDUC, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET03_DE_2016$NFE_FED, SUBSET03_DE_2016$EMPL, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET03_DE_2016$NFE_FED, SUBSET03_DE_2016$TIME, plot=FALSE,prop.c = TRUE)

#Chi2 Test // previous level of education and Adult education
chisq.test(SUBSET03_DE_2016$NFE_FED, SUBSET03_DE_2016$SKILL, correct= F,)

#########Logistic Regression#####################
####HYPOTHESIS 1###################################
#######Socio-demographics###################################
OUTPUT02 <-glm(NFE_FED~ GENDER+AGE+MIGR+EDUC, data=SUBSET03_DE_2016,family=binomial())
OR.vector02 <-exp(coef(OUTPUT02))
CI.vector02 <-exp(confint(OUTPUT02))
p.values02 <-list(summary(OUTPUT02)$coefficients[,4])

stargazer(OUTPUT02,
          coef= list(OR.vector02), ci = T,
          ci.custom= list(CI.vector02),
          single.row= F,
          type = "text",
          p=c(p.values02))

#######Employment/skill-related variable###################################
OUTPUT03 <-glm(NFE_FED~ EMPL+SKILL+ORG+TIME, data=SUBSET03_DE_2016,family=binomial())
OR.vector03 <-exp(coef(OUTPUT03))
CI.vector03 <-exp(confint(OUTPUT03))
p.values03 <-list(summary(OUTPUT03)$coefficients[,4])

stargazer(OUTPUT03,
          coef= list(OR.vector03), ci = T,
          ci.custom= list(CI.vector03),
          single.row= F,
          type = "text",
          p=c(p.values03))

#######FINAL Model 2016: Hypothesis 1###################################
OUTPUT04 <-glm(NFE_FED~ GENDER+AGE+MIGR+EDUC+EMPL+SKILL+ORG+TIME, data=SUBSET03_DE_2016,family=binomial())
OR.vector04 <-exp(coef(OUTPUT04))
CI.vector04 <-exp(confint(OUTPUT04))
p.values04 <-list(summary(OUTPUT04)$coefficients[,4])

stargazer(OUTPUT04,
          coef= list(OR.vector04), ci = T,
          ci.custom= list(CI.vector04),
          single.row= F,
          type = "text",
          p=c(p.values04))

####################################################################
#######Hypothesis 2###################################
#############Logistic Regressions: HYPO 2######################
#######MODEL1#####################################################
OUTPUT05 <-glm(BARRIER~ GENDER, data=SUBSET08_DE_2016,family=binomial())
OR.vector05 <-exp(coef(OUTPUT05))
CI.vector05 <-exp(confint(OUTPUT05))
p.values05 <-list(summary(OUTPUT05)$coefficients[,4])

stargazer(OUTPUT05,
          coef= list(OR.vector05), ci = T,
          ci.custom= list(CI.vector05),
          single.row= F,
          type = "text",
          p=c(p.values05))

#######MODEL2: Socio-demographics###################################
OUTPUT06 <-glm(BARRIER~ GENDER+ALTER+MIGR+EDUC, data=SUBSET08_DE_2016,family=binomial())
OR.vector06 <-exp(coef(OUTPUT06))
CI.vector06 <-exp(confint(OUTPUT06))
p.values06 <-list(summary(OUTPUT06)$coefficients[,4])

stargazer(OUTPUT06,
          coef= list(OR.vector06), ci = T,
          ci.custom= list(CI.vector06),
          single.row= F,
          type = "text",
          p=c(p.values06))

#######MODEL3: Employment/skill-related variable###################################
OUTPUT07 <-glm(BARRIER~ EMPL+SKILL+ORG+TIME, data=SUBSET08_DE_2016,family=binomial())
OR.vector07 <-exp(coef(OUTPUT07))
CI.vector07 <-exp(confint(OUTPUT07))
p.values07 <-list(summary(OUTPUT07)$coefficients[,4])

stargazer(OUTPUT07,
          coef= list(OR.vector07), ci = T,
          ci.custom= list(CI.vector07),
          single.row= F,
          type = "text",
          p=c(p.values07))

#######MODEL4: ALL IVs#########################################
OUTPUT08 <-glm(BARRIER~ GENDER+AGE+MIGR+EDUC+EMPL+SKILL+ORG+TIME, data=SUBSET08_DE_2016,family=binomial())
OR.vector08 <-exp(coef(OUTPUT08))
CI.vector08 <-exp(confint(OUTPUT08))
p.values08 <-list(summary(OUTPUT08)$coefficients[,4])

stargazer(OUTPUT08,
          coef= list(OR.vector08), ci = T,
          ci.custom= list(CI.vector08),
          single.row= F,
          type = "text",
          p=c(p.values08))

#####Cross-tab#######################
crosstab(SUBSET08_DE_2016$BARRIER, SUBSET08_DE_2016$EDUC, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET08_DE_2016$BARRIER, SUBSET08_DE_2016$GENDER, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET08_DE_2016$BARRIER, SUBSET08_DE_2016$MIGR, plot=FALSE,prop.c = TRUE)
crosstab(SUBSET08_DE_2016$BARRIER, SUBSET08_DE_2016$SKILL, plot=FALSE,prop.c = TRUE)

########################################## 

#COMBINE DATASETS FROM 2012 AND 2016 
#New Variable: YEAR 2012  
SUBSET04a_DE_2012$YEAR <- "2012" 
SUBSET04a_DE_2012$YEAR <- factor(SUBSET04a_DE_2012$YEAR) 


#Select variables & save data FOR 2012 
SUBSET_DE_2012 <-subset(SUBSET04a_DE_2012, select=c(NFE_FED, GENDER, AGE, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, BARRIER)) 
saveRDS(SUBSET_DE_2012, file= "AES_2012_DE.rds") 


#New Variable: YEAR 2016 
SUBSET03b_DE_2016$YEAR <- "2016" 
SUBSET03b_DE_2016$YEAR <- factor(SUBSET03b_DE_2016$YEAR) 


#Select variables & save data FOR 2016 
SUBSET_DE_2016 <-subset(SUBSET03b_DE_2016, select=c(NFE_FED, GENDER, AGE, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, BARRIER)) 
saveRDS(SUBSET_DE_2016, file= "AES_2016_DE.rds") 

#######HIERHIERHIER#########

#Combining Data Sets of 2012 and 2016 for GERMANY with new variables 
AES_2012_DE <-readRDS("AES_2012_DE.rds") 
AES_2016_DE <-readRDS("AES_2016_DE.rds") 
AES_2012_2016_DE <-rbind(AES_2012_DE,AES_2016_DE) 


#Sample Statistics 
#Set Reference Category 
AES_2012_2016_DE$YEAR   <-relevel(AES_2012_2016_DE$YEAR  , ref="2012") 


#Crosstab of dependent variable and years (2012 and 2016) 
descr::crosstab(AES_2012_2016_DE$NFE_FED, AES_2012_2016_DE$YEAR, plot=FALSE, prop.c = TRUE) 

#Crosstab of dependent variable and years (2012 and 2016) 
descr::crosstab(AES_2012_2016_DE$BARRIER, AES_2012_2016_DE$YEAR, plot=FALSE, prop.c = TRUE) 


#########REASONS######################
###############NEW variable: reasons to participate in NFE (REASON)#############################
attributes(SUBSET03_DE_2016$F117_1)
table(SUBSET03_DE_2016$F117_1)
freq(SUBSET03_DE_2016$F117_1)

SUBSET03_DE_2016$REASON_NFE <- "NA"
SUBSET03_DE_2016$REASON_NFE[SUBSET03_DE_2016$F117_1==1 | SUBSET03_DE_2016$F117_1==2 | SUBSET03_DE_2016$F117_1==3 | SUBSET03_DE_2016$F117_1==4 | SUBSET03_DE_2016$F117_1==5] <-"0" #job_rel #job-related reasons to participate in NFE
SUBSET03_DE_2016$REASON_NFE[SUBSET03_DE_2016$F117_1==6 | SUBSET03_DE_2016$F117_1==7 | SUBSET03_DE_2016$F117_1==8 | SUBSET03_DE_2016$F117_1==9 | SUBSET03_DE_2016$F117_1==10 | SUBSET03_DE_2016$F117_1==11] <-"1" #personal #personal REASON_NFEs to participate in NFE
SUBSET03_DE_2016$REASON_NFE[SUBSET03_DE_2016$F117_1==97 | SUBSET03_DE_2016$F117_1==98 | SUBSET03_DE_2016$F117_1==99] <- "NA" #no answer or no reason
SUBSET03_DE_2016$REASON_NFE <-factor(SUBSET03_DE_2016$REASON_NFE)
attributes(SUBSET03_DE_2016$REASON_NFE)
table(SUBSET03_DE_2016$REASON_NFE)
freq(SUBSET03_DE_2016$REASON_NFE)

######Regression Analysis for 2016: Incentives and IVs###########
OUTPUT07 <-glm(REASON_NFE~ GENDER+AGE+MIGR+EDUC+EMPL+SKILL+ORG+TIME, data=SUBSET03_DE_2016,family=binomial())
OR.vector07 <-exp(coef(OUTPUT07))
CI.vector07 <-exp(confint(OUTPUT07))
p.values07 <-list(summary(OUTPUT07)$coefficients[,4])

stargazer(OUTPUT07,
          coef= list(OR.vector07), ci = T,
          ci.custom= list(CI.vector07),
          single.row= F,
          type = "text",
          p=c(p.values07))


###############new variable: reasons to participate in FED (REASON)#############################
attributes(SUBSET03_DE_2016$F07101)
table(SUBSET03_DE_2016$F07101)
freq(SUBSET03_DE_2016$F07101)

attributes(SUBSET03_DE_2016$F071a)
table(SUBSET03_DE_2016$F071a)
freq(SUBSET03_DE_2016$F071a)

SUBSET03_DE_2016$REASON_FED <- "NA"
SUBSET03_DE_2016$REASON_FED[SUBSET03_DE_2016$F071a==1 | SUBSET03_DE_2016$F071a==2 | SUBSET03_DE_2016$F071a==3 | SUBSET03_DE_2016$F071a==4 | SUBSET03_DE_2016$F071a==5] <-"1_job_rel" #job-related REASON_FEDs to participate in NFE
SUBSET03_DE_2016$REASON_FED[SUBSET03_DE_2016$F071a==6 | SUBSET03_DE_2016$F071a==7 | SUBSET03_DE_2016$F071a==8 | SUBSET03_DE_2016$F071a==9 | SUBSET03_DE_2016$F071a==10 | SUBSET03_DE_2016$F071a==11] <-"0_personal" #personal REASON_FEDs to participate in NFE
SUBSET03_DE_2016$REASON_FED[SUBSET03_DE_2016$F071a==97 | SUBSET03_DE_2016$F071a==99] <- "NA" #no answer or no REASON_FED
SUBSET03_DE_2016$REASON_FED <-factor(SUBSET03_DE_2016$REASON_FED)
attributes(SUBSET03_DE_2016$REASON_FED)
table(SUBSET03_DE_2016$REASON_FED)
freq(SUBSET03_DE_2016$REASON_FED)
###!!!!!PROBLEM: the ones that only mentioned one possible utility in F071 (FED) were not autmatically transferred into the question #F071a
#hence we have rather small sample size (n=200)


#Incentives to participate in NFE 
attributes(SUBSET04_DE_2016$F09901_1)
table(SUBSET04_DE_2016$F09901_1)
freq(SUBSET04_DE_2016$F09901_1)

attributes(SUBSET04_DE_2016$F09902_1)
table(SUBSET04_DE_2016$F09902_1)
freq(SUBSET04_DE_2016$F09902_1)

attributes(SUBSET04_DE_2016$F09903_1)
table(SUBSET04_DE_2016$F09903_1)
freq(SUBSET04_DE_2016$F09903_1)

attributes(SUBSET04_DE_2016$F09904_1)
table(SUBSET04_DE_2016$F09904_1)
freq(SUBSET04_DE_2016$F09904_1)

attributes(SUBSET04_DE_2016$F09905_1)
table(SUBSET04_DE_2016$F09905_1)
freq(SUBSET04_DE_2016$F09905_1)

attributes(SUBSET04_DE_2016$F09906_1)
table(SUBSET04_DE_2016$F09906_1)
freq(SUBSET04_DE_2016$F09906_1)

attributes(SUBSET04_DE_2016$F09907_1)
table(SUBSET04_DE_2016$F09907_1)
freq(SUBSET04_DE_2016$F09907_1)

attributes(SUBSET04_DE_2016$F09908_1)
table(SUBSET04_DE_2016$F09908_1)
freq(SUBSET04_DE_2016$F09908_1)

attributes(SUBSET04_DE_2016$F09909_1)
table(SUBSET04_DE_2016$F09909_1)
freq(SUBSET04_DE_2016$F09909_1)

attributes(SUBSET04_DE_2016$F09910_1)
table(SUBSET04_DE_2016$F09910_1)
freq(SUBSET04_DE_2016$F09910_1)

attributes(SUBSET04_DE_2016$F09911_1)
table(SUBSET04_DE_2016$F09911_1)
freq(SUBSET04_DE_2016$F09911_1)

attributes(SUBSET04_DE_2016$F09912_1)
table(SUBSET04_DE_2016$F09912_1)
freq(SUBSET04_DE_2016$F09912_1)

attributes(SUBSET04_DE_2016$F09913_1)
table(SUBSET04_DE_2016$F09913_1)
freq(SUBSET04_DE_2016$F09913_1)

attributes(SUBSET04_DE_2016$F09914_1)
table(SUBSET04_DE_2016$F09914_1)
freq(SUBSET04_DE_2016$F09914_1)

attributes(SUBSET04_DE_2016$F09998_1)
table(SUBSET04_DE_2016$F09998_1)
freq(SUBSET04_DE_2016$F09998_1)

attributes(SUBSET04_DE_2016$F09999_1)
table(SUBSET04_DE_2016$F09999_1)
freq(SUBSET04_DE_2016$F09999_1)



#ALTERNATIVE
attributes(SUBSET04_DE_2016$F071a)
table(SUBSET04_DE_2016$F071a)
freq(SUBSET04_DE_2016$F071a)


###RANDOM TRIES#####################
attributes(SUBSET05_DE_2016$F123)
table(SUBSET05_DE_2016$F123) 
freq(SUBSET05_DE_2016$F123)

attributes(SUBSET03_DE_2016$F120)
table(SUBSET03_DE_2016$F120) 
freq(SUBSET03_DE_2016$F120)

attributes(SUBSET03_DE_2016$F122)
table(SUBSET03_DE_2016$F122) 
freq(SUBSET03_DE_2016$F122)

#new variable: barriers to prevent participation in NFE (BARRIER)
#"prerequisits" #does not fulfill the prerequisits
attributes(SUBSET03_DE_2016$F12201)
table(SUBSET03_DE_2016$F12201) 
freq(SUBSET03_DE_2016$F12201)

attributes(SUBSET05_DE_2016$F12201)
table(SUBSET05_DE_2016$F12201) 
freq(SUBSET05_DE_2016$F12201)

#"costs" #too expensive
attributes(SUBSET03_DE_2016$F12202)
table(SUBSET03_DE_2016$F12202) 
freq(SUBSET03_DE_2016$F12202)

#"lack_support" #lack of employer/public service support
attributes(SUBSET03_DE_2016$F12203)
table(SUBSET03_DE_2016$F12203) 
freq(SUBSET03_DE_2016$F12203)

#"lack_support" #lack of employer/public service support
attributes(SUBSET03_DE_2016$F12204)
table(SUBSET03_DE_2016$F12204)
freq(SUBSET03_DE_2016$F12204)

#"time_conflict" #education cannot be combined with work time or other duties
attributes(SUBSET03_DE_2016$F12205)
table(SUBSET03_DE_2016$F12205)
freq(SUBSET03_DE_2016$F12205)

#"family" #no time due to family obligations
attributes(SUBSET03_DE_2016$F12206)
table(SUBSET03_DE_2016$F12206)
freq(SUBSET03_DE_2016$F12206)

#"distance" #no suitable educational offer in reachable distance
attributes(SUBSET03_DE_2016$F12207)
table(SUBSET03_DE_2016$F12207)
freq(SUBSET03_DE_2016$F12207)

#"offer" #there is no suitable offer available. 
attributes(SUBSET03_DE_2016$F12208)
table(SUBSET03_DE_2016$F12208)
freq(SUBSET03_DE_2016$F12208)

#"bad_exp" #bad prior learning experience
attributes(SUBSET03_DE_2016$F12209)
table(SUBSET03_DE_2016$F12209)
freq(SUBSET03_DE_2016$F12209)

#"health" #health-reasons
attributes(SUBSET03_DE_2016$F12210)
table(SUBSET03_DE_2016$F12210)
freq(SUBSET03_DE_2016$F12210)

#"age" #due to age constraints
attributes(SUBSET03_DE_2016$F12211)
table(SUBSET03_DE_2016$F12211)
freq(SUBSET03_DE_2016$F12211)

#"personal" #other personal reasons
attributes(SUBSET03_DE_2016$F12212)
table(SUBSET03_DE_2016$F12212)
freq(SUBSET03_DE_2016$F12212)

#"counseling" #counseling required to decide on adult education activity 
attributes(SUBSET03_DE_2016$F12213)
table(SUBSET03_DE_2016$F12213)
freq(SUBSET03_DE_2016$F12213)

#"computer" #no access to computer or internet
attributes(SUBSET03_DE_2016$F12214)
table(SUBSET03_DE_2016$F12214)
freq(SUBSET03_DE_2016$F12214)

#"personal" #other personal reasons
attributes(SUBSET03_DE_2016$F12215)
table(SUBSET03_DE_2016$F12215)
freq(SUBSET03_DE_2016$F12215)

#"personal" #other personal reasons
attributes(SUBSET03_DE_2016$F12217)
table(SUBSET03_DE_2016$F12217)
freq(SUBSET03_DE_2016$F12217)

#combine variables
SUBSET04_DE_2016 <- subset(SUBSET03_DE_2016)
#SUBSET05_DE_2016 <- subset(SUBSET04_DE_2016, SUBSET04_DE_2016$F120==2) #non-participation in FED and NFE
SUBSET05_DE_2016 <- subset(SUBSET04_DE_2016)
SUBSET05_DE_2016$BARRIER <- -1
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12201==1] <- "prerequisits" #does not fulfill the prerequisits"
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12202==1] <- "costs" #too expensive
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12203==1 | SUBSET05_DE_2016$F12204==1] <- "lack_support" #lack of employer/public service support
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12205==1] <- "time_conflict" #education cannot be combined with work time or other duties
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12206==1] <- "family" #no time due to family obligations
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12207==1] <- "distance" #no suitable educational offer in reachable distance
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12208==1] <- "offer" #there is no suitable offer available. 
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12209==1] <- "bad_exp" #bad prior learning experience
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12210==1] <- "health" #health-reasons
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12211==1] <- "age" #due to age constraints
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12212==1 | SUBSET05_DE_2016$F12215==1 | SUBSET05_DE_2016$F12217==1] <- "personal" #other personal reasons
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12213==1] <- "counseling" #counseling required to decide on adult education activity 
SUBSET05_DE_2016$BARRIER[SUBSET05_DE_2016$F12214==1] <- "computer" #no access to computer or internet
SUBSET05_DE_2016$BARRIER <- factor(SUBSET05_DE_2016$BARRIER)

attributes(SUBSET05_DE_2016$BARRIER)
table(SUBSET05_DE_2016$BARRIER)
freq(SUBSET05_DE_2016$BARRIER)




