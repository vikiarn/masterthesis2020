
########GERMAN SAMPLE############

# Packages
install.packages("haven")
install.packages("stargazer")
install.packages("descr")
install.packages("ggplot2")
install.packages("questionr")
install.packages("tidyverse")
install.packages("margins")
install.packages("sjPlot")
install.packages("effects")
install.packages("nnet")
install.packages("reshape2")
install.packages("RColorBrewer")

#Libraries
library(haven)
library(descr)
library(stargazer)
library(ggplot2)
library(questionr)
library(tidyverse)
library(margins) 
library(sjPlot)
library(haven)
library(nnet)
library(effects)
library(reshape2)
library(RColorBrewer)

#Set Working Directory
setwd("~/OneDrive/Master Thesis/Data")

########GERMANY_Read in data & Select sample for DE AES 2012###############
AES2012_DE <- read_dta("DE_AES_2012_ZA5354_Personen_v1-0-0.dta")
SUBSET01a_DE_2012<-subset(AES2012_DE)

#FILTER: limit samples to potential AE participants and are out of their primary education
attributes(SUBSET01a_DE_2012$F006)
SUBSET01_DE_2012 <- subset(SUBSET01a_DE_2012, SUBSET01a_DE_2012$F006==1 | SUBSET01a_DE_2012$F006==2 | SUBSET01a_DE_2012$F006==3 | SUBSET01a_DE_2012$F006==4 | SUBSET01a_DE_2012$F006==8 | SUBSET01a_DE_2012$F006==9 | SUBSET01a_DE_2012$F006==10 | SUBSET01a_DE_2012$F006==11 | SUBSET01a_DE_2012$F006==99)
table(SUBSET01_DE_2012$F006)

#DV: Participation in non-formal (NFE) or formal adult education (FED) 
#new variable: Participation in adult education (formal and non-formal)
attributes(SUBSET01_DE_2012$F120)
SUBSET01_DE_2012$NFE_FED <- -1
SUBSET01_DE_2012$NFE_FED[SUBSET01_DE_2012$F120==1] <- "1-yes" 
SUBSET01_DE_2012$NFE_FED[SUBSET01_DE_2012$F120==2] <- "0-no" 
SUBSET01_DE_2012$NFE_FED <-factor(SUBSET01_DE_2012$NFE_FED)
table(SUBSET01_DE_2012$NFE_FED)
freq(SUBSET01_DE_2012$NFE_FED)

#new variable: GENDER
attributes(SUBSET01_DE_2012$F001)
SUBSET01_DE_2012$GENDER <- -1
SUBSET01_DE_2012$GENDER[SUBSET01_DE_2012$F001==1] <-"Male"
SUBSET01_DE_2012$GENDER[SUBSET01_DE_2012$F001==2] <-"Female"
SUBSET01_DE_2012$GENDER <-factor(SUBSET01_DE_2012$GENDER)
table(SUBSET01_DE_2012$GENDER)
freq(SUBSET01_DE_2012$GENDER)

#new variable: AGE 
attributes(SUBSET01_DE_2012$alter)
sum(is.na(SUBSET01_DE_2012$alter))
SUBSET01_DE_2012$AGE <- "NA"
SUBSET01_DE_2012$AGE[SUBSET01_DE_2012$alter>17 & SUBSET01_DE_2012$alter<35] <-"1-young"
SUBSET01_DE_2012$AGE[SUBSET01_DE_2012$alter>35 & SUBSET01_DE_2012$alter<53] <-"2-medium"
SUBSET01_DE_2012$AGE[SUBSET01_DE_2012$alter>54] <-"3-old"
table(SUBSET01_DE_2012$AGE)

#delete missing values
SUBSET01a_DE_2012 <-subset(SUBSET01_DE_2012, AGE!="NA")
SUBSET01a_DE_2012$AGE <- droplevels(SUBSET01a_DE_2012$AGE)
SUBSET01a_DE_2012$AGE <-factor(SUBSET01a_DE_2012$AGE)
table(SUBSET01a_DE_2012$AGE)
freq(SUBSET01a_DE_2012$AGE)

#new variable: migrant background (MIGR)
attributes(SUBSET01a_DE_2012$migra)
#delete missing values
SUBSET02_DE_2012 <- subset(SUBSET01a_DE_2012, SUBSET01a_DE_2012$migra<8)
SUBSET02_DE_2012$MIGR <- -1
SUBSET02_DE_2012$MIGR[SUBSET02_DE_2012$migra==1] <- "DE" #no migrant background
SUBSET02_DE_2012$MIGR[SUBSET02_DE_2012$migra==2 | SUBSET02_DE_2012$migra==3] <- "non-DE" #migrant background
SUBSET02_DE_2012$MIGR <-factor(SUBSET02_DE_2012$MIGR)
table(SUBSET02_DE_2012$MIGR)
freq(SUBSET02_DE_2012$MIGR)

#new variable: highest primary education degree (EDUC)
attributes(SUBSET02_DE_2012$schulab3)
#exclude the ones who are still pupils and missing values
SUBSET03_DE_2012 <- subset(SUBSET02_DE_2012, SUBSET02_DE_2012$schulab3<4)
SUBSET03_DE_2012$EDUC <- -1
SUBSET03_DE_2012$EDUC[SUBSET03_DE_2012$schulab3==0 | SUBSET03_DE_2012$schulab3==1] <- "3_low" 
SUBSET03_DE_2012$EDUC[SUBSET03_DE_2012$schulab3==2] <- "2_middle" 
SUBSET03_DE_2012$EDUC[SUBSET03_DE_2012$schulab3==3] <- "1_high" 
attributes(SUBSET03_DE_2012$EDUC)
table(SUBSET03_DE_2012$EDUC)
freq(SUBSET03_DE_2012$EDUC)

#new variable: part-time/full-time work (TIME)
attributes(SUBSET03_DE_2012$F006)
SUBSET03_DE_2012$TIME <- -1
SUBSET03_DE_2012$TIME[SUBSET03_DE_2012$F006==1] <- "1-FT" 
SUBSET03_DE_2012$TIME[SUBSET03_DE_2012$F006==2] <- "2-PT"
SUBSET03_DE_2012$TIME[SUBSET03_DE_2012$F006==3 | SUBSET03_DE_2012$F006==4 | SUBSET03_DE_2012$F006==5 | SUBSET03_DE_2012$F006==6 | SUBSET03_DE_2012$F006==7 | SUBSET03_DE_2012$F006==8 | SUBSET03_DE_2012$F006==9 | SUBSET03_DE_2012$F006==10 | SUBSET03_DE_2012$F006==11 | SUBSET03_DE_2012$F006==99] <- "3-OT" 
attributes(SUBSET03_DE_2012$TIME)
table(SUBSET03_DE_2012$TIME)
freq(SUBSET03_DE_2012$TIME)

#new variable: establishment size (ORG)
attributes(SUBSET03_DE_2012$x019)
SUBSET03_DE_2012$ORG <- "4-NA"
SUBSET03_DE_2012$ORG[SUBSET03_DE_2012$x019==1 | SUBSET03_DE_2012$x019==2 | SUBSET03_DE_2012$x019==3] <-"1-small"
SUBSET03_DE_2012$ORG[SUBSET03_DE_2012$x019==4] <-"2-medium" 
SUBSET03_DE_2012$ORG[SUBSET03_DE_2012$x019==5 | SUBSET03_DE_2012$x019==6] <-"3-large"
SUBSET03_DE_2012$ORG[SUBSET03_DE_2012$x019==8 | SUBSET03_DE_2012$x019==9] <-"4-NA"
SUBSET03_DE_2012$ORG <-factor(SUBSET03_DE_2012$ORG)
table(SUBSET03_DE_2012$ORG)
freq(SUBSET03_DE_2012$ORG)

#new variable: employment status (EMPL)
attributes(SUBSET03_DE_2012$stelk)

#delete missing values
SUBSET04_DE_2012 <- subset(SUBSET03_DE_2012, SUBSET03_DE_2012$stelk<5 | SUBSET03_DE_2012$stelk>5)
SUBSET04_DE_2012$EMPL <- "NA"
SUBSET04_DE_2012$EMPL[SUBSET04_DE_2012$stelk==6] <-"3_not-empl" 
SUBSET04_DE_2012$EMPL[SUBSET04_DE_2012$stelk==4] <-"2_self-empl" 
SUBSET04_DE_2012$EMPL[SUBSET04_DE_2012$stelk==1 | SUBSET04_DE_2012$stelk==2 | SUBSET04_DE_2012$stelk==3] <-"1_employee" 
SUBSET04_DE_2012$EMPL <-factor(SUBSET04_DE_2012$EMPL)
table(SUBSET04_DE_2012$EMPL)
freq(SUBSET04_DE_2012$EMPL)

#new variable: skill level (Occupational Group / ISCO08) (SKILL)
attributes(SUBSET04_DE_2012$isco08_1)
table(SUBSET04_DE_2012$isco08_1)
#relabel into five categories 
SUBSET04_DE_2012$SKILL <- "NA"
SUBSET04_DE_2012$SKILL[SUBSET04_DE_2012$isco08_1==1 | SUBSET04_DE_2012$isco08_1==2] <-"1-Managers" 
SUBSET04_DE_2012$SKILL[SUBSET04_DE_2012$isco08_1==3] <-"2-Technicians" 
SUBSET04_DE_2012$SKILL[SUBSET04_DE_2012$isco08_1==4 | SUBSET04_DE_2012$isco08_1==5 | SUBSET04_DE_2012$isco08_1==6 | SUBSET04_DE_2012$isco08_1==7 | SUBSET04_DE_2012$isco08_1==8] <-"3-Clerks_Workers" 
SUBSET04_DE_2012$SKILL[SUBSET04_DE_2012$isco08_1==9] <-"4-Elementary" 
SUBSET04_DE_2012$SKILL[SUBSET04_DE_2012$isco08_1==-1 | SUBSET04_DE_2012$isco08_1==0] <-"NA"
table(SUBSET04_DE_2012$SKILL)
freq(SUBSET04_DE_2012$SKILL)

########Sample statistics GER 2012#########################################
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

#new variable: barriers to participation in NFE and FED (BARRIER)
attributes(SUBSET04_DE_2012$F123)
SUBSET04_DE_2012$BARRIER <- "NA"
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==3] <-"1.1_prerequisits" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==4] <-"3.2_costs" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==5] <-"4.2_lack_support" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==6] <-"4.3_time_conflict" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==7] <-"1.4_family" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==8] <-"3.3_distance" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==9] <-"3.1_offer" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==10] <- "2.1_neg_prior_exp" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==11 | SUBSET04_DE_2012$F123==12] <-"1.3_age_health" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==13 | SUBSET04_DE_2012$F123==16] <-"2.2_personal" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==14] <- "4.1_counselling" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==15] <- "1.2_computer" 
SUBSET04_DE_2012$BARRIER[SUBSET04_DE_2012$F123==98 | SUBSET04_DE_2012$F123==1 | SUBSET04_DE_2012$F123==2 | SUBSET04_DE_2012$F123==99] <- "NA" 
SUBSET04_DE_2012$BARRIER <-factor(SUBSET04_DE_2012$BARRIER)

#delete missing values
SUBSET04a_DE_2012 <-subset(SUBSET04_DE_2012, BARRIER!= "NA")
SUBSET04a_DE_2012$BARRIER <- droplevels(SUBSET04a_DE_2012$BARRIER)
table(SUBSET04a_DE_2012$BARRIER)
freq(SUBSET04a_DE_2012$BARRIER)

#RE-CODE TO BINARY 0/1
SUBSET05_DE_2012 <- subset(SUBSET04a_DE_2012)
SUBSET05_DE_2012$BARRIER2 <- -1
SUBSET05_DE_2012$BARRIER2[SUBSET05_DE_2012$F123==3 | SUBSET05_DE_2012$F123==15 | SUBSET05_DE_2012$F123==7 | SUBSET05_DE_2012$F123==12 | SUBSET05_DE_2012$F123==11 | SUBSET05_DE_2012$F123==10 | SUBSET05_DE_2012$F123==13 | SUBSET05_DE_2012$F123==16] <- "1_ind_level" 
SUBSET05_DE_2012$BARRIER2[SUBSET05_DE_2012$F123==9 | SUBSET05_DE_2012$F123==4 | SUBSET05_DE_2012$F123==8 | SUBSET05_DE_2012$F123==14 | SUBSET05_DE_2012$F123==5 | SUBSET05_DE_2012$F123==6] <- "0_inst_level" 
SUBSET05_DE_2012$BARRIER2[SUBSET05_DE_2012$F123==99 | SUBSET05_DE_2012$F123==98 | SUBSET05_DE_2012$F123==1 | SUBSET05_DE_2012$F123==2] <- "N_A" #no answer or no reason
SUBSET05_DE_2012$BARRIER2 <-factor(SUBSET05_DE_2012$BARRIER2)
table(SUBSET05_DE_2012$BARRIER2)
freq(SUBSET05_DE_2012$BARRIER2)

#delete missing values
SUBSET06_DE_2012 <-subset(SUBSET05_DE_2012, BARRIER2!= "N_A")
SUBSET07_DE_2012 <-subset(SUBSET06_DE_2012, BARRIER2!= -1)
SUBSET07_DE_2012$BARRIER2 <- droplevels(SUBSET07_DE_2012$BARRIER2)
table(SUBSET07_DE_2012$BARRIER2)
freq(SUBSET07_DE_2012$BARRIER2)

#RE-CODE TO 4 Categories
SUBSET07_DE_2012$BARRIER4 <- -1
SUBSET07_DE_2012$BARRIER4[SUBSET07_DE_2012$F123==3 | SUBSET07_DE_2012$F123==11 | SUBSET07_DE_2012$F123==12 | SUBSET07_DE_2012$F123==15 | SUBSET07_DE_2012$F123==7] <-"1_socio_econ_dem" 
SUBSET07_DE_2012$BARRIER4[SUBSET07_DE_2012$F123==10 | SUBSET07_DE_2012$F123==13 | SUBSET07_DE_2012$F123==16] <-"2_psych" 
SUBSET07_DE_2012$BARRIER4[SUBSET07_DE_2012$F123==8 | SUBSET07_DE_2012$F123==9 | SUBSET07_DE_2012$F123==4] <-"3_direct" 
SUBSET07_DE_2012$BARRIER4[SUBSET07_DE_2012$F123==14 | SUBSET07_DE_2012$F123==6 | SUBSET07_DE_2012$F123==5 | SUBSET07_DE_2012$F123==5] <-"4_indirect" 
SUBSET07_DE_2012$BARRIER4[SUBSET07_DE_2012$F123==99 | SUBSET07_DE_2012$F123==98] <- "N_A"
SUBSET07_DE_2012$BARRIER4 <-factor(SUBSET07_DE_2012$BARRIER4)
table(SUBSET07_DE_2012$BARRIER4)
freq(SUBSET07_DE_2012$BARRIER4)

#delete missing values
SUBSET08_DE_2012 <-subset(SUBSET07_DE_2012, BARRIER4!= "N_A")
SUBSET08_DE_2012$BARRIER4 <- droplevels(SUBSET08_DE_2012$BARRIER4)
table(SUBSET08_DE_2012$BARRIER4)
freq(SUBSET08_DE_2012$BARRIER4)

#Sample statistic
table8<-table(SUBSET07_DE_2012$BARRIER2, SUBSET07_DE_2012$NFE_FED)
round(prop.table(table8,2), digits=2)

########Descriptive Statistics#############################################
#Cross-Table // occupational skill level and Adult education, 2012
crosstab(SUBSET04_DE_2012$NFE_FED, SUBSET04_DE_2012$SKILL, plot=FALSE,prop.c = TRUE)

#Chi2 Test 
chisq.test(SUBSET04_DE_2012$NFE_FED, SUBSET04_DE_2012$SKILL, correct= F,)

########Logistic Regression: HYPOTHESIS 1##################################
########Model 1: Socio-demographics########################################
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

########Model 2: Employment/skill-related variable#########################
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

########Model 3: Final Model###############################################
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

########HYPOTHESIS 2: INCENTIVES###########################################
#new variable: reasons to participate in NFE and FED (REASON)
attributes(SUBSET04_DE_2012$F09901_1)
table(SUBSET04_DE_2012$F09901_1)
SUBSET04_DE_2012$INCENTIVENFE <- "NA"
SUBSET04_DE_2012$INCENTIVENFE[SUBSET04_DE_2012$F09906_1==1 | SUBSET04_DE_2012$F09910_1==1 | SUBSET04_DE_2012$F09908_1==1 | SUBSET04_DE_2012$F09907_1==1] <- "2_personal"
SUBSET04_DE_2012$INCENTIVENFE[(SUBSET04_DE_2012$F09901_1==1 | SUBSET04_DE_2012$F09912_1==1 | SUBSET04_DE_2012$F09902_1==1 | SUBSET04_DE_2012$F09903_1==1 | SUBSET04_DE_2012$F09904_1==1 | SUBSET04_DE_2012$F09905_1==1 | SUBSET04_DE_2012$F09909_1==1) & (SUBSET04_DE_2012$INCENTIVENFE=="2_personal")] <- "1_job-rel + personal"
SUBSET04_DE_2012$INCENTIVENFE[(SUBSET04_DE_2012$F09901_1==1 | SUBSET04_DE_2012$F09912_1==1 | SUBSET04_DE_2012$F09902_1==1 | SUBSET04_DE_2012$F09903_1==1 | SUBSET04_DE_2012$F09904_1==1 | SUBSET04_DE_2012$F09905_1==1 | SUBSET04_DE_2012$F09909_1==1) & (SUBSET04_DE_2012$INCENTIVENFE!="2_personal" & SUBSET04_DE_2012$INCENTIVENFE!="1_job-rel + personal")] <- "3_job-rel"
SUBSET04m_DE_2012 <- subset(SUBSET04_DE_2012, INCENTIVENFE!= "NA")
SUBSET04m_DE_2012$INCENTIVENFE <- droplevels(SUBSET04m_DE_2012$INCENTIVENFE)
table(SUBSET04m_DE_2012$INCENTIVENFE)
freq(SUBSET04m_DE_2012$INCENTIVENFE)

#Sample statistics
table9<-table(SUBSET04m_DE_2012$INCENTIVENFE, SUBSET04m_DE_2012$NFE_FED)
round(prop.table(table9,2), digits=2)

########MULTINOMINAL#######################################################
OUTPUT01m <- multinom(INCENTIVENFE ~ SKILL + GENDER + AGE + ORG + EDUC + TIME + MIGR + EMPL, data = SUBSET04m_DE_2012)
# to see output
summary(OUTPUT01m)
#Calculation of Significance
## z value:
z <- summary(OUTPUT01m)$coefficients/summary(OUTPUT01m)$standard.errors
## 2-tailed z test
p <- (1-pnorm(abs(z), 0, 1))*2
p
#Display of results
stargazer(OUTPUT01m, type="text", p.auto=F)
#Table with RRR
OUTPUT01rrr=exp(coef(OUTPUT01m))
stargazer(OUTPUT01m, coef=list(OUTPUT01rrr), type="text", p.auto=F)
#Calculation of predicted probabilities using the effects package
OUTPUT01PP <- Effect("SKILL", OUTPUT01m)
OUTPUT01PP$model.matrix
#Preparation for Graph
pp <- data.frame(OUTPUT01PP$prob)
# Assign the labels in the order of the rows that you identified with the model matrix
pp$labels <- c("1_Managers", "2_Technicians", "3_Clerks_Workers", "4_Elementary", "5_NA")
pp <- melt(pp, id="labels")
names(pp)[names(pp)=="value"] <- "pp"

#Extraction Names DV
pp$INCENTIVENFE <-substring(pp[,2],6)
lp <- data.frame(OUTPUT01PP$lower.prob)
lp <- melt(lp)
names(lp)[names(lp)=="value"] <- "l"
up <- data.frame(OUTPUT01PP$upper.prob)
up <- melt(up)
names(up)[names(up)=="value"] <- "u"
toplot <- cbind(pp, lp[,2], up[,2])

#Organisation DV
toplot$INCENTIVENFE <- factor(toplot$INCENTIVENFE )
levels(toplot$INCENTIVENFE )
toplot$INCENTIVENFE= factor(toplot$INCENTIVENFE, levels(toplot$INCENTIVENFE)[c(1,2,3)])

#Organisation IV
toplot$labels <- factor(toplot$labels)
levels(toplot$labels)
toplot$labels = factor(toplot$labels, levels(toplot$labels)[c(1,2,3,4,5)])

#Plotting the Multinominals
getPalette = colorRampPalette(brewer.pal(5, "RdBu"))
ggplot(toplot, aes(x=INCENTIVENFE, y=pp, fill=labels)) + 
  geom_bar( stat="identity", position = "dodge") +
  geom_errorbar( aes( ymin=lp[,2], ymax=up[,2]), width=0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = getPalette(5)) +
  theme_bw() +
  ggtitle("Incentives to participate in AE in Germany, 2012") +
  xlab("Incentives")

#Cross-tabs
crosstab(SUBSET04m_DE_2012$INCENTIVENFE, SUBSET04m_DE_2012$SKILL, plot=FALSE,prop.c = TRUE)
#Chi-Square test
chisq.test(SUBSET04m_DE_2012$INCENTIVENFE, SUBSET04m_DE_2012$SKILL, correct= F,)


########Hypothesis 3#######################################################
########MODEL 1: Socio-demographics########################################
OUTPUT06 <-glm(BARRIER2~ GENDER+AGE+MIGR+EDUC, data=SUBSET07_DE_2012,family=binomial())
OR.vector06 <-exp(coef(OUTPUT06))
CI.vector06 <-exp(confint(OUTPUT06))
p.values06 <-list(summary(OUTPUT06)$coefficients[,4])

stargazer(OUTPUT06,
          coef= list(OR.vector06), ci = T,
          ci.custom= list(CI.vector06),
          single.row= F,
          type = "text",
          p=c(p.values06))

########MODEL 2: Employment/skill-related variable#########################
OUTPUT07 <-glm(BARRIER2~ EMPL+SKILL+ORG+TIME, data=SUBSET07_DE_2012,family=binomial())
OR.vector07 <-exp(coef(OUTPUT07))
CI.vector07 <-exp(confint(OUTPUT07))
p.values07 <-list(summary(OUTPUT07)$coefficients[,4])

stargazer(OUTPUT07,
          coef= list(OR.vector07), ci = T,
          ci.custom= list(CI.vector07),
          single.row= F,
          type = "text",
          p=c(p.values07))

########MODEL 3: ALL IVs: final model######################################
OUTPUT08 <-glm(BARRIER2~ GENDER+AGE+MIGR+EDUC+SKILL+ORG+TIME+NFE_FED, data=SUBSET07_DE_2012,family=binomial())
OR.vector08 <-exp(coef(OUTPUT08))
CI.vector08 <-exp(confint(OUTPUT08))
p.values08 <-list(summary(OUTPUT08)$coefficients[,4])

stargazer(OUTPUT08,
          coef= list(OR.vector08), ci = T,
          ci.custom= list(CI.vector08),
          single.row= F,
          type = "text",
          p=c(p.values08))

########Cross-Table: Skill intensity level and Barrier#####################
crosstab(SUBSET08_DE_2012$BARRIER4, SUBSET08_DE_2012$SKILL, plot=FALSE,prop.c = TRUE)
#Chi2 Test 
chisq.test(SUBSET08_DE_2012$BARRIER4, SUBSET08_DE_2012$SKILL, correct= F,)

###########################################################################
########GERMANY_Read in data & Select sample for DE AES 2016###############
AES2016_DE <- read_dta("DE_AES_2016_ZA6887_Personen_v1-0-0.dta")
SUBSET01a_DE_2016<-subset(AES2016_DE)

#FILTER: limit samples to potential AE participants and are out of their primary education
attributes(SUBSET01a_DE_2016$F006)
table(SUBSET01a_DE_2016$F006)
SUBSET01_DE_2016 <- subset(SUBSET01a_DE_2016, SUBSET01a_DE_2016$F006==1 | SUBSET01a_DE_2016$F006==2 | SUBSET01a_DE_2016$F006==3 | SUBSET01a_DE_2016$F006==4 | SUBSET01a_DE_2016$F006==8 | SUBSET01a_DE_2016$F006==9 | SUBSET01a_DE_2016$F006==10 | SUBSET01a_DE_2016$F006==11 | SUBSET01a_DE_2016$F006==99)
table(SUBSET01_DE_2016$F006)

#DV: Participation in non-formal (NFE) or formal adult education (FED) 
#new variable: Participation in adult education (formal and non-formal)
attributes(SUBSET01_DE_2016$F120)
table(SUBSET01_DE_2016$F120)
SUBSET01_DE_2016$NFE_FED <- -1
SUBSET01_DE_2016$NFE_FED[SUBSET01_DE_2016$F120==1] <- "1-yes" #yes
SUBSET01_DE_2016$NFE_FED[SUBSET01_DE_2016$F120==2] <- "0-no" #no
SUBSET01_DE_2016$NFE_FED <-factor(SUBSET01_DE_2016$NFE_FED)
table(SUBSET01_DE_2016$NFE_FED)
freq(SUBSET01_DE_2016$NFE_FED)

#Creating Independent Variables (IVs)
#new variable: GENDER
attributes(SUBSET01_DE_2016$F001)
SUBSET01_DE_2016$GENDER <- -1
SUBSET01_DE_2016$GENDER[SUBSET01_DE_2016$F001==1] <-"Male"
SUBSET01_DE_2016$GENDER[SUBSET01_DE_2016$F001==2] <-"Female"
SUBSET01_DE_2016$GENDER <-factor(SUBSET01_DE_2016$GENDER)
table(SUBSET01_DE_2016$GENDER)
freq(SUBSET01_DE_2016$GENDER)

#new variable: AGE
attributes(SUBSET01_DE_2016$alter)
sum(is.na(SUBSET01_DE_2016$alter))
SUBSET01_DE_2016$AGE <- "NA"
SUBSET01_DE_2016$AGE[SUBSET01_DE_2016$alter>17 & SUBSET01_DE_2016$alter<35] <-"1-young"
SUBSET01_DE_2016$AGE[SUBSET01_DE_2016$alter>35 & SUBSET01_DE_2016$alter<53] <-"2-medium"
SUBSET01_DE_2016$AGE[SUBSET01_DE_2016$alter>54] <-"3-old"
#delete missing values
SUBSET01a_DE_2016 <-subset(SUBSET01_DE_2016, AGE!="NA")
SUBSET01a_DE_2016$AGE <- droplevels(SUBSET01a_DE_2016$AGE)
SUBSET01a_DE_2016$AGE <-factor(SUBSET01a_DE_2016$AGE)
table(SUBSET01a_DE_2016$AGE)
freq(SUBSET01a_DE_2016$AGE)

#new variable: migrant background (MIGR)
attributes(SUBSET01a_DE_2016$migra)
#delete missing values
SUBSET02_DE_2016 <- subset(SUBSET01a_DE_2016, SUBSET01a_DE_2016$migra<8)
SUBSET02_DE_2016$MIGR <- -1
SUBSET02_DE_2016$MIGR[SUBSET02_DE_2016$migra==1] <- "DE" 
SUBSET02_DE_2016$MIGR[SUBSET02_DE_2016$migra==2 | SUBSET02_DE_2016$migra==3] <- "non-DE"
SUBSET02_DE_2016$MIGR <-factor(SUBSET02_DE_2016$MIGR)
table(SUBSET02_DE_2016$MIGR)
freq(SUBSET02_DE_2016$MIGR)

#new variable: highest primary education degree (EDUC)
attributes(SUBSET02_DE_2016$schulab3)
#exclude the ones who are still pupils and missing values
SUBSET02a_DE_2016 <- subset(SUBSET02_DE_2016, SUBSET02_DE_2016$schulab3<4)
SUBSET02a_DE_2016$EDUC <- -1
SUBSET02a_DE_2016$EDUC[SUBSET02a_DE_2016$schulab3==0 | SUBSET02a_DE_2016$schulab3==1] <- "3_low" 
SUBSET02a_DE_2016$EDUC[SUBSET02a_DE_2016$schulab3==2] <- "2_middle" 
SUBSET02a_DE_2016$EDUC[SUBSET02a_DE_2016$schulab3==3] <- "1_high"
attributes(SUBSET02a_DE_2016$EDUC)
table(SUBSET02a_DE_2016$EDUC)
freq(SUBSET02a_DE_2016$EDUC)

#new variable: part-time/full-time work (TIME)
attributes(SUBSET02a_DE_2016$F006)
SUBSET02a_DE_2016$TIME <- "NA"
SUBSET02a_DE_2016$TIME[SUBSET02a_DE_2016$F006==1] <- "1-FT" 
SUBSET02a_DE_2016$TIME[SUBSET02a_DE_2016$F006==2] <- "2-PT" 
SUBSET02a_DE_2016$TIME[SUBSET02a_DE_2016$F006==3 | SUBSET02a_DE_2016$F006==4 | SUBSET02a_DE_2016$F006==5 | SUBSET02a_DE_2016$F006==6 | SUBSET02a_DE_2016$F006==7 | SUBSET02a_DE_2016$F006==8 | SUBSET02a_DE_2016$F006==9 | SUBSET02a_DE_2016$F006==10 | SUBSET02a_DE_2016$F006==11 | SUBSET02a_DE_2016$F006==99 | SUBSET02a_DE_2016$F006==12] <- "3-OT" 
attributes(SUBSET02a_DE_2016$TIME)
table(SUBSET02a_DE_2016$TIME)
freq(SUBSET02a_DE_2016$TIME)

#new variable: establishment size (ORG)
attributes(SUBSET02a_DE_2016$x019)
SUBSET02a_DE_2016$ORG <- "NA"
SUBSET02a_DE_2016$ORG[SUBSET02a_DE_2016$x019==1 | SUBSET02a_DE_2016$x019==2 | SUBSET02a_DE_2016$x019==3] <-"1-small"
SUBSET02a_DE_2016$ORG[SUBSET02a_DE_2016$x019==4] <-"2-medium" 
SUBSET02a_DE_2016$ORG[SUBSET02a_DE_2016$x019==5 | SUBSET02a_DE_2016$x019==6] <-"3-large"
SUBSET02a_DE_2016$ORG[SUBSET02a_DE_2016$x019==8 | SUBSET02a_DE_2016$x019==9] <-"NA"
SUBSET02a_DE_2016$ORG <-factor(SUBSET02a_DE_2016$ORG)
table(SUBSET02a_DE_2016$ORG)
freq(SUBSET02a_DE_2016$ORG)

#new variable: employment status (EMPL)
attributes(SUBSET02a_DE_2016$stelk)
#delete missing values
SUBSET03_DE_2016 <- subset(SUBSET02a_DE_2016, SUBSET02a_DE_2016$stelk<5 | SUBSET02a_DE_2016$stelk>5)
SUBSET03_DE_2016$EMPL <- "NA"
SUBSET03_DE_2016$EMPL[SUBSET03_DE_2016$stelk==6] <-"3_not-empl" 
SUBSET03_DE_2016$EMPL[SUBSET03_DE_2016$stelk==4] <-"2_self-empl" 
SUBSET03_DE_2016$EMPL[SUBSET03_DE_2016$stelk==1 | SUBSET03_DE_2016$stelk==2 | SUBSET03_DE_2016$stelk==3] <-"1_employee" 
SUBSET03_DE_2016$EMPL <-factor(SUBSET03_DE_2016$EMPL)
table(SUBSET03_DE_2016$EMPL)
freq(SUBSET03_DE_2016$EMPL)


#new variable: skill level (Occupational Group / ISCO08) (SKILL)
attributes(SUBSET03_DE_2016$isco08_1)
#relabel into five categories 
SUBSET03_DE_2016$SKILL <- "NA"
SUBSET03_DE_2016$SKILL[SUBSET03_DE_2016$isco08_1==1 | SUBSET03_DE_2016$isco08_1==2] <-"1-Managers" 
SUBSET03_DE_2016$SKILL[SUBSET03_DE_2016$isco08_1==3] <-"2-Technicians" 
SUBSET03_DE_2016$SKILL[SUBSET03_DE_2016$isco08_1==4 | SUBSET03_DE_2016$isco08_1==5 | SUBSET03_DE_2016$isco08_1==6 | SUBSET03_DE_2016$isco08_1==7 | SUBSET03_DE_2016$isco08_1==8] <-"3-Clerks_Workers" 
SUBSET03_DE_2016$SKILL[SUBSET03_DE_2016$isco08_1==9] <-"4-Elementary" 
SUBSET03_DE_2016$SKILL[SUBSET03_DE_2016$isco08_1==-1 | SUBSET03_DE_2016$isco08_1==0] <-"NA" 
table(SUBSET03_DE_2016$SKILL)
freq(SUBSET03_DE_2016$SKILL)

########Descriptive Statistics: Participation in AE########################
freq(SUBSET03_DE_2016$NFE_FED)
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

#new variable: barriers to participation in NFE and FED (BARRIER)
attributes(SUBSET03_DE_2016$F123)
SUBSET03_DE_2016$BARRIER <- -1
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==1] <-"1.1_prerequisits" 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==2] <-"3.2_costs" 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==3 | SUBSET03_DE_2016$F123==4] <-"4.2_lack_support" 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==5] <-"4.3_time_conflict" 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==6] <-"1.4_family" 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==7] <-"3.3_distance
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==8] <-"3.1_offer" 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==9] <- "2.1_neg_prior_exp" 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==10 | SUBSET03_DE_2016$F123==11] <-"1.3_age_health" 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==12 | SUBSET03_DE_2016$F123==15 | SUBSET03_DE_2016$F123==17] <-"2.2_personal" 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==13] <- "4.1_counselling" 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==14] <- "1.2_computer" 
SUBSET03_DE_2016$BARRIER[SUBSET03_DE_2016$F123==99 | SUBSET03_DE_2016$F123==98] <-"NA" 
SUBSET03_DE_2016$BARRIER <-factor(SUBSET03_DE_2016$BARRIER)
table(SUBSET03_DE_2016$BARRIER)
freq(SUBSET03_DE_2016$BARRIER)
#delete missing values
SUBSET03a_DE_2016 <-subset(SUBSET03_DE_2016, BARRIER!= "NA")
SUBSET03b_DE_2016 <-subset(SUBSET03a_DE_2016, BARRIER!= -1)
SUBSET03b_DE_2016$BARRIER <- droplevels(SUBSET03b_DE_2016$BARRIER)
table(SUBSET03b_DE_2016$BARRIER)
freq(SUBSET03b_DE_2016$BARRIER)

#RE-CODE TO 4 Categories
SUBSET04_DE_2016 <- subset(SUBSET03b_DE_2016)
attributes(SUBSET04_DE_2016$F123)
table(SUBSET04_DE_2016$F123)
freq(SUBSET04_DE_2016$F123)
SUBSET04_DE_2016$BARRIER4 <- "NA"
SUBSET04_DE_2016$BARRIER4[SUBSET04_DE_2016$F123==1 | SUBSET04_DE_2016$F123==6 | SUBSET04_DE_2016$F123==14 | SUBSET04_DE_2016$F123==11 | SUBSET04_DE_2016$F123==10] <-"1_socio_econ_dem" 
SUBSET04_DE_2016$BARRIER4[SUBSET04_DE_2016$F123==9 | SUBSET04_DE_2016$F123==12 | SUBSET04_DE_2016$F123==15 | SUBSET04_DE_2016$F123==17] <-"2_psych" 
SUBSET04_DE_2016$BARRIER4[SUBSET04_DE_2016$F123==8 | SUBSET04_DE_2016$F123==2 | SUBSET04_DE_2016$F123==7] <-"3_direct" 
SUBSET04_DE_2016$BARRIER4[SUBSET04_DE_2016$F123==13 | SUBSET04_DE_2016$F123==3 | SUBSET04_DE_2016$F123==4 | SUBSET04_DE_2016$F123==5] <-"4_indirect" 
SUBSET04_DE_2016$BARRIER4[SUBSET04_DE_2016$F123==99 | SUBSET04_DE_2016$F123==98] <- "N_A" #no answer or no reason
SUBSET04_DE_2016$BARRIER4 <-factor(SUBSET04_DE_2016$BARRIER4)
table(SUBSET04_DE_2016$BARRIER4)
freq(SUBSET04_DE_2016$BARRIER4)
#delete missing values
SUBSET05_DE_2016 <-subset(SUBSET04_DE_2016, BARRIER4!= "NA")
SUBSET05_DE_2016$BARRIER4 <- droplevels(SUBSET05_DE_2016$BARRIER4)
table(SUBSET05_DE_2016$BARRIER4)
freq(SUBSET05_DE_2016$BARRIER4)

#RE-CODE TO BINARY 0/1
SUBSET06_DE_2016 <- subset(SUBSET05_DE_2016)
attributes(SUBSET06_DE_2016$F123)
SUBSET06_DE_2016$BARRIER2 <- -1
SUBSET06_DE_2016$BARRIER2[SUBSET06_DE_2016$F123==1 | SUBSET06_DE_2016$F123==6 | SUBSET06_DE_2016$F123==14 | SUBSET06_DE_2016$F123==11 | SUBSET06_DE_2016$F123==10 | SUBSET06_DE_2016$F123==9 | SUBSET06_DE_2016$F123==12 | SUBSET06_DE_2016$F123==15 | SUBSET06_DE_2016$F123==17] <- "1_ind_level" 
SUBSET06_DE_2016$BARRIER2[SUBSET06_DE_2016$F123==8 | SUBSET06_DE_2016$F123==2 | SUBSET06_DE_2016$F123==7 | SUBSET06_DE_2016$F123==13 | SUBSET06_DE_2016$F123==3 | SUBSET06_DE_2016$F123==4 | SUBSET06_DE_2016$F123==5] <- "0_inst_level" 
SUBSET06_DE_2016$BARRIER2[SUBSET06_DE_2016$F123==99 | SUBSET06_DE_2016$F123==98] <- "N_A" 
SUBSET06_DE_2016$BARRIER2 <-factor(SUBSET06_DE_2016$BARRIER2)
table(SUBSET06_DE_2016$BARRIER2)
freq(SUBSET06_DE_2016$BARRIER2)
#delete missing values
SUBSET07_DE_2016 <-subset(SUBSET06_DE_2016, BARRIER2!= "N_A")
SUBSET07_DE_2016$BARRIER2 <- droplevels(SUBSET06_DE_2016$BARRIER2)
SUBSET08_DE_2016 <- subset(SUBSET07_DE_2016)
attributes(SUBSET08_DE_2016$F123)
table(SUBSET08_DE_2016$F123)
freq(SUBSET08_DE_2016$F123)
SUBSET08_DE_2016$BARRIER <- -1
SUBSET08_DE_2016$BARRIER[SUBSET08_DE_2016$F123==1 | SUBSET08_DE_2016$F123==6 | SUBSET08_DE_2016$F123==14 | SUBSET08_DE_2016$F123==11 | SUBSET08_DE_2016$F123==10 | SUBSET08_DE_2016$F123==9 | SUBSET08_DE_2016$F123==12 | SUBSET08_DE_2016$F123==15 | SUBSET08_DE_2016$F123==17] <- "1_IND_LEVEL" #BARRIERS AT PERSONAL/INDIVIDUAL-LEVEl 
SUBSET08_DE_2016$BARRIER[SUBSET08_DE_2016$F123==8 | SUBSET08_DE_2016$F123==2 | SUBSET08_DE_2016$F123==7 | SUBSET08_DE_2016$F123==13 | SUBSET08_DE_2016$F123==3 | SUBSET08_DE_2016$F123==4 | SUBSET08_DE_2016$F123==5] <- "0_INST_LEVEL" #BARRIERS AT EDUCATIONAL INSTITUTION-LEVEL
SUBSET08_DE_2016$BARRIER <-factor(SUBSET08_DE_2016$BARRIER)
table(SUBSET08_DE_2016$BARRIER)
freq(SUBSET08_DE_2016$BARRIER)

########Sample statistic###################################################
table8<-table(SUBSET08_DE_2016$BARRIER, SUBSET08_DE_2016$NFE_FED)
round(prop.table(table8,2), digits=2)

########Descriptive Statistics#############################################
#Cross-Table // previous level of education and Adult education, 2016
crosstab(SUBSET03_DE_2016$NFE_FED, SUBSET03_DE_2016$SKILL, plot=FALSE,prop.c = TRUE)

#Chi2 Test // previous level of education and Adult education
chisq.test(SUBSET03_DE_2016$NFE_FED, SUBSET03_DE_2016$SKILL, correct= F,)

########Logistic Regression: HYPOTHESIS 1##################################
########Model 1: Socio-demographics########################################
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

########Model 2: Employment/skill-related variable#########################
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

########FINAL Model: All IVs###############################################
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

########HYPOTHESIS 2: INCENTIVES###########################################
#new variable: reasons to participate in NFE (REASON)
attributes(SUBSET03_DE_2016$F09901_1)
table(SUBSET03_DE_2016$F09901_1)
SUBSET03_DE_2016$INCENTIVENFE <- "NA"
SUBSET03_DE_2016$INCENTIVENFE[SUBSET03_DE_2016$F09906_1==1 | SUBSET03_DE_2016$F09910_1==1 | SUBSET03_DE_2016$F09908_1==1 | SUBSET03_DE_2016$F09907_1==1 | SUBSET03_DE_2016$F09914_1==1] <- "2_personal"
SUBSET03_DE_2016$INCENTIVENFE[(SUBSET03_DE_2016$F09901_1==1 | SUBSET03_DE_2016$F09912_1==1 | SUBSET03_DE_2016$F09902_1==1 | SUBSET03_DE_2016$F09903_1==1 | SUBSET03_DE_2016$F09904_1==1 | SUBSET03_DE_2016$F09905_1==1 | SUBSET03_DE_2016$F09909_1==1 | SUBSET03_DE_2016$F09913_1==1) & (SUBSET03_DE_2016$INCENTIVENFE=="2_personal")] <- "1_job-rel + personal"
SUBSET03_DE_2016$INCENTIVENFE[(SUBSET03_DE_2016$F09901_1==1 | SUBSET03_DE_2016$F09912_1==1 | SUBSET03_DE_2016$F09902_1==1 | SUBSET03_DE_2016$F09903_1==1 | SUBSET03_DE_2016$F09904_1==1 | SUBSET03_DE_2016$F09905_1==1 | SUBSET03_DE_2016$F09909_1==1 | SUBSET03_DE_2016$F09913_1==1) & (SUBSET03_DE_2016$INCENTIVENFE!="2_personal" & SUBSET03_DE_2016$INCENTIVENFE!="1_job-rel + personal")] <- "3_job-rel"
SUBSET03m_DE_2016 <- subset(SUBSET03_DE_2016, INCENTIVENFE!= "NA")
SUBSET03m_DE_2016$INCENTIVENFE <- droplevels(SUBSET03m_DE_2016$INCENTIVENFE)
table(SUBSET03m_DE_2016$INCENTIVENFE)
freq(SUBSET03m_DE_2016$INCENTIVENFE)

########Sample Statistics##################################################
table9<-table(SUBSET03m_DE_2016$INCENTIVENFE, SUBSET03m_DE_2016$NFE_FED)
round(prop.table(table9,2), digits=2)

########MULTINOMIAL########################################################
OUTPUT01m <- multinom(INCENTIVENFE ~ SKILL + GENDER + AGE + ORG + EDUC + TIME + MIGR + EMPL, data = SUBSET03m_DE_2016)
#to see output
summary(OUTPUT01m)
#Calculation of Significance
## z value:
z <- summary(OUTPUT01m)$coefficients/summary(OUTPUT01m)$standard.errors
## 2-tailed z test
p <- (1-pnorm(abs(z), 0, 1))*2
p
#Display of results
stargazer(OUTPUT01m, type="text", p.auto=F)
#Table with RRR
OUTPUT01rrr=exp(coef(OUTPUT01m))
stargazer(OUTPUT01m, coef=list(OUTPUT01rrr), type="text", p.auto=F)
#Calculation of predicted probabilities using the effects package
OUTPUT01PP <- Effect("SKILL", OUTPUT01m)
OUTPUT01PP$model.matrix
#Preparation for Graph
pp <- data.frame(OUTPUT01PP$prob)
# Assign the labels in the order of the rows that you identified with the model matrix
pp$labels <- c("1_Managers", "2_Technicians", "3_Clerks_Workers", "4_Elementary", "5_NA")
pp <- melt(pp, id="labels")
names(pp)[names(pp)=="value"] <- "pp"
#Extraction Names DV
pp$INCENTIVENFE <-substring(pp[,2],6)
lp <- data.frame(OUTPUT01PP$lower.prob)
lp <- melt(lp)
names(lp)[names(lp)=="value"] <- "l"
up <- data.frame(OUTPUT01PP$upper.prob)
up <- melt(up)
names(up)[names(up)=="value"] <- "u"
toplot <- cbind(pp, lp[,2], up[,2])
#Orgnanisation DV
toplot$INCENTIVENFE <- factor(toplot$INCENTIVENFE )
levels(toplot$INCENTIVENFE )
toplot$INCENTIVENFE= factor(toplot$INCENTIVENFE, levels(toplot$INCENTIVENFE)[c(1,2,3)])

#Orgnanisation IV
toplot$labels <- factor(toplot$labels)
levels(toplot$labels)
toplot$labels = factor(toplot$labels, levels(toplot$labels)[c(1,2,3,4,5)])

#Plotting the Multinominals
getPalette = colorRampPalette(brewer.pal(5, "RdBu"))
ggplot(toplot, aes(x=INCENTIVENFE, y=pp, fill=labels)) + 
  geom_bar( stat="identity", position = "dodge") +
  geom_errorbar( aes( ymin=lp[,2], ymax=up[,2]), width=0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = getPalette(5)) +
  theme_bw() +
  ggtitle("Incentives to participate in AE in Germany, 2016") +
  xlab("Incentives")

#Cross-tabs
crosstab(SUBSET03m_DE_2016$INCENTIVENFE, SUBSET03m_DE_2016$SKILL, plot=FALSE,prop.c = TRUE)
#Chi-Square test
chisq.test(SUBSET03m_DE_2016$INCENTIVENFE, SUBSET03m_DE_2016$SKILL, correct= F,)

########Hypothesis 3#######################################################
########MODEL 1: Socio-demographics########################################
OUTPUT06 <-glm(BARRIER2~ GENDER+ALTER+MIGR+EDUC, data=SUBSET08_DE_2016,family=binomial())
OR.vector06 <-exp(coef(OUTPUT06))
CI.vector06 <-exp(confint(OUTPUT06))
p.values06 <-list(summary(OUTPUT06)$coefficients[,4])

stargazer(OUTPUT06,
          coef= list(OR.vector06), ci = T,
          ci.custom= list(CI.vector06),
          single.row= F,
          type = "text",
          p=c(p.values06))

########MODEL2: Employment/skill-related variable##########################
OUTPUT07 <-glm(BARRIER2~ EMPL+SKILL+ORG+TIME, data=SUBSET08_DE_2016,family=binomial())
OR.vector07 <-exp(coef(OUTPUT07))
CI.vector07 <-exp(confint(OUTPUT07))
p.values07 <-list(summary(OUTPUT07)$coefficients[,4])

stargazer(OUTPUT07,
          coef= list(OR.vector07), ci = T,
          ci.custom= list(CI.vector07),
          single.row= F,
          type = "text",
          p=c(p.values07))

########MODEL3: ALL IVs####################################################
OUTPUT08 <-glm(BARRIER2~ GENDER+AGE+MIGR+EDUC+SKILL+ORG+TIME+NFE_FED, data=SUBSET08_DE_2016,family=binomial())
OR.vector08 <-exp(coef(OUTPUT08))
CI.vector08 <-exp(confint(OUTPUT08))
p.values08 <-list(summary(OUTPUT08)$coefficients[,4])

stargazer(OUTPUT08,
          coef= list(OR.vector08), ci = T,
          ci.custom= list(CI.vector08),
          single.row= F,
          type = "text",
          p=c(p.values08))

########Cross-tab##########################################################
crosstab(SUBSET08_DE_2016$BARRIER, SUBSET08_DE_2016$SKILL, plot=FALSE,prop.c = TRUE)

########COMBINE DATASETS FROM 2012 AND 2016################################
#New Variable: YEAR 2012  
SUBSET04_DE_2012$YEAR <- "2012" 
SUBSET04_DE_2012$YEAR <- factor(SUBSET04_DE_2012$YEAR) 

#Select variables & save data FOR 2012 
SUBSET_DE_2012 <-subset(SUBSET04_DE_2012, select=c(NFE_FED, GENDER, AGE, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR)) 
saveRDS(SUBSET_DE_2012, file= "AES_2012_DE.rds") 

#New Variable: YEAR 2016 
SUBSET03_DE_2016$YEAR <- "2016" 
SUBSET03_DE_2016$YEAR <- factor(SUBSET03_DE_2016$YEAR) 

#Select variables & save data FOR 2016 
SUBSET_DE_2016 <-subset(SUBSET03_DE_2016, select=c(NFE_FED, GENDER, AGE, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR)) 
saveRDS(SUBSET_DE_2016, file= "AES_2016_DE.rds") 

#Combining Data Sets of 2012 and 2016 for GERMANY with new variables 
AES_2012_DE <-readRDS("AES_2012_DE.rds") 
AES_2016_DE <-readRDS("AES_2016_DE.rds") 
AES_2012_2016_DE <-rbind(AES_2012_DE,AES_2016_DE) 

#Sample Statistics 
#Set Reference Category 
AES_2012_2016_DE$YEAR   <-relevel(AES_2012_2016_DE$YEAR  , ref="2012") 

#Crosstab of dependent variable and years (2012 and 2016) 
descr::crosstab(AES_2012_2016_DE$NFE_FED, AES_2012_2016_DE$YEAR, plot=FALSE, prop.c = TRUE)

#Barplot Participation and Years DEU
TABLE1 <- table(AES_2012_2016_DE$NFE_FED, AES_2012_2016_DE$YEAR)
TABLE1
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)
ggplot(TABLE3, aes(fill=Var1, x=Var2, Var3, y=Freq)) + 
  geom_bar(stat="identity") + 
  ggtitle("Germany, 2012/2016") + ylim(0,1) + ylab("%")+ xlab("") + 
  scale_fill_manual(name = "Participated in adult education?", labels = c("No", "Yes"), values = c("#032F5E", "#B4C6E7")) +
theme_bw()

########BARPLOT: HYPOTHESIS 1##############################################
TABLE1 <- table(AES_2012_2016_DE$NFE_FED, AES_2012_2016_DE$SKILL)
TABLE1
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)
ggplot(TABLE3, aes(fill=Var1, x=Var2, Var3, y=Freq)) + 
  geom_bar(stat="identity") + 
  ggtitle("Germany, 2012/2016") + ylim(0,1) + ylab("%")+ xlab("") + 
  scale_fill_manual(name = "Participated in adult education?", labels = c("No", "Yes"), values = c("#032F5E", "#B4C6E7")) +
  theme_bw()

########COMBINE DATASETS FROM 2012 AND 2016 for Hypothesis 2###############
#New Variable: YEAR 2012  
SUBSET07_DE_2012$YEAR <- "2012" 
SUBSET07_DE_2012$YEAR <- factor(SUBSET07_DE_2012$YEAR) 

#Select variables & save data FOR 2012 
SUBSET08_DE_2012 <-subset(SUBSET07_DE_2012, select=c(NFE_FED, GENDER, AGE, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, BARRIER2)) 
saveRDS(SUBSET08_DE_2012, file= "AES_2012_DE_01.rds") 

#New Variable: YEAR 2016 
SUBSET08_DE_2016$YEAR <- "2016" 
SUBSET08_DE_2016$YEAR <- factor(SUBSET08_DE_2016$YEAR) 

#Select variables & save data FOR 2016 
SUBSET09_DE_2016 <-subset(SUBSET08_DE_2016, select=c(NFE_FED, GENDER, AGE, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, BARRIER2)) 
saveRDS(SUBSET09_DE_2016, file= "AES_2016_DE_01.rds") 

#Combining Data Sets of 2012 and 2016 for GERMANY with new variables 
AES_2012_DE_01 <-readRDS("AES_2012_DE_01.rds") 
AES_2016_DE_01 <-readRDS("AES_2016_DE_01.rds") 
AES_2012_2016_DE_01 <-rbind(AES_2012_DE_01,AES_2016_DE_01) 

########BARPLOT: HYPOTHESIS 2##############################################
TABLE1 <- table(AES_2012_2016_DE_01$BARRIER2, AES_2012_2016_DE_01$SKILL)
TABLE2 <- prop.table(TABLE1,2)
TABLE3 <- as.data.frame(TABLE2)
ggplot(TABLE3, aes(fill=Var1, x=Var2, Var3, y=Freq)) + 
  geom_bar(stat="identity") + 
  ggtitle("Germany, 2012/2016") + ylim(0,1) + ylab("%")+ xlab("") + 
  scale_fill_manual(name = "Barriers on institutional or individual level?", labels = c("Institutional-level", "Individual-level"), values = c("#032F5E", "#B4C6E7")) +
  theme_bw()


########BARPLOT HYPOTHESIS: 4##############################################
library(RColorBrewer)

#COMBINE DATASETS FROM 2012 AND 2016 
#New Variable: YEAR 2012  
SUBSET04a_DE_2012$YEAR <- "2012" 
SUBSET04a_DE_2012$YEAR <- factor(SUBSET04a_DE_2012$YEAR) 

#Select variables & save data FOR 2012 
SUBSET_DE_2012 <-subset(SUBSET04a_DE_2012, select=c(NFE_FED, GENDER, AGE, MIGR, EDUC, EMPL, SKILL, ORG, TIME, BARRIER, YEAR)) 
saveRDS(SUBSET_DE_2012, file= "AES_2012_DE.rds") 

#New Variable: YEAR 2016 
SUBSET03b_DE_2016$YEAR <- "2016" 
SUBSET03b_DE_2016$YEAR <- factor(SUBSET03b_DE_2016$YEAR) 

#Select variables & save data FOR 2016 
SUBSET_DE_2016 <-subset(SUBSET03b_DE_2016, select=c(NFE_FED, GENDER, AGE, MIGR, EDUC, EMPL, SKILL, ORG, TIME, BARRIER, YEAR)) 
saveRDS(SUBSET_DE_2016, file= "AES_2016_DE.rds") 

#Combining Data Sets of 2012 and 2016 for GERMANY with new variables 
AES_2012_DE <-readRDS("AES_2012_DE.rds") 
AES_2016_DE <-readRDS("AES_2016_DE.rds") 
AES_2012_2016_DE_04 <-rbind(AES_2012_DE,AES_2016_DE) 

# Blue Colors
getPalette = colorRampPalette(brewer.pal(12, "RdBu"))

ggplot(AES_2012_2016_DE_04, aes(x=YEAR, fill=BARRIER)) +
  geom_bar(stat="count", position="fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Germany") +
  xlab("Year") +
  ylab("Percentage [%]") +
  scale_fill_manual(values = getPalette(12)) +
  theme_bw()

########BARPLOT: HYPOTHESIS 3##############################################
#COMBINE DATASETS FROM 2012 AND 2016
#New Variable: YEAR 2012  
SUBSET04m_DE_2012$YEAR <- "2012" 
SUBSET04m_DE_2012$YEAR <- factor(SUBSET04m_DE_2012$YEAR) 

#Select variables & save data FOR 2012 
SUBSET08z_DE_2012 <-subset(SUBSET04m_DE_2012, select=c(NFE_FED, GENDER, AGE, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, INCENTIVENFE)) 
saveRDS(SUBSET08z_DE_2012, file= "AES_2012_DE_02.rds") 

#New Variable: YEAR 2016 
SUBSET03m_DE_2016$YEAR <- "2016" 
SUBSET03m_DE_2016$YEAR <- factor(SUBSET03m_DE_2016$YEAR) 

#Select variables & save data FOR 2016 
SUBSET09z_DE_2016 <-subset(SUBSET03m_DE_2016, select=c(NFE_FED, GENDER, AGE, MIGR, EDUC, EMPL, SKILL, ORG, TIME, YEAR, INCENTIVENFE)) 
saveRDS(SUBSET09z_DE_2016, file= "AES_2016_DE_02.rds") 

#Combining Data Sets of 2012 and 2016 for GERMANY with new variables 
AES_2012_DE_02 <-readRDS("AES_2012_DE_02.rds") 
AES_2016_DE_02 <-readRDS("AES_2016_DE_02.rds") 
AES_2012_2016_DE_02 <-rbind(AES_2012_DE_02,AES_2016_DE_02) 

getPalette = colorRampPalette(brewer.pal(3, "Blues"))

ggplot(AES_2012_2016_DE_02, aes(x=YEAR, fill=INCENTIVENFE)) +
  geom_bar(stat="count", position="fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Germany") +
  xlab("Year") +
  ylab("Percentage [%]") +
  scale_fill_manual(values = getPalette(3), 
                    name="Incentives",
                    labels=c("both", "personal", "job-related")) +
  theme_bw()

