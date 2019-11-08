library(dplyr)
library(naniar)
library(mice)
library(magrittr)
library(Metrics)
library(plotly)
library(corrplot)
library(PerformanceAnalytics)
library(Hmisc)
library(knitr)
library(mctest)
library(caret)
library(tidyverse)
library(rpart)
library(rpart)
library(caret)
library(e1071)
library(randomForest)
#dataset 1.1 

d3 <- select(cchs11, 'GEOAGPRV','DHHAGAGE','DHHA_SEX','DHHAGMS','ALCA_1','ALCADDLY','SMKA_202','HWTAGBMI','INCAGHH','PACADPAI','SDCAFIMM','SDCAGRES','SDCAGRAC',
             'TWDA_5','EDUADR04','CCCA_071','CCCA_101','CCCA_05A','CCCA_121','DRGAF1')
summary(d3)

DF2 <- d3[which(d3$DHHAGAGE != '12 TO 14 YEARS'), ]
DF2 <- DF2[which(DF2$DHHAGAGE != '15 TO 19 YEARS'), ]
DF2 <- DF2[which(DF2$DHHAGAGE != '65 TO 69 YEARS'), ]
DF2 <- DF2[which(DF2$DHHAGAGE != '70 TO 74 YEARS'), ]
DF2 <- DF2[which(DF2$DHHAGAGE != '75 TO 79 YEARS'), ]
DF2 <- DF2[which(DF2$DHHAGAGE != '80  YEARS OR OLDER'), ]

summary(DF2)

#converting categories of DRGAF1
DF2$DRGAF1 <- as.character(DF2$DRGAF1)
df2 <- mutate( DF2, DRGAF1 = ifelse(DF2$DRGAF1 == "NOT APPLICABLE", "NO", DRGAF1))
df2$DRGAF1 <- factor(df2$DRGAF1, ordered = TRUE)
summary(df2)

#creating categories to record if arthritis yes or no
df2$CCCA_05A <- as.character(df2$CCCA_05A)
df3 <- mutate( df2, CCCA_05A = ifelse(df2$CCCA_05A == "OSTEOARTHRITIS", "YES", "NO"))
df3$CCCA_05A <- factor(df3$CCCA_05A, ordered = TRUE)
summary(df3)

#replacing not applicable in years since immigration to NO
df3$SDCAGRES <- as.character(df3$SDCAGRES)
dfq <- mutate( df3, SDCAGRES = ifelse(df3$SDCAGRES == "NOT APPLICABLE", "NO", SDCAGRES))
dfq$SDCAGRES <- factor(dfq$SDCAGRES, ordered = TRUE)
summary(dfq)

df3 <- dfq

df3[df3 == "NOT APPLICABLE"] <- NA

df4 <- na.omit(df3)
summary(df4)

names(df4) <- c("Geographical Region", "Age Range","Gender","Marital Status","Alcohol Past", "Alcohol Daily","Smoker Type","BMI","Household Income","Physical Activity","Immigrated?","Years Since Immigration","Cultural/Racial Origin","Has Regular Doctor Access","Highest Level of Education", "Has High Blood Pressure","Has Diabetes","Arthritis Type","Has Heart Disease","Medication in Past month")

#----------------------------------------------------------------------
da <- select(cchs21, 'GEOCGPRV','DHHCGAGE','DHHC_SEX','DHHCGMS','ALCC_1','ALCCDDLY','SMKC_202','HWTCGBMI','INCCGHH','PACCDPAI','SDCCFIMM','SDCCGRES','SDCCGRAC',
             'HCUC_1AA','EDUCDR04','CCCC_071','CCCC_101','CCCC_05A','CCCC_121','MEDCF1')
summary(da)

DF2 <- da[which(da$DHHCGAGE != '12 TO 14 YEARS'), ]
DF2 <- DF2[which(DF2$DHHCGAGE != '15 TO 19 YEARS'), ]
DF2 <- DF2[which(DF2$DHHCGAGE != '65 TO 69 YEARS'), ]
DF2 <- DF2[which(DF2$DHHCGAGE != '70 TO 74 YEARS'), ]
DF2 <- DF2[which(DF2$DHHCGAGE != '75 TO 79 YEARS'), ]
DF2 <- DF2[which(DF2$DHHCGAGE != '80  YEARS OR OLDER'), ]

summary(DF2)

DF2$MEDCF1 <- as.character(DF2$MEDCF1)
dfb <- mutate( DF2, MEDCF1 = ifelse(DF2$MEDCF1 == "NOT APPLICABLE", "NO", MEDCF1))
dfb$MEDCF1 <- factor(dfb$MEDCF1, ordered = TRUE)
summary(dfb)

dfb$CCCC_05A <- as.character(dfb$CCCC_05A)
dfc <- mutate( dfb, CCCC_05A = ifelse(dfb$CCCC_05A == "OSTEOARTHRITIS", "YES", "NO"))
dfc$CCCC_05A <- factor(dfc$CCCC_05A, ordered = TRUE)
summary(dfc)

dfc$SDCCGRES <- as.character(dfc$SDCCGRES)
dfw <- mutate( dfc, SDCCGRES = ifelse(dfc$SDCCGRES == "NOT APPLICABLE", "NO", SDCCGRES))
dfw$SDCCGRES <- factor(dfw$SDCCGRES, ordered = TRUE)
summary(dfw)

dfc <- dfw

dfc[dfc == "NOT APPLICABLE"] <- NA
dfd <- na.omit(dfc)
summary(dfd)

names(dfd) <- c("Geographical Region", "Age Range","Gender","Marital Status","Alcohol Past", "Alcohol Daily","Smoker Type","BMI","Household Income","Physical Activity","Immigrated?","Years Since Immigration","Cultural/Racial Origin","Has Regular Doctor Access","Highest Level of Education", "Has High Blood Pressure","Has Diabetes","Arthritis Type","Has Heart Disease","Medication in Past month")


#----------------------------------------------------------------------
d10 <- select(cchs31, 'GEOEGPRV','DHHEGAGE','DHHE_SEX','DHHEGMS','ALCE_1','ALCEDDLY','SMKE_202','HWTEGBMI','INCEGHH','PACEDPAI','SDCEFIMM','SDCEGRES','SDCEGCGT',
             'HCUE_1AA','EDUEDR04','CCCE_071','CCCE_101','CCCE_05A','CCCE_121','MEDEF1')
summary(d10)


DF2 <- d10[which(d10$DHHEGAGE != '12 TO 14 YEARS'), ]
DF2 <- DF2[which(DF2$DHHEGAGE != '15 TO 19 YEARS'), ]
DF2 <- DF2[which(DF2$DHHEGAGE != '65 TO 69 YEARS'), ]
DF2 <- DF2[which(DF2$DHHEGAGE != '70 TO 74 YEARS'), ]
DF2 <- DF2[which(DF2$DHHEGAGE != '75 TO 79 YEARS'), ]
DF2 <- DF2[which(DF2$DHHEGAGE != '80  YEARS OR OLDER'), ]

summary(DF2)

DF2$MEDEF1 <- as.character(DF2$MEDEF1)
d11 <- mutate( DF2, MEDEF1 = ifelse(DF2$MEDEF1 == "NOT APPLICABLE", "NO", MEDEF1))
d11$MEDEF1 <- factor(d11$MEDEF1, ordered = TRUE)
summary(d11)

d11$CCCE_05A <- as.character(d11$CCCE_05A)
d12 <- mutate( d11, CCCE_05A = ifelse(d11$CCCE_05A == "OSTEOARTHRITIS", "YES", "NO"))
d12$CCCE_05A <- factor(d12$CCCE_05A, ordered = TRUE)
summary(d12)

d12$SDCEGRES <- as.character(d12$SDCEGRES)
dfr <- mutate( d12, SDCEGRES = ifelse(d12$SDCEGRES == "NOT APPLICABLE", "NO", SDCEGRES))
dfr$SDCEGRES <- factor(dfr$SDCEGRES, ordered = TRUE)
summary(dfr)

d12 <- dfr

d12[d12 == "NOT APPLICABLE"] <- NA
d13 <- na.omit(d12)
summary(d13)

names(d13) <- c("Geographical Region", "Age Range","Gender","Marital Status","Alcohol Past", "Alcohol Daily","Smoker Type","BMI","Household Income","Physical Activity","Immigrated?","Years Since Immigration","Cultural/Racial Origin","Has Regular Doctor Access","Highest Level of Education", "Has High Blood Pressure","Has Diabetes","Arthritis Type","Has Heart Disease","Medication in Past month")


#----------------------------------------------------------------------

df_master <- bind_rows(df4,dfd, d13)
df_master$`Geographical Region` <- factor(df_master$`Geographical Region`,ordered = TRUE)
df_master$`Age Range` <- factor(df_master$`Age Range`,ordered = TRUE)
df_master$Gender <- factor(df_master$Gender,ordered = TRUE)
df_master$`Marital Status` <- factor(df_master$`Marital Status`,ordered = TRUE)
df_master$`Alcohol Daily` <- factor(df_master$`Alcohol Daily`,ordered = TRUE)
df_master$BMI <- factor(df_master$BMI ,ordered = TRUE)
df_master$`Household Income` <- factor(df_master$`Household Income` ,ordered = TRUE)
df_master$`Physical Activity` <- factor(df_master$`Physical Activity` ,ordered = TRUE)
df_master$`Years Since Immigration` <- factor(df_master$`Years Since Immigration` ,ordered = TRUE)
df_master$`Cultural/Racial Origin` <- factor(df_master$`Cultural/Racial Origin` ,ordered = TRUE)
df_master$`Highest Level of Education` <- factor(df_master$`Highest Level of Education` ,ordered = TRUE)
df_master$`Arthritis Type` <- factor(df_master$`Arthritis Type` ,ordered = TRUE)
df_master$`Medication in Past month` <- factor(df_master$`Medication in Past month` ,ordered = TRUE)

summary(df_master)

df_master$`Geographical Region` <- as.character(df_master$`Geographical Region`)
df_master1 <- mutate( df_master, `Geographical Region` = ifelse(df_master$`Geographical Region` == "QU\xc9BEC", "QUEBEC",df_master$`Geographical Region` ))
df_master$`Geographical Region` <- factor(df_master1$`Geographical Region`, ordered = TRUE)
summary(df_master)


df_master[df_master == "REFUSAL"] <- NA
df_master[df_master == "DON'T KNOW"] <- NA

#temp dataframe to check without imputation
df_inputation <- df_master

df_master[df_master == "NOT STATED"] <- NA

summary(df_master)

#------------------------------------------------------------
#imputation
helperFunc <- function(x){
  sample(levels(x), sum(is.na(x)), replace = TRUE,
         prob = as.numeric(table(x))/sum(!is.na(x)))   
}

dftest <- df_master

dftest[sapply(dftest, is.na)]  <- unlist(sapply(dftest, helperFunc))
summary(dftest)

df_inputation[sapply(df_inputation, is.na)]  <- unlist(sapply(df_inputation, helperFunc))
summary(df_inputation)
#imputation end
#------------------------------------------------------------




#re-factoring
df_master <- dftest
summary(df_master)
df_master$`Geographical Region` <- factor(df_master$`Geographical Region`,ordered = FALSE)
df_master$`Age Range` <- factor(df_master$`Age Range`,ordered = FALSE)
df_master$Gender <- factor(df_master$Gender,ordered = FALSE)
df_master$`Marital Status` <- factor(df_master$`Marital Status`,ordered = FALSE)
df_master$`Alcohol Daily` <- factor(df_master$`Alcohol Daily`,ordered = FALSE)
df_master$BMI <- factor(df_master$BMI ,ordered = FALSE)
df_master$`Household Income` <- factor(df_master$`Household Income` ,ordered = FALSE)
df_master$`Physical Activity` <- factor(df_master$`Physical Activity` ,ordered = FALSE)
df_master$`Years Since Immigration` <- factor(df_master$`Years Since Immigration` ,ordered = FALSE)
df_master$`Cultural/Racial Origin` <- factor(df_master$`Cultural/Racial Origin` ,ordered = FALSE)
df_master$`Highest Level of Education` <- factor(df_master$`Highest Level of Education` ,ordered = FALSE)
df_master$`Arthritis Type` <- factor(df_master$`Arthritis Type` ,ordered = FALSE)
df_master$`Medication in Past month` <- factor(df_master$`Medication in Past month` ,ordered = FALSE)
summary(df_master)
str(df_master)

str(df_inputation)
summary(df_inputation)
df_inputation$`Geographical Region` <- factor(df_inputation$`Geographical Region`,ordered = FALSE)
df_inputation$`Age Range` <- factor(df_inputation$`Age Range`,ordered = FALSE)
df_inputation$Gender <- factor(df_inputation$Gender,ordered = FALSE)
df_inputation$`Marital Status` <- factor(df_inputation$`Marital Status`,ordered = FALSE)
df_inputation$`Alcohol Daily` <- factor(df_inputation$`Alcohol Daily`,ordered = FALSE)
df_inputation$BMI <- factor(df_inputation$BMI ,ordered = FALSE)
df_inputation$`Household Income` <- factor(df_inputation$`Household Income` ,ordered = FALSE)
df_inputation$`Physical Activity` <- factor(df_inputation$`Physical Activity` ,ordered = FALSE)
df_inputation$`Years Since Immigration` <- factor(df_inputation$`Years Since Immigration` ,ordered = FALSE)
df_inputation$`Cultural/Racial Origin` <- factor(df_inputation$`Cultural/Racial Origin` ,ordered = FALSE)
df_inputation$`Highest Level of Education` <- factor(df_inputation$`Highest Level of Education` ,ordered = FALSE)
df_inputation$`Arthritis Type` <- factor(df_inputation$`Arthritis Type` ,ordered = FALSE)
df_inputation$`Medication in Past month` <- factor(df_inputation$`Medication in Past month` ,ordered = FALSE)
str(df_inputation)
#with no imputation 
mylogit_check <- glm(`Has Heart Disease` ~ `Geographical Region` + Gender +`Household Income`+`Physical Activity`+ `Marital Status` + `Years Since Immigration` + `Arthritis Type` + `Marital Status`:`Arthritis Type` + `Years Since Immigration`:`Arthritis Type` + Gender:`Arthritis Type`+`Physical Activity`:`Arthritis Type`+`Geographical Region`:`Arthritis Type`, data = df_inputation, family = "binomial")
summary(mylogit_check)


#fct_drop(df_master$`Has Diabetes`, only="REFUSAL")

levels(df_master$`Geographical Region`)
levels(df_master$`Smoker Type`)
#converting/preparing data for modeling
set.seed(123)
splitrule <- sample(nrow(df_master), 0.7*nrow(df_master))
train <- data.frame(df_master[splitrule,])
test <- data.frame(df_master[-splitrule,])
summary(train)


#random forest
model1 <- randomForest(Has.Heart.Disease ~ Geographical.Region + Gender + Marital.Status + Years.Since.Immigration + Arthritis.Type, data = train, importance = TRUE)
importance(model1)
print_score(model1)
model1

#logistic regression without interactions
df_master$`Has Heart Disease` <- relevel(df_master$`Has Heart Disease`, ref="NO")
mylogit <- glm(`Has Heart Disease` ~ `Geographical Region` + Gender + `Marital Status` + `Years Since Immigration` + `Arthritis Type`, data = df_master, family = "binomial")
summary(mylogit)
confint(mylogit)

#with interactions
df_master$`Has Heart Disease` <- relevel(df_master$`Has Heart Disease`, ref="NO")
mylogit1 <- glm(`Has Heart Disease` ~ `Geographical Region` + Gender +`Household Income`+`Physical Activity`+ `Marital Status` + `Years Since Immigration` + `Arthritis Type` + `Marital Status`:`Arthritis Type` + `Years Since Immigration`:`Arthritis Type` + Gender:`Arthritis Type`+`Physical Activity`:`Arthritis Type`+`Geographical Region`:`Arthritis Type`, data = df_master, family = "binomial")
summary(mylogit1)

logitx <- glm(`Has Heart Disease` ~ `Arthritis Type`,data = df_master, family = "binomial")
summary(logitx)

levels(df_master$`Household Income`)
levels(df_inputation$`Household Income`)

write.csv(df_master, file='DATACHALLENGE.csv')

