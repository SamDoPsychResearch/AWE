##Load Packages
library(tidyverse)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(car)
library('GPArotation')
library(psych)
library(fastDummies)

##Useful Function
meansd <- function(x){
  y <- c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
  return(y)
}

##Load Data
data <- read.csv("Qualtrics.Awe.csv")

##Exclude unnecessary data
data <- select(data, -c("StartDate",  "EndDate", "Status", "Finished", "RecordedDate", "ResponseId", "DistributionChannel", 
                        "UserLanguage", 'Progress', 'ExternalReference', 'Q_RelevantIDDuplicate', 'Q_RelevantIDDuplicateScore',
                        'Q_RelevantIDLastStartDate', 'Gender_3_TEXT', 'Race_6', 'Race_7', 
                        paste0('Positive.Awe.Timing_', c('First.Click', 'Last.Click', 'Page.Submit', 'Click.Count')),
                        paste0('Negative.Awe.Time_', c('First.Click', 'Last.Click', 'Page.Submit', 'Click.Count')),
                        paste0('Control.Time_', c('First.Click', 'Last.Click', 'Page.Submit', 'Click.Count')), 'opp', 'gc', 
                        'term', 'transaction_id', 'SVID', 'rid', 'RISN', 'V', 'Q_BallotBoxStuffing', 'Q_TotalDuration', 'Race',
                        'PS', 'LS'))

##Exclude subjects who failed two or more checks
check1 = matrix(nrow=613, ncol=1)
for (i in 1:nrow(data)){
  if (data[i,'BFI2O_10'] != 'Agree a little'){
    check1[i,1] = 1
  }
  else if (data[i, 'BFI2O_10'] == 'Agree a little'){
    check1[i,1] = 0
  }
}
check2 = matrix(nrow=613, ncol=1)
for (i in 1:nrow(data)){
  if (data[i,'MTAS_10'] != 'Often'){
    check2[i,1] = 1
  }
  else if (data[i, 'MTAS_10'] == 'Often'){
    check2[i,1] = 0
  }
}
check3 = matrix(nrow=613, ncol=1)
for (i in 1:nrow(data)){
  if (data[i,'SAS_4'] != 'Somewhat Agree'){
    check3[i,1] = 1
  }
  else if (data[i, 'SAS_4'] == 'Somewhat Agree'){
    check3[i,1] = 0
  }
}

data$fail = check1 + check2 + check3
data <- subset(data, fail<2)
##Removing those who did not consent
data <- data[data$Consent == 'I consent.',]
##Removing attention check & Consent items
data <- select(data, -c('BFI2O_10', 'MTAS_10', 'SAS_4', 'Consent', 'fail'))
rm(check1, check2, check3)

##Removing people from the positive and negative awe conditions who said they could not recall an experience of their 
##respective emotion in the emotion recall check
for (i in 1:nrow(data)){
  if (is.na(data$Emotion.Recall_3[i]) | is.na(data$Emotion.Recall_4[i])){
    data <- data[-c(i),]
  }
  else if (data$Emotion.Recall_3[i] != 'Yes' | data$Emotion.Recall_4[i] != 'Yes'){
    data <- data[-c(i),]
  }
}

##Transforming response data into numerical
##BFI2O
mat = matrix(nrow=389, ncol=12)
i = 1;
while (i <390) {
  j = 1
  while (j < 13){
    y = data[i, j+14]
    if (y == "Agree strongly"){
      mat[i,j] = 5;
    }
    else if (y == "Agree a little"){
      mat[i,j] = 4;
    }
    else if (y == "Neutral; no opinion"){
      mat[i,j] = 3;
    }
    else if (y == "Disagree a little"){
      mat[i,j] = 2
    }
    else if (y == "Strongly disagree"){
      mat[i,j] = 1
    }
    j=j+1
  }
  i=i+1
}

##AWE-S
mat2 = matrix(ncol=30, nrow=389)
i=1;
while(i<390){
  j=1
  while(j<31){
    y=data[i,j+65]
    if (y == "Strongly disagree"){
      mat2[i,j] = 1;
    }
    else if (y == "Moderately Disagree"){
      mat2[i,j] = 2;
    }
    else if (y == "Somewhat disagree"){
      mat2[i,j] = 3;
    }
    else if (y == "Neutral"){
      mat2[i,j] = 4
    }
    else if (y == "Somewhat agree"){
      mat2[i,j] = 5
    }
    else if (y == "Moderately Agree"){
      mat2[i,j] = 6
    }
    else if (y == "Strongly agree"){
      mat2[i,j] = 7
    }
    j=j+1
  }
  i=i+1
}

##DPES
mat4 = matrix(ncol=6, nrow=389)
i=1;
while(i<390){
  j=1
  while(j<7){
    y=data[i,j+56]
    if (y == "Strongly disagree"){
      mat4[i,j] = 1;
    }
    else if (y == "Moderately disagree"){
      mat4[i,j] = 2;
    }
    else if (y == "Somewhat disagree"){
      mat4[i,j] = 3;
    }
    else if (y == "Neutral"){
      mat4[i,j] = 4
    }
    else if (y == "Somewhat agree"){
      mat4[i,j] = 5
    }
    else if (y == "Moderately agree"){
      mat4[i,j] = 6
    }
    else if (y == "Strongly agree"){
      mat4[i,j] = 7
    }
    j=j+1
  }
  i=i+1
}

##SAS
mat5 = matrix(ncol=15, nrow=389)
i=1;
while(i<390){
  j=1
  while(j<16){
    y=data[i,j+95]
    if (y == "Strongly disagree"){
      mat5[i,j] = 1;
    }
    else if (y == "Moderately disagree"){
      mat5[i,j] = 2;
    }
    else if (y == "Somewhat disagree"){
      mat5[i,j] = 3;
    }
    else if (y == "Neutral"){
      mat5[i,j] = 4
    }
    else if (y == "Somewhat agree"){
      mat5[i,j] = 5
    }
    else if (y == "Moderately agree"){
      mat5[i,j] = 6
    }
    else if (y == "Strongly agree"){
      mat5[i,j] = 7
    }
    j=j+1
  }
  i=i+1
}

##Small Self
mat6 = matrix(ncol=10, nrow=389)
i=1;
while(i<390){
  j=1
  while(j<11){
    y=data[i,j+110]
    if (y == "Strongly disagree"){
      mat6[i,j] = 1;
    }
    else if (y == "Moderately disagree"){
      mat6[i,j] = 2;
    }
    else if (y == "Somewhat disagree"){
      mat6[i,j] = 3;
    }
    else if (y == "Neutral"){
      mat6[i,j] = 4
    }
    else if (y == "Somewhat agreee"){
      mat6[i,j] = 5
    }
    else if (y == "Moderately agree"){
      mat6[i,j] = 6
    }
    else if (y == "Strongly agree"){
      mat6[i,j] = 7
    }
    j=j+1
  }
  i=i+1
}

##Shiota
mat7 = matrix(ncol=7, nrow=389)
i=1;
while(i<390){
  j=1
  while(j<8){
    y=data[i,j+120]
    if (y == "Not true at all"){
      mat7[i,j] = 1;
    }
    else if (y == "Moderately untrue"){
      mat7[i,j] = 2;
    }
    else if (y == "Somewhat untrue"){
      mat7[i,j] = 3;
    }
    else if (y == "Neutral"){
      mat7[i,j] = 4
    }
    else if (y == "Somewhat true"){
      mat7[i,j] = 5
    }
    else if (y == "Moderately true"){
      mat7[i,j] = 6
    }
    else if (y == "Very true"){
      mat7[i,j] = 7
    }
    j=j+1
  }
  i=i+1
}

##MTAS
mat8 = matrix(ncol=24, nrow=389)
i=1;
while(i<390){
  j=1
  while(j<25){
    y=data[i,j+26]
    if (y == "Never"){
      mat8[i,j] = 1;
    }
    else if (y == "Rarely"){
      mat8[i,j] = 2;
    }
    else if (y == "Occasionally"){
      mat8[i,j] = 3;
    }
    else if (y == "Neutral"){
      mat8[i,j] = 4
    }
    else if (y == "Somewhat"){
      mat8[i,j] = 5
    }
    else if (y == "Often"){
      mat8[i,j] = 6
    }
    else if (y == "Always"){
      mat8[i,j] = 7
    }
    j=j+1
  }
  i=i+1
}

data[,15:26] <- mat
data[,66:95] <- mat2
data[,57:62] <- mat4
data[,96:110] <- mat5
data[,111:120] <- mat6
data[,121:127] <- mat7
data[,27:50] <- mat8
rm(mat, mat2, mat4, mat5, mat6, mat7, mat8, i, j, y)

##Removing this now because I hadn't removed it when writing the above code
data <- data %>% select(-c(paste0('Emotion.Recall_', 1:6)))

##Dispositional Positive Emotions Scale
dpes.items <- paste0("DPES_",1:6)
dpes.key <- rep(1,6)
dpes.results <- scoreItems(keys = dpes.key, items = data[dpes.items], missing = TRUE, impute = "none", min = 1, max = 7)
dpes.results 
data$dpes <- dpes.results$score
describe(data$dpes)

##Awe Experience Scale
awes.items <- paste0(rep("AWE.S_",30),1:30)
awes.key <- rep(1,30)
awes.results <- scoreItems(keys = awes.key, items = data[awes.items], missing = TRUE, impute = "none", min = 1, max = 7)
awes.results 
data$awes <- awes.results$score
describe(data$awes)

awes.td.items <- paste0(rep("AWE.S_",5),1:5)
awes.td.key <- rep(1,5)
awes.td.results <- scoreItems(keys = awes.td.key, items = data[awes.td.items], missing = TRUE, impute = "none", min = 1, max = 7)
awes.td.results
data$awes.td <- awes.td.results$score

awes.sd.items <- paste0(rep("AWE.S_",5),6:10)
awes.sd.key <- rep(1,5)
awes.sd.results <- scoreItems(keys = awes.sd.key, items = data[awes.sd.items], missing = TRUE, impute = "none", min = 1, max = 7)
awes.sd.results
data$awes.sd <- awes.sd.results$score

awes.c.items <- paste0(rep("AWE.S_",5),11:15)
awes.c.key <- rep(1,5)
awes.c.results <- scoreItems(keys = awes.c.key, items = data[awes.c.items], missing = TRUE, impute = "none", min = 1, max = 7)
awes.c.results
data$awes.c <- awes.c.results$score

awes.v.items <- paste0(rep("AWE.S_",5),16:20)
awes.v.key <- rep(1,5)
awes.v.results <- scoreItems(keys = awes.v.key, items = data[awes.v.items], missing = TRUE, impute = "none", min = 1, max = 7)
awes.v.results
data$awes.v <- awes.v.results$score

awes.ps.items <- paste0(rep("AWE.S_",5),21:25)
awes.ps.key <- rep(1,5)
awes.ps.results <- scoreItems(keys = awes.ps.key, items = data[awes.ps.items], missing = TRUE, impute = "none", min = 1, max = 7)
awes.ps.results
data$awes.ps <- awes.ps.results$score

awes.na.items <- paste0(rep("AWE.S_",5),26:30)
awes.na.key <- rep(1,5)
awes.na.results <- scoreItems(keys = awes.na.key, items = data[awes.na.items], missing = TRUE, impute = "none", min = 1, max = 7)
awes.na.results
data$awes.na <- awes.na.results$score

##Situational Awe Scale
sas.items <- paste0(rep("SAS_",15),c(1:3, 5:16))
sas.key <- rep(1,15)
sas.results <- scoreItems(keys = sas.key, items = data[sas.items], missing = TRUE, impute = "none", min = 1, max = 7)
sas.results 
data$sas <- sas.results$score
describe(data$sas)

sas.co.items <- paste0(rep("SAS_",4),c(1:3, 5))
sas.co.key <- rep(1,4)
sas.co.results <- scoreItems(keys = sas.co.key, items = data[sas.co.items], missing = TRUE, impute = "none", min = 1, max = 7)
sas.co.results 
data$sas.co <- sas.co.results$score

sas.o.items <- paste0(rep("SAS_",4),c(6:8,14))
sas.o.key <- rep(1,4)
sas.o.results <- scoreItems(keys = sas.o.key, items = data[sas.o.items], missing = TRUE, impute = "none", min = 1, max = 7)
sas.o.results 
data$sas.o <- sas.o.results$score

sas.ch.items <- paste0(rep("SAS_",4),9:12)
sas.ch.key <- rep(1,4)
sas.ch.results <- scoreItems(keys = sas.ch.key, items = data[sas.ch.items], missing = TRUE, impute = "none", min = 1, max = 7)
sas.ch.results 
data$sas.ch <- sas.ch.results$score

sas.ds.items <- paste0(rep("SAS_",3),c(13,15,16))
sas.ds.key <- rep(1,3)
sas.ds.results <- scoreItems(keys = sas.ds.key, items = data[sas.ds.items], missing = TRUE, impute = "none", min = 1, max = 7)
sas.ds.results 
data$sas.ds <- sas.ds.results$score

##Small Self Scale
ss.items <- paste0(rep("Small.Self_",10),1:10)
ss.key <- rep(1,10)
ss.results <- scoreItems(keys = ss.key, items = data[ss.items], missing = TRUE, impute = "none", min = 1, max = 7)
ss.results 
data$ss <- ss.results$score
describe(data$ss)

ss.v.items <- paste0(rep("Small.Self_",5),c(1:4,10))
ss.v.key <- rep(1,5)
ss.v.results <- scoreItems(keys = ss.v.key, items = data[ss.v.items], missing = TRUE, impute = "none", min = 1, max = 7)
ss.v.results 
data$ss.v <- ss.v.results$score

ss.sd.items <- paste0(rep("Small.Self_",5),5:9)
ss.sd.key <- rep(1,5)
ss.sd.results <- scoreItems(keys = ss.sd.key, items = data[ss.sd.items], missing = TRUE, impute = "none", min = 1, max = 7)
ss.sd.results 
data$ss.sd <- ss.sd.results$score

##Shiota's Scale
shiota.items <- paste0(rep("Shiota_",7),1:7)
shiota.key <- rep(1,7)
shiota.results <- scoreItems(keys = shiota.key, items = data[shiota.items], missing = TRUE, impute = "none", min = 1, max = 7)
shiota.results 
data$shiota <- shiota.results$score
describe(data$shiota)

#BFI2O
bfi2o.items <- paste0(rep("BFI2O_",12),c(1:9,11:13))
bfi2o.key <- c(-1, 1, 1, 1, -1, -1, 1, 1, -1, -1, -1, 1)
bfi2o.results <- scoreItems(keys = bfi2o.key, items = data[bfi2o.items], missing = TRUE, impute = "none", min = 1, max = 5)
data$bfi2o <- bfi2o.results$score

##MTAS
love.items <- paste0(rep("MTAS_",4),1:4)
love.key <- rep(1,4)
love.results <- scoreItems(keys = love.key, items = data[love.items], missing = TRUE, impute = "none", min = 1, max = 7)
data$love <- love.results$score

joy.items <- paste0(rep("MTAS_",4),5:8)
joy.key <- rep(1,4)
joy.results <- scoreItems(keys = joy.key, items = data[joy.items], missing = TRUE, impute = "none", min = 1, max = 7)
data$joy <- joy.results$score

fear.items <- paste0(rep("MTAS_",4),c(9, 11:13))
fear.key <- rep(1,4)
fear.results <- scoreItems(keys = fear.key, items = data[fear.items], missing = TRUE, impute = "none", min = 1, max = 7)
data$fear <- fear.results$score

anger.items <- paste0(rep("MTAS_",4),14:17)
anger.key <- rep(1,4)
anger.results <- scoreItems(keys = anger.key, items = data[anger.items], missing = TRUE, impute = "none", min = 1, max = 7)
data$anger <- anger.results$score

shame.items <- paste0(rep("MTAS_",4),18:21)
shame.key <- rep(1,4)
shame.results <- scoreItems(keys = shame.key, items = data[shame.items], missing = TRUE, impute = "none", min = 1, max = 7)
data$shame <- shame.results$score

sad.items <- paste0(rep("MTAS_",4),22:25)
sad.key <- rep(1,4)
sad.results <- scoreItems(keys = sad.key, items = data[sad.items], missing = TRUE, impute = "none", min = 1, max = 7)
data$sad <- sad.results$score

##Demographic Measures
meansd(data$Age)
table(data$Gender)
table(data$Transgender.not)
table(data$race) ##FIGURE OUT HOW I WILL BE REPORTING RACE
table(data$Ethnicity)
table(data$Education)