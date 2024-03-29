##Load Packages
library(tidyverse)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(car)
library('GPArotation')
library(psych)
library(fastDummies)
#Worked on by Jenny and Sam 

##Useful Function
meansd <- function(x){
  y <- c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
  return(y)
}

##Load Data
data <- read.csv("AWE.student.registered.FINAL.csv")

##Removing people whose data we collected before fixing the wait time to 5 minutes 
##before continuing past the writing task
data <- data[-c(1:52),]

##Exclude unnecessary data
data <- select(data, -c("StartDate",  "EndDate", "Status", "Finished", "RecordedDate", "ResponseId", "DistributionChannel", 
                        "UserLanguage", 'Progress', 'Q_RelevantIDDuplicate', 'Q_RelevantIDDuplicateScore',
                        'Q_RelevantIDLastStartDate', 'Gender_3_TEXT', 'Duration..in.seconds.', 'id', 'Consent', 
                        paste0('Positive.Awe.Timing_', c('First.Click', 'Last.Click', 'Page.Submit', 'Click.Count')),
                        paste0('Negative.Awe.Time_', c('First.Click', 'Last.Click', 'Page.Submit', 'Click.Count')),
                        paste0('Control.Time_', c('First.Click', 'Last.Click', 'Page.Submit', 'Click.Count')),'Race', 'id',
                        paste0('Q_', c('RecaptchaScore', 
                                       paste0('RelevantID', c('Duplicate', 'DuplicateScore', 'LastStartDate', 'FraudScore'))))
                        ))
##I removed consent here after checking that every person had responded that they consent

##Removing people who said they could not recall an experience of pos or neg awe in the emotion recall check
data <- data %>% filter(Emotion.Recall_3 == "Yes" & Emotion.Recall_4 == "Yes")

##Exclude subjects who failed two or more checks
check1 = matrix(nrow=689, ncol=1)
for (i in 1:nrow(data)){
  if (data[i,'BFI2O_10'] != 'Agree a little'){
    check1[i,1] = 1
  }
  else if (data[i, 'BFI2O_10'] == 'Agree a little'){
    check1[i,1] = 0
  }
}
check2 = matrix(nrow=689, ncol=1)
for (i in 1:nrow(data)){
  if (data[i,'Q1_42'] != 'Agree a little'){
    check2[i,1] = 1
  }
  else if (data[i, 'Q1_42'] == 'Agree a little'){
    check2[i,1] = 0
  }
}
##Q 142 is the SAS for the Non-pos/neg awe condition respondents
check3 = matrix(nrow=689, ncol=1)
for (i in 1:nrow(data)){
  if (data[i,'SAS_4'] != 'Somewhat agree' & data[i,'Q142_4'] != "Somewhat agree"){
    check3[i,1] = 1
  }
  else if (data[i, 'SAS_4'] == 'Somewhat agree' | data[i,'Q142_4'] == "Somewhat agree"){
    check3[i,1] = 0
  }
}

data$fail = check1 + check2 + check3
data <- subset(data, fail<2)

##Removing attention check & Consent items
data <- select(data, -c('BFI2O_10', 'Q1_42', 'SAS_4', 'Q142_4', 'fail'))
rm(check1, check2, check3)

##Transforming response data into numerical
##BFI2O
number_vals <- function(data, cols, vals){
  for (col in cols) {
    data[[col]] <- lapply(data[[col]], function(x) {
      for (value in names(vals)) {
        x[x == value] <- vals[[value]]
      }
      return(x)
    })
  }
  return(data)
}

bfi2_cols <- c(7:18, 132:179) ##includes the separate bfi2o given at the beginning and the rest at the end
bfi2_vals <- c("Agree strongly" = 5, "Agree a little" = 4, "Neutral; no opinion" = 3, "Disagree a little" = 2, "Strongly disagree" = 1)

data <- number_vals(data, bfi2_cols, bfi2_vals)

##AWE-S
awes_col <- c(58:87, 180:209) ##this includes the block for the pos/neg awe and control conditions
awes_vals <- c("Strongly disagree" = 1, "Moderately Disagree" = 2, "Somewhat disagree" = 3, "Neutral" = 4, "Somewhat agree" = 5,
               "Moderately Agree" = 6, "Strongly agree" = 7)

data <- number_vals(data, awes_col, awes_vals)

##DPES
dpes_col <- 49:54 
dpes_vals <- c("Strongly disagree" = 1, "Moderately disagree" = 2, "Somewhat disagree" = 3, "Neutral" = 4, "Somewhat agree" = 5,
               "Moderately agree" = 6, "Strongly agree" = 7)

data <- number_vals(data, dpes_col, dpes_vals)

##SAS
sas_col <- c(88:102, 210:224) ##this includes the block for the pos/neg awe and control conditions
sas_vals <- c("Strongly disagree" = 1, "Moderately disagree" = 2, "Somewhat disagree" = 3, "Neutral" = 4, "Somewhat agree" = 5,
               "Moderately agree" = 6, "Strongly agree" = 7)

data <- number_vals(data, sas_col, sas_vals)

##Small Self
ss_col <- c(103:112, 225:234) ##this includes the block for the pos/neg awe and control conditions
ss_vals <- c("Strongly disagree" = 1, "Moderately disagree" = 2, "Somewhat disagree" = 3, "Neutral" = 4, "Somewhat agree" = 5,
              "Moderately agree" = 6, "Strongly agree" = 7, "Somewhat agreee" = 5) ##there must have been a typo on this one

data <- number_vals(data, ss_col, ss_vals)

##Shiota
shiota_col <- c(113:119, 235:241) ##this includes the block for the pos/neg awe and control conditions
shiota_vals <- c("Not true at all" = 1, "Moderately untrue" = 2, "Somewhat untrue" = 3, "Neutral" = 4, "Somewhat true" = 5,
             "Moderately true" = 6, "Very true" = 7)

data <- number_vals(data, shiota_col, shiota_vals)

##MTAS
mtas_col <- c(19:42) ##this includes the block for the pos/neg awe and control conditions
mtas_vals <- c("Never" = 1, "Rarely" = 2, "Occasionally" = 3, "Neutral" = 4, "Somewhat" = 5,
                 "Often" = 6, "Always" = 7)

data <- number_vals(data, mtas_col, mtas_vals)

##TAS
tas_col <- 114:125
tas_vals <- c("True" = 1, "False" = 0)

data <- number_vals(data, tas_col, tas_vals)

##Getting rid of all of this stuff to keep the environment less overwhelming
rm(awes_col, awes_vals, bfi2_vals, bfi2_cols, dpes_col, dpes_vals, mtas_col, mtas_vals, sas_col, sas_vals, shiota_col, shiota_vals,
   ss_col, ss_vals, tas_col, tas_vals)

##Removing this now because I hadn't removed it when writing the above code
data <- data %>% select(-c(paste0('Emotion.Recall_', 1:6)))

##ALL OF THE VALUES ARE LISTS AND NEED TO BE TURNED INTO DOUBLES/INTEGERS
# List of column names containing lists
list_columns <- c(paste0("DPES_",1:6), paste0("AWE.S_",1:30), paste0(rep("SAS_",15),c(1:3, 5:16)), 
                  paste0(rep("Small.Self_",10),1:10), paste0(rep("Shiota_",7),1:7), paste0("MTAS_",1:24), 
                  paste0("BFI2O_", c(1:9, 11:13)), paste0("Q1_", c(1:41, 43:49)), paste0("Q141_",1:30), 
                  paste0(rep("Q142_",15),c(1:3, 5:16)), paste0("Q143_", 1:10), paste0("Q144_", 1:7), paste0("Q71_", 1:12))

# Loop through each column and convert lists to doubles
for(col in list_columns) {
  data <- data %>%
    mutate(!!col := as.integer(!!sym(col))) 
}

# Rename columns
names(data)[grep("^Q141_", names(data))] <- paste0("AWE.S_", 1:30)
names(data)[grep("^Q142_", names(data))] <- paste0(rep("SAS_",15),c(1:3, 5:16))
names(data)[grep("^Q143_", names(data))] <- paste0(rep("Small.Self_",10),1:10)
names(data)[grep("^Q144_", names(data))] <- paste0(rep("Shiota_",7),1:7)

# Identifying the unique names of the columns with duplicates
unique_names <- names(data)[duplicated(names(data)) | duplicated(names(data), fromLast = TRUE)]

# Initializing an empty dataframe for the new columns
new_data <- as.data.frame(matrix(nrow = 625))

# Looping through each unique name, finding all columns with that name, and combining them
for (name in unique_names) {
  # Get all columns with this name
  cols <- data[, names(data) == name, drop = FALSE]
  # Combine them by taking the first non-NA value across each row
  combined <- coalesce(!!!as.list(cols))
  # Add the combined column to the new dataframe
  new_data[[name]] <- combined
}

# Adding the columns from the original dataframe that were not duplicated
unique_cols <- names(data)[!names(data) %in% unique_names]
new_data <- bind_cols(new_data, data[unique_cols])

##Replacing data with new_data
data <- new_data
rm(new_data)

##JENNYS CHANGED

##Dispositional Positive Emotions Scale
dpes.items <- paste0("DPES_",1:6)
dpes.key <- rep(1,6)
dpes.results <- scoreItems(keys = dpes.key, items = data[dpes.items], missing = TRUE, impute = "none", min = 1, max = 7)
dpes.results 
data$dpes <- dpes.results$score
describe(data$dpes)

##Awe Experience Scale
awes.items <- c(paste0(rep("AWE.S_",30),1:30))
awes.key <- rep(1,30)
awes.results <- scoreItems(keys = awes.key, items = data[awes.items], missing = TRUE, impute = "none", min = 1, max = 7)
awes.results 
data$awes <- awes.results$score
describe(data$awes)

awes.td.items <- c(paste0(rep("AWE.S_",5),1:5))
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

##Tellegen's Absorption Scale
tas.items <- paste0(rep("Q71_",12),1:12)
tas.key <- rep(1,12)
tas.results <- scoreItems(keys = tas.key, items = data[tas.items], missing = TRUE, impute = "none", min = 1, max = 7)
tas.results
data$tas <- tas.results$score
describe(data$tas)

#BFI 2
bfi2e.items <- paste0("Q1_",c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 46))
bfi2e.key <- c(1, 1, -1, -1, 1, -1, -1, -1, 1, 1, -1, 1)
bfi2e.results <- scoreItems(keys = bfi2e.key, items = data[bfi2e.items], missing = TRUE, impute = "none", min = 1, max = 5)
data$bfi2e <- bfi2e.results$score

bfi2a.items <- paste0("Q1_",c(2, 6, 10, 14, 18, 22, 26, 30, 34, 38, 43, 47))
bfi2a.key <- c(1, 1, -1, -1, -1, 1, 1, -1, -1, -1, 1, 1)
bfi2a.results <- scoreItems(keys = bfi2a.key, items = data[bfi2a.items], missing = TRUE, impute = "none", min = 1, max = 5)
data$bfi2a <- bfi2a.results$score

bfi2c.items <- paste0("Q1_",c(3, 7, 11, 15, 19, 23, 27, 31, 35, 39, 44, 48))
bfi2c.key <- c(-1, -1, 1, 1, -1, -1, 1, 1, 1, -1, 1, -1)
bfi2c.results <- scoreItems(keys = bfi2c.key, items = data[bfi2c.items], missing = TRUE, impute = "none", min = 1, max = 5)
data$bfi2c <- bfi2c.results$score

bfi2n.items <- paste0("Q1_",c(4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 45, 49))
bfi2n.key <- c(-1, -1, 1, 1, -1, -1, 1, 1, -1, -1, 1, 1)
bfi2n.results <- scoreItems(keys = bfi2n.key, items = data[bfi2n.items], missing = TRUE, impute = "none", min = 1, max = 5)
data$bfi2n <- bfi2n.results$score

bfi2o.items <- paste0("BFI2O_", c(1:9, 11:13))
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

fear.items <- paste0(rep("MTAS_",4), 9:12)
fear.key <- rep(1,4)
fear.results <- scoreItems(keys = fear.key, items = data[fear.items], missing = TRUE, impute = "none", min = 1, max = 7)
data$fear <- fear.results$score

anger.items <- paste0(rep("MTAS_",4),13:16)
anger.key <- rep(1,4)
anger.results <- scoreItems(keys = anger.key, items = data[anger.items], missing = TRUE, impute = "none", min = 1, max = 7)
data$anger <- anger.results$score

shame.items <- paste0(rep("MTAS_",4),17:20)
shame.key <- rep(1,4)
shame.results <- scoreItems(keys = shame.key, items = data[shame.items], missing = TRUE, impute = "none", min = 1, max = 7)
data$shame <- shame.results$score

sad.items <- paste0(rep("MTAS_",4),21:24)
sad.key <- rep(1,4)
sad.results <- scoreItems(keys = sad.key, items = data[sad.items], missing = TRUE, impute = "none", min = 1, max = 7)
data$sad <- sad.results$score

##Renaming the BFI2 & TAS items
names(data)[grep("^Q1_", names(data))] <- paste0(rep("BFI2_",48),c(1:41, 43:49))
names(data)[grep("^Q71_", names(data))] <- paste0(rep("TAS_",12), 1:12)

##Dropping the Random column of NAs made by the prior process
data <- data %>% select(-c("V1"))

##Separating Into Conditions 
pos <- subset(data, data$Positive.Awe.Write != '')
neg <- subset(data, data$Negative.Awe.Write != '')
con <- subset(data, data$Control.Write != '')

##Demographic Measures
meansd(data$Age)
table(data$Gender)
table(data$Transgender.not)
table(data$race) ##FIGURE OUT HOW I WILL BE REPORTING RACE
table(data$Ethnicity)
table(data$Education)