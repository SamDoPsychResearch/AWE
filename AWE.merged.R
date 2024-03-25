##Bringing the student and nonstudent samples together for regression analyses

##Run the student file
data_stu <- data

##Run the non-student file
data_nonstu <- data

# Merging data frames by all variables of the same name
merged_data <- merge(data_stu, data_nonstu, by = intersect(names(data_stu), names(data_nonstu)), all = TRUE)

##Running the Regressions
##Variables to include in the model i
model_vars <- c("awes", "awes.td", "awes.sd", "awes.c", "awes.v", "awes.ps", "awes.na", "sas", "sas.co", "sas.o", "sas.ch", "sas.ds", 
                "ss", "ss.v", "ss.sd", "shiota")

# Initialize an empty list to store regression models
model_list <- list()

# Loop through each variable in model_vars
for (i in 1:length(model_vars)) {
  
  # Create formula for regression
  formula <- as.formula(paste(model_vars[i], " ~ condition_p + condition_n")) ##SHOULD HAVE THE DUMMY VARS HERE
  
  # Fit regression model
  model_list[[i]] <- lm(formula, data = merged_data)
}

##Making some sort of table output
model_data <- data.frame(matrix(nrow = length(model_vars), ncol = 5))
colnames(model_data) <- c("Scale", "Estimate", "P-value", "Estimate", "P-value")
model_data[,1] <- model_vars

for (i in 1:length(model_vars)) {
  model <- model_list[[i]]
  modelsum <- summary(model)
  model_data[i,2:3] <- unname(modelsum$coefficients[2:3,1])
  model_data[i,4:5] <- unname(modelsum$coefficients[2:3,4])
}

# This line creates a new order for your columns where 3 and 4 are switched
new_order <- c(1:2, 4, 3, 5:ncol(model_data))

# Apply the new order to your dataframe
model_data <- model_data[, new_order]

##Creating Merged Positive, Negative, and Control subsample dataframes for the convergent and criterion validity tables
pos <- subset(merged_data, condition_p == 1)
neg <- subset(merged_data, condition_n == 1)
con <- subset(merged_data, condition_c == 1)

pos.conv <- cor(as.matrix(pos[,model_vars]),as.matrix(pos[,model_vars]))
neg.conv <- cor(as.matrix(neg[,model_vars]),as.matrix(neg[,model_vars]), use = "complete.obs") ##there is an NA here
con.conv <- cor(as.matrix(con[,model_vars]),as.matrix(con[,model_vars]))

##Variables for the criterion validity tables
crit_vars <- c("dpes", paste0("bfi2", c('o', 'c', 'e', 'a', 'n')), "tas")

pos.crit <- cor(as.matrix(pos[,model_vars]),as.matrix(pos[,crit_vars]), use = "complete.obs")
neg.crit <- cor(as.matrix(neg[,model_vars]),as.matrix(neg[,crit_vars]), use = "complete.obs")
con.crit <- cor(as.matrix(con[,model_vars]),as.matrix(con[,crit_vars]), use = "complete.obs")

##Turning these into lower triangle matrix so not to have useless correlations/each correlation repeated
pos.conv[upper.tri(pos.conv, diag = TRUE)] <- NA
neg.conv[upper.tri(neg.conv, diag = TRUE)] <- NA
con.conv[upper.tri(con.conv, diag = TRUE)] <- NA

conv_data <- rbind(as.data.frame(pos.conv), as.data.frame(neg.conv), as.data.frame(con.conv))
crit_data <- rbind(as.data.frame(pos.crit), as.data.frame(neg.crit), as.data.frame(con.crit))

##Outputting these to be shared with Jenny and Dr. Lucas
write.csv(model_data, "model_data.csv")
write.csv(conv_data, "conv_data.csv")
write.csv(crit_data, "crit_data.csv")