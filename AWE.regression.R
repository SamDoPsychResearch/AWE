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
for (outcome in model_vars) {
  
  # Create formula for regression
  formula <- as.formula(paste(outcome, " ~ condition_p + condition_n")) ##SHOULD HAVE THE DUMMY VARS HERE
  
  # Fit regression model
  model_list[[i]] <- lm(formula, data = merged_data)
}