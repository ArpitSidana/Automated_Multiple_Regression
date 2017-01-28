# Read in Item level dataset

df <- read.csv("<insert_dataframe_filename_with_file_extension>")

# Every record of dataframe contains unique product/item with a numeric item number in column "Item"

# We assume dataframe to contain columns "Item", along with 3 numeric attributes called "Attr1", "Attr2" and "Attr3" 
# and the target variable (numeric) to be "Sales"

head(df)

# Split the dataframe into groups based on column "Item"

out <- split(df,df$Item)

# Note that since every record contains a unique product/item, number of groups in "out" = number of rows in df

# Run a for loop to create dateframes for each item in group of "out" 
# We follow the notation of "df_<ItemNumber>" where each dataframe is uniquely identified by its item number in column "Item"

for (i in 1:length(out)) {
  assign(paste0("df_",df$UPC[i],sep=""), as.data.frame(out[i]))  
}

# Create a blank list to add calculated regression coefficients
coeff <- list()

# Create a blank list to add calculated p-value coefficients
p <- list()

# Access all unique dataframes created in previous for loop and run a multiple regression where the target variable
# (dependent variable) is "Sales" and factors for the regression model (independent variables) are "Attr1", "Attr2" and "Attr3" 

# Note that "Sales" is assumed to be 2nd column and other attributes are columns 3 to 5 while column 1 is "Item"

for (i in 1:length(out)) {
  
  # Access every dataframe
  t <- get(paste0("df_",df$Item[i],sep=""))
  
  # Run regression based on dependent and independent variables defined above and store coefficients for each 
  # column/feature in "coeff" as a new row
  coeff[[i]] <- coef(lm(formula = t[,2] ~ t[,3] + t[,4] + t[,5], data = t))
  
  # Run regression based on dependent and independent variables defined above and store p-values for each 
  # column/feature in "p" as a new row
  p[[i]] <- t(as.matrix(summary(lm(formula = t[,2] ~ t[,3] + t[,4] + t[,5], data = t))$coefficients[,4]))
}

# Transform the coefficients and p-value list to dataframes 
p_df <- plyr::ldply(p1, rbind)
coeff_df <- plyr::ldply(coeff, rbind)

# Rename column names for coefficients and p-values to suitable names

# Note that despite running a multiple regression on 3 features, we get an additional set of values for the "intercept"
# term. Here we named its pvalue as "Pval_1" and its coefficient as "Coeff_Intercept" and assume for it to have significance
# in analysing items. These can be dropped if not required.

# p_df <- p_df[,-1]
# coeff_df <- coeff_df[,-1]

colnames(p_df) <- c("Pval_1","Pval_2","Pval_3","Pval_4")
colnames(coefdf1_df) <- c("Coeff_Intercept","Coeff_Attr1","Coeff_Attr2","Coeff_Attr3","Coeff_Attr4")

# Append each item to represent all regression coefficients and p-values in a single row and
# description to the unique UPC from original dataframe
df2 <- cbind(p_df,coeff_df)
df2$Item <- df$Item

# Convert NA coefficients (if any) to 0
df2[is.na(df2)] <- 0

# Retain only significant regression coefficients (based on assumed significance level of 0.05)
df2 <- df2[(df2$Pval_1<0.05 & df2$Pval_2<0.05 & df2$Pval_3 <0.05 & df2$Pval_4<0.05),] 

# Check for positive coefficients

# Retain only those Items for which all attributes (Attr1, Attr2 and Attr3) exhibit positive coefficients 
obs2 <- df2[(df2$Coeff_Intercept>0 & df2$Coeff_Attr1>0 & df2$Coeff_Attr2>0 & df2$Coeff_Attr3>0),]

# Double check to remove records that contain NA's
obs1 <- obs1[complete.cases(obs1),]

# Number of items that exhibit positive effect for all attributes (Attr1, Attr2 and Attr3)
nrow(obs1)

# Check for negative coefficients

# Retain only those items for which all attributes (Attr1, Attr2 and Attr3) are negative
obs2 <- df2[(df2$Coeff_Intercept<0 & df2$Coeff_Attr1<0 & df2$Coeff_Attr2<0 & df2$Coeff_Attr3<0),]

# Double check to remove records that contain NA's
obs2 <- obs2[complete.cases(obs2),]

# Number of items that exhibit negative effect for all attributes (Attr1, Attr2 and Attr3)
nrow(obs2)

###############################################################################################################################