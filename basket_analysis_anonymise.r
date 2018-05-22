#####Load Libraries
library(readxl)
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}
library(plyr)
library(plyr)
library(arules)
library(arulesViz)
#Read Xlsx file
df_df_maaz <- read_xlsx("D:\\XYZ.xlsx", sheet = "Sheet1", range = NULL, col_names = TRUE)
#Pick a subset of Purchase
df_df_maaz <- df_df_maaz[df_df_maaz$TransDesc =='PURCHASE',]
#Pick relevant columns for basket analysis
df_df_maaz <- df_df_maaz[,c("Fund Name", "CustomerID","DealDate")] 
colnames(df_df_maaz) <- c("Fund_Name", "CustomerID", "DealDate")

# Sort the dataframe by CustomerID
df_sorted <- df_df_maaz[order(df_df_maaz$CustomerID),]


df_itemList <- ddply(df_df_maaz,c("CustomerID","DealDate"), 
                     function(df1)paste(df1$Fund_Name, 
                                        collapse = ","))

df_itemList$CustomerID <- NULL
df_itemList$DealDate <- NULL

#Rename column headers for ease of use
colnames(df_itemList) <- c("FundList")

# Save as csv
write.csv(df_itemList,"D:\\Al df_maaz\\ItemList.csv", row.names = TRUE, quote=FALSE)
##Check write csv has no qoutes
#Read as Transactions for Apriori Algorithm
txn = read.transactions(file="D:\\Al df_maaz\\ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1)
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)
rules <- apriori(txn, 
                parameter = list(supp = 0.001, conf = 0.5,target="rules"))



options(digits=2)
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:tm, unload=TRUE)
}
inspect(rules)

# PLOTS ####################################################

# Scatterplot of support x confidence (colored by lift)
plot(rules)

# Graph of top 20 rules
plot(rules[1:17], 
     method = "graph", 
     control = list(type = "items"))

# Parallel coordinates plot of top 20 rules
plot(rules[1:17], 
     method = "paracoord", 
     control = list(reorder = TRUE))

# Matrix plot of antecedents and consequents
plot(rules[1:17], 
     method = "matrix", 
     control = list(reorder = 'support/confidence'))

# Grouped matrix plot of antecedents and consequents
plot(rules, method = "grouped")
