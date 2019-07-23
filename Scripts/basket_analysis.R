############################ Installing packages #################################
pacman::p_load(readr,rstudioapi,ggplot2,party,arules,arulesViz,RColorBrewer,readxl,tidyr)
#Setting directory path
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
  
############################ Importing datasets #####################################
trans <- read.transactions("./Datasets/ElectronidexTransactions2017.csv", 
                  format = "basket",sep=",", rm.duplicates=TRUE)
labels <- read_excel("./Datasets/ElectronidexItems2017.xlsx")

labels[is.na(labels)] <-"Unknown"
summary(trans)
head(itemInfo(trans))
trans@itemInfo$labels<- labels$ProductType
trans@itemInfo$category <- labels$Category
trans@itemInfo$brand <- labels$Brand
trans@itemInfo$value <- labels$Avg_Price
trans_matrix <- as(trans,"matrix")
trans_df <- as.data.frame(trans_matrix)
#Turning into 1s and 0s.
for (i in 1:ncol(trans_df)){
  trans_df[,i] <- as.integer(trans_df[,i])
}
#Sumrows
nitems <- c()
for (i in 1:nrow(trans_df)) {
  nitems <- c(nitems, sum(trans_df[i,]))
}

################################### Subsetting ###################################
ptrans@itemInfo$labels <- labels$Category
ptrans_df <- as.data.frame(as(ptrans,"matrix"))
for (i in c(1:ncol(ptrans_df))){
  ptrans_df[,i] <- as.integer(ptrans_df[,i])
}
for (i in nrow(ptrans_df)){
  ptrans_df$accesories <- sum(ptrans_df[i,which(colnames(ptrans_df) == "Accessories")])
}

ptrans_df <- unite(ptrans_df,col = "Accessories",
                   ptrans_df[,which(colnames(ptrans_df) %in% "Accessories")], remove = T)
inspect (trans) # You can view the transactions. 
#Is there a way to see a certain # of transactions?
inspect (trans[1:10])
# Number of transactions.
length (trans) 
# Number of items per transaction
size (trans) 
# Lists the transactions by conversion (LIST must be capitalized)
LIST(trans) 
# To see the item labels
itemLabels(trans)

############################ Visualization ########################################

itemFrequencyPlot(trans,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'),
                  main="Absolute Item Frequency Plot")
itemFrequencyPlot(trans,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), 
                  main="Relative Item Frequency Plot")
itemFrequencyPlot(trans, support = 0.1, cex.names=0.8)

image(sample(trans, 125))
image(trans[25])

############################ Apriori algorithm #####################################
rules <- apriori (trans, parameter = list(supp = 0.01, conf = 0.5))

rules
length(rules)
summary(rules)
inspect(rules)

###Finding rules for specific item and removing redundant rules

srules <- function(i){
  rules<- apriori(trans, parameter = list(supp=0.01, conf=0.8),
                         appearance = list(default="lhs",rhs=i))
  subsetRules <- which(colSums(is.subset(rules, rules)) > 1) 
  rules <- rules[-subsetRules] 
  return(rules)
}

imac_rules <- srules("iMac")
summary(imac_rules)
irules <- list()
for (i in names(x)){
  irules[[i]] <- srules(i)
}
############################### See by category ################################
trans_category <- aggregate(trans, by = "category")
rules_category <- apriori (trans_category, parameter = list(supp = 0.01, conf = 0.01,
                                                            minlen = 2))
summary(rules_category)
#The most populars categories
inspect(head(sort(rules_category, by ="support"),20))
#Items that have high chances of being bought together
inspect(head(sort(rules_category, by ="confidence"),20))
#Lift
inspect(head(sort(rules_category, by ="lift"),20))
############################### See by brand #####################################
trans_brand <- aggregate(trans, by = "brand")
rules_brand <- apriori (trans_brand, parameter = list(supp = 0.01, conf = 0.01,
                                                            minlen = 2))
summary(rules_brand)
#The most populars categories
inspect(head(sort(rules_brand, by ="support"),20))
#Items that have high chances of being bought together
inspect(head(sort(rules_category, by ="confidence"),20))
#Lift
inspect(head(sort(rules_category, by ="lift"),20))

############################### Rules Visualization ############################
