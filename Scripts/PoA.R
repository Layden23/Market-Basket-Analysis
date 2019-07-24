############################ Installing packages #################################
pacman::p_load(readr,rstudioapi,ggplot2,party,dplyr,arules,arulesViz,
               RColorBrewer,readxl,tidyr,BiocManager,devtools,
               shiny,Rsenal)
source_gist(id='706a28f832a33e90283b')

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

trans_matrix <- as(trans,"matrix")
trans_df <- as.data.frame(trans_matrix)


################################### Plan of Attack ###############################
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

# Apriori algorithm 
rules <- apriori (trans, parameter = list(supp = 0.01, conf = 0.5))

rules
length(rules)
summary(rules)
inspect(rules)

############################ Visualization 

itemFrequencyPlot(trans,topN=10,type="absolute",col=brewer.pal(8,'Pastel2'),
                  main="Absolute Item Frequency Plot")
itemFrequencyPlot(trans,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), 
                  main="Relative Item Frequency Plot")
itemFrequencyPlot(trans, support = 0.1, cex.names=0.8)

image(sample(trans, 125))
image(trans[25])



plot(rules_retcat, method="graph",control=list(type="items"))

#Removing redundant rules
cleanrules <- function(rules){
  subsetRules <- which(colSums(is.subset(rules, rules)) > 1) 
  rules <- rules[-subsetRules] 
  return(rules)
}
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
############################### See by category 
trans_category <- aggregate(trans, by = "category")
rules_category <- apriori (trans_category, parameter = list(supp = 0.01, conf = 0.01,
                                                            minlen = 2))
summary(rules_category)
#The most populars categories
inspect(head(sort(rules_category, by ="support"),10))
#Items that have high chances of being bought together
inspect(head(sort(rules_category, by ="confidence"),10))
#Lift
inspect(head(sort(rules_category, by ="lift"),10))
############################### See by brand 
rules_brand <- apriori (trans_brand, parameter = list(supp = 0.01, conf = 0.01,
                                                      minlen = 2))
summary(rules_brand)
#The most populars categories
inspect(head(sort(rules_brand, by ="support"),20))
#Items that have high chances of being bought together
inspect(head(sort(rules_category, by ="confidence"),20))
#Lift
inspect(head(sort(rules_category, by ="lift"),20))
