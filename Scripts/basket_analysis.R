############################ Installing packages #################################

pacman::p_load(readr,rstudioapi,ggplot2,party,arules,arulesViz,RcolorBrewer)
#Setting directory ath
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)

############################ Importing datasets #####################################
trans <- read.transactions("./Datasets/ElectronidexTransactions2017.csv", 
                  format = "basket",sep=",", rm.duplicates=TRUE)
head(itemInfo(trans))
myLabels <- c("LG Touchscreen Laptop","Acer Aspire","HP Laptop","ASUS Chromebook",
              "Apple Macbook Pro","Apple MacBook Air","Dell Laptop",
             "Eluktronics Pro Gaming Laptop","Alienware AW17R4-7345SLV-PUS 17 Laptop", "HP Notebook Touchscreen Laptop PC")
myLevel1 <- c("Laptops","Laptops","Laptops","Laptops","Laptops","Laptops","Laptops","Laptops","Laptops","Laptops")
itemInfo(trans) <- data.frame(labels = myLabels, level1 = myLevel1)

?aggregate
x <- as(trans,"matrix")
x <- as.data.frame(x)
for (i in c("LG Touchscreen Laptop","Acer Aspire","HP Laptop","ASUS Chromebook",
            "Apple Macbook Pro","Apple MacBook Air","Dell Laptop",
            "Eluktronics Pro Gaming Laptop","Alienware Laptop", "HP Notebook Touchscreen Laptop PC")) {
  names(x)[names(x) == i] <- "Laptops"
}
##############################
x
class(trans)
summary(trans)
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

############################### Rules Visualization ############################
plot(rules)
