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

################################## New dataset #####################################
trans_df$nitems <- nitems
trans_df$laptops <- trans_df[,which(colnames(trans_df) == "LG Touchscreen Laptop")] +
                      trans_df[,which(colnames(trans_df) == "Acer Aspire")] +
                      trans_df[,which(colnames(trans_df) == "HP Laptop")] +
  trans_df[,which(colnames(trans_df) == "ASUS Chromebook")] +
  trans_df[,which(colnames(trans_df) == "Apple Macbook Pro")] +
  trans_df[,which(colnames(trans_df) == "Apple MacBook Air")] +
  trans_df[,which(colnames(trans_df) == "Dell Laptop")] +
  trans_df[,which(colnames(trans_df) == "Eluktronics Pro Gaming Laptop")] +
  trans_df[,which(colnames(trans_df) == "Alienware AW17R4-7345SLV-PUS 17\" Laptop")] +
  trans_df[,which(colnames(trans_df) == "HP Notebook Touchscreen Laptop PC")]
trans_df$desktop <- trans_df[,which(colnames(trans_df) == "Lenovo Desktop Computer")] +
  trans_df[,which(colnames(trans_df) == "iMac")] +
  trans_df[,which(colnames(trans_df) == "HP Desktop")] +
  trans_df[,which(colnames(trans_df) == "ASUS Desktop")] +
  trans_df[,which(colnames(trans_df) == "Dell Desktop")] +
  trans_df[,which(colnames(trans_df) == "Intel Desktop")] +
  trans_df[,which(colnames(trans_df) == "Acer Desktop")] +
  trans_df[,which(colnames(trans_df) == "CYBERPOWER Gamer Desktop")] +
  trans_df[,which(colnames(trans_df) == "Dell 2 Desktop")] 
trans_df$tablet <- trans_df[,which(colnames(trans_df) == "iPad")] +
  trans_df[,which(colnames(trans_df) == "iPad Pro")] +
  trans_df[,which(colnames(trans_df) == "Fire HD Tablet")] +
  trans_df[,which(colnames(trans_df) == "Samsung Galaxy Tab")] +
  trans_df[,which(colnames(trans_df) == "Kindle")]
trans_df$printer <- trans_df$`Epson Printer`+ trans_df$`HP Wireless Printer` +
  trans_df$`Canon Office Printer` + trans_df$`Brother Printer` + trans_df$`DYMO Label Manker`
trans_df$nmain <- trans_df$printer + trans_df$laptops + trans_df$desktop +
  trans_df$tablet
trans_df$ncomp <- trans_df$nitems - trans_df$nmain
trans_df$comp_items <- trans_df$nitems - trans_df$nmain
trans_df$value <- 10*trans_df$nmain + trans_df$ncomp

#Plots
ggplot(trans_df, aes(x = value)) + geom_histogram(colour = "blue",bins = 100) +
  scale_x_continuous(breaks=seq(0, 120,5))
#Laptop count
ggplot(trans_df, aes(x = laptops)) + geom_bar(bins = 50) + 
  scale_x_continuous(breaks=seq(0, 9, 1))
#Desktop count
ggplot(trans_df, aes(x = desktop)) + geom_bar(bins = 50) 
#Tablet count
ggplot(trans_df, aes(x = tablet)) + geom_bar(bins = 50) + 
  scale_y_continuous(breaks=seq(0, 8000, 1000))
#Printer count
ggplot(trans_df, aes(x = printer)) + geom_bar(bins = 50) 
#Printer vs tablet
ggplot(trans_df, aes(x = tablet, y = printer)) + geom_jitter()
#Printer vs desktop
ggplot(trans_df, aes(x = desktop, y = printer)) + geom_jitter()
#Laptops vs printer
ggplot(trans_df, aes(x = laptops, y = printer)) + geom_jitter()
#Laptop vs desktop
ggplot(trans_df, aes(x = laptops, y = desktop)) + geom_jitter()



ggplot(trans_df, aes(x = nmain, y = comp_items), color = "lightblue") + geom_jitter()
################################### Subsetting ######################################
corporate <- filter(trans_df, nmain >= 2 | ncomp >= 3)
retailer <- filter(trans_df,nmain <= 1 & ncomp <= 2)

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
