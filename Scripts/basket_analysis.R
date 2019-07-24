############################ Installing packages #################################
pacman::p_load(readr,rstudioapi,ggplot2,party,dplyr,arules,arulesViz,
               RColorBrewer,readxl,tidyr,BiocManager,devtools,
               shiny)


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
#Turning into 1s and 0s.
for (i in 1:ncol(trans_df)){
  trans_df[,i] <- as.integer(trans_df[,i])
}
#Counting number of items
nitems <- c()
for (i in 1:nrow(trans_df)) {
  nitems <- c(nitems, sum(trans_df[i,]))
}
################################## Functions ##################################

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
  trans_df$`Canon Office Printer` + trans_df$`Brother Printer` + 
  trans_df$`DYMO Label Manker`
trans_df$nmain <- trans_df$printer + trans_df$laptops + trans_df$desktop +
  trans_df$tablet
trans_df$ncomp <- trans_df$nitems - trans_df$nmain
trans_df$value <- 10*trans_df$nmain + trans_df$ncomp

################################ Visualizations ############################
products <- c(laptops, desktop)
for (i in products){
  print(ggplot(trans_df, aes_string(x = value)) + 
          geom_histogram(colour = "blue",bins = 100))
}
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
corporate <- corporate[,-which(colnames(corporate) %in% c("laptops","desktop",
                                                          "printer","tablet",
                                                          "nitems","nmain","value",
                                                          "ncomp"))]
retailer <- filter(trans_df,nmain <= 1 & ncomp <= 2)
retailer <- retailer[,-which(colnames(retailer) %in% c("laptops","desktop",
                                                          "printer","tablet",
                                                          "nitems","nmain","value",
                                                          "ncomp"))]
trans_corp <- as(corporate == 1, "transactions")
trans_retail <- as(retailer == 1, "transactions")
trans_corp@itemInfo$labels <- labels$ProductType
trans_corp@itemInfo$category <- labels$Category
trans_retail@itemInfo$labels <- labels$ProductType
trans_retail@itemInfo$category <- labels$Category

itemFrequencyPlot(trans_corp,topN=10,type="relative",col=brewer.pal(8,'Pastel2'), 
                  main="Corp Relative Item Frequency Plot")

itemFrequencyPlot(trans_retail,topN=10,type="relative",col=brewer.pal(8,'Pastel2'), 
                  main="Retail Relative Item Frequency Plot")
################################### Loop try ###############################
#rules <- list(
#transactions <- c("trans_corp","trans_retail")
#for (i in transactions){
#  rules[[i]]<- apriori(formula(i), parameter = list(supp = 0.01, conf = 0.01, 
#                                                        minlen = 2))
#}
################################### Corported by products ########################

rules_corpro <- apriori (trans_corp, parameter = list(supp = 0.01, conf = 0.01, 
                                                 minlen = 2))
summary(rules_corpro)
#The most populars products
corpro_sup <- inspect(head(sort(rules_corpro, by ="support"),10))
#Items that have high chances of being bought together
corpro_con <- inspect(head(sort(rules_corpro, by ="confidence"),10))
#Lift
corpro_lift <- inspect(head(sort(rules_corpro, by ="lift"),10))

################################### Corported by category ########################
trans_corcat <- aggregate(trans_corp, by = "category")
rules_corcat <- apriori(trans_corcat, parameter = list(supp = 0.05, conf = 0.01, 
                                                 minlen = 2))
summary(rules_corcat)
#The most populars categories
corcat_sup <- inspect(head(sort(rules_corcat, by ="support"),10))
#Items that have high chances of being bought together
corcat_con <- inspect(head(sort(rules_corcat, by ="confidence"),10))
#Lift
corcat_lift <- inspect(head(sort(rules_corcat, by ="lift"),10))

################################### Retailers by products ########################
rules_retpro <- apriori(trans_retail, parameter = list(supp = 0.005, conf = 0.01, 
                                                 minlen = 2))
summary(rules_retpro)
#The most populars products
retpro_sup <- inspect(head(sort(rules_retpro, by ="support"),10))
#Items that have high chances of being bought together
retpro_con <- inspect(head(sort(rules_retpro, by ="confidence"),10))
#Lift
retpro_lift <- inspect(head(sort(rules_retpro, by ="lift"),10))

################################### Retailers by category ########################
trans_retcat <- aggregate(trans_retail, by = "category")
rules_retcat <- apriori(trans_retcat,parameter = list(supp = 0.01, conf = 0.01, 
                                                      minlen = 2))
summary(rules_retcat)
#The most populars products
retcat_sup <- inspect(head(sort(rules_retcat, by ="support"),10))
#Items that have high chances of being bought together
retcat_con <- inspect(head(sort(rules_retcat, by ="confidence"),10))
#Lift
retcat_lift <- inspect(head(sort(rules_retcat, by ="lift"),10))


############################### Rules Visualization ############################
plot(rules_retcat, method = "graph",control=list(type="items"), max = 10)
plot(rules_retcat, method = "scatterplot")
plot(rules_retcat, method = "grouped")

