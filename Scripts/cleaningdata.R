################################Cleaning data #################################################
pacman::p_load(readr,rstudioapi,ggplot2,party,arules,arulesViz,RColorBrewer,textclean)
#Setting directory path
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)
getwd()
############################ Importing datasets #####################################

trans <- read.transactions("./Datasets/ElectronidexTransactions2017.csv", 
                           format = "basket",sep=",", rm.duplicates=TRUE)
x <- as(trans,"matrix")
x <- as.data.frame(x)
x <- mgsub(x = x, replacement = "Laptopts",pattern = c("LG Touchscreen Laptop Acer Aspire","HP Laptop",
                                     "ASUS Chromebook","Apple Macbook Pro","Apple MacBook Air",
                                     "Dell Laptop", "Eluktronics Pro Gaming Laptop",
                                     "Alienware AW17R4-7345SLV-PUS 17 Laptop", "HP Notebook Touchscreen Laptop PC")
              
               )
