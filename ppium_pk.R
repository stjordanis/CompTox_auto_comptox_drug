#### pk data api pullback ####
rm(list=ls())
library(data.table)
library(jsonlite)
url <- 'https://api.elsevier.com/pharma/pk/search?APIKey=035e274e3b4e2f68e0723e7550154328'
#need code to get N or the total count for the query

total_count <- 2058398 #set total_count to the total
firstRows <- unique(c(seq(1, total_count, 500), total_count))  #then get the first row starting at 1 and going by 500 until the end of the list (including the final value in case it is not even)
data_all <- data.table()

for(i in firstRows){
  y <- paste0(url,"&limitation.firstRow=",i)
  ppium1 <- fromJSON(txt=y)
  ppium_dt1 <- as.data.table(do.call(c, unlist(ppium1, recursive=FALSE)))
  data_all <- rbind(data_all, ppium_dt1, fill=TRUE)
}

#save.image("safety_ppium_6_25_20.RData") # change to current date 

#### pk_api table (pharmapendium) ####