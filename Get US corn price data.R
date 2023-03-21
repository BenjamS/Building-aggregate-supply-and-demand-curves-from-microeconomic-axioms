library(tidyquant)

df <- c("ZC=F") %>%
  tq_get(get  = "stock.prices",
         from = "2013-09-01",
         to   = "2022-09-01")

thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/A theory of aggregate supply and demand/"
thisFile <-"US corn future price.csv"
thisFilePath <- paste0(thisFolder, thisFile)
write.csv(df, thisFilePath, col.names = colnames(df))
