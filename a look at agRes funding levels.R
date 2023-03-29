thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/A theory of aggregate supply and demand/"
thisFile <- "FAO agResFunding_3-17-2023.csv"
thisFilePath <- paste0(thisFolder, thisFile)
df <- read.csv(thisFilePath, stringsAsFactors = F)
keepCols <- c("Donor", "Recipient.Country",
              "Element", "Purpose", "Year",
              "Unit", "Value")
df <- df[, keepCols]


a <- runif(3, max = 1.5)
sum(a^2)-sum(a)

