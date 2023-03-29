library(tidyverse)
thisFolder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/A theory of aggregate supply and demand/Eldoret weather data/"
thisFile <-"1902020_0.53_35.26_2018.csv"
thisFilePath <- paste0(thisFolder, thisFile)
df <- read.csv(thisFilePath, stringsAsFactors = F)
colnames(df) <- df[2,]
keepCols <- c("Year", "Month", "Day", "Hour", "Temperature", "Clearsky DNI", "Relative Humidity")
df <- df[, keepCols]
df <- df[-c(1, 2), ]
#df[, 5:7] <- as.data.frame(apply(df[, 5:7], 2, as.numeric ))
df <- subset(df, Hour %in% c(7:18) &
               Month %in% c(3:7))
df <- df %>% gather(var, val, Temperature:`Relative Humidity`)
df$val <- as.numeric(df$val)
df <- df %>% group_by(Year, Month, Day, var) %>%
  summarise(val = mean(val, na.rm = T)) %>% as.data.frame()

df$var <- gsub("Temperature", "Temp (C)", df$var)
df$var <- gsub(" Humidity", "\nHumidity (%)", df$var)
df$var <- gsub("Clearsky DNI", "Solar Irradiance\n(DNI w/m2)", df$var)

df$Date <- paste(df$Day, df$Month, df$Year, sep = "/")
df$Date <- as.Date(df$Date, "%d/%m/%Y")
df <- df[, -c(1:3)]

df <- df %>% group_by(var) %>%
  mutate(`Log change` = c(NA, diff(log(val)))) %>% as.data.frame()

#-----------------------------------------------------------------------
thisFile <- "NSRDB weather data Eldoret 2018 maize season.csv"
thisFilePath <- paste0(thisFolder, thisFile)
write.csv(df, thisFilePath, row.names = F)
#-----------------------------------------------------------------------
# Try
gg <- ggplot(df, aes(x = Date, y = val))
gg <- gg + geom_line()
gg <- gg + facet_wrap(~var, ncol = 1, scales = "free_y", strip.position = "right")
gg

gg <- ggplot(df, aes(x = `Log change`))
gg <- gg + geom_histogram(color = 4, fill = "white", bins = 20)
gg <- gg + facet_wrap(~var, ncol = 1, scales = "free", strip.position = "right")
gg
