library(tidyverse)
#D:\OneDrive - CGIAR\Documents 1\CIAT 2\FnM Initiative\New PE Model\Tegemeo Data.zip\Tegemeo Data
setwd("D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/New PE Model/Tegemeo Data/")
this_file <- "Kenya Tegemeo maize and beans.csv"
#this_folder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/Kcal price/"
#this_folder <- "C:/Users/bensc/OneDrive/Documents/Data/Tegemeo Data/"
#this_filepath <- paste0(this_folder, this_file)
df <- read.csv(this_file, stringsAsFactors = F)
#===========================================================================
colnames(df) <- gsub(".kg.", "(kg)", colnames(df))
colnames(df) <- gsub(".kg.acre.", "(kg/acre)", colnames(df))
colnames(df) <- gsub(".man.hours.", "(man hours)", colnames(df))
colnames(df) <- gsub(".man.hours.acre.", "(man hours/acre)", colnames(df))
colnames(df) <- gsub(".man.days.", "(man days)", colnames(df))
colnames(df) <- gsub(".man.days.acre.", "(man days/acre)", colnames(df))
colnames(df) <- gsub("landprep..", "landprep: ", colnames(df))
colnames(df) <- gsub("tenure..", "tenure: ", colnames(df))
colnames(df) <- gsub("acres..", "acres_", colnames(df))
colnames(df) <- gsub("pest.plague", "pest/plague", colnames(df))
colnames(df) <- gsub("\\.\\.", "\\. ", colnames(df))
colnames(df) <- gsub("\\.", " ", colnames(df))
colnames(df) <- gsub("  ", " ", colnames(df))
#===========================================================================
# Test it out
#colnames(df)
input_vars <- c("Adult family labor (man hours/acre)",
  "Child family labor (man hours)",
  #"Wage labor (man days/acre)",
  "Total synth fert (kg/acre)",
  "Total organic fert (kg/acre)",
  "seed (kg/acre)",
  "pest/plague chems (kg/acre)"
)
demog_vars <- c("age",
  "aehh07"#,
  #"hhsize07"
  #"km to organic fert mkt",
  #"km to synth fert mkt",
  #"km to pest/plague chem mkt" # omit because lots of NA
)
clim_vars <- c(#"Rain anomaly"
  "qwetpre",
  # #"main07"#,
  "qwetxt",
  "qwetit"
)
bin_vars <- c("adopter",
              "gend",
              "irrigated",
              #"tenure: govt/communal/cooperative",
              #"tenure: owned by parent/relative",
              "tenure: owned with title deed",
              #"tenure: owned without title deed",
              #"tenure: rented",
              #"landprep: manual",
              #"landprep: none",
              "landprep: oxen",
              "landprep: tractor"
)
mod_vars <- c("dist", "crop", "yield (kg/acre)", "lpcost", "acres",
              input_vars, bin_vars, demog_vars, clim_vars)
df_mod <- subset(df[, mod_vars], crop == "maize" &
                   #dist == "Kakamega" &
                   `yield (kg/acre)` > 1)
df_mod$crop <- NULL
# df_mod$dist <- NULL
df_look <- df_mod
ind_bin <- which(colnames(df_mod) %in% bin_vars)
ind_char <- which(colnames(df_mod) %in% c("hhid", "dist"))
ind_lpcost <- which(colnames(df_mod) == "lpcost")
#----------------------------------------------------------------------------
# Get prices
df_inPrice <- read.csv("Kenya Tegemeo input prices.csv", stringsAsFactors = F)
df_inSeedPrice <- read.csv("Kenya Tegemeo seed prices.csv", stringsAsFactors = F)
df_maizePrice <- read.csv("Kenya Tegemeo maize bean crop prices.csv", stringsAsFactors = F)
df_inSeedPrice <- subset(df_inSeedPrice, Item != "beans seed price (KES/kg)")
df_inSeedPrice <- df_inSeedPrice %>% spread(Item, Value) %>% as.data.frame()
df_maizePrice <- subset(df_maizePrice,
                        Item == "maize price (KES/kg)")
colnames(df_maizePrice)[ncol(df_maizePrice)] <- "maize price (KES/kg)"
df_maizePrice$Item <- NULL
df_mod <- merge(df_mod, df_inPrice, by = "dist")
df_mod <- merge(df_mod, df_maizePrice, by = "dist")
df_mod <- merge(df_mod, df_inSeedPrice, by = "dist")
colnames(df_mod)[grep("organic.fert.price..KES.kg.", colnames(df_mod))] <- "wOrgFert"
colnames(df_mod)[grep("synth.fert.price..KES.kg.", colnames(df_mod))] <- "wFert"
colnames(df_mod)[grep("pest.plague.chem.price..KES.kg.", colnames(df_mod))] <- "wPD"
colnames(df_mod)[grep("maize price", colnames(df_mod))] <- "P"
df_mod$wSeed <- df_mod$`maize seed price (KES/kg)`
df_mod$wSeed[which(df_mod$adopter == 1)] <- df_mod$`hybrid maize seed price (KES/kg)`[which(df_mod$adopter == 1)]
df_mod$`hybrid maize seed price (KES/kg)` <- NULL
df_mod$`maize seed price (KES/kg)` <- NULL
df_mod$wage..KES.day. <- NULL
df_mod$wage..KES.hour. <- NULL
colnames(df_mod)
df_mod$Cstar <- df_mod$`Total synth fert (kg/acre)` * df_mod$wFert +
  df_mod$`Total organic fert (kg/acre)` * df_mod$wOrgFert +
  df_mod$`seed (kg/acre)` * df_mod$wSeed +
  df_mod$`pest/plague chems (kg/acre)` * df_mod$wPD +
  df_mod$lpcost / df_mod$acres
df_mod$CtildStar <- df_mod$Cstar / df_mod$`yield (kg/acre)`
#hist(log(df_mod$P / df_mod$CtildStar))
#----------------------------------------------------------------------------
not_these <- unique(c(ind_bin, ind_char))
df_mod[, -not_these] <- log(df_mod[, -not_these])
fun <- function(x){
  x[which(is.infinite(x))] <- 0
  return(x)}
df_mod[, -not_these] <- as.data.frame(apply(df_mod[, -not_these], 2, fun))
indRm <- which(is.na(df_mod$aehh07))
if(length(indRm) != 0){
  df_mod <- df_mod[-indRm, ]
}
#----------------------------------------------------------------------------
mod2Vars <- c("dist", "yield (kg/acre)", "Cstar", "acres",
              #input_vars[7],
              bin_vars[-c(2:3)], demog_vars[2], clim_vars[c(2, 3)])
df_mod2 <- df_mod[, mod2Vars]
mod1Vars <- c("dist", "yield (kg/acre)", "acres",
              input_vars[-c(1, 2)], bin_vars[-c(2, 3)],
              demog_vars[-1])
df_mod1 <- df_mod[, mod1Vars]
#----------------------------------------------------------------------------
not_these <- which(colnames(df_mod2) %in% c("dist"))
mod2 <- lm(`yield (kg/acre)`~.-1, df_mod2[, -not_these])
#mod2 <- lm(Cstar ~., df_mod2[, -not_these])
summary(mod2)
sum(mod2$residuals^2)
plot(mod2$fitted.values, mod2$residuals)
not_these <- which(colnames(df_mod1) %in% c("dist", "lpcost"))
mod1 <- lm(`yield (kg/acre)`~., df_mod1[, -not_these])
summary(mod1)
sum(mod1$residuals^2)
plot(mod1$fitted.values, mod1$residuals)
#car::vif(mod1)
#intersect(mod_vars, colnames(df))
#----------------------------------------------------------------------------
# Any NAs omitted? Where are they?
count_missing <- function(x){n_na <- length(which(is.na(x))); return(n_na)}
apply(df_mod2, 2, count_missing)
apply(df_mod1, 2, count_missing)
#===========================================================================
a0 <- mod1$coefficients[1]
y0 <- exp(a0)
theseCols <- gsub("`", "", names(mod1$coefficients))
aVec <- mod1$coefficients[which(theseCols %in% input_vars)]
# tfpVec <- mod1$coefficients[which(!(theseCols %in% input_vars))]
# kVec <- tfpVec[which(names(tfpVec) %in% c("acres", "aehh07"))]
# tfpVec <- tfpVec[which(!(names(tfpVec) %in% c("acres", "aehh07")))]
# names(tfpVec) <- gsub("`", "", names(tfpVec))
# yBasal <- exp(sum(tfpVec))
kVec <- mod1$coefficients[-which(theseCols %in% input_vars)]
kVec <- kVec[-1]

inputMat <- as.matrix(df_mod1[, input_vars[input_vars %in% colnames(df_mod1)]])
inputMat[inputMat != 0] <- 1
h <-  inputMat %*% aVec
#---
# Optimal farm size
Astar <- (1 + kVec["acres"]) / h
hist(Astar)
hist(log(exp(df_mod1$acres) - Astar))
hist(df_mod1$acres - log(Astar))
hist(df_mod1$acres)
#(1 + mod2$coefficients["acres"]) / mod2$coefficients["Cstar"]
#---
lwMat <- as.matrix((df_mod[, c("wFert", "wOrgFert", "wPD", "wSeed")]))
betaW <- exp((lwMat * inputMat) %*% aVec)
alogaVec <- aVec * log(aVec)
betaA <- exp(inputMat %*% alogaVec)
#betaA <- exp(sum(aVec * log(aVec)))
names(kVec) <- gsub("`", "", names(kVec))
lkMat <- as.matrix(df_mod2[, names(kVec)])
betaK <- exp(lkMat %*% kVec)
beta <- betaA * betaK / betaW
Cstar <- exp(df_mod$Cstar)
CtildeStar <- exp(df_mod$CtildStar)
EyStar <- y0 * beta * (Cstar / h)^h
#EyStar <- (y0 * beta * (CtildeStar / h)^h)^(1 / (1-h))
yStar <- exp(df_mod$`yield (kg/acre)`)
P <- exp(df_mod$P)
ERstar <- EyStar * P
Rstar <- yStar * P
hist(log(ERstar / Cstar))
hist(log(Rstar / Cstar * 1 / lambdaCstarP))
hist(log(EyStar / yStar))
lambdaCstarP <- y0 * beta * P * (Cstar / h)^(h - 1)
#lambdaRoCstar <- P / CtildeStar * h
CtildeStar2 <- Cstar / EyStar
CtildeStar2 / h * lambdaCstarP / P

hist((lambdaCstarP))
length(lambdaCstarP[lambdaCstarP < h]) / length(lambdaCstarP)
hist(log(lambdaRoCstar))
length(lambdaRoCstar[lambdaRoCstar < h]) / length(lambdaRoCstar)

hist(betaK)
hist(beta)
hist(log(EyStar), breaks = 20)
hist(mod2$fitted.values)
hist(mod1$fitted.values)
hist(df_mod$`yield (kg/acre)`)
hist(log(EyStar) - df_mod$`yield (kg/acre)`)
# lambdaExPost <- exp(df_mod$`yield (kg/acre)` + df_mod$P - df_mod$Cstar) * h
# hist(log(lambdaExPost))
# How many irrational?
length(lambdaCstarP[lambdaCstarP < h]) / length(lambdaCstarP)
df_mod$lambdaCstarP <- lambdaCstarP
df_mod$betaK <- betaK
df_mod$beta <- beta
df_rational <- subset(df_mod, lambdaCstarP > h)
df_irrational <- subset(df_mod, lambdaCstarP < h)
#---
hist(df_irrational$acres)
hist(df_rational$acres)
hist(df_rational$`yield (kg/acre)`)
hist(df_irrational$`yield (kg/acre)`)
hist(df_irrational$`Total synth fert (kg/acre)`)
hist(df_rational$`Total synth fert (kg/acre)`)
hist(df_irrational$`Total organic fert (kg/acre)`)
hist(df_rational$`Total organic fert (kg/acre)`)
hist(df_irrational$`seed (kg/acre)`)
hist(df_rational$`seed (kg/acre)`)
hist(df_irrational$beta)
hist(df_rational$beta)
hist(beta)
hist(df_irrational$aehh07)
hist(df_rational$aehh07)
hist(df_irrational$Cstar)
hist(df_rational$Cstar)
hist(df_irrational$gend)
hist(df_rational$gend)
hist(df_irrational$irrigated)
hist(df_rational$irrigated)
hist(df_irrational$age)
hist(df_rational$age)
hist(df_irrational$`Child family labor (man hours)`)
hist(df_rational$`Child family labor (man hours)`)








# These are similarly peaked around 5-6
ratio <- (wFert * xFert * aSeed) / (wSeed * xSeed * aFert)
ratio <- (wFert * xFert * aPD) / (wPD * xPD * aFert)
ratio <- (wOrgFert * xOrgFert * aFert) / (wFert * xFert * aOrgFert)
hist(log(ratio))
# Via tech share = cost share
ratio <- (aFert / h) / (wFert * xFert / Cstar)
ratio <- (aPD / h) / (wPD * xPD / Cstar)
ratio <- (aSeed / h) / (wSeed * xSeed / Cstar)
hist(log(ratio))
# Do expenditure shares change with total expenditure?
cFert <- wFert * xFert
cOrgFert <- wOrgFert * xOrgFert
cSeed <- wSeed * xSeed
cPD <- wPD * xPD
cMat <- cbind(cFert, cOrgFert, cSeed, cLprep, cPD)
cShareMat <- cMat / Cstar
cShareMat <- cbind(cShareMat, Cstar)
cMat <- cbind(cMat, Cstar)
xMat <- cbind(xFert, xOrgFert, xSeed, xPD)
xMat <- cbind(xMat, Cstar)
df_plot <- as.data.frame(cMat)
df_plot$adopter <- df_look$adopter
df_plot$dist <- df_look$dist
#df_plot <- df_plot %>% gather(Type, Share, xFert:xPD)
df_plot <- df_plot %>% gather(Type, Share, cFert:cPD)
df_plot[, -c(2, 3, 4)] <- log(df_plot[, -c(2, 3, 4)])
df_plot <- subset(df_plot, adopter == 0)
rmInf <- which(is.infinite(df_plot$Share))
if(length(rmInf) != 0){
  df_plot <- df_plot[-rmInf, ]
}
df_plot <- subset(df_plot, dist %in% theseDist)
typeVec <- unique(df_plot$Type)
yInt <- c()
slope <- c()
for(i in 1:length(typeVec)){
  thisType <- typeVec[i]
  mod <- lm(Cstar ~ Share, subset(df_plot, Type == thisType))
  print(thisType)
  print(summary(mod))
  yInt[i] <- coefficients(mod)[1]
  slope[i] <- coefficients(mod)[2]
}
names(slope) <- typeVec
gg <- ggplot(df_plot, aes(x = Cstar, y = Share))
gg <- gg + geom_point()
gg <- gg + geom_smooth(method = "lm")
gg <- gg + facet_wrap(~Type)#, scales = "free_y")
#gg <- gg + facet_grid(Type~dist)#, scales = "free_y")
gg
#-------------
# Can work out what farmer expected yields were at time of planting
EyStar <- (lambdaFert * Cstar / (h * P))
EyStar <- (lambdaY * Cstar / (h * P))
hist(log(EyStar))
#===========================================================================
#===========================================================================
#===========================================================================
# PCA
#====================================================
# Define PC graph function
# Barchart of variable-signal correlations
plot_corrXS_barchart <- function(mat_L, group_info = NULL, xAxis_title = NULL, sigNames = NULL){
  
  n_signals <- ncol(mat_L)
  df_plot <- data.frame(Item = row.names(mat_L), mat_L)
  df_plot$Item <- as.character(df_plot$Item)
  #-------------------------------------------------------
  if(is.null(sigNames)){
    signal_id <- paste("Signal", 1:n_signals)
  }else{
    #signal_id <- paste("Signal", 1:n_signals, "\n", sigNames)
    signal_id <- sigNames
  }
  colnames(df_plot)[2:(n_signals + 1)] <- signal_id
  #-------------------------------------------------------
  gathercols <- as.character(signal_id) 
  df_plot <- gather_(df_plot, "Signal", "Correlation", gathercols)
  df_plot <- transform(df_plot,
                       Signal = factor(Signal, levels = gathercols))
  
  if(!is.null(group_info)){
    outlist <- group_fn(group_info)
    cols_ordered_by_group <- outlist[[1]]
    group_color_vec <- outlist[[2]]
    group_vec_ordered <- outlist[[3]]
    df_match_group <- data.frame(Item = cols_ordered_by_group, Group = group_vec_ordered)
    df_plot <- merge(df_plot, df_match_group, by = "Item")
    df_plot <- df_plot[order(df_plot$Group), ]
    df_plot$Item <- factor(df_plot$Item, levels = unique(df_plot$Item))
    gg <- ggplot(df_plot, aes(x = Item, y = Correlation, fill = Group))
    gg <- gg + scale_fill_manual(values = unique(group_color_vec))
  }else{
    df_plot$Item <- factor(df_plot$Item,
                           levels = rev(unique(df_plot$Item)))
    gg <- ggplot(df_plot, aes(x = Item, y = Correlation))
  }
  gg <- gg + geom_bar(stat = "identity", color = "black", position = "dodge")
  gg <- gg + ylim(limits = c(-1, 1))
  gg <- gg + facet_wrap(~ Signal, nrow = 1)
  if(!is.null(xAxis_title)){
    gg <- gg + labs(y = xAxis_title)
  }
  gg <- gg + theme(axis.text = element_text(size = 7),
                   axis.title.x = element_text(size = 7),
                   axis.title.y = element_blank(),
                   legend.title = element_blank(),
                   legend.text = element_text(size = 7),
                   strip.text = element_text(size = 7))
  gg <- gg + coord_equal()
  gg <- gg + coord_flip()
  gg
  
}
#====================================================
# Do the PCA
dfPCA <- df_mod[, -1]
dfPCA <- dfPCA[-which(is.na(rowSums(dfPCA))), ]
matDat <- as.matrix(dfPCA)
matCcor <- cor(matDat, matDat)
image(matCcor)
eVals <- eigen(matCcor)$values
ind_cutOff <- which(round(cumsum(eVals / sum(eVals)), 2) >= 0.8)[1]
eVals <- eVals[1:ind_cutOff]
eVecs <- eigen(matCcor)$vectors[, 1:ind_cutOff]
L <- eVecs %*% diag(sqrt(eVals))
Lrot <- varimax(L)$loadings
#Lrot <- varimax(L)[[1]]
Lrot <- matrix(as.numeric(Lrot),
               attributes(Lrot)$dim,
               dimnames = attributes(Lrot)$dimnames)
# mat_R <- varimax(L)[[2]]
# mat_R <- matrix(as.numeric(mat_R),
#                 attributes(mat_R)$dim,
#                 dimnames = attributes(mat_R)$dimnames)
row.names(Lrot) <- colnames(dfPCA)
row.names(L) <- colnames(dfPCA)
xAxis_title <- "Varimax Rotated Correlation"
plot_corrXS_barchart(Lrot, group_info = NULL, xAxis_title, sigNames = NULL)
