library(tidyverse)
library(foreign)
#---------------------------------------------------------------------------
de_factorizer <- function(this_df){
  i <- sapply(this_df, is.factor)
  this_df[i] <- lapply(this_df[i], as.character)
  return(this_df)
}
#---------------------------------------------------------------------------
this_folder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/New PE Model/Tegemeo Data/data needed for 2007/"
#this_folder <- "C:/Users/bensc/OneDrive/Documents/Data/Tegemeo Data/data needed for 2007/"
list.files(this_folder)
#---------------------------------------------------------------------------
this_filepath <- paste0(this_folder, "croplev07.dta")
df_crop <- read.dta(this_filepath)
df_crop <- de_factorizer(df_crop)
these_crops <- c("maize-dry", "maize-green", "beans")
df_crop <- subset(df_crop, crop %in% these_crops &
                    harvest == "main")
ind_green <- which(df_crop$crop == "maize-green")
df_crop$kgharv[ind_green] <- df_crop$kgharv[ind_green] * 0.85
df_crop$crop[grep("maize", df_crop$crop)] <- "maize"

#df_sdtype <- df_crop[, c("hhid", "field", "crop", "sdtype")]

df_crop$adopter <- 0
ind_purch_seed <- which(df_crop$sdtype == "purchased new hybrid" |
                          df_crop$sdtype == "purchased hybrid & retained")
df_crop$adopter[ind_purch_seed] <- 1

zone_id <- c("aez", "aezsmall", "zone", "prov", "dist", "div", "vil")
keep_vars <- c("kgharv", "kgsold", "seedkg", "scost", "adopter")
keep_cols <- c(zone_id, "hhid", "field", "crop", keep_vars)
df_crop <- df_crop[, keep_cols] %>% 
  group_by(aez, aezsmall, zone, prov, dist, div, vil, hhid, field, crop) %>%
  summarise(kgharv = sum(kgharv, na.rm = T),
            kgsold = sum(kgsold, na.rm = T),
            seedkg = sum(seedkg, na.rm = T),
            scost = sum(scost, na.rm = T),
            adopter = mean(adopter, na.rm = T))
#---------------------------------------------------------------------------
# Define adopters
# length(df_crop$adopter[which(df_crop$adopter == 0.5)])
# length(df_crop$adopter[which(df_crop$adopter == 1)])
# length(df_crop$adopter[which(df_crop$adopter == 0)])
# length(df_crop$adopter[which(df_crop$adopter > 0)])
df_crop$adopter[which(df_crop$adopter > 0)] <- 1
#---------------------------------------------------------------------------
this_filepath <- paste0(this_folder, "field07.dta")
df_field <- read.dta(this_filepath)
df_field <- de_factorizer(df_field)
df_field <- subset(df_field, harvest == "main")
df_field <- df_field[, c("hhid", "field", "acres", "tenure", "watersrc", "landprep", "lpcost")]
#---
ggplot(df_field, aes(x=watersrc))+geom_histogram(stat="count")+coord_flip()
ggplot(df_field, aes(x=tenure))+geom_histogram(stat="count")+coord_flip()
ggplot(df_field, aes(x=landprep))+geom_histogram(stat="count")+coord_flip()
#---
df_field$irrigated <- 0
df_field$irrigated[grep("irrig", df_field$watersrc)] <- 1
df_field$watersrc <- NULL
df_field <- fastDummies::dummy_cols(df_field)
df_field$landprep <- NULL
df_field$tenure <- NULL
# df_field$`watersrc_can/bucket irrigation` <- NULL
# df_field$`watersrc_irrigated (gravity)` <- NULL
# df_field$`watersrc_irrigated (piped)` <- NULL
# Get total acres for later merge
df_acresTot <- df_field[, c("hhid", "field", "acres")]
df_acresTot <- df_acresTot %>% group_by(hhid) %>% summarise(acres_tot = sum(acres, na.rm = T))
# Merge production and acres for later calculation of yield
df_crop <- merge(df_crop, df_field, by = c("hhid", "field"))
#---------------------------------------------------------------------------
# Before calculating yield, have to aggregate data up from field to crop.
# Before doing that, have to merge with fertilizer data because it is
# also field level.
#---------------------------------------------------------------------------
# Fertilizer
this_filepath <- paste0(this_folder, "fert07.dta")
df_fert <- read.dta(this_filepath)
df_fert <- de_factorizer(df_fert)
keep_cols <- c("hhid", "field", "harvest", "ferttype", "fertotal", "fertcost", "pfert")
df_fert <- subset(df_fert[, keep_cols], harvest == "main")
#---
# unique(df_fert$ferttype)
# ggplot(df_fert, aes(x = ferttype))+geom_histogram(stat = "count")+coord_flip()
#---
df_fert <- subset(df_fert, ferttype != "NPK + CAN")
ind_npk <- grep("NPK", df_fert$ferttype)
df_fert$ferttype[ind_npk] <- "npk"
ind_other <- which(df_fert$ferttype %in% c("mavuno-basal", "foliar feed", "compost"))
df_fert$ferttype[ind_other] <- "other organic"
df_fert <- df_fert %>% group_by(hhid, field, ferttype) %>%
  summarise(fertotal = sum(fertotal, na.rm = T),
            fertcost = sum(fertcost, na.rm = T),
            pfert = mean(pfert, na.rm = T))
ferts_keep <- c("dap", "npk", "CAN (26:0:0)", "UREA (46:0:0)",
                "other organic", "manure")
df_fert <- subset(df_fert, ferttype %in% ferts_keep)
#---
ggplot(df_fert, aes(x=pfert))+geom_histogram()+coord_flip()+facet_wrap(~ferttype, scales = "free")
ggplot(df_fert, aes(x = ferttype))+geom_histogram(stat = "count")+coord_flip()
#---
#df_fertp <- df_fert[, c("hhid", "field", "ferttype", "pfert")] %>% spread(ferttype, pfert)
df_fert$ferttype[grep("UREA", df_fert$ferttype)] <- "urea"
df_fert$ferttype[grep("CAN", df_fert$ferttype)] <- "can"
#---
df_fertCost <- df_fert[, c("hhid", "field", "ferttype", "fertcost")] %>% spread(ferttype, fertcost)
df_fertCost[is.na(df_fertCost)] <- 0
df_fertCost$`Total synth. fert. (KES)` <- df_fertCost$can +
  df_fertCost$dap + df_fertCost$npk + df_fertCost$urea
df_fertCost$`Total organic fert. (KES)` <- df_fertCost$manure +df_fertCost$`other organic`
colnames(df_fertCost)[3:8] <- paste(colnames(df_fertCost)[3:8], "(KES)")
#---
df_fert <- df_fert[, c("hhid", "field", "ferttype", "fertotal")] %>% spread(ferttype, fertotal)
df_fert[is.na(df_fert)] <- 0
df_fert$`Total synth. fert. (kg)` <- df_fert$can +
  df_fert$dap + df_fert$npk + df_fert$urea
df_fert$`Total organic fert. (kg)` <- df_fert$manure +df_fert$`other organic`
colnames(df_fert)[3:8] <- paste(colnames(df_fert)[3:8], "(kg)")
#---
df_fert <- merge(df_fert, df_fertCost, by = c("hhid", "field"))
#---------------------------------------------------------------------------
# Merge fert data with production, area data
df_yield <- merge(df_crop, df_fert, by = c("hhid", "field"))
# Now can aggregate up from field to crop
# When aggregating, have to separate binary vars from continuous
# Continuous vars are summed, while bin vars are averaged.
bin_vars <- c("adopter", "irrigated", "tenure_govt /communal /cooperative",
              "tenure_owned by parent /relative",
              "tenure_owned with title deed",
              "tenure_owned without title deed",
              "tenure_rented", "landprep_manual",
              "landprep_none", "landprep_oxen",
              "landprep_tractor")
keep_cols <- c("hhid", "crop", bin_vars)
df_yield_bin <- df_yield[, keep_cols]
df_yield <- df_yield[, setdiff(colnames(df_yield), bin_vars)]
df_yield$field = NULL
df_yield <- df_yield %>% group_by(aez, aezsmall,
                                  zone, prov, dist,
                                  div, vil, hhid, crop) %>%
  summarise_all(sum, na.rm = T)

df_yield_bin <- df_yield_bin %>% group_by(hhid, crop) %>%
  summarise_all(mean, na.rm = T)
#---
# Define fn to set dumvars to 1 for whole farm if dumvars =1 on enough fields
# ("enough" as determined by thresh)
dumfun <- function(this_col){
  thresh <- 0.15 #15% of fields with this crop (maize probably)
  this_col[which(this_col >= thresh)] <- 1
  this_col[which(this_col < thresh)] <- 0
  return(this_col)
}
#---
df_yield_bin[, bin_vars] <- as.data.frame(apply(df_yield_bin[, bin_vars], 2, dumfun))
# Merge binary with continuous vars
df_yield <- merge(df_yield, df_yield_bin, by = c("hhid", "crop"))
# Merge with total acres
df_yield <- merge(df_yield, df_acresTot, by = "hhid")
# Calculate acres as pct of total
df_yield$acres_pct <- df_yield$acres / df_yield$acres_tot
# Calculate yield
df_yield$`yield (kg/acre)` <- df_yield$kgharv / df_yield$acres
# hist(df_yield$yield)
# hist(df_yield$acres)
# Get fert per acre
fertcols <- colnames(df_fert)[3:ncol(df_fert)]
df_yield[, fertcols] <- df_yield[, fertcols] / df_yield$acres
colnames(df_yield)[which(colnames(df_yield) %in% fertcols)] <- gsub("\\)", "/acre\\)", colnames(df_yield[, fertcols]))
#---------------------------------------------------------------------------
# Get seed price
df_seedPrice <- df_yield[, c("hhid", "dist", "crop", "adopter", "seedkg", "scost")]
df_seedPrice$`seed price (KES/kg)` <- df_seedPrice$scost / df_seedPrice$seedkg
df_seedPrice$crop[which(df_seedPrice$adopter == 1 & df_seedPrice$crop == "maize")] <- "hybrid maize"
df_seedPrice <- df_seedPrice %>% group_by(dist, crop) %>%
  summarise(`seed price (KES/kg)` = mean(`seed price (KES/kg)`, na.rm = T))
#---------------------------------------------------------------------------
# Get labor and wage rate
this_filepath <- paste0(this_folder, "labour07.dta")
df_labor <- read.dta(this_filepath)
df_labor <- de_factorizer(df_labor)
#lb01 = number wage workers hired
#lb02 = number days worked by each wage worker
#lb03 = day wage
#lb05 = number male family workers
#lb06 = hours worked by each male family worker
#lb07 = number female family workers
#lb08 = hours worked by each female family worker
#lb09 = number children family workers
#lb10 = hours worked by each child family worker
df_labor$lab_wg <- df_labor$lb01 * df_labor$lb02
df_labor$lab_wg <- df_labor$lab_wg * 8 # Convert wage labor days to hours
df_labor$lab_fam_male <- df_labor$lb05 * df_labor$lb06
df_labor$lab_fam_fem <- df_labor$lb07 * df_labor$lb08
df_labor$lab_fam_ch <- df_labor$lb09 * df_labor$lb10
df_labor$lab_fam_adult <- df_labor$lab_fam_fem + df_labor$lab_fam_male
df_labor$lab_fam_all <- df_labor$lab_fam_adult + df_labor$lab_fam_ch
colnames(df_labor)[which(colnames(df_labor) == "lb03")] <- "Wage (KES/day)"
df_labor$`Wage (KES/hour)` <- df_labor$`Wage (KES/day)` / 8
keep_cols <- c("dist", "hhid", "activity", "Wage (KES/hour)", "lab_wg", "lab_fam_male",
               "lab_fam_fem", "lab_fam_ch", "lab_fam_adult", "lab_fam_all")
df_labor <- df_labor[, keep_cols]
#---
# Which labor type is predominant?
df_look <- df_labor[, keep_cols[-c(2:4)]]
df_look <- df_look %>% group_by(dist) %>% summarise_all(sum, na.rm = T)
df_look <- df_look %>% gather(type, personHours, lab_wg:lab_fam_ch) %>% as.data.frame()
df_look <- df_look %>% group_by(dist) %>%
  mutate(personHrsPct = 100 * personHours / sum(personHours)) %>% as.data.frame()
df_look <- df_look %>% gather(barType, value, personHours:personHrsPct) %>% as.data.frame()
#gg <- ggplot(df_look, aes(x=dist,y=personHrsPct, fill = type))
gg <- ggplot(df_look, aes(x=dist,y=value, fill = type))
gg <- gg + geom_bar(position = "stack", stat = "identity") + coord_flip()
gg <- gg + facet_wrap(~barType, scales = "free_x")
gg
#---
keep_cols <- c("hhid", "dist", "activity", "lab_fam_fem")
df_labFemQty <- df_labor[, keep_cols] %>% spread(activity, lab_fam_fem)
df_labFemQty$Total <- rowSums(df_labFemQty[, -c(1, 2)], na.rm = T)
colnames(df_labFemQty)[-c(1, 2)] <- paste(colnames(df_labFemQty)[-c(1, 2)], "fam lab fem (pers-hrs)")
keep_cols <- c("hhid", "dist", "activity", "lab_fam_male")
df_labMalQty <- df_labor[, keep_cols] %>% spread(activity, lab_fam_male)
df_labMalQty$Total <- rowSums(df_labMalQty[, -c(1, 2)], na.rm = T)
colnames(df_labMalQty)[-c(1, 2)] <- paste(colnames(df_labMalQty)[-c(1, 2)], "fam lab male (pers-hrs)")
keep_cols <- c("hhid", "dist", "activity", "lab_fam_adult")
df_labAdQty <- df_labor[, keep_cols] %>% spread(activity, lab_fam_adult)
df_labAdQty$Total <- rowSums(df_labAdQty[, -c(1, 2)], na.rm = T)
colnames(df_labAdQty)[-c(1, 2)] <- paste(colnames(df_labAdQty)[-c(1, 2)], "fam lab adult (pers-hrs)")
keep_cols <- c("hhid", "dist", "activity", "lab_fam_ch")
df_labChQty <- df_labor[, keep_cols] %>% spread(activity, lab_fam_ch)
df_labChQty$Total <- rowSums(df_labChQty[, -c(1, 2)], na.rm = T)
colnames(df_labChQty)[-c(1, 2)] <- paste(colnames(df_labChQty)[-c(1, 2)], "fam lab child (pers-hrs)")
keep_cols <- c("hhid", "dist", "activity", "lab_fam_all")
df_labFamQty <- df_labor[, keep_cols] %>% spread(activity, lab_fam_all)
df_labFamQty$Total <- rowSums(df_labFamQty[, -c(1, 2)], na.rm = T)
colnames(df_labFamQty)[-c(1, 2)] <- paste(colnames(df_labFamQty)[-c(1, 2)], "fam lab (pers-hrs)")
keep_cols <- c("hhid", "dist", "activity", "lab_wg")
df_labWgQty <- df_labor[, keep_cols] %>% spread(activity, lab_wg)
df_labWgQty$Total <- rowSums(df_labWgQty[, -c(1, 2)], na.rm = T)
colnames(df_labWgQty)[-c(1, 2)] <- paste(colnames(df_labWgQty)[-c(1, 2)], "wage labor (pers-hrs)")
list_df <- list(df_labAdQty, df_labChQty,
                df_labFemQty, df_labMalQty,
                df_labFamQty, df_labWgQty)
df_labQty <- plyr::join_all(list_df)
df_labQty[is.na(df_labQty)] <- 0
#---
# Get labor cost for wage labor
df_labor$wgLabCost <- df_labor$lab_wg * df_labor$`Wage (KES/hour)`
df_labWgCost <- df_labor[, keep_cols] %>% spread(activity, lab_wg)
df_labWgCost[is.na(df_labWgCost)] <- 0
df_labWgCost$`Total wage labor cost (KES)` <- rowSums(df_labWgCost[, -c(1:2)])
#---
# Get wage for each labor activity averaged over hh in a given district
keep_cols <- c("dist", "activity", "Wage (KES/hour)")
df_laborPrice <- df_labor[, keep_cols]
df_laborPrice <- df_laborPrice %>% group_by(dist, activity) %>% summarise(meanWageDist = mean(`Wage (KES/hour)`, na.rm = T))
df_laborPrice <- df_laborPrice[-which(is.nan(df_laborPrice$meanWageDist)), ]
#---
ggplot(df_laborPrice,aes(x=dist,y=meanWageDist))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~activity)
#---
# # Implied labor cost for family labor (experimental)
# famLabType <- c("lab_fam_fem", "lab_fam_male",
#                 "lab_fam_ch", "lab_fam_adult", "lab_fam_all")
# for(i in 1:length(famLabType)){
#   thisFamLabType <- famLabType[i]
#   keep_cols <- c("hhid", "dist", "activity", thisFamLabType)
#   df_famLabCost <- merge(df_labor[, keep_cols], df_laborPrice, by = c("dist", "activity"))
#   df_famLabCost$approxImpliedCost <- df_famLabCost[, thisFamLabType] * df_labor$meanWageDist
#   keep_cols <- c("hhid", "dist", "activity", "approxImpliedCost")
#   df_labFemCost <- df_labFemCost[, keep_cols] %>% spread(activity, approxImpliedLabCostFem)
#   
# }
df_laborPrice <- df_laborPrice %>% group_by(dist) %>%
  summarise(meanWageDist = mean(meanWageDist, na.rm = T)) %>%
  as.data.frame()
#-------------------------------------------------------------------------
# # Set apart labor wage
# # df_laborPrice <- df_labor[, c("dist", "hhid", "activity", "lb03")]
# # colnames(df_laborPrice)[4] <- "wage (KES/day)"
# # main_activities <- c("1st weeding", "harvesting", "planting",
# #                      "1st ploughing", "2nd weeding", "shelling")
# # df_laborPrice <- subset(df_laborPrice, activity %in% main_activities)
# this_filepath <- paste0(this_folder, "hh07.dta")
# df_laborPrice2 <- read.dta(this_filepath)
# df_laborPrice2 <- de_factorizer(df_laborPrice2)
# df_laborPrice2 <- df_laborPrice2[, c("hhid", "dist", "wagerate")]
# #hist(df_laborPrice2$wagerate)
# df_laborPrice <- merge(df_laborPrice, df_laborPrice2, by = c("hhid", "dist"))
# df_laborPrice <- df_laborPrice %>% group_by(dist) %>%
#   summarise_all(mean, na.rm = T)
# # At first I thought it's to go with the wage rate reported in the hh07.dta file because the other one
# # (from labor07.dta) only reports wage for activities for which wage labor was
# # hired, and so averaging over activities may be skewed (b/c in some areas 
# # no wage labor hired for main activities like harvesting--it's all family labor).
# # However, now I think it's better to go with the wage calculated directly from the labor07.dta.
# # I need the wage directly deduced from costs actually incurred by the farm
# df_laborPrice$`wage (KES/day)` <- NULL
# df_laborPrice$activity <- NULL
# df_laborPrice$hhid <- NULL
# colnames(df_laborPrice)[2] <- "wage (KES/day)"
# ggplot(df_laborPrice, aes(x=dist,y=`wage (KES/day)`))+geom_bar(stat="identity")+coord_flip()
# df_laborPrice$`wage (KES/hour)` <- df_laborPrice$`wage (KES/day)` / 8
#---------------------------------------------------------------------------
# Which labor activities are predominant?
keep_cols <- c("hhid", "activity", "lab_wg", "lab_fam_male",
               "lab_fam_fem", "lab_fam_ch")
df_labor <- df_labor[, keep_cols]
#---
df_look_wg <- df_labor[, c("hhid", "activity", "lab_wg")] %>%
  spread(activity, lab_wg) 
df_look_wg$hhid <- NULL
gathercols <- colnames(df_look_wg)
df_look_wg <- df_look_wg %>% summarise_all(sum, na.rm = T) %>%
  gather_("activity", "qty", gathercols)
df_look_wg$labtype <- "lab_wg"
#--
df_look_fam_male <- df_labor[, c("hhid", "activity", "lab_fam_male")] %>%
  spread(activity, lab_fam_male) 
df_look_fam_male$hhid <- NULL
gathercols <- colnames(df_look_fam_male)
df_look_fam_male <- df_look_fam_male %>% summarise_all(sum, na.rm = T) %>%
  gather_("activity", "qty", gathercols)
df_look_fam_male$labtype <- "lab_fam_male"
#--
df_look_fam_fem <- df_labor[, c("hhid", "activity", "lab_fam_fem")] %>%
  spread(activity, lab_fam_fem) 
df_look_fam_fem$hhid <- NULL
gathercols <- colnames(df_look_fam_fem)
df_look_fam_fem <- df_look_fam_fem %>% summarise_all(sum, na.rm = T) %>%
  gather_("activity", "qty", gathercols)
df_look_fam_fem$labtype <- "lab_fam_fem"
#--
df_look_fam_ch <- df_labor[, c("hhid", "activity", "lab_fam_ch")] %>%
  spread(activity, lab_fam_ch) 
df_look_fam_ch$hhid <- NULL
gathercols <- colnames(df_look_fam_ch)
df_look_fam_ch <- df_look_fam_ch %>% summarise_all(sum, na.rm = T) %>%
  gather_("activity", "qty", gathercols)
df_look_fam_ch$labtype <- "lab_fam_ch"
#---
df_look <- as.data.frame(do.call(rbind, list(df_look_wg,
                                             df_look_fam_male,
                                             df_look_fam_fem,
                                             df_look_fam_ch)))
#---
gg <- ggplot(df_look, aes(x = activity, y = qty))
gg <- gg + geom_bar(stat = "identity") + coord_flip()
gg <- gg + facet_wrap(~labtype, scales = "free_x")
gg
# Main activities are:
#1st weeding, harvesting, planting, 1st ploughing, 2nd weeding,
#shelling, stooking
#---
# df_labor$activity <- NULL
# df_labor <- df_labor %>% group_by(hhid) %>%
#   summarise_all(sum, na.rm = T)
# df_labor$`Adult family labor (man hours)` <- df_labor$lab_fam_male + df_labor$lab_fam_fem
# colnames(df_labor)[c(2:5)] <- c("Wage labor (man days)",
#                                 "Male adult family labor (man hours)",
#                                 "Female adult family labor (man hours)",
#                                 "Child family labor (man hours)")
# rm(df_look, df_look_fam_ch, df_look_fam_fem, df_look_fam_male, df_look_wg, df_labor2)
#---------------------------------------------------------------------------
# Get pest/plague control inputs and distace to input markets
this_filepath <- paste0(this_folder, "input07.dta")
df_pest <- read.dta(this_filepath)
df_pest <- de_factorizer(df_pest)
df_pest <- subset(df_pest, mcrop %in% c("maize-dry", "maize-green", "beans"))
df_pest$mcrop[grep("maize", df_pest$mcrop)] <- "maize"
# Get distance to input market for pest/plague control and fert
df_km <- df_pest
df_km <- subset(df_km, inputype != "NPK + CAN")
ind_npk <- grep("NPK", df_km$inputype)
df_km$inputype[ind_npk] <- "npk"
ind_other <- which(df_km$inputype %in% c("mavuno-basal", "foliar feed", "compost"))
df_km$inputype[ind_other] <- "other organic"
df_km <- df_km %>% group_by(hhid, inputype, mcrop) %>%
  summarise(kms = mean(kms, na.rm = T))
df_km$inputype[grep("Manure", df_km$inputype)] <- "manure"
organic_ferts <- c("other organic", "manure")
synth_ferts <- c("dap", "can", "urea", "npk")
ferts_keep <- c(synth_ferts, organic_ferts)
pest_keep <- c("Herbicide", "Pesticide", "Insecticide", "Fungicide")
inputs_keep <- c(ferts_keep, pest_keep)
df_km <- subset(df_km, inputype %in% inputs_keep)
#---
df_look <- subset(df_km, mcrop == "maize")
ggplot(df_look, aes(x=kms))+geom_histogram()+coord_flip()+facet_wrap(~inputype, ncol=2, scales = "free")
#---
# df_km <- df_km %>% group_by(hhid, mcrop) %>%
#   summarise(kms = mean(kms, na.rm = T))
df_km$inputype[which(df_km$inputype %in% organic_ferts)] <- "organic fert"
df_km$inputype[which(df_km$inputype %in% synth_ferts)] <- "synth fert"
df_km$inputype[which(df_km$inputype %in% pest_keep)] <- "P&D chems"
df_km <- df_km %>% group_by(hhid, mcrop, inputype) %>%
  summarise(kms = mean(kms, na.rm = T))
#---
df_look <- subset(df_km, mcrop == "maize")
ggplot(df_look, aes(x=kms))+geom_histogram()+coord_flip()+facet_wrap(~inputype, ncol=2, scales = "free")
#---
# ggplot(df_km, aes(x =kms))+geom_histogram()+coord_flip()+facet_wrap(~mcrop, ncol=2, scales = "free")
#---
#colnames(df_km)[2:3] <- c("crop", "km to chem/fert mkts")
df_km <- df_km %>% spread(inputype, kms)
colnames(df_km)[2:5] <- c("crop", "km to organic fert mkt",
                          "km to pest/plague chem mkt",
                          "km to synth fert mkt")
df_km[is.na(df_km)] <- 0
#----------------------------------------------------------------------------
df_pest <- subset(df_pest, inputype %in% pest_keep)
unique(df_pest$punit)
# Convert all units to kg
df_pest$kgpur <- df_pest$numpur
df_pest$kgpur[which(df_pest$punit == "gram")] <- df_pest$kgpur[which(df_pest$punit == "gram")] / 100
df_pest$kgpur[which(df_pest$punit == "gorogoro")] <- df_pest$kgpur[which(df_pest$punit == "gorogoro")] * 2
df_pest$kgpur[which(df_pest$punit == "90 kg bag")] <- df_pest$kgpur[which(df_pest$punit == "90 kg bag")] * 90
df_pest$kgpur[which(df_pest$punit == "litre")] <- df_pest$kgpur[which(df_pest$punit == "litre")]
df_pest$kgpur[which(df_pest$punit == "kg")] <- df_pest$kgpur[which(df_pest$punit == "kg")]

df_pest$inputprkg <- df_pest$inputpr
df_pest$inputprkg[which(df_pest$punit == "gram")] <- df_pest$inputprkg[which(df_pest$punit == "gram")] / 100
df_pest$inputprkg[which(df_pest$punit == "gorogoro")] <- df_pest$inputprkg[which(df_pest$punit == "gorogoro")] * 2
df_pest$inputprkg[which(df_pest$punit == "90 kg bag")] <- df_pest$inputprkg[which(df_pest$punit == "90 kg bag")] * 90
df_pest$inputprkg[which(df_pest$punit == "litre")] <- df_pest$inputprkg[which(df_pest$punit == "litre")]
df_pest$inputprkg[which(df_pest$punit == "kg")] <- df_pest$inputprkg[which(df_pest$punit == "kg")]
#---
ind_error <- which(df_pest$inputprkg == 76500)
#df_pest$inputprkg[ind_error] <- 7650 #correcting error
df_pest <- df_pest[-ind_error, ]
df_look <- subset(df_pest, mcrop == "maize")
ggplot(df_look, aes(x=inputprkg))+geom_histogram()+coord_flip()+facet_wrap(~inputype, scales="free")
#---
df_pestPrice <- df_pest[, c("hhid", "mcrop", "dist", "inputype", "inputprkg")] %>%
  group_by(dist, mcrop, inputype) %>%
  summarise(inputprkg = mean(inputprkg, na.rm = T))
#---
df_look <- subset(df_pestPrice, mcrop == "maize")
ggplot(df_look, aes(x=dist,y=inputprkg))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~inputype, scales="free")
#---
keep_cols <- c("hhid", "mcrop", "dist", "inputype", "inputprkg")
df_pestPrice <- df_pest[, keep_cols] %>%
  group_by(dist, mcrop, inputype) %>%
  summarise(inputprkg = mean(inputprkg, na.rm = T))
#---
df_look <- subset(df_pestPrice, mcrop = "maize")
ggplot(df_look, aes(x=dist,y=inputprkg))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~inputype, scales = "free")
#---
df_pestPrice <- df_pestPrice %>% spread(inputype, inputprkg)
colnames(df_pestPrice)[-1] <- c("crop", paste(colnames(df_pestPrice)[-c(1:2)], "mean dist price (KES/kg)"))
#---------------------------------------------------------------------------
df_pest$`P&D chem cost (KES)` <- df_pest$kgpur * df_pest$inputprkg
keep_cols <- c("hhid", "dist", "inputype")
df_pest <- df_pest[, keep_cols]
df_pest <- df_pest %>% group_by(hhid, mcrop) %>%
  summarise(kgpur = sum(kgpur, na.rm = T))
colnames(df_pest)[c(2:3)] <- c("crop", "pest/plague chems (kg)")
#---------------------------------------------------------------------------
# Get demographic vars
this_filepath <- paste0(this_folder, "ae_hhsize_07.dta")
df_hhsize <- read.dta(this_filepath)
df_hhsize <- de_factorizer(df_hhsize)
this_filepath <- paste0(this_folder, "demog07.dta")
df_demog <- read.dta(this_filepath)
df_demog <- de_factorizer(df_demog)

df_demog <- subset(df_demog, da03 == "head")

colnames(df_demog)[5] <- "gend"
df_demog <- merge(df_demog[, c("hhid", "age", "gend")], df_hhsize, by = "hhid")
df_demog$gend[which(df_demog$gend == "male")] <- 0
df_demog$gend[which(df_demog$gend == "female")] <- 1
rm(df_hhsize)
#---------------------------------------------------------------------------
# Get climate variables
this_folder97 <- "C:/Users/bensc/OneDrive/Documents/Data/Tegemeo Data/data needed for 1997/"
this_filepath <- paste0(this_folder97, "climate.dta")
df_clim <- read.dta(this_filepath)
df_clim <- de_factorizer(df_clim)
keep_cols <- c("prov", "dist", "div", "vil", "qwetpre", "qwetit", "qwetxt")
df_clim <- df_clim[, keep_cols]
df_clim <- df_clim %>% group_by(prov, dist, div) %>%
  summarise_all(mean, na.rm = T)
df_clim$vil <- NULL
# Get Tavneet Suri's hhid level main season rainfall variable "main07"
this_filepath <- paste0(this_folder, "tampa_rain_96_07.dta")
df_suri <- read.dta(this_filepath)
df_suri <- de_factorizer(df_suri)
df_suri <- df_suri[, c("hhid", "prov", "dist", "div", "main07")]
df_suri[, c("prov", "dist", "div")] <-
  apply(df_suri[, c("prov", "dist", "div")], 2, tools::toTitleCase)
df_clim <- merge(df_suri, df_clim, by = c("prov", "dist", "div"))
rm(df_suri)
#---------------------------------------------------------------------------
# Get price info
this_filepath <- paste0(this_folder, "pricecrop.dta")
df_cropPrice <- read.dta(this_filepath)
df_cropPrice <- de_factorizer(df_cropPrice)
#unique(df_cropPrice$crop)
df_cropPrice <- subset(df_cropPrice, crop %in% c("maize-dry", "beans"))
# Note price for green maize is about half of price for dry maize
df_cropPrice$crop[grep("maize", df_cropPrice$crop)] <- "maize"
df_cropPrice$pkgrep <- NULL
colnames(df_cropPrice)[3] <- "crop price (KES/kg)"
#---
ggplot(df_cropPrice, aes(x=dist,y=pkg))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~crop, scales = "free")
#---
this_filepath <- paste0(this_folder, "pricefert.dta")
df_fertPrice <- read.dta(this_filepath)
df_fertPrice <- de_factorizer(df_fertPrice)
df_fertPrice <- subset(df_fertPrice, inputype != "NPK + CAN")
ind_npk <- grep("NPK", df_fertPrice$inputype)
df_fertPrice$inputype[ind_npk] <- "npk"
ind_other <- which(df_fertPrice$inputype %in% c("mavuno-basal", "foliar feed", "compost"))
df_fertPrice$inputype[ind_other] <- "other organic"
df_fertPrice <- df_fertPrice %>% group_by(inputype, dist) %>%
  summarise(pfert = mean(pfert, na.rm = T))
synth_ferts <- c("dap", "npk", "CAN (26:0:0)", "UREA (46:0:0)")
ferts_keep <- c(synth_ferts, organic_ferts)
df_fertPrice <- subset(df_fertPrice, inputype %in% ferts_keep)
#---
ggplot(df_fertPrice, aes(x=dist,y=pfert))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~inputype, scales = "free")
#---
df_fertPrice$inputype[which(df_fertPrice$inputype %in% synth_ferts)] <- "synthetic"
df_fertPrice$inputype[which(df_fertPrice$inputype %in% organic_ferts)] <- "organic"
df_fertPrice <- df_fertPrice %>% group_by(inputype, dist) %>%
  summarise(pfert = mean(pfert, na.rm = T))
#---
ggplot(df_fertPrice, aes(x=dist,y=pfert))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~inputype, scales = "free")
#---
df_fertPrice <- df_fertPrice %>% spread(inputype, pfert)
colnames(df_fertPrice)[2:3] <- c("organic fert price (KES/kg)",
                                 "synth fert price (KES/kg)")
#===========================================================================
list_df <- list(df_yield, df_labor, df_pest, df_clim, df_demog, df_km)#,
                # df_cropPrice, df_fertPrice, df_pestPrice, 
                # df_seedPrice, df_laborPrice)
df <- plyr::join_all(list_df)
colnames(df) <- gsub("_", ": ", colnames(df))
colnames(df) <- gsub(" /", "/", colnames(df))
df$`pest/plague chems (kg)`[which(is.na(df$`pest/plague chems (kg)`))] <- 0
df$`pest/plague chems (kg/acre)` <- df$`pest/plague chems (kg)` / df$acres
df$`seed (kg/acre)` <- df$seedkg / df$acres
df$`Wage labor (man days/acre)` <- df$`Wage labor (man days)` / df$acres
df$`Adult family labor (man hours/acre)` <- df$`Adult family labor (man hours)` / df$acres
df$`Rain anomaly` <- df$main07 - df$qwetpre
#===========================================================================
# Test it out
#colnames(df)
input_vars <- c("Adult family labor (man hours/acre)",
                #"Wage labor (man days/acre)",
                "Total synth. fert. (kg/acre)",
                "Total organic fert. (kg/acre)",
                "seed (kg/acre)", "pest/plague chems (kg/acre)")
demog_vars <- c("age"#,
                #"aehh07",
                #"hhsize07"
                # "km to organic fert mkt",
                #"km to synth fert mkt",
                #"km to pest/plague chem mkt"
                )
clim_vars <- c(#"Rain anomaly",
               "qwetpre"
               #"main07",
                #"qwetxt",
                #"qwetit"
               )
bin_vars <- c("adopter", #"gend",
              #"irrigated",
              # "tenure: govt/communal/cooperative",
              # "tenure: owned by parent/relative", 
              # "tenure: owned with title deed",
              # "tenure: owned without title deed",
              # "tenure: rented", "landprep: manual",
              #"landprep: none",
              "landprep: oxen",
              "landprep: tractor")
mod_vars <- c("dist", "crop", "yield (kg/acre)", input_vars, bin_vars, demog_vars, clim_vars)
df_mod <- subset(df[, mod_vars], crop == "maize" &
                   dist != "Kakamega" &
                   `yield (kg/acre)` > 1)
df_mod$crop <- NULL
df_mod$dist <- NULL
ind_bin <- which(colnames(df_mod) %in% bin_vars)
df_mod[, -ind_bin] <- log(df_mod[, -ind_bin])
fun <- function(x){
  x[which(is.infinite(x))] <- 0
  return(x)}
df_mod[, -ind_bin] <- as.data.frame(apply(df_mod[, -ind_bin], 2, fun))
#----------------------------------------------------------------------------
mod <- lm(`yield (kg/acre)`~., df_mod)
summary(mod)
#car::vif(mod)
#----------------------------------------------------------------------------
plot(mod$fitted.values, mod$residuals)
#120 NAs omitted. Where are they?
count_missing <- function(x){n_na <- length(which(is.na(x))); return(n_na)}
apply(df_mod, 2, count_missing)
#===========================================================================
# Create files just for prices too
df_pestPrice <- subset(df_pestPrice, crop == "maize") # pest control price same for all crops
df_pestPrice$crop <- NULL
df_inputPrices <- plyr::join_all(list(df_fertPrice, df_pestPrice, df_laborPrice))
colnames(df_cropPrice)
df_cropPrice <- df_cropPrice[, c("dist", "crop", "crop price (KES/kg)")]
colnames(df_cropPrice)[2:3] <- c("Item", "Value")
df_cropPrice$Item <- paste(df_cropPrice$Item, "price (KES/kg)")
colnames(df_seedPrice)[2:3] <- c("Item", "Value")
df_seedPrice$Item <- paste(df_seedPrice$Item, "seed price (KES/kg)")
gathercols <- colnames(df_inputPrices)[-1]
df_inputPriceLong <- df_inputPrices %>% gather_("Item", "Value", gathercols)
df_seedPrice <- as.data.frame(df_seedPrice)
df_cropPrice <- as.data.frame(df_cropPrice)
list_df <- list(df_cropPrice, df_seedPrice, df_inputPriceLong)
df_priceLong <- as.data.frame(do.call(rbind, list_df))
colnames(df_priceLong)[1] <- "District"
#---------------------------------------------------------------------------
# Check price variation across districts
ggplot(df_priceLong,aes(x=District,y=Value))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~Item, scales = "free_x")
#===========================================================================
# Write files
this_file <- "Kenya Tegemeo maize and beans.csv"
this_folder <- "C:/Users/bensc/OneDrive/Documents/Data/Tegemeo Data/"
this_filepath <- paste0(this_folder, this_file)
write.csv(df, this_filepath, row.names = F)
this_file <- "Kenya Tegemeo maize bean crop prices.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_cropPrice, this_filepath, row.names = F)
this_file <- "Kenya Tegemeo input prices.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_inputPrices, this_filepath, row.names = F)
this_file <- "Kenya Tegemeo seed prices.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_seedPrice, this_filepath, row.names = F)
this_file <- "Kenya Tegemeo all bean maize input prices long.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_priceLong, this_filepath, row.names = F)