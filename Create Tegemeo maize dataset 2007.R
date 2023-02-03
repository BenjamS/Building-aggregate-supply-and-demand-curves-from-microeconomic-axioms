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
            adopter = mean(adopter, na.rm = T)) %>%
  as.data.frame()
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
df_acresTot <- df_acresTot %>% group_by(hhid) %>% 
  summarise(acres_tot = sum(acres, na.rm = T)) %>%
  as.data.frame()
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
keep_cols <- c("dist", "hhid", "field", "harvest", "ferttype", "fertotal", "fertcost", "pfert")
df_fert <- subset(df_fert[, keep_cols], harvest == "main")
df_fert$pfert <- df_fert$fertcost / df_fert$fertotal
#---
#unique(df_fert$ferttype)
df_look <- df_fert %>% group_by(ferttype) %>%
  summarise(qty = sum(fertotal), cost = sum(fertcost, na.rm = T))
ind <- which(df_look$qty > 200)
#View(df_look)
#View(df_look[ind, ])
#---
# Get aggregate categories first
#df_fert <- subset(df_fert, !(ferttype %in% c("NPK+CAN", "DAP + CAN", "UREA + CAN")))
dfAgCat <- subset(df_fert, !(ferttype %in% c("NPK+CAN", "DAP + CAN", "UREA + CAN")))
dfAgCat$ferttype[grep("CAN", dfAgCat$ferttype)] <- "Total CAN fert"
dfAgCat$ferttype[grep("NPK", dfAgCat$ferttype)] <- "Total NPK fert"
dfAgCat$ferttype[grep("UREA", dfAgCat$ferttype)] <- "Total UREA fert"
totFerts <- c("Total CAN fert",
              "Total NPK fert",
              "Total UREA fert")
dfAgCatTotSynth <- subset(dfAgCat, ferttype %in% totFerts)
dfAgCatTotSynth$ferttype <- "Total synth fert"
dfAgCatTotSynth <- dfAgCatTotSynth %>%
  group_by(dist, hhid, field, harvest, ferttype) %>%
  summarise(fertotal = sum(fertotal, na.rm = T),
            fertcost = sum(fertcost, na.rm = T)) %>%
  as.data.frame()
dfAgCatTotSynth$pfert <- dfAgCatTotSynth$fertcost / dfAgCatTotSynth$fertotal
orgFerts <- c("compost", "foliar feed", "manure", "mavuno-basal")
dfAgCat$ferttype[which(dfAgCat$ferttype %in% orgFerts)] <- "Total organic fert"
totFerts <- c(totFerts, "Total organic fert")
dfAgCat <- subset(dfAgCat, ferttype %in% totFerts)
dfAgCat <- dfAgCat %>% group_by(dist, hhid, field, harvest, ferttype) %>%
  summarise(fertotal = sum(fertotal, na.rm = T),
            fertcost = sum(fertcost, na.rm = T)) %>%
  as.data.frame()
dfAgCat$pfert <- dfAgCat$fertcost / dfAgCat$fertotal
df_fert <- as.data.frame(do.call(rbind, list(df_fert, dfAgCat, dfAgCatTotSynth)))
# Now get individual fert types
keepFerts <- df_look$ferttype[ind]
totFerts <- c(totFerts, "Total synth fert")
keepFerts <- c(keepFerts, totFerts)
df_fert <- subset(df_fert, ferttype %in% keepFerts)
ggplot(df_fert, aes(x = ferttype))+geom_histogram(stat = "count")+coord_flip()
# Also get rid of ferts that almost no one is using
df_fert <- subset(df_fert, !(ferttype %in% c("SA (21:0:0)",
                                             "ssp", "map",
                                             "DAP + CAN")))
keepFerts <- unique(df_fert$ferttype)
#---
ggplot(df_fert, aes(x = ferttype))+geom_histogram(stat = "count")+coord_flip()
# df_look <- df_fert %>% 
#   group_by(dist, ferttype) %>% 
#   summarise(fertotal = sum(fertotal, na.rm = T)) %>%
#   as.data.frame()
#ggplot(df_fert, aes(x=fertcost,y=ferttype))+geom_bar(stat="identity")+facet_wrap(~dist)
# The districts with significant fertilizer expenditures are
distFert <- c("Bomet", "Bungoma", "Kakamega", "Kisii", "Laikipia", "Makueni",
              "Meru", "Nakuru", "Muranga", "Nyeri", "Uasin Gishu", "Trans Nzoia")
ggplot(subset(df_fert, dist %in% distFert), aes(x=fertcost,y=ferttype))+geom_bar(stat="identity")+facet_wrap(~dist)
#---
orgFerts <- c(orgFerts, "Total organic fert")
synthFerts <- setdiff(keepFerts, orgFerts)
#---
# # ind_npk <- grep("NPK", df_fert$ferttype)
# # df_fert$ferttype[ind_npk] <- "npk"
# otherOrganic <- c("mavuno-basal", "foliar feed", "compost")
# # ind_other <- which(df_fert$ferttype %in% otherOrganic)
# # df_fert$ferttype[ind_other] <- "other organic"
# # df_fert <- df_fert %>% group_by(hhid, field, ferttype) %>%
# #   summarise(fertotal = sum(fertotal, na.rm = T),
# #             fertcost = sum(fertcost, na.rm = T),
# #             pfert = mean(pfert, na.rm = T)) %>%
# #   as.data.frame()
# ferts_keep <- c("dap", "npk", "CAN (26:0:0)", "UREA (46:0:0)",
#                 otherOrganic, "manure")
# df_fert <- subset(df_fert, ferttype %in% ferts_keep)
#---
ggplot(df_fert,aes(x=pfert))+geom_histogram()+facet_wrap(~ferttype,scales="free")
#ggplot(df_fert,aes(x=ferttype))+geom_histogram(stat="count")+coord_flip()
#---
# #df_fertp <- df_fert[, c("hhid", "field", "ferttype", "pfert")] %>% spread(ferttype, pfert)
# df_fert$ferttype[grep("UREA", df_fert$ferttype)] <- "urea"
# df_fert$ferttype[grep("CAN", df_fert$ferttype)] <- "can"
#---
df_fertCost <- df_fert[, c("hhid", "field", "ferttype", "fertcost")] %>% spread(ferttype, fertcost)
df_fertCost[is.na(df_fertCost)] <- 0
colnames(df_fertCost)[3:ncol(df_fertCost)] <- paste(colnames(df_fertCost)[3:ncol(df_fertCost)], "(KES)")
#---
df_fertQty <- df_fert[, c("hhid", "field", "ferttype", "fertotal")] %>% spread(ferttype, fertotal)
df_fertQty[is.na(df_fertQty)] <- 0
colnames(df_fertQty)[3:ncol(df_fertQty)] <- paste(colnames(df_fertQty)[3:ncol(df_fertQty)], "(kg)")
#---
df_fertPrice <- df_fert[, c("hhid", "field", "ferttype", "pfert")] %>% spread(ferttype, pfert)
df_fertPrice[is.na(df_fertPrice)] <- 0
colnames(df_fertPrice)[3:ncol(df_fertPrice)] <- paste(colnames(df_fertPrice)[3:ncol(df_fertPrice)], "(KES/kg)")
#---
rm(df_fert)
#df_fert <- merge(df_fert, df_fertCost, by = c("hhid", "field"))
list_df <- list(df_fertQty, df_fertCost, df_fertPrice)
df_fert <- plyr::join_all(list_df)
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
  summarise_all(sum, na.rm = T) %>%
  as.data.frame()

df_yield_bin <- df_yield_bin %>% group_by(hhid, crop) %>%
  summarise_all(mean, na.rm = T) %>%
  as.data.frame()
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
# Calculate seed qty and cost per acre, and seed price (KES/kg)
df_yield$`seed (kg/acre)` <- df_yield$seedkg / df_yield$acres
df_yield$`seed (KES/acre)` <- df_yield$scost / df_yield$acres
df_yield$`seed (KES/kg)` <- df_yield$scost / df_yield$seedkg
# hist(df_yield$`yield (kg/acre)`)
# hist(log(df_yield$`yield (kg/acre)`))
# hist(log(df_yield$acres))
# hist(log(df_yield$`seed (kg/acre)`))
# Get fert per acre
fertcols <- colnames(df_fert)[3:ncol(df_fert)]
fertPriceCols <- fertcols[grep("KES/kg", fertcols)]
fertNotPriceCols <- setdiff(fertcols, fertPriceCols)
df_yield[, fertNotPriceCols] <- df_yield[, fertNotPriceCols] / df_yield$acres
colnames(df_yield)[which(colnames(df_yield) %in% fertNotPriceCols)] <- gsub("\\)$", "/acre\\)", colnames(df_yield[, fertNotPriceCols]))
#---
nrow(df_yield)
df_look <- subset(df_yield, crop == "maize")
plot(log(df_look$`seed (kg/acre)`), log(df_look$`yield (kg/acre)`))
nrow(df_look)
#---------------------------------------------------------------------------
# # Get seed price
# df_seedPrice <- df_yield[, c("hhid", "dist", "crop", "adopter", "seedkg", "scost")]
# df_seedPrice$`seed price (KES/kg)` <- df_seedPrice$scost / df_seedPrice$seedkg
# df_seedPrice$crop[which(df_seedPrice$adopter == 1 & df_seedPrice$crop == "maize")] <- "hybrid maize"
# df_seedPrice <- df_seedPrice %>% group_by(dist, crop) %>%
#   summarise(`seed price (KES/kg)` = mean(`seed price (KES/kg)`, na.rm = T)) %>%
#   as.data.frame()
#---------------------------------------------------------------------------
# Get labor and wage rate
# Note labor data not disaggregated by crop or field
# Probably not an issue so long as analyzing the main crop (maize)
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
  mutate(personHrsPct = 100 * personHours / sum(personHours)) %>%
  as.data.frame()
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
rm(df_labAdQty, df_labChQty, df_labFamQty, df_labFemQty, df_labMalQty)
df_look <- df_labQty %>% gather()
#---
# Get labor cost for wage labor
df_labor$wgLabCost <- df_labor$lab_wg * df_labor$`Wage (KES/hour)`
df_labWgCost <- df_labor[, keep_cols] %>% spread(activity, lab_wg)
df_labWgCost[is.na(df_labWgCost)] <- 0
df_labWgCost$`Total wage labor` <- rowSums(df_labWgCost[, -c(1:2)])
# Get non-lpcost wage labor
indLabNonLandPrep <- which(colnames(df_labWgCost) %in% c("transport",
                                                         "harvesting",
                                                         "storage",
                                                         "irrigation",
                                                         "security",
                                                         "stooking",
                                                         "shelling",
                                                         "bagging",
                                                         "drying",
                                                         "dusting (post harvest)"))
df_labWgCost$`Non-land prep wage labor` <- rowSums(df_labWgCost[, indLabNonLandPrep])
colnames(df_labWgCost)[-c(1:2)] <- paste(colnames(df_labWgCost)[-c(1:2)], "(KES)")
#---
# Get wage for each labor activity averaged over hh in a given district
keep_cols <- c("dist", "hhid", "activity", "Wage (KES/hour)")
df_labWgHhAct <- df_labor[, keep_cols]
df_labWgHh <- df_labWgHhAct %>% spread(activity, `Wage (KES/hour)`) %>%
  as.data.frame()
colnames(df_labWgHh)[-c(1, 2)] <- paste(colnames(df_labWgHh)[-c(1, 2)], "(KES/hour)")
df_labWgDistAct <- df_labWgHhAct %>% group_by(dist, activity) %>% 
  summarise(meanWageDist = mean(`Wage (KES/hour)`, na.rm = T)) %>%
  as.data.frame()
df_labWgDistAct <- df_labWgDistAct[-which(is.nan(df_labWgDistAct$meanWageDist)), ]
#---
ggplot(df_labWgDistAct,aes(x=dist,y=meanWageDist))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~activity)
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
df_labWgDist <- df_labWgHhAct %>% group_by(dist) %>%
  summarise(meanWageDist = mean(`Wage (KES/hour)`, na.rm = T)) %>%
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
gg <- gg + facet_wrap(~labtype)
#gg <- gg + facet_wrap(~labtype, scales = "free_x")
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
#---
# Separate out data for distance-to-market vars, if you want to do that
# (Sort of interesting but lots of missing data)
#df_km <- df_pest
#---
pest_keep <- c("Herbicide", "Pesticide", "Insecticide", "Fungicide")
df_pest <- subset(df_pest, inputype %in% pest_keep)
# There is just one entry with green maize.
# The corresponding hhid has no other entries, so no need to aggregate
# ind_greenMaize <- which(df_pest$mcrop == "maize-green")
# df_pest[ind_greenMaize, ]
df_pest$mcrop[grep("maize", df_pest$mcrop)] <- "maize"
#------------------------------------------------------------------------
# # Get distance to input market for pest/plague control and fert
# df_km <- subset(df_km, inputype != "NPK + CAN")
# ind_npk <- grep("NPK", df_km$inputype)
# df_km$inputype[ind_npk] <- "npk"
# ind_other <- which(df_km$inputype %in% c("mavuno-basal", "foliar feed", "compost"))
# df_km$inputype[ind_other] <- "other organic"
# df_km <- df_km %>% group_by(hhid, inputype, mcrop) %>%
#   summarise(kms = mean(kms, na.rm = T))
# df_km$inputype[grep("Manure", df_km$inputype)] <- "manure"
# organic_ferts <- c("other organic", "manure")
# synth_ferts <- c("dap", "can", "urea", "npk")
# ferts_keep <- c(synth_ferts, organic_ferts)
# pest_keep <- c("Herbicide", "Pesticide", "Insecticide", "Fungicide")
# inputs_keep <- c(ferts_keep, pest_keep)
# df_km <- subset(df_km, inputype %in% inputs_keep)
# #---
# df_look <- subset(df_km, mcrop == "maize")
# ggplot(df_look, aes(x=kms))+geom_histogram()+coord_flip()+facet_wrap(~inputype, ncol=2, scales = "free")
# #---
# # df_km <- df_km %>% group_by(hhid, mcrop) %>%
# #   summarise(kms = mean(kms, na.rm = T))
# df_km$inputype[which(df_km$inputype %in% organic_ferts)] <- "organic fert"
# df_km$inputype[which(df_km$inputype %in% synth_ferts)] <- "synth fert"
# df_km$inputype[which(df_km$inputype %in% pest_keep)] <- "P&D chems"
# df_km <- df_km %>% group_by(hhid, mcrop, inputype) %>%
#   summarise(kms = mean(kms, na.rm = T))
# #---
# df_look <- subset(df_km, mcrop == "maize")
# ggplot(df_look, aes(x=kms))+geom_histogram()+coord_flip()+facet_wrap(~inputype, ncol=2, scales = "free")
# #---
# # ggplot(df_km, aes(x =kms))+geom_histogram()+coord_flip()+facet_wrap(~mcrop, ncol=2, scales = "free")
# #---
# #colnames(df_km)[2:3] <- c("crop", "km to chem/fert mkts")
# df_km <- df_km %>% spread(inputype, kms)
# colnames(df_km)[2:5] <- c("crop", "km to organic fert mkt",
#                           "km to pest/plague chem mkt",
#                           "km to synth fert mkt")
# df_km[is.na(df_km)] <- 0
#----------------------------------------------------------------------------
# unique(df_pest$punit)
# lookCols <- c("hhid", "dist", "mcrop", "inputype", "numpur", "inputpr", "punit")
# #setdiff(lookCols, colnames(df_pest))
# df_look <- df_pest[, lookCols]
# Convert all units to kg
df_pest$kgpur <- df_pest$numpur
df_pest$kgpur[which(df_pest$punit == "gram")] <- df_pest$kgpur[which(df_pest$punit == "gram")] / 1000
df_pest$kgpur[which(df_pest$punit == "gorogoro")] <- df_pest$kgpur[which(df_pest$punit == "gorogoro")] * 2
df_pest$kgpur[which(df_pest$punit == "90 kg bag")] <- df_pest$kgpur[which(df_pest$punit == "90 kg bag")] * 90
df_pest$kgpur[which(df_pest$punit == "litre")] <- df_pest$kgpur[which(df_pest$punit == "litre")]
df_pest$kgpur[which(df_pest$punit == "kg")] <- df_pest$kgpur[which(df_pest$punit == "kg")]
# Convert prices to per kg
df_pest$inputprkg <- df_pest$inputpr
df_pest$inputprkg[which(df_pest$punit == "gram")] <- df_pest$inputprkg[which(df_pest$punit == "gram")] * 1000
df_pest$inputprkg[which(df_pest$punit == "gorogoro")] <- df_pest$inputprkg[which(df_pest$punit == "gorogoro")] * 2
df_pest$inputprkg[which(df_pest$punit == "90 kg bag")] <- df_pest$inputprkg[which(df_pest$punit == "90 kg bag")] * 90
df_pest$inputprkg[which(df_pest$punit == "litre")] <- df_pest$inputprkg[which(df_pest$punit == "litre")]
df_pest$inputprkg[which(df_pest$punit == "kg")] <- df_pest$inputprkg[which(df_pest$punit == "kg")]
#---
ind_error <- which(df_pest$inputprkg == 76500)
#df_pest$inputprkg[ind_error] <- 7650 #correcting error
df_pest <- df_pest[-ind_error, ]
#---
# Some hhid bought same input but in different punit and/or inputprkg
# Have to aggregate (add) these now that everything is in kg
# Might also have to aggregate due to green and dry maize being combined
#---
# P&D chem cost
keep_cols <- c("hhid", "dist", "mcrop", "inputype", "kgpur", "inputprkg", "punit")
df_pestCost <- df_pest[, keep_cols]
df_pestCost$PnDchemCost <- df_pestCost$kgpur * df_pestCost$inputprkg
df_pestCost <- df_pestCost %>% group_by(dist, hhid, mcrop, inputype) %>%
  summarise(PnDchemCost = sum(PnDchemCost, na.rm = T)) %>%
  as.data.frame()
df_pestCost <- df_pestCost %>% spread(inputype, PnDchemCost)
df_pestCost$kgpur <- NULL
df_pestCost$inputprkg <- NULL
df_pestCost$`Total P&D chem` <- rowSums(df_pestCost[, -c(1:3)], na.rm = T)
colnames(df_pestCost)[-c(1:2)] <- c("crop", paste(colnames(df_pestCost)[-c(1:3)], "(KES)"))
df_pestCost[is.na(df_pestCost)] <- 0
# One duplicate hhid but it is because the farm applies P&D chems
# to both maize and beans (So no worries)
# (If don't first summarise then there are loads of duplicates,
# so summarise step is key)
# indDup <- which(duplicated(df_pestCost$hhid))
# indDup <- c((indDup - 1), indDup)
# View(df_pestCost[indDup, ])
hist(log(df_pestCost$`Total P&D chem (KES)`))
#---
# P&D chem qty
keep_cols <- c("hhid", "dist", "mcrop", "inputype", "kgpur")
df_pestQty <- df_pest[, keep_cols]
df_pestQty <- df_pestQty %>% group_by(dist, hhid, mcrop, inputype) %>%
  summarise(kgpur = sum(kgpur, na.rm = T)) %>%
  as.data.frame()
df_pestQty <- df_pestQty %>% spread(inputype, kgpur)
df_pestQty$`Total P&D chem` <- rowSums(df_pestQty[, -c(1:3)], na.rm = T)
colnames(df_pestQty)[-c(1:2)] <- c("crop", paste(colnames(df_pestQty)[-c(1:3)], "(kg)"))
df_pestQty[is.na(df_pestQty)] <- 0
# Same duplicate hhid as for df_pestCost
# indDup <- which(duplicated(df_pestQty$hhid))
# indDup <- c((indDup - 1), indDup)
# View(df_pestQty[indDup, ])
#---
df_pestPriceHh <- df_pestCost
df_pestPriceHh[, -c(1:3)] <- df_pestPriceHh[, -c(1:3)] / df_pestQty[, -c(1:3)]
for(i in 4:ncol(df_pestPriceHh)){
  df_pestPriceHh[which(is.nan(df_pestPriceHh[, i])) , i] <- NA
}
colnames(df_pestPriceHh)[-c(1:3)] <- gsub("KES", "KES/kg", colnames(df_pestPriceHh)[-c(1:3)])
# hist(log(df_pestPriceHh$`Pesticide (KES/kg)`))
# hist(log(df_pestPriceHh$`Insecticide (KES/kg)`))
# hist(log(df_pestPriceHh$`Herbicide (KES/kg)`))
hist(log(df_pestPriceHh$`Total P&D chem (KES/kg)`))
indLook <- which(log(df_pestPriceHh$`Total P&D chem (KES/kg)`) < 0)
df_pestPriceDist <- df_pestPriceHh %>% group_by(dist, crop) %>%
  summarise_all(mean, na.rm = T) %>% as.data.frame()
colnames(df_pestPriceDist)[-c(1:3)] <- gsub("KES", "KES/kg", colnames(df_pestPriceDist)[-c(1:3)])
#---
# P&D chem "price" (marg cost) at hhid level
keep_cols <- c("hhid", "mcrop", "dist", "inputype", "inputprkg")
df_pestPriceHh <- df_pest[, keep_cols] %>%
  group_by(dist, hhid, mcrop, inputype) %>%
  summarise(inputprkg = mean(inputprkg, na.rm = T)) %>%
  as.data.frame()
df_pestPriceHh <- df_pestPriceHh %>% spread(inputype, inputprkg)
df_pestPriceHh$`Total P&D chem` <- df_pestCost$`Total P&D chem (KES)` / df_pestQty$`Total P&D chem (kg)`
colnames(df_pestPriceHh)[-c(1, 2)] <- c("crop", paste(colnames(df_pestPriceHh)[-c(1:3)], "(KES/kg)"))
# df_look <- subset(df_pestPriceHh, crop = "maize") %>% 
#   gather(type, price, `Fungicide (KES/kg)`:`Total P&D chem (KES/kg)`) %>%
#   as.data.frame()
# ggplot(df_look,aes(x=price))+geom_histogram(stat="count")+facet_wrap(~type, scales = "free")
hist(log(df_pestPriceHh$`Pesticide (KES/kg)`))
hist(log(df_pestPriceHh$`Insecticide (KES/kg)`))
hist(log(df_pestPriceHh$`Total P&D chem (KES/kg)`))
# Mean P&D chem price at dist level
# Judging by the histograms immediately above, price is lognormally distributed
# So calculate mean as exp(m + s^2/2) because the arithmetic mean will be misleading.
df_pestPriceDist <- df_pestPriceHh %>%
  gather(type, price, `Fungicide (KES/kg)`:`Total P&D chem (KES/kg)`) %>%
  as.data.frame()
df_pestPriceDist$lprice <- log(df_pestPriceDist$price)
df_pestPriceDist <- df_pestPriceDist %>% group_by(dist, crop, type) %>%
  summarise(m = mean(lprice, na.rm = T),
            s = sd(lprice, na.rm = T)) %>% as.data.frame()
df_pestPriceDist$mu <- exp(df_pestPriceDist$m + df_pestPriceDist$s^2 / 2)
df_pestPriceDist$s <- NULL
df_pestPriceDist$m <- NULL
df_pestPriceDist <- df_pestPriceDist %>% spread(type, mu)
# %>% mutate_all(~replace(., is.nan(.), NA))
# df_look <- subset(df_pestPriceDist, mcrop = "maize")
# ggplot(df_look, aes(x=dist,y=`Total P&D chem (KES/kg)`))+geom_bar(stat="identity")+coord_flip()
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
# this_folder97 <- "C:/Users/bensc/OneDrive/Documents/Data/Tegemeo Data/data needed for 1997/"
this_folder97 <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/New PE Model/Tegemeo Data/data needed for 1997/"
this_filepath <- paste0(this_folder97, "climate.dta")
df_clim <- read.dta(this_filepath)
df_clim <- de_factorizer(df_clim)
keep_cols <- c("prov", "dist", "div", "vil", "qwetpre", "qwetit", "qwetxt")
df_clim <- df_clim[, keep_cols]
df_clim <- df_clim %>% group_by(prov, dist, div) %>%
  summarise_all(mean, na.rm = T) %>% as.data.frame()
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
# Get crop and fert price info
# Crop prices already in per kg terms. Fert prices not all in per kg terms.
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
ggplot(df_cropPrice, aes(x=dist,y=`crop price (KES/kg)`))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~crop, scales = "free")
#---
# !!If going to use the following, have to convert fert prices in pricefert.dta to per kg price!!
# (fert prices in df_yield were converted to per kg)
#---
# this_filepath <- paste0(this_folder, "pricefert.dta")
# df_fertPriceDist <- read.dta(this_filepath)
# df_fertPriceDist <- de_factorizer(df_fertPriceDist)
# df_fertPriceDist <- subset(df_fertPriceDist, inputype %in% keepFerts)
# #---
# ggplot(df_fertPriceDist, aes(x=dist,y=pfert))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~inputype, scales = "free")
# #---
# df_add <- df_fertPrice
# df_add$inputype[which(df_add$inputype %in% synth_ferts)] <- "mean synthetic"
# df_add$inputype[which(df_add$inputype %in% organic_ferts)] <- "mean organic"
# df_add <- df_add %>% group_by(inputype, dist) %>%
#   summarise(pfert = mean(pfert, na.rm = T)) %>%
#   as.data.frame()
# df_fertPrice <- as.data.frame(rbind(df_fertPrice, df_add))
# #---
# ggplot(df_fertPrice, aes(x=inputype,y=pfert))+geom_bar(stat="identity")+coord_flip()+facet_wrap(~dist)
# #---
# df_fertPrice <- df_fertPrice %>% spread(inputype, pfert)
# colnames(df_fertPrice)[-c(1)] <- paste(colnames(df_fertPrice)[-c(1)], "price (KES/kg)")
#===========================================================================
#===========================================================================
#===========================================================================
# Put it all together
list_df <- list(df_yield, df_labQty, df_pestQty, df_clim, df_demog,
                df_labWgCost, df_pestCost, df_labWgHh, df_pestPriceHh)#, df_km)
df <- plyr::join_all(list_df)
colnames(df) <- gsub("_", ": ", colnames(df))
colnames(df) <- gsub(" /", "/", colnames(df))
# All input vars should be per acre
labCols <- grep("\\(pers-hrs\\)", colnames(df))
kgCols <- grep("\\(kg\\)", colnames(df))
KEScols <- grep("\\(KES\\)", colnames(df))
df[, c(labCols, kgCols, KEScols)] <- df[, c(labCols, kgCols, KEScols)] / df$acres
these_cols <- grep("\\(pers-hrs\\)|\\(kg\\)|\\(KES\\)", colnames(df))
colnames(df)[these_cols] <- gsub("\\)", "/acre\\)", colnames(df)[these_cols])
count_missing <- function(x){n_na <- length(which(is.na(x))); return(n_na)}
apply(df, 2, count_missing)
PnDcols <- grep("cide|chem", colnames(df))
df[which(is.na(df[, PnDcols[1]])), PnDcols] <- 0
df$gend <- as.numeric(df$gend)
# df$`P&D chems (kg)`[which(is.na(df$`pest/plague chems (kg)`))] <- 0
# df$`pest/plague chems (kg/acre)` <- df$`pest/plague chems (kg)` / df$acres
# df$`seed (kg/acre)` <- df$seedkg / df$acres
# df$`Wage labor (man days/acre)` <- df$`Wage labor (man days)` / df$acres
# df$`Adult family labor (man hours/acre)` <- df$`Adult family labor (man hours)` / df$acres
df$`Rain anomaly` <- df$main07 / df$qwetpre
costVars <- grep("KES/acre", colnames(df))
colnames(df)[costVars]
df$Cstar <- df$`Total synth fert (KES/acre)` + 
  #df$`Total organic fert (KES/acre)` +
  #df$`manure (KES/acre)` +
  df$`seed (KES/acre)` +
  df$lpcost +
  #df$`Non-land prep wage labor (KES/acre)` +
  df$`Total P&D chem (KES/acre)`
df <- merge(df, df_cropPrice, by = c("crop", "dist"))
df$Rstar <- df$`yield (kg/acre)` * df$`crop price (KES/kg)`
#---
df_look <- subset(df, crop == "maize")
#df_look <- subset(df_yield, crop == "maize")
#plot(log(df_look$`seed (kg/acre)`), log(df_look$`yield (kg/acre)`))
hist(log(df_look$Cstar))
hist(log(df_look$Rstar / df_look$Cstar))
#===========================================================================
# Test it out
#colnames(df)
# synthFertVars <- synthFerts[-which(synthFerts %in% c("Total NPK fert",
#                                                      "Total UREA fert",
#                                                      "Total CAN fert",
#                                                      "Total synth fert"))]
# synthFertVars <- paste(synthFertVars, "(kg/acre)")
input_vars <- c(#"Total fam lab child (pers-hrs/acre)",
                #"Total fam lab (pers-hrs/acre)",
                #"Total fam lab adult (pers-hrs/acre)",
                #"Total wage labor (pers-hrs/acre)",
              #"1st weeding fam lab child (pers-hrs/acre)",
               #"2nd weeding fam lab fem (pers-hrs/acre)",
               #"2nd weeding fam lab adult (pers-hrs/acre)",
              #"1st weeding fam lab fem (pers-hrs/acre)",
              #"1st weeding fam lab adult (pers-hrs/acre)",
              "1st weeding fam lab male (pers-hrs/acre)",
                #"1st weeding wage labor (pers-hrs/acre)",
                #"2nd weeding wage labor (pers-hrs/acre)",
               # "1st ploughing fam lab child (pers-hrs/acre)",
              #"1st ploughing fam lab adult (pers-hrs/acre)",
               # "1st ploughing fam lab fem (pers-hrs/acre)",
               #"1st ploughing fam lab male (pers-hrs/acre)",
                #"1st ploughing wage labor (pers-hrs/acre)",
              #"shelling fam lab adult (pers-hrs/acre)",
              #"shelling fam lab fem (pers-hrs/acre)",
                #"shelling fam lab male (pers-hrs/acre)",
              #"shelling fam lab child (pers-hrs/acre)",
              # "shelling wage labor (pers-hrs/acre)",
              "harvesting fam lab male (pers-hrs/acre)",
              #"harvesting fam lab adult (pers-hrs/acre)", *
              #"harvesting fam lab fem (pers-hrs/acre)",
            #"harvesting fam lab child (pers-hrs/acre)",
              #"harvesting wage labor (pers-hrs/acre)",
            #  "planting fam lab adult (pers-hrs/acre)",
               # "planting fam lab fem (pers-hrs/acre)",
                #"planting fam lab male (pers-hrs/acre)",
              "planting fam lab child (pers-hrs/acre)",
               #"planting wage labor (pers-hrs/acre)",
                #"Total synth. fert. (kg/acre)",
                "seed (kg/acre)",
              "Total CAN fert (kg/acre)",      
              "Total NPK fert (kg/acre)",  
              "Total UREA fert (kg/acre)",
              "dap (kg/acre)",
                #"UREA (46:0:0) (kg/acre)",
                #"CAN (26:0:0) (kg/acre)",
                # "NPK (23:23:23) (kg/acre)",
                # "NPK (23:23:0) (kg/acre)",
              #"NPK (17:17:0) (kg/acre)",
               #"manure (kg/acre)",
              #"compost (kg/acre)",
              #"mavuno-basal (kg/acre)",
              #"foliar feed (kg/acre)",
               # "Fungicide qty (kg/acre)",
               # "Herbicide qty (kg/acre)",
               # "Insecticide qty (kg/acre)",
               # "Pesticide qty (kg/acre)"
              "Total P&D chem (kg/acre)"
              )
demog_vars <- c(#"age",
                "aehh07"#,
                #"hhsize07"
                # "km to organic fert mkt",
                #"km to synth fert mkt",
                #"km to pest/plague chem mkt"
                )
clim_vars <- c("Rain anomaly"#,
               #"qwetpre",
               #"main07",
                # "qwetxt",
                # "qwetit"
               )
bin_vars <- c("adopter", #"gend",
              #"irrigated",
               # "tenure: govt/communal/cooperative",
               # "tenure: owned by parent/relative", 
              "tenure: owned with title deed",
              # "tenure: owned without title deed",
              # "tenure: rented",
              # "landprep: manual",
              # "landprep: none",
              "landprep: oxen",
              "landprep: tractor")
#----------------------------------------------------------------------------
costVars <- c("Cstar", #"seed (KES/acre)",
              "dap (KES/acre)",
              #"UREA (46:0:0) (KES/acre)",
              "Total CAN fert (KES/acre)",
              #"CAN (26:0:0) (KES/acre)",
              "Total NPK fert (KES/acre)",
              #"NPK (23:23:23) (KES/acre)",
              #"NPK (23:23:0) (KES/acre)",
              "Total UREA fert (KES/acre)",
              #"manure (KES/acre)",
              "Total P&D chem (KES/acre)"
              )
priceVars <- gsub("acre", "kg", costVars[-1])
priceVars <- c(priceVars, "harvesting (KES/hour)")
#----------------------------------------------------------------------------
mod_vars <- c("dist", "crop", "yield (kg/acre)", "acres",
              input_vars, bin_vars,
              demog_vars, clim_vars,
              costVars, "Rstar", priceVars, "crop price (KES/kg)")
setdiff(mod_vars, colnames(df))
#----------------------------------------------------------------------------
df_mod <- subset(df[, mod_vars], crop == "maize" &
                   Cstar > 0 &
                   acres > 0 &
                   `yield (kg/acre)` > 1)
df_mod <- df_mod[-which(is.na(df_mod$`Rain anomaly`)), ]
df_mod$crop <- NULL
#df_mod$dist <- NULL
colnames(df_mod)[which(colnames(df_mod) == "crop price (KES/kg)")] <- "P"
ind_bin <- which(colnames(df_mod) %in% bin_vars)
ind_char <- which(colnames(df_mod) %in% c("dist"))
indPrice <- which(colnames(df_mod) %in% c(priceVars, "P"))
indCostVars <- which(colnames(df_mod) %in% costVars[-1])
not_these <- unique(c(ind_bin, ind_char, indPrice, indCostVars))
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
mod1Vars <- c("dist", "yield (kg/acre)", "acres",
              input_vars, bin_vars, demog_vars, clim_vars,
              costVars, priceVars, "P")
df_mod1 <- df_mod[, mod1Vars]
not_these <- which(colnames(df_mod1) %in% c("dist", costVars, priceVars, "P"))
mod1 <- lm(`yield (kg/acre)`~., df_mod1[, -not_these])
summary(mod1)
sum(mod1$residuals^2)
plot(mod1$fitted.values, mod1$residuals)
#car::vif(mod1)
#View(df_mod1[which(mod1$residuals < -2), ])
coefsNeg <- mod1$coefficients[which(mod1$coefficients < 0)]
coefsNegNames <- gsub("`", "", names(coefsNeg))
inputsNegCoef <- coefsNegNames[which(coefsNegNames %in% input_vars)]
inputsPosCoef <- setdiff(input_vars, inputsNegCoef)
#----------------------------------------------------------------------------
mod2Vars <- c("dist", "yield (kg/acre)", "Cstar", "acres", "seed (kg/acre)",
              inputsNegCoef[-1], bin_vars, demog_vars, clim_vars)
df_mod2 <- df_mod[, mod2Vars]
not_these <- which(colnames(df_mod2) %in% c("dist", costVars[-1]))
mod2 <- lm(`yield (kg/acre)`~., df_mod2[, -not_these])
summary(mod2)
sum(mod2$residuals^2)
plot(mod2$fitted.values, mod1$residuals)
#car::vif(mod2)
#View(df_mod2[which(mod2$residuals < -1.5), ])
#----------------------------------------------------------------------------
# Model validation tests
#----------------------------------------------------------------------------
coefsNames <- gsub("`", "", names(mod1$coefficients))
aVec <- mod1$coefficients[which(coefsNames %in% inputsPosCoef)]
# If seed coef removed, calculation of h based on model 1 input coefs
# matches estimation of h in model 2 
rmSeed <- which(names(aVec) == "`seed (kg/acre)`")
aVec <- aVec[-rmSeed]
h <- sum(aVec)
#aVec / h
inputMat <- as.matrix(df_mod1[, inputsPosCoef[-2]]) # (if removing seed coef)
#inputMat <- as.matrix(df_mod1[, inputsPosCoef])
inputMat[inputMat != 0] <- 1
h <-  inputMat %*% aVec
hist(h)
#---
# Validation test 1 using cost shares vs. tech shares
# (Theory predicts that input cost_i / total input cost = alpha_i / h)
# (So the following ratios should = 1, or log(ratio) = 0)
# Encouraging results, make sure Cstar only includes inputs used in model,
# and that seed might better not be considered an input
#---
Cstar <- exp(df_mod1$Cstar)
Cstar[which(Cstar == 1)] <- 0
#ratio <- aVec[which(names(aVec) == "`seed (kg/acre)`")] / h * Cstar / df_mod1$`seed (KES/acre)`
#ratio <- aVec[which(names(aVec) == "`manure (kg/acre)`")] / h * Cstar / df_mod1$`manure (KES/acre)`
ratio <- aVec[which(names(aVec) == "`Total NPK fert (kg/acre)`")] / h * Cstar / df_mod1$`Total NPK fert (KES/acre)`
ratio <- aVec[which(names(aVec) == "`Total CAN fert (kg/acre)`")] / h * Cstar / df_mod1$`Total CAN fert (KES/acre)`
ratio <- aVec[which(names(aVec) == "`Total UREA fert (kg/acre)`")] / h * Cstar / df_mod1$`Total UREA fert (KES/acre)`
ratio <- aVec[which(names(aVec) == "`dap (kg/acre)`")] / h * Cstar / df_mod1$`dap (KES/acre)`
ratio <- aVec[which(names(aVec) == "`Total P&D chem (kg/acre)`")] / h * Cstar / df_mod1$`Total P&D chem (KES/acre)`
hist(log(ratio))
#---
# Validation test 2 based only on input costs and tech coeffs
# (Theory says input cost_i / alpha_i = input cost_j / alpha_j)
# (So the following ratios should = 1, or log(ratio) = 0)
# More encouraging results!
# Ratios involving seed continue to diverge from theory,
# maybe b/c the seed cost is incomplete,
# maybe farmers have ways of getting it for free
# maybe b/c seed cost is not really a management variable
# Note this way of validating opens the door to deducing family labor wage
#---
ratio <- df_mod1$`Total CAN fert (KES/acre)` * 
  aVec[which(names(aVec) == "`Total NPK fert (kg/acre)`")] / (df_mod1$`Total NPK fert (KES/acre)` * aVec[which(names(aVec) == "`Total CAN fert (kg/acre)`")])
ratio <- df_mod1$`Total CAN fert (KES/acre)` *
  aVec[which(names(aVec) == "`Total UREA fert (kg/acre)`")] / (df_mod1$`Total UREA fert (KES/acre)` * aVec[which(names(aVec) == "`Total CAN fert (kg/acre)`")])
ratio <- df_mod1$`Total CAN fert (KES/acre)` *
  aVec[which(names(aVec) == "`dap (kg/acre)`")] / (df_mod1$`dap (KES/acre)` * aVec[which(names(aVec) == "`Total CAN fert (kg/acre)`")])
ratio <- df_mod1$`Total UREA fert (KES/acre)` *
  aVec[which(names(aVec) == "`dap (kg/acre)`")] / (df_mod1$`dap (KES/acre)` * aVec[which(names(aVec) == "`Total UREA fert (kg/acre)`")])
#ratio <- df_mod1$`seed (KES/acre)` * aVec[which(names(aVec) == "`dap (kg/acre)`")] / (df_mod1$`dap (KES/acre)` * aVec[which(names(aVec) == "`seed (kg/acre)`")])
ratio <- df_mod1$`Total NPK fert (KES/acre)` *
  aVec[which(names(aVec) == "`dap (kg/acre)`")] / (df_mod1$`dap (KES/acre)` * aVec[which(names(aVec) == "`Total NPK fert (kg/acre)`")])
ratio <- df_mod1$`Total NPK fert (KES/acre)` *
  aVec[which(names(aVec) == "`Total P&D chem (kg/acre)`")] / (df_mod1$`Total P&D chem (KES/acre)` * aVec[which(names(aVec) == "`Total NPK fert (kg/acre)`")])
ratio <- df_mod1$`Total CAN fert (KES/acre)` *
  aVec[which(names(aVec) == "`Total P&D chem (kg/acre)`")] / (df_mod1$`Total P&D chem (KES/acre)` * aVec[which(names(aVec) == "`Total CAN fert (kg/acre)`")])
ratio <- df_mod1$`Total P&D chem (kg/acre)` *
  aVec[which(names(aVec) == "`dap (kg/acre)`")] / (df_mod1$`dap (kg/acre)` * aVec[which(names(aVec) == "`Total P&D chem (kg/acre)`")])
# Fam labor is in qty terms, not cost terms, but exhibits stable ratio values
ratio <- df_mod1$`Total CAN fert (KES/acre)` * aVec[which(names(aVec) == "`harvesting fam lab male (pers-hrs/acre)`")] / (df_mod1$`harvesting fam lab male (pers-hrs/acre)` * aVec[which(names(aVec) == "`Total CAN fert (kg/acre)`")])
ratio <- df_mod1$`Total NPK fert (KES/acre)` * aVec[which(names(aVec) == "`harvesting fam lab male (pers-hrs/acre)`")] / (df_mod1$`harvesting fam lab male (pers-hrs/acre)` * aVec[which(names(aVec) == "`Total NPK fert (kg/acre)`")])
ratio <- df_mod1$`Total UREA fert (KES/acre)` * aVec[which(names(aVec) == "`harvesting fam lab male (pers-hrs/acre)`")] / (df_mod1$`harvesting fam lab male (pers-hrs/acre)` * aVec[which(names(aVec) == "`Total UREA fert (kg/acre)`")])
ratio <- df_mod1$`dap (KES/acre)` * aVec[which(names(aVec) == "`harvesting fam lab male (pers-hrs/acre)`")] / (df_mod1$`harvesting fam lab male (pers-hrs/acre)` * aVec[which(names(aVec) == "`dap (kg/acre)`")])
hist(log(ratio))
#---------------------------------------------------------------------
# Can deduce the implied modal (most likely) family wage
lratio <- log(ratio)
indRm <- unique(c(which(is.infinite(lratio)), which(is.nan(lratio))))
lratio <- lratio[-indRm]
mWgFam <- mean(lratio, na.rm = T)
sWgFam <- sd(lratio, na.rm = T)
meanWgFamMaleImp <- exp(mWgFam + sWgFam^2 / 2)
modeWgFamMaleImp <- exp(mWgFam - sWgFam^2)
# Modal fam adult male harvesting wage is ~73 KES/hour,
# with some in the tail >> 120.
# Much higher than the dist avg wage of 7-15 KES/hour
# A basis for developing a labor theory of surplus value?
#---------------------------------------------------------------------
y <- df_mod1$`Total CAN fert (KES/acre)` / aVec[which(names(aVec) == "`Total CAN fert (kg/acre)`")]
x <- df_mod1$`dap (KES/acre)` / aVec[which(names(aVec) == "`dap (kg/acre)`")]
y <- df_mod1$`Total NPK fert (KES/acre)` / aVec[which(names(aVec) == "`Total NPK fert (kg/acre)`")]
x <- df_mod1$`Total P&D chem (KES/acre)` / aVec[which(names(aVec) == "`Total P&D chem (kg/acre)`")]
hist(log(df_mod1$`Total P&D chem (KES/acre)`))
plot(log(x), log(y))
dfx <- data.frame(lx = log(x), ly = log(y))
dfx <- dfx[-which(is.infinite(dfx$lx)),]
dfx <- dfx[-which(is.infinite(dfx$ly)),]
summary(lm(ly~lx, dfx))
#---
kVec <- mod1$coefficients[-which(coefsNames %in% input_vars[-c(1, 3:4)])]
kVec <- kVec[-1]
#---
# Optimal farm size
# (Optimal area allocated to maize)
Astar <- (1 + kVec["acres"]) / h
hist(log(Astar))
hist(log(exp(df_mod1$acres) - Astar)) #nice
hist(df_mod1$acres - log(Astar))
hist(df_mod1$acres)
#(1 + mod2$coefficients["acres"]) / mod2$coefficients["Cstar"]
#------------------------------------------------------------------------
lwMat <- as.matrix(log(df_mod[, priceVars]))
for(i in 1:ncol(lwMat)){
  indInf <- which(is.infinite(lwMat[, i]))
  print(length(indInf))
  if(length(indInf) != 0){
  lwMat[indInf, i] <- 0
  }
}

betaW <- exp(lwMat %*% aVec)
hist(betaW)
alogaVec <- aVec * log(aVec)
betaA <- exp(inputMat %*% alogaVec)
hist(betaA)
names(kVec) <- gsub("`", "", names(kVec))
lkMat <- as.matrix(df_mod[, names(kVec)])
betaK <- exp(lkMat %*% kVec)
hist(betaK)
beta <- betaA * betaK / betaW
hist(log(beta))
a0 <- mod1$coefficients[1]
y0 <- exp(a0)
EyStar <- y0 * beta * (Cstar / h)^h
#EyStar <- (y0 * beta * (CtildeStar / h)^h)^(1 / (1-h))
yStar <- exp(df_mod$`yield (kg/acre)`)
hist(log(EyStar/yStar), breaks = 20)
ERstar <- EyStar * df_mod$P
hist(log(ERstar / Cstar))
lambdaCstarP <- y0 * beta * df_mod$P * (Cstar / h)^(h - 1)
lambdaRhoC <- ERstar / Cstar * h
hist(log(lambdaCstarP))
hist(log(lambdaRhoC))
#hist(log(ERstar / Cstar * 1 / lambdaCstarP))
# How many are rational
length(lambdaCstarP[lambdaCstarP > h]) / length(lambdaCstarP)
#-----------------------------------------------------------------------
# Looking at expenditure shares w.r.t. total expenditure
# hist(df_mod$`seed (KES/acre)` - df_mod$Cstar)
# hist(df_mod$`CAN (26:0:0) (KES/acre)` - df_mod$Cstar)
# plot(df_mod$Cstar, df_mod$`seed (kg/acre)`)
# plot(df_mod$Cstar, df_mod$`dap (kg/acre)`)
# plot(df_mod$Cstar, df_mod$`CAN (26:0:0) (kg/acre)`)
# plot(df_mod$Cstar, df_mod$`UREA (46:0:0) (kg/acre)`)
df_look <- df_mod[, c("Cstar", input_vars[-c(1)])]
colnames(df_look)[1] <- "lCstar"
gathercols <- colnames(df_look)[-c(1)]
df_plot <- df_look %>% gather_("type", "lVal", gathercols) %>%
  as.data.frame()
rm0 <- which(df_plot$lVal == 0)
if(length(rm0) != 0){
  df_plot <- df_plot[-rm0, ]
}
# rmInf <- which(is.infinite(df_plot$lVal))
# if(length(rmInf) != 0){
#   df_plot <- df_plot[-rmInf, ]
# }
#df_plot <- subset(df_plot, dist %in% theseDist)
typeVec <- unique(df_plot$type)
yInt <- c()
slope <- c()
for(i in 1:length(typeVec)){
  thisType <- typeVec[i]
  mod <- lm(lCstar ~ lVal, subset(df_plot, type == thisType))
  print(thisType)
  print(summary(mod))
  yInt[i] <- coefficients(mod)[1]
  slope[i] <- coefficients(mod)[2]
}
names(slope) <- typeVec
names(yInt) <- typeVec
slope

#df_plot$div <- df_plot$Cstar / df_plot$Val
#gg <- ggplot(df_plot, aes(x = div))
gg <- ggplot(df_plot, aes(x = lCstar, y = lVal))
#gg <- gg + geom_histogram()
gg <- gg + geom_point()
#gg <- gg + geom_smooth(method = "lm")
gg <- gg + geom_abline(slope = 1, intercept = 0, color = "coral")
gg <- gg + facet_wrap(~type)#, scales = "free")
#gg <- gg + facet_grid(Type~dist)#, scales = "free_y")
gg
# df_hist <- subset(df_plot, type == "CAN (26:0:0) (kg/acre)" &
#                     abs(div) < 10)
# hist(df_hist$div, breaks = 20)
thisType <- "manure (kg/acre)"
thisType <- "manure (kg/acre)"
df_plot2 <- subset(df_plot, type == thisType)
gg <- ggplot(df_plot2, aes(x = lCstar, y = lVal))
gg <- gg + geom_point()
gg <- gg + geom_abline(slope = 1, intercept = 0, color = "coral")
gg
#-------------------------------------------------------------

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
this_file <- "Kenya Tegemeo maize and beans 2007.csv"
this_folder <- "D:/OneDrive - CGIAR/Documents 1/CIAT 2/FnM Initiative/New PE Model/Tegemeo Data/"
#this_folder <- "C:/Users/bensc/OneDrive/Documents/Data/Tegemeo Data/"
this_filepath <- paste0(this_folder, this_file)
write.csv(df, this_filepath, row.names = F)
this_file <- "Kenya Tegemeo maize bean crop prices 2007.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_cropPrice, this_filepath, row.names = F)
this_file <- "Kenya Tegemeo input prices 2007.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_inputPrices, this_filepath, row.names = F)
this_file <- "Kenya Tegemeo seed prices 2007.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_seedPrice, this_filepath, row.names = F)
this_file <- "Kenya Tegemeo all bean maize input prices long 2007.csv"
this_filepath <- paste0(this_folder, this_file)
write.csv(df_priceLong, this_filepath, row.names = F)
#=========================================================================
# # Jeremiah's run times
# x <- c(100, 1609, 5000, 21000, 42195)#, 51500)
# y <- c(12, 310, 1074, 4980, 10800)#, 28800)
# plot(x, y)
# x <- log(x)
# y <- log(y)
# runMod <- lm(y~x, data.frame(x,y))
# summary(runMod)
# plot(runMod$fitted.values, runMod$residuals)
# runMod$coefficients
