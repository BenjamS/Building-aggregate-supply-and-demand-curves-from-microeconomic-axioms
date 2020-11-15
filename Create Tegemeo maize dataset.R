library(tidyverse)
library(foreign)
#---------------------------------------------------------------------------
de_factorizer <- function(this_df){
  i <- sapply(this_df, is.factor)
  this_df[i] <- lapply(this_df[i], as.character)
  return(this_df)
}
#---------------------------------------------------------------------------
this_folder <- "C:/Users/bensc/OneDrive/Documents/Data/Tegemeo Data/data needed for 2007/"
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
            scost = sum(seedkg, na.rm = T),
            adopter = mean(adopter, na.rm = T))
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
df_field <- df_field %>% group_by(hhid) %>%
  mutate(acres_tot = sum(acres, na.rm = T)) %>% as.data.frame
#---------------------------------------------------------------------------
# Get yield
df_yield <- merge(df_crop, df_field, by = c("hhid", "field"))
df_yield$acres_pct <- df_yield$acres / df_yield$acres_tot
df_yield$yield <- df_yield$kgharv / df_yield$acres
# hist(df_yield$yield)
# hist(df_yield$acres)
#---------------------------------------------------------------------------
# Get inputs
#---------------------------------------------------------------------------
# Fertilizer
this_filepath <- paste0(this_folder, "fert07.dta")
df_fert <- read.dta(this_filepath)
df_fert <- de_factorizer(df_fert)
keep_cols <- c("hhid", "field", "harvest", "ferttype", "fertotal", "fertcost", "pfert")
df_fert <- subset(df_fert[, keep_cols], harvest == "main")
#---
unique(df_fert$ferttype)
ggplot(df_fert, aes(x = ferttype))+geom_histogram(stat = "count")+coord_flip()
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
#---------------------------------------------------------------------------
# Labor
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
df_labor$lab_fam_male <- df_labor$lb05 * df_labor$lb06
df_labor$lab_fam_fem <- df_labor$lb07 * df_labor$lb08
df_labor$lab_fam_ch <- df_labor$lb09 * df_labor$lb10

keep_cols <- c("hhid", "activity", "lb03", "lab_wg", "lab_fam_male",
               "lab_fam_fem", "lab_fam_ch")
df_labor <- df_labor[, keep_cols]
colnames(df_labor)[3] <- "wage (KES/day)"

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
#---
df_labor <- df_labor %>% group_by(hhid) %>%
  summarise(lab_fam_ch = sum(lab_fam_ch, na.rm = T),
            lab_fam_male = sum(lab_fam_male, na.rm = T),
            lab_fam_fem = sum(lab_fam_fem, na.rm = T),
            lab_wg = sum(lab_wg, na.rm = T),
            `wage (KES/day)` = mean(`wage (KES/day)`, na.rm = T))
df_labor$`wage (KES/day)`[which(is.nan(df_labor$`wage (KES/day)`))] <- NA
#---------------------------------------------------------------------------
# Get seed
# Seed qty and cost included in df_crop from croplev07.dta
#---------------------------------------------------------------------------
# Get pesticides
# this_filepath <- paste0(this_folder, "input07.dta")
# df_pest <- read.dta(this_filepath)
# df_pest <- de_factorizer(df_pest)
# df_pest <- subset(df_pest, mcrop %in% c("maize-dry", "maize-green", "beans"))
# unique(df_pest$inputype)
# keep_these <- c("Herbicide", "Pesticide", "Insecticide", "Fungicide")
# df_pest <- subset(df_pest, inputype %in% keep_these)
#---------------------------------------------------------------------------
# Get demographic vars
this_filepath <- paste0(this_folder, "ae_hhsize_07.dta")
df_hhsize <- read.dta(this_filepath)
df_hhsize <- de_factorizer(df_hhsize)
this_filepath <- paste0(this_folder, "demog07.dta")
df_demog <- read.dta(this_filepath)
df_demog <- de_factorizer(df_demog)
colnames(df_demog)[5] <- "gend"
df_demog <- merge(df_demog[, c("hhid", "age", "gend")], df_hhsize, by = "hhid")
df_demog$gend[which(df_demog$gend == "male")] <- 0
df_demog$gend[which(df_demog$gend == "female")] <- 1
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
#---------------------------------------------------------------------------
# Get price info
this_filepath <- paste0(this_folder, "pricecrop.dta")
df_cropPrice <- read.dta(this_filepath)
df_cropPrice <- de_factorizer(df_cropPrice)
#unique(df_cropPrice$crop)
df_cropPrice <- subset(df_cropPrice, crop %in% c("maize-dry", "beans"))
# Note price for green maize is about half of price for dry maize
df_cropPrice$crop[grep("maize", df_cropPrice$crop)] <- "maize"

"pricefert.dta"

