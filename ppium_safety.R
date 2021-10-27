#### safety data api pullback ####
rm(list=ls())
library(data.table)
library(jsonlite)

# safety data url + api key
url <- 'https://api.elsevier.com/pharma/safety/search?APIKey=b357b739c13c72c788e021829ba42545'

total_count <- 2210522  #set total_count to the total row length
# can find total by using url above or running first few rows
firstRows <- unique(c(seq(1, total_count, 500), total_count))  #then get the first row starting at 1 and going by 500 until the end of the list (including the final value in case it is not even)
data_all <- data.table()

for(i in firstRows){
  y <- paste0(url,"&limitation.firstRow=",i)
  ppium1 <- fromJSON(txt=y)
  ppium_dt1 <- as.data.table(do.call(c, unlist(ppium1, recursive=FALSE)))
  data_all <- rbind(data_all, ppium_dt1, fill=TRUE)
}

#save.image("safety_ppium_6_25_20.RData") # change to current date 

#### ppium_api table (pharmapendium) ####

setwd("C:/Users/CAPUND/OneDrive - Pfizer/Desktop/ppium_db/CompTox_auto_comptox_drug")
library(data.table)
library(RMySQL)
library(dplyr)

# db connection to pharmapendium db
zoomapcon <- function(){dbConnect(MySQL(), user='martm255',
                                  password='GiaRose#0509',
                                  dbname='pharmapendium',
                                  host='10.131.112.20')}
zoomapdb <- zoomapcon()

# load rds/RData from api pull
load("safety_ppium_6_25_20.RData", envir = parent.frame(), verbose = FALSE)

# sub and set new names, may need to change to specific col names
data_sub<- data_all[, c(3:31)]
setnames(data_sub, old = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29),
         new=c("ppium_id", "drug","doc_srcshort", "doc_src", "doc_year", "historic",
               "article", "heading", "doc_id", "doc_nm", "doc_type", "doc_length", "doc_pg", "doc_file", "doc_sect", "doc_citation",
               "doc_issn", "journal", "doc_pgs", "doc_vol",
               "doc_doi", "dose", "dose_type", "pt", "species", "src", "smiles", "doc_year_2", "route"))

# map in meddra from ppium_meddra table in pharmapendium 
map_query <- "SELECT
ppium_meddra.*
FROM ppium_meddra;"
map <- as.data.table(zoomapdb %>% dbGetQuery(map_query))

data_merge <- merge(data_sub, map, by = "pt", allow.cartesian = TRUE)

# inital dose clean up, ~ 3 minutes locally
data_merge$units <- gsub('[[:digit:]]+', '', data_merge$dose)
data_merge$units_cleaned <- gsub('[[:digit:]]+', '', data_merge$dose)
data_merge$units_cleaned<- gsub("-", "", data_merge$units_cleaned)
data_merge$units_cleaned<- gsub("\\.", "", data_merge$units_cleaned)
data_merge$units_cleaned<- gsub(">", "", data_merge$units_cleaned)
data_merge$units_cleaned<- gsub(" ", "", data_merge$units_cleaned)

data_merge$value<- gsub("m.*", "", data_merge$dose)
data_merge$value<- gsub("-.*", "", data_merge$value)
data_merge$value<- gsub("ug.*", "", data_merge$value)
data_merge$value<- gsub("g.*", "", data_merge$value)
data_merge$value<- gsub("IU.*", "", data_merge$value)
data_merge$value<- gsub("d.*", "", data_merge$value)
data_merge$value<- gsub("%.*", "", data_merge$value)
data_merge$value<- gsub(">", "", data_merge$value)


ppium_api <- data_merge # all species saved up front
#add species_category/ keep preclinical species of interest
monkey_category <- grep("*.onkey.*", ppium_api$species, value = TRUE)
monkey_category <- unique(monkey_category)
monkey_category <- c(monkey_category, "Baboon")
for(i in monkey_category) {
  ppium_api <- ppium_api[species==i,species_category:="Non-Rodent"]
}
grep("*.rat", ppium_api$species, value = TRUE)
mice_category <- grep("*.ouse.*", ppium_api$species, value = TRUE) 
mice_category <- unique(mice_category)
rodent_category <- c("Rice rat", "Grass rat", "Rat", "Rodent (unspecified)", mice_category)
for(i in rodent_category) {
  ppium_api <- ppium_api[species==i,species_category:="Rodent"]
}

ppium_api <- ppium_api[species=="Human",species_category:="Human"]

# species_pf for pfizer preclin of interest
ppium_api <- ppium_api[species=="Human",species_pf:="Human"]
ppium_api <- ppium_api[species=="Dog",species_pf:="Dog"]
ppium_api <- ppium_api[species=="Rabbit",species_pf:="Rabbit"]
for(i in monkey_category) {
  ppium_api <- ppium_api[species==i,species_pf:="Monkey"]
}
for(i in mice_category) {
  ppium_api <- ppium_api[species==i,species_pf:="Mouse"]
}
rat_category <- c("Rice rat", "Grass rat", "Rat", "Rodent (unspecified)")
for(i in rat_category) {
  ppium_api <- ppium_api[species==i,species_pf:="Rat"]
}

# preclin/ clin species
ppium_api <- ppium_api[species=="Human", clin_preclin :="Clinical"]
ppium_api <- ppium_api[species_category !="Human" & !is.na(species_category), clin_preclin :="Preclinical"]

# species_pf for pfizer preclin of interest
ppium_api <- ppium_api[species=="Human",species_pf:="Human"]
ppium_api <- ppium_api[species=="Dog",species_pf:="Dog"]

# added to remove strange carry over of non-important (pfizer) species
#dose_info <- dose_info[species_pf=="Therapeutic", species_pf:= NA]

# rename columns
setnames(ppium_api, old = c("species", "species_category"), 
         new = c("species_src", "rodent_non_rodent"))

### ppium (safety) data
ppium_api.sql1 <- "DROP TABLE IF EXISTS ppium_api;"      
ppium_api.sql2 <- "CREATE TABLE IF NOT EXISTS ppium_api (
pt varchar(100) NOT NULL,
ppium_id varchar(50) NOT NULL,
drug varchar(300) DEFAULT NULL,
doc_srcshort varchar(25) DEFAULT NULL,
doc_src varchar(50) DEFAULT NULL,
doc_year varchar(10) DEFAULT NULL,
historic varchar(10) NOT NULL,
article varchar(50) NOT NULL,
heading varchar(50) NOT NULL,
doc_id varchar(50) NOT NULL,
doc_nm varchar(50) NOT NULL,
doc_type varchar(50) NOT NULL,
doc_length varchar(10) NOT NULL,
doc_pg varchar(10) NOT NULL,
doc_file varchar(50) DEFAULT NULL,
doc_sect varchar(50) DEFAULT NULL,
doc_citation varchar(100) DEFAULT NULL,
doc_issn varchar(50) NOT NULL,
journal varchar(50) NOT NULL,
doc_pgs varchar(10) NOT NULL,
doc_vol varchar(50) NOT NULL,
doc_doi varchar(100) NOT NULL,
dose varchar(100) DEFAULT NULL,
dose_type varchar(25) DEFAULT NULL,
species_src varchar(25) DEFAULT NULL,
src varchar(50) NOT NULL,
smiles varchar(100) DEFAULT NULL,
doc_year_2 varchar(10) NOT NULL,
route varchar(50) NOT NULL,
soc varchar(100) DEFAULT NULL,
hlgt varchar(100) DEFAULT NULL,
hlt varchar(100) DEFAULT NULL,
units varchar(100) DEFAULT NULL,
units_cleaned varchar(50) DEFAULT NULL,
value varchar(50) DEFAULT NULL,
rodent_non_rodent varchar(50) DEFAULT NULL,
species_pf varchar(50) DEFAULT NULL,
clin_preclin varchar(50) DEFAULT NULL
) ENGINE = MYISAM DEFAULT CHARSET = utf8;"
ppium_api.sql3 <- "ALTER TABLE ppium_api
ADD KEY pt (pt),
ADD KEY ppium_id (ppium_id),
ADD KEY drug (drug),
ADD KEY doc_srcshort (doc_srcshort),
ADD KEY doc_src (doc_src),
ADD KEY doc_year (doc_year),
ADD KEY historic (historic),
ADD KEY article (article),
ADD KEY heading (heading),
ADD KEY doc_id (doc_id),
ADD KEY doc_nm (doc_nm),
ADD KEY doc_type (doc_type),
ADD KEY doc_length (doc_length),
ADD KEY doc_pg (doc_pg),
ADD KEY doc_file (doc_file),
ADD KEY doc_sect (doc_sect),
ADD KEY doc_citation (doc_citation),
ADD KEY doc_issn (doc_issn),
ADD KEY journal (journal),
ADD KEY doc_pgs (doc_pgs),
ADD KEY doc_vol (doc_vol),
ADD KEY doc_doi (doc_doi),
ADD KEY dose (dose),
ADD KEY dose_type (dose_type),
ADD KEY species_src (species_src),
ADD KEY src (src),
ADD KEY smiles (smiles),
ADD KEY doc_year_2 (doc_year_2),
ADD KEY route (route),
ADD KEY soc (soc),
ADD KEY hlgt (hlgt),
ADD KEY hlt (hlt),
ADD KEY units (units),
ADD KEY units_cleaned (units_cleaned),
ADD KEY value (value),
ADD KEY rodent_non_rodent (rodent_non_rodent),
ADD KEY species_pf (species_pf),
ADD KEY clin_preclin (clin_preclin)
;"
dbSendQuery(conn = zoomapdb, ppium_api.sql1)
dbSendQuery(conn = zoomapdb, ppium_api.sql2)
dbSendQuery(conn = zoomapdb, ppium_api.sql3)

write.table(ppium_api,file="tmp.txt", fileEncoding ="utf8", row.names = FALSE)
ppium_api <- read.table(file="tmp.txt",encoding="latin1", fill = TRUE, header = TRUE) #header was false

dbWriteTable(conn  = zoomapdb,
             name  = "ppium_api",
             value = ppium_api,
             row.names = FALSE,
             append = TRUE)

#### cleaned_dose table (pharmapendium) ####
library(data.table)
library(RMariaDB)
library(stringr)

# make db connection, do not need if running above all at once
dbc <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                           dbname = "pharmapendium",
                           host= "10.131.112.20",
                           username = 'zoomap',
                           password = 'ZooMap#2018')

# read in previous table from above
dose_info <- as.data.table(dbGetQuery(dbc,
                                      "SELECT ppium_api.* FROM ppium_api"))
setwd("C:/Users/CAPUND/OneDrive - Pfizer/Desktop/ppium_db/CompTox_auto_comptox_drug")
# read in manually created table from Matt, may need updates
dose_curated <- fread("ppm_dose_curation_upload_20200331.csv")
dose_curated[ , cid:= NULL]
dose_curated[ , dups := duplicated(dose)]
dose_curated <- dose_curated[dups==F]
dose_curated[ , dups := NULL]
dose_merge1 <- merge(dose_info, dose_curated, by = "dose")

# read in conversion csv, static 
dose_conv <- fread("dose_unit_conversion.csv")
dose_conv <- as.data.table(dose_conv)
dose_conv <- dose_conv[, c(1,3,4)]
dose_merge <- merge(dose_merge1, dose_conv, by = "dose_unit", all = TRUE)

#setorder(dose_merge, c()) #if we want to change order of table

# start of db creation
library(RMySQL)
zoomapcon <- function(){dbConnect(MySQL(), user='martm255',
                                  password='GiaRose#0509',
                                  dbname='pharmapendium',
                                  host='10.131.112.20')}
zoomapdb <- zoomapcon()
### ppium (safety) data with dose
cleaned_dose.sql1 <- "DROP TABLE IF EXISTS cleaned_dose;"      
cleaned_dose.sql2 <- "CREATE TABLE IF NOT EXISTS cleaned_dose (
dose_unit varchar(15) NOT NULL,
dose varchar(100) NOT NULL,
pt varchar(100) NOT NULL,
ppium_id varchar(50) NOT NULL,
drug varchar(300) DEFAULT NULL,
doc_srcshort varchar(25) DEFAULT NULL,
doc_src varchar(50) DEFAULT NULL,
doc_year varchar(10) DEFAULT NULL,
historic varchar(10) NOT NULL,
article varchar(50) NOT NULL,
heading varchar(50) NOT NULL,
doc_id varchar(50) NOT NULL,
doc_nm varchar(50) NOT NULL,
doc_type varchar(50) NOT NULL,
doc_length varchar(10) NOT NULL,
doc_pg varchar(10) NOT NULL,
doc_file varchar(50) DEFAULT NULL,
doc_sect varchar(50) DEFAULT NULL,
doc_citation varchar(100) DEFAULT NULL,
doc_issn varchar(50) NOT NULL,
journal varchar(50) NOT NULL,
doc_pgs varchar(10) NOT NULL,
doc_vol varchar(50) NOT NULL,
doc_doi varchar(100) NOT NULL,
dose_type varchar(25) DEFAULT NULL,
species_src varchar(25) DEFAULT NULL,
src varchar(50) NOT NULL,
smiles varchar(100) DEFAULT NULL,
doc_year_2 varchar(10) NOT NULL,
route varchar(50) NOT NULL,
soc varchar(100) DEFAULT NULL,
hlgt varchar(100) DEFAULT NULL,
hlt varchar(100) DEFAULT NULL,
units varchar(100) DEFAULT NULL,
units_cleaned varchar(50) DEFAULT NULL,
value varchar(50) DEFAULT NULL,
rodent_non_rodent varchar(50) DEFAULT NULL,
species_pf varchar(50) DEFAULT NULL,
clin_preclin varchar(50) DEFAULT NULL,
dose_min varchar(50) DEFAULT NULL,
dose_max varchar(50) DEFAULT NULL,
dose_conf varchar(50) DEFAULT NULL,
dose_unit_type varchar(50) DEFAULT NULL,
duration varchar(50) DEFAULT NULL,
duration_unit varchar(50) DEFAULT NULL,
frequency varchar(50) DEFAULT NULL,
frequency_unit varchar(50) DEFAULT NULL,
rate varchar(50) DEFAULT NULL,
rate_unit varchar(50) DEFAULT NULL,
volume varchar(50) DEFAULT NULL,
volume_unit varchar(50) DEFAULT NULL,
lifestage varchar(50) DEFAULT NULL,
dose_conversion_factor varchar(20) DEFAULT NULL,
dose_standard_unit_primary varchar(20) DEFAULT NULL
) ENGINE = MYISAM DEFAULT CHARSET = utf8;"
cleaned_dose.sql3 <- "ALTER TABLE cleaned_dose
ADD KEY dose_unit (dose_unit),
ADD KEY dose (dose),
ADD KEY pt (pt),
ADD KEY ppium_id (ppium_id),
ADD KEY drug (drug),
ADD KEY doc_srcshort (doc_srcshort),
ADD KEY doc_src (doc_src),
ADD KEY doc_year (doc_year),
ADD KEY historic (historic),
ADD KEY article (article),
ADD KEY heading (heading),
ADD KEY doc_id (doc_id),
ADD KEY doc_nm (doc_nm),
ADD KEY doc_type (doc_type),
ADD KEY doc_length (doc_length),
ADD KEY doc_pg (doc_pg),
ADD KEY doc_file (doc_file),
ADD KEY doc_sect (doc_sect),
ADD KEY doc_citation (doc_citation),
ADD KEY doc_issn (doc_issn),
ADD KEY journal (journal),
ADD KEY doc_pgs (doc_pgs),
ADD KEY doc_vol (doc_vol),
ADD KEY doc_doi (doc_doi),
ADD KEY dose_type (dose_type),
ADD KEY species_src (species_src),
ADD KEY src (src),
ADD KEY smiles (smiles),
ADD KEY doc_year_2 (doc_year_2),
ADD KEY route (route),
ADD KEY soc (soc),
ADD KEY hlgt (hlgt),
ADD KEY hlt (hlt),
ADD KEY units (units),
ADD KEY units_cleaned (units_cleaned),
ADD KEY value (value),
ADD KEY rodent_non_rodent (rodent_non_rodent),
ADD KEY species_pf (species_pf),
ADD KEY clin_preclin (clin_preclin),
ADD KEY dose_min (dose_min),
ADD KEY dose_max (dose_max),
ADD KEY dose_conf (dose_conf),
ADD KEY dose_unit_type (dose_unit_type),
ADD KEY duration (duration),
ADD KEY duration_unit (duration_unit),
ADD KEY frequency (frequency),
ADD KEY frequency_unit (frequency_unit),
ADD KEY rate (rate),
ADD KEY rate_unit (rate_unit),
ADD KEY volume (volume),
ADD KEY volume_unit (volume_unit),
ADD KEY lifestage (lifestage),
ADD KEY dose_conversion_factor (dose_conversion_factor),
ADD KEY dose_standard_unit_primary (dose_standard_unit_primary)
;"
dbSendQuery(conn = zoomapdb, cleaned_dose.sql1)
dbSendQuery(conn = zoomapdb, cleaned_dose.sql2)
dbSendQuery(conn = zoomapdb, cleaned_dose.sql3)

write.table(dose_merge,file="tmp.txt", fileEncoding ="utf8", row.names = FALSE)
dose_merge <- read.table(file="tmp.txt",encoding="latin1", fill = TRUE, header = TRUE) #header was false

dbWriteTable(conn  = zoomapdb,
             name  = "cleaned_dose",
             value = dose_merge,
             row.names = FALSE,
             append = TRUE)

#### drug_safety table (comptox_drug) ####
library(RMySQL)
library(data.table)
# make first connection to read in dleaned_dose from pharmapendium db
zoomapcon <- function(){dbConnect(MySQL(), user='martm255',
                                  password='GiaRose#0509',
                                  dbname='pharmapendium',
                                  host='10.131.112.20')}
zoomapdb <- zoomapcon()

drug_safety <- as.data.table(dbGetQuery(zoomapdb,
                                        "SELECT cleaned_dose.* FROM cleaned_dose"))
drug_safety <- drug_safety[doc_srcshort=="EMEA", doc_srcshort :="EMA"]

# drug safety links
drug_safety[!is.na(doc_file), link := paste0("https://www.pharmapendium.com/browse/",
                                             tolower(doc_srcshort),
                                             "/",
                                             gsub(pattern = " ",
                                                  replacement = "%20",
                                                  drug,
                                                  fixed = TRUE),
                                             "/",
                                             doc_file,
                                             "?reference=",
                                             doc_pg)]

#### adding doc preference scores ###
#safety_fda <- drug_safety[doc_srcshort=="FDA"]

drug_safety <- drug_safety[doc_type=="Label", doc_preference :="1"]
drug_safety <- drug_safety[doc_type=="Pharmacology Review", doc_preference :="2"]
drug_safety <- drug_safety[doc_type=="Medical/Clinical Review", doc_preference :="3"]
drug_safety <- drug_safety[doc_type=="Approval Package", doc_preference :="4"]
drug_safety <- drug_safety[doc_type=="Printed Labeling", doc_preference :="5"]

#safety_ema <- drug_safety[doc_srcshort=="EMA"]

drug_safety <- drug_safety[doc_type=="ANNEX I", doc_preference :="1"]
drug_safety <- drug_safety[doc_type=="Assessment Report", doc_preference :="2"]
drug_safety <- drug_safety[doc_type=="Scientific Discussion", doc_preference :="3"]
drug_safety <- drug_safety[doc_type=="Public Assessment Report", doc_preference :="4"]
drug_safety <- drug_safety[doc_type=="Withdrawal Public Statement", doc_preference :="5"]
drug_safety <- drug_safety[doc_type=="Procedural Steps", doc_preference :="6"]

# rename mosby and tox to LIT
drug_safety <- drug_safety[doc_srcshort=="TOXICITY", doc_srcshort :="LIT"]
drug_safety <- drug_safety[doc_srcshort=="MOSBY", doc_srcshort :="LIT"]

drug_safety <- drug_safety[, list(dose_unit, pt, drug, doc_type, dose, dose_type, 
                                  src, route, units_cleaned, value, species_pf, dose_min, dose_max, dose_conf,
                                  dose_unit_type, duration, duration_unit, frequency, frequency_unit, rate,
                                  rate_unit, volume, volume_unit, lifestage, dose_conversion_factor,
                                  dose_standard_unit_primary, link, doc_srcshort, doc_preference)]

setnames(drug_safety, old = c("species_pf"), 
         new = c("species"))
# start of Matt's code here #
dat <- drug_safety
dat_original <- copy(dat)
#dat <- copy(dat_original) #get back to original state
dat[ , drug := tolower(drug)]
#dat <- dat[!grepl(";", drug)] #remove combo treatments, turn off or skip for testing
dat <- dat[!is.na(species)]
dat[ , species := tolower(species)]
dat[ , route := tolower(route)]
dat[ , dose_type := tolower(dose_type)]
dat[ , src := tolower(src)]
dat[ , pt := tolower(pt)]

setnames(dat, "pt", "effect")
setnames(dat, "dose", "dose_src")
setnames(dat, "dose_min", "dose")
setnames(dat, "src", "doc_src")

dat[ , dose := as.numeric(dose)]
dat[ , dose_conversion_factor := as.numeric(dose_conversion_factor)]
dat[ , dose_std := dose * dose_conversion_factor]
dat[ , dose_std_unit := dose_standard_unit_primary]

# need to insert to switch to comptox_drug 
dbc <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                           dbname = "comptox_drug",
                           host= "10.131.112.20",
                           username = 'zoomap',
                           password = 'ZooMap#2018')

dc <- as.data.table(dbGetQuery(dbc,
                               "SELECT * FROM drug_unit_conversions;"))
dat <- merge(dat, dc, by = "species")

sort(table(dat$dose_unit_type), decreasing=T)

dat[dose_unit_type == "mass",      dose_mg := dose_std]
dat[dose_unit_type == "mass_mass", dose_mg := dose_std * ref_weight]
#not convinced this is appropriate conversion bc the area is likely applied area not bsa
#dat[dose_unit_type == "mass_area", dose_mg := dose_std * 1E5 * bsa]  

dat[dose_unit_type == "mass_mass", dose_mgkg := dose_std]
dat[dose_unit_type == "mass",      dose_mgkg := dose_std / ref_weight]

dat[dose_unit_type == "mass_mass", dose_mgm2 := dose_std * ref_weight / bsa]
dat[dose_unit_type == "mass",      dose_mgm2 := dose_std / bsa]

dat[species=="human", dose_hed  := dose_mgkg]
dat[!species=="human", dose_hed := dose_mgkg * hed_conv_mult]

dat[!is.na(dose_hed), c("dose_all", "dose_all_unit") := list(dose_hed, 'mg/kg (hed)')]
dat[is.na(dose_hed), c("dose_all", "dose_all_unit") := list(dose, dose_std_unit)]
dat[is.na(dose_all), c("dose_all", "dose_all_unit") := list(dose, dose_unit)]

dat[ , dose_typical_human_mg := median(dose_mg[species=='human'], na.rm =T),
     by = drug]
dat[ , dose_typical_human_mgkg := median(dose_mgkg[species=='human'], na.rm=T),
     by = drug]

dat[ , dose_margin := dose_hed/dose_typical_human_mgkg]

out <- dat[ , 
            list(drug, effect, 
                 species, route,
                 dose_src,
                 dose, dose_unit,
                 dose_unit_type,
                 dose_std, dose_std_unit,
                 dose_mg, dose_mgkg,
                 dose_mgm2, dose_hed,
                 dose_all, dose_all_unit,
                 dose_typical_human_mg,
                 dose_typical_human_mgkg,
                 dose_margin,
                 dose_type, doc_srcshort, doc_src, doc_preference, link)]
setnames(out, old = c("doc_srcshort", "doc_src", "doc_preference"), 
         new = c("doc_src", "doc_type", "doc_order"))

# add T/F column for multiple drugs
out[drug %like% ";", multiple := "TRUE"]
out[is.na(multiple), multiple := "FALSE"]

# db connection/upload
zoomapcon <- function(){dbConnect(MySQL(), user='martm255',
                                  password='GiaRose#0509',
                                  dbname='comptox_drug',
                                  host='10.131.112.20')}
zoomapdb <- zoomapcon()

drug_safety2.sql1 <- "DROP TABLE IF EXISTS drug_safety;"      
drug_safety2.sql2 <- "CREATE TABLE IF NOT EXISTS drug_safety (
drug varchar(300) NOT NULL,
effect varchar(150) NOT NULL,
species varchar(10) DEFAULT NULL,
route varchar(100) DEFAULT NULL,
dose_src varchar(100) DEFAULT NULL,
dose double DEFAULT NULL, # remove specificity of double
dose_unit varchar(10) NOT NULL,
dose_unit_type varchar(25) NOT NULL,
dose_std double DEFAULT NULL,
dose_std_unit varchar(25) DEFAULT NULL,
dose_mg double DEFAULT NULL,
dose_mgkg double DEFAULT NULL,
dose_mgm2 double DEFAULT NULL,
dose_hed double DEFAULT NULL,
dose_all double DEFAULT NULL,
dose_all_unit varchar(25) DEFAULT NULL,
dose_typical_human_mg double DEFAULT NULL,
dose_typical_human_mgkg double DEFAULT NULL,
dose_margin double DEFAULT NULL,
dose_type varchar(25) DEFAULT NULL,
doc_src varchar(20) DEFAULT NULL,
doc_type varchar(100) DEFAULT NULL,
doc_order integer(1) DEFAULT NULL,
link varchar(250) DEFAULT NULL,
multiple varchar(5) DEFAULT NULL
) ENGINE = MYISAM DEFAULT CHARSET = utf8;"
drug_safety2.sql3 <- "ALTER TABLE drug_safety
ADD KEY drug (drug),
ADD KEY effect (effect),
ADD KEY species (species),
ADD KEY route (route),
ADD KEY dose_src (dose_src),
ADD KEY dose (dose),
ADD KEY dose_unit (dose_unit),
ADD KEY dose_unit_type (dose_unit_type),
ADD KEY dose_std (dose_std),
ADD KEY dose_std_unit (dose_std_unit),
ADD KEY dose_mg (dose_mg),
ADD KEY dose_mgkg (dose_mgkg),
ADD KEY dose_mgm2 (dose_mgm2),
ADD KEY dose_hed (dose_hed),
ADD KEY dose_all (dose_all),
ADD KEY dose_all_unit (dose_all_unit),
ADD KEY dose_typical_human_mg (dose_typical_human_mg),
ADD KEY dose_typical_human_mgkg (dose_typical_human_mgkg),
ADD KEY dose_margin (dose_margin),
ADD KEY dose_type (dose_type),
ADD KEY doc_src (doc_src),
ADD KEY doc_type (doc_type),
ADD KEY doc_order (doc_order),
ADD KEY link (link),
ADD KEY multiple (multiple)
;"
dbSendQuery(conn = zoomapdb, drug_safety2.sql1)
dbSendQuery(conn = zoomapdb, drug_safety2.sql2)
dbSendQuery(conn = zoomapdb, drug_safety2.sql3)

write.table(out,file="tmp.txt", fileEncoding ="utf8", row.names = FALSE)
out <- read.table(file="tmp.txt",encoding="latin1", fill = TRUE, header = TRUE) #header was false

dbWriteTable(conn  = zoomapdb,
             name  = "drug_safety",
             value = out,
             row.names = FALSE,
             append = TRUE)
