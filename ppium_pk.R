#### pk data api pullback ####
rm(list=ls())
library(data.table)
library(jsonlite)
url <- 'https://api.elsevier.com/pharma/pk/search?APIKey=035e274e3b4e2f68e0723e7550154328'
#need code to get N or the total count for the query

total_count <- 2175249 #set total_count to the total
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
setwd("C:/Users/CAPUND/OneDrive - Pfizer/Desktop/ppium_db/CompTox_auto_comptox_drug")
library(data.table)
library(RMySQL)
library(dplyr)

zoomapcon <- function(){dbConnect(MySQL(), user='martm255',
                                  password='GiaRose#0509',
                                  dbname='pharmapendium',
                                  host='10.131.112.20')}
zoomapdb <- zoomapcon()
load("pk_api_8_20_20.RData", envir = parent.frame(), verbose = FALSE)

data_sub<- data_all[, c(3,5:53)]

setnames(data_sub, old = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,
                           22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                           41,42,43,44,45,46,47,48,49,50),
         new=c("ppium_id", "drug","doc_id", "doc_nm", "doc_srcshort", "doc_src", "doc_type", "doc_year",
               "doc_length", "doc_pg", "historic", "doc_file", "doc_feature", "doc_citation",
               "journal", "doc_pgs", "doc_vol", "doc_doi", "doc_issn", "structure",
               "smiles", "species", "study_group", "study_number_internal", "dose",
               "duration", "route", "parameter", "param_display", "display_value", 
               "value_original", "value_normalized", "unit_original", "unit_normal",
               "comments", "metabolites", "tissue_specific", "source", "doc_year2",
               "study_number", "sample_size", "sex", "study_name", "standard_dev",
               "age", "assay", "concomitant", "time_of_measure", "pk_analysis", "radiolabel"))

pk_api <- data_sub 
#add species_category
monkey_category <- grep("*.onkey.*", pk_api$species, value = TRUE)
monkey_category <- unique(monkey_category)
monkey_category <- c(monkey_category, "Baboon")
for(i in monkey_category) {
  pk_api <- pk_api[species==i,species_category:="Non-Rodent"]
}
grep("*.rat", pk_api$species, value = TRUE)
mice_category <- grep("*.ouse.*", pk_api$species, value = TRUE) 
mice_category <- unique(mice_category)
rodent_category <- c("Rice rat", "Grass rat", "Rat", "Rodent (unspecified)", mice_category)
for(i in rodent_category) {
  pk_api <- pk_api[species==i,species_category:="Rodent"]
}

pk_api <- pk_api[species=="Human",species_category:="Human"]

# species_pf for pfizer preclin of interest
pk_api <- pk_api[species=="Human",species_pf:="Human"]
pk_api <- pk_api[species=="Dog",species_pf:="Dog"]
pk_api <- pk_api[species=="Rabbit",species_pf:="Rabbit"]
for(i in monkey_category) {
  pk_api <- pk_api[species==i,species_pf:="Monkey"]
}
for(i in mice_category) {
  pk_api <- pk_api[species==i,species_pf:="Mouse"]
}
rat_category <- c("Rice rat", "Grass rat", "Rat", "Rodent (unspecified)")
for(i in rat_category) {
  pk_api <- pk_api[species==i,species_pf:="Rat"]
}

### ppium (pk) data
pk_api.sql1 <- "DROP TABLE IF EXISTS pk_api;"      
pk_api.sql2 <- "CREATE TABLE IF NOT EXISTS pk_api (
ppium_id varchar(50) NOT NULL,
drug varchar(300) DEFAULT NULL,
doc_id varchar(25) DEFAULT NULL,
doc_nm varchar(100) DEFAULT NULL,
doc_srcshort varchar(10) DEFAULT NULL,
doc_src varchar(30) NOT NULL,
doc_type varchar(100) NOT NULL,
doc_year varchar(10) NOT NULL,
doc_length varchar(20) NOT NULL,
doc_pg varchar(10) NOT NULL,
historic varchar(10) NOT NULL,
doc_file varchar(50) NOT NULL,
doc_feature varchar(100) NOT NULL,
doc_citation varchar(50) DEFAULT NULL,
journal varchar(50) NOT NULL,
doc_pgs varchar(10) NOT NULL,
doc_vol varchar(50) NOT NULL,
doc_doi varchar(100) NOT NULL,
doc_issn varchar(10) DEFAULT NULL,
structure varchar(20) DEFAULT NULL,
smiles varchar(50) DEFAULT NULL,
species varchar(50) NOT NULL,
study_group varchar(100) DEFAULT NULL,
study_number_internal varchar(100) NOT NULL,
dose varchar(50) NOT NULL,
duration varchar(100) DEFAULT NULL,
route varchar(100) DEFAULT NULL,
parameter varchar(100) DEFAULT NULL,
param_display varchar(100) DEFAULT NULL,
display_value varchar(50) DEFAULT NULL,
value_original varchar(50) DEFAULT NULL,
value_normalized varchar(50) DEFAULT NULL,
unit_original varchar(50) DEFAULT NULL,
unit_normal varchar(50) DEFAULT NULL,
comments varchar(100) DEFAULT NULL,
metabolites varchar(50) DEFAULT NULL,
tissue_specific varchar(40) DEFAULT NULL,
source varchar(50) DEFAULT NULL,
doc_year2 varchar(10) DEFAULT NULL,
study_number varchar(50) DEFAULT NULL,
sample_size varchar(20) DEFAULT NULL,
sex varchar(50) DEFAULT NULL,
study_name varchar(100) DEFAULT NULL,
standard_dev varchar(50) DEFAULT NULL,
age varchar(50) DEFAULT NULL,
assay varchar(50) DEFAULT NULL,
concomitant varchar(50) DEFAULT NULL,
time_of_measure varchar(50) DEFAULT NULL,
pk_analysis varchar(50) DEFAULT NULL,
radiolabel varchar(50) DEFAULT NULL,
species_category varchar(50) DEFAULT NULL,
species_pf varchar(50) DEFAULT NULL
) ENGINE = MYISAM DEFAULT CHARSET = utf8;"
pk_api.sql3 <- "ALTER TABLE pk_api
ADD KEY ppium_id (ppium_id),
ADD KEY drug (drug),
ADD KEY doc_id (doc_id),
ADD KEY doc_nm (doc_nm),
ADD KEY doc_srcshort (doc_srcshort),
ADD KEY doc_src (doc_src),
ADD KEY doc_type (doc_type),
ADD KEY doc_year (doc_year),
ADD KEY doc_length (doc_length),
ADD KEY doc_pg (doc_pg),
ADD KEY historic (historic),
ADD KEY doc_file (doc_file),
ADD KEY doc_feature (doc_feature),
ADD KEY doc_citation (doc_citation),
ADD KEY journal (journal),
ADD KEY doc_pgs (doc_pgs),
ADD KEY doc_vol (doc_vol),
ADD KEY doc_doi (doc_doi),
ADD KEY doc_issn (doc_pgs),
ADD KEY structure (structure),
ADD KEY smiles (smiles),
ADD KEY species (species),
ADD KEY study_group (study_group),
ADD KEY study_number_internal (study_number_internal),
ADD KEY dose (dose),
ADD KEY duration (duration),
ADD KEY route (route),
ADD KEY parameter (parameter),
ADD KEY param_display (param_display),
ADD KEY display_value (display_value),
ADD KEY value_original (value_original),
ADD KEY value_normalized (value_normalized),
ADD KEY unit_normal (unit_normal),
ADD KEY comments (comments),
ADD KEY metabolites (metabolites),
ADD KEY tissue_specific (tissue_specific),
ADD KEY source (source),
ADD KEY doc_year2 (doc_year2),
ADD KEY study_number (study_number),
ADD KEY sample_size (sample_size),
ADD KEY sex (sex),
ADD KEY study_name (study_name),
ADD KEY standard_dev (standard_dev),
ADD KEY age (age),
ADD KEY assay (assay),
ADD KEY concomitant (concomitant),
ADD KEY time_of_measure (time_of_measure),
ADD KEY pk_analysis (pk_analysis),
ADD KEY radiolabel (radiolabel),
ADD KEY species_category (species_category),
ADD KEY species_pf (species_pf)
;"
dbSendQuery(conn = zoomapdb, pk_api.sql1)
dbSendQuery(conn = zoomapdb, pk_api.sql2)
dbSendQuery(conn = zoomapdb, pk_api.sql3)

pk_api <- as.data.table(pk_api)
pk_api[, ] <- lapply(pk_api[, ], as.character)

write.table(pk_api,file="tmp.txt", fileEncoding ="utf8", row.names = FALSE)
pk_api <- read.table(file="tmp.txt",encoding="latin1", fill = TRUE, header = TRUE) #header was false

dbWriteTable(conn  = zoomapdb,
             name  = "pk_api",
             value = pk_api,
             row.names = FALSE,
             append = TRUE)

#### drug_pk table (comptox_drug)  ####
library(data.table)
library(MASS)
library(RMariaDB)

setwd("C:/Users/CAPUND/OneDrive - Pfizer/Desktop/ppium_db/CompTox_auto_comptox_drug/dili_pk_files")

dbc <- RMariaDB::dbConnect(drv = RMariaDB::MariaDB(),
                           dbname = "pharmapendium",
                           host = '10.131.112.20',
                           username = 'ppium2019',
                           password = 'PpIum#2019')

pk_dat_cleanup <- function(pk){
  pk[ , drug := tolower(drug)]
  #pk <- pk[concomitants=="NULL"]
  #pk <- pk[!is.na(unit_normal)]
  pk[ , val_norm := tstrsplit(value_normalized, " ", fixed=TRUE, keep = 1)]
  #pk[grepl(">|<|=", val_norm)]  #limited number.. could resolve but not necessary
  pk[ , val_norm := as.numeric(val_norm)]
  
  pk[ , val_orig := tstrsplit(value_original, " ", fixed=TRUE, keep = 1)]
  #pk[grepl(">|<|=", val_orig)]  #limited number.. could resolve but not necessary
  pk[ , val_orig := as.numeric(val_orig)]
  
  #pk <- pk[!is.na(value)]
  pk[ , dose_src := dose]
  pk[ , c("dose","dose_unit") := tstrsplit(dose_src, " ", fixed=TRUE, keep = 1:2)]
  pk[ , dose := as.numeric(dose)]  #many doses with ranges of values or not reported (removing for now)
  #pk <- pk[!is.na(dose)]  
  #pk <- pk[value!=0]
  pk[grepl("kg", dose_unit), dose := dose*70]
  pk[dose_unit %in% c("g","gm","g/day"), dose := dose*1000]
  pk[dose_unit %in% c("ug","mcg/kg","ug/day","ug/kg"), dose := dose/1000]
  pk[dose_unit %in% c("mg","mg/kg","mg/day","mg/kg/day",
                      "g","gm","g/day","ug","mcg/kg","ug/day","ug/kg"),
     dose_unit_norm := "mg"]
  #pk <- pk[!is.na(dose_unit_norm)]
  pk[ , logdose := round(log10(dose),1)]
  
  return(pk[])
}


drug_pk <- as.data.table(dbGetQuery(dbc,
                                    "SELECT drug, dose, route, duration,
                parameter, param_display, display_value, species_pf, study_group, #added all species
                value_original, unit_original, value_normalized, unit_normal,
                doc_srcshort, doc_type, doc_pg, doc_file, concomitant
                FROM pk_api
                WHERE
                route IN ('Oral','Intravenous','In Vitro', 'Oral/Intravenous') 
                AND metabolites = 'Not metabolites/enantiomers'
                AND tissue_specific = 'Not tissue-specific';"))
# remove species not of importance and rename species_pf to species
drug_pk <- drug_pk[!is.na(species_pf)]
setnames(drug_pk, old = "species_pf", new = "species")

drug_pk_original <- copy(drug_pk)
#drug_pk <- copy(drug_pk_original)
drug_pk[doc_srcshort=="EMEA", doc_srcshort := "EMA"]
drug_pk <- drug_pk[!grepl(";", drug)]
#drug_pk[route=="In Vitro", dose := "1 mg"]
drug_pk <- pk_dat_cleanup(drug_pk)
drug_pk[ , adult_healthy := ifelse(grepl("ealth", study_group) | is.na(study_group), TRUE, FALSE)]
drug_pk[route=="Intravenous", route:="IV"]

ccs <- sort(table(drug_pk$concomitant), decreasing = T)
ccs <- ccs[grep("null|meal|breakfast|lunch|fast|fed|water|liquid",
                tolower(names(ccs)))]
ccs <- ccs[-grep("c(", names(ccs), fixed = T)]
ccs <- ccs[-grep("Nifedipine", names(ccs), fixed = T)]

drug_pk <- drug_pk[concomitant %in% names(ccs)]

drug_pk[!is.na(doc_file), link := paste0("https://www.pharmapendium.com/browse/",
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
#https://www.pharmapendium.com/browse/fda/Fluconazole/bf6eeb39f22cb9cd91be1fe19fd1fa44?reference=4


# units_conv <- drug_pk[is.na(unit_normal) & !is.na(unit_orig),
#         list(cnt = length(unique(drug))),
#         by = list(parameter, unit_orig)][order(cnt, decreasing = T)]
# units_norm <- unique(drug_pk[!is.na(unit_normal),list(parameter, unit_normal)])
# units_norm <- units_norm[!duplicated(parameter)]
# units_conv <- merge(units_conv, units_norm, by = "parameter")
# unique(units_conv[cnt>10,
#                   list(unit_orig, unit_normal)])

drug_pk[is.na(unit_normal) & !is.na(val_orig) & unit_original == "mL/min/kg",
        c("val_norm", "unit_normal") := list(val_orig*70, "mL/min")]
drug_pk[is.na(unit_normal) & !is.na(val_orig) & unit_original == "L/h/kg",
        c("val_norm", "unit_normal") := list(val_orig*70*1000/60, "mL/min")]
drug_pk[is.na(unit_normal) & !is.na(val_orig) & unit_original == "mL/h/kg",
        c("val_norm", "unit_normal") := list(val_orig*70/60, "mL/min")]

drug_pk <- unique(drug_pk)
drug_pk[is.na(unit_original) & is.na(unit_normal),
        unit_normal := "ratio"]

drug_pk[ , parameter_group := parameter]
drug_pk[ , route_group := route]
drug_pk[ , final_value := val_norm]

drug_pk[parameter=="%absorbed", parameter_group := "Fa"]
drug_pk[parameter %in% c("CLpl", "CLss", "CLt"), parameter_group := "CL"]

drug_pk[parameter %in% c("Fu","Plasma protein binding","Serum protein binding"),
        parameter_group := "Fu"]
drug_pk[parameter %in% c("Plasma protein binding","Serum protein binding"),
        final_value := 100-val_norm]
drug_pk[parameter_group == "Fu", route_group := "Any"]
drug_pk[parameter=="Bioavailability", parameter := 'F'] #shortening for ease of use
drug_pk[parameter_group=="Bioavailability", parameter_group := 'F'] #shortening for ease of use
drug_pk[parameter %in% c("Blood/plasma ratio", "Blood/serum ratio", "Plasma/blood ratio"),
        parameter_group := "Rbp"]
drug_pk[parameter == "Plasma/blood ratio", final_value := 1/val_norm]
drug_pk[parameter %in% c("Blood/plasma ratio"), parameter := "Rbp"] #shortening for ease and to match group name
drug_pk[parameter_group == "Rbp", route_group := "Any"]

drug_pk[ , endpoint := paste(route_group, parameter_group, sep = "|")]
endpoints <- c("Oral|AUC", "Oral|Cmax", "Oral|Tmax", "Oral|CL",
               "Oral|CLr", "Oral|Fa", "Oral|Ka", "Oral|F",
               "IV|AUC", "IV|CL", "IV|CLr", "Any|Fu", "Any|Rbp")
drug_pk <- drug_pk[endpoint %in% endpoints]

drug_pk[ , final_value_min := min(final_value[final_value>0]), by = endpoint]
drug_pk[final_value <= 0, final_value := final_value_min]

#drug_pk <- drug_pk[!is.na(val_norm) & !is.na(unit_normal)]

# fu <- dcast.data.table(drug_pk[parameter_group=="Fu"], drug~parameter, fun.aggregate = median,
#                  value.var = "val_norm")
# ggplot(fu, aes_string(x="`Serum protein binding`", y = "`Plasma protein binding`")) + geom_point()

drug_pk[ , unit_default := names(sort(table(unit_normal[!is.na(unit_normal)]),
                                      decreasing = T))[1],
         by = list(endpoint)]

drug_pk[!unit_normal==unit_default, final_value := NA]
drug_pk[ , drug_count := length(unique(drug)),
         by = list(endpoint)]

drug_pk[ , full_display_value := paste0(param_display,": ",
                                        display_value,
                                        " @ ", dose_src,
                                        ifelse(!concomitant=="NULL",
                                               paste0(" (", concomitant,")"),
                                               "")
)]

doct1 <- c("Label", "ANNEX I")
doct2 <- c("Pharmacology Review", "Medical/Clinical Review",
           "Approval Package", "Printed Labeling",
           "Assessment Report", "Scientific Discussion",
           "Public Assessment Report", "Withdrawal Public Statement",
           "Procedural Steps")


drug <- fread("pubchem_drug_agg.csv")
wwm <- fread("dat_wwm.csv")
wwm <- wwm[ , list(drug = tolower(`Active Ingredient`),
                   dose_typical = `Dose typical daily PO (mg/day)`)]
drug_wwm <- merge(drug, wwm, by.x = "wwm_name", by.y = "drug")
drug_pk <- merge(x = drug_pk, y = drug_wwm,
                 by.y = "wwm_name", by.x = "drug",
                 all.x = TRUE)
drug_pk[is.na(dose_typical),
        dose_typical := signif(10^mean(logdose[route=="Oral"],
                                       na.rm = T),2),
        by = drug]
drug_pk[ , log_dose_typical := log10(dose_typical)]
drug_pk[ , log_dose_diff := round(abs(log_dose_typical-logdose),1)]

drug_pk[ , dose_weight := ifelse(log_dose_diff<=0.5, 2, 1)]
drug_pk[is.na(dose_weight), dose_weight := 1]
drug_pk[ , doct_weight := ifelse(doc_type %in% doct1, 4,
                                 ifelse(doc_type %in% doct2, 2, 1))]
drug_pk[ , prmt_weight := ifelse(parameter_group==parameter, 2, 1)]
drug_pk[ , sgrp_weight := ifelse(adult_healthy==T, 2, 1)]
drug_pk[ , ccmt_weight := ifelse(concomitant=="NULL", 2, 1)]
drug_pk[ , final_weight := dose_weight * doct_weight * prmt_weight * sgrp_weight * ccmt_weight] #multiply or add?

# tmp <- unique(drug_pk[, list(drug, log_dose_mean, log_dose_typical)])
# ggscatter(tmp,
#           x= "log_dose_mean", y = "log_dose_typical", add = "reg.line") +
#   stat_cor(label.x = -3, label.y = 3) +
#   stat_regline_equation(label.x = -3, label.y = 2)
# ggplot(tmp, aes(x = log_dose_typical, y = log_dose_mean)) + geom_point()

weighted.geomean <- function(x, w, ...) exp(weighted.mean(log(x), w, ...))
gm_mean = function(x, na.rm=TRUE){
  exp(mean(log(x[x > 0]), na.rm = na.rm))
}

weighted.rep.index <- function(x, w){
  tmp <- cbind.data.frame(x,w,i=1:length(x))
  tmp <- tmp[rep(seq_len(nrow(tmp)), tmp$w),]
  tmp <- tmp[order(tmp$x), ]
  index <- tmp[ceiling(nrow(tmp)/2), ]$i
}

drug_pk[ , winner := 1:.N == weighted.rep.index(final_value, final_weight),
         by = list(drug, parameter_group, route_group, unit_default)]

#### rb data ####
rb_data <- fread("rb_data.csv")
rb_data$drug <- tolower(rb_data$drug)

rb_clean <- melt(rb_data, id.vars = c("drug"),
                 measure.vars = c("fup_rat", "fup_dog", "fup_monk", "fup_hum", "rb_hum",
                                  "vss_rat", "vss_dog", "vss_monk", "vss_hum"))
rb_clean <- as.data.table(rb_clean)
rb_clean <- rb_clean[variable=="fup_rat", species :="Rat"]
rb_clean <- rb_clean[variable=="fup_rat", parameter :="Fu"]
rb_clean <- rb_clean[variable=="fup_rat", parameter_group :="Fu"]
rb_clean <- rb_clean[variable=="fup_rat", unit_normal :="%"]
rb_clean <- rb_clean[variable=="fup_rat", route_group :="Any"]
rb_clean <- rb_clean[variable=="fup_dog", species :="Dog"]
rb_clean <- rb_clean[variable=="fup_dog", parameter :="Fu"]
rb_clean <- rb_clean[variable=="fup_dog", unit_normal :="%"]
rb_clean <- rb_clean[variable=="fup_dog", parameter_group :="Fu"]
rb_clean <- rb_clean[variable=="fup_dog", route_group :="Any"]
rb_clean <- rb_clean[variable=="fup_monk", species :="Monkey"]
rb_clean <- rb_clean[variable=="fup_monk", parameter :="Fu"]
rb_clean <- rb_clean[variable=="fup_monk", unit_normal :="%"]
rb_clean <- rb_clean[variable=="fup_monk", parameter_group :="Fu"]
rb_clean <- rb_clean[variable=="fup_monk", route_group :="Any"]
rb_clean <- rb_clean[variable=="fup_hum", species :="Human"]
rb_clean <- rb_clean[variable=="fup_hum", parameter :="Fu"]
rb_clean <- rb_clean[variable=="fup_hum", unit_normal :="%"]
rb_clean <- rb_clean[variable=="fup_hum", parameter_group :="Fu"]
rb_clean <- rb_clean[variable=="fup_hum", route_group :="Any"]
rb_clean <- rb_clean[variable=="rb_hum", species :="Human"]
rb_clean <- rb_clean[variable=="rb_hum", parameter :="Rb"] # what are units?
rb_clean <- rb_clean[variable=="rb_hum", parameter_group :="Rb"]
rb_clean <- rb_clean[variable=="rb_hum", route_group :="Any"]
rb_clean <- rb_clean[variable=="vss_rat", species :="Rat"]
rb_clean <- rb_clean[variable=="vss_rat", parameter :="Vss"]
rb_clean <- rb_clean[variable=="vss_rat", unit_normal :="L/kg"]
rb_clean <- rb_clean[variable=="vss_rat", parameter_group :="Vss"]
rb_clean <- rb_clean[variable=="vss_rat", route_group :="Oral"] # assumed...may need to be changed
rb_clean <- rb_clean[variable=="vss_dog", species :="Dog"]
rb_clean <- rb_clean[variable=="vss_dog", parameter :="Vss"]
rb_clean <- rb_clean[variable=="vss_dog", unit_normal :="L/kg"]
rb_clean <- rb_clean[variable=="vss_dog", parameter_group :="Vss"]
rb_clean <- rb_clean[variable=="vss_dog", route_group :="Oral"] # assumed...may need to be changed
rb_clean <- rb_clean[variable=="vss_monk", species :="Monkey"]
rb_clean <- rb_clean[variable=="vss_monk", parameter :="Vss"]
rb_clean <- rb_clean[variable=="vss_monk", unit_normal :="L/kg"]
rb_clean <- rb_clean[variable=="vss_monk", parameter_group :="Vss"]
rb_clean <- rb_clean[variable=="vss_monk", route_group :="Oral"] # assumed...may need to be changed
rb_clean <- rb_clean[variable=="vss_hum", species :="Human"]
rb_clean <- rb_clean[variable=="vss_hum", parameter :="Vss"]
rb_clean <- rb_clean[variable=="vss_hum", unit_normal :="L/kg"]
rb_clean <- rb_clean[variable=="vss_hum", parameter_group :="Vss"]
rb_clean <- rb_clean[variable=="vss_hum", route_group :="Oral"] # assumed...may need to be changed

rb_clean$value<- gsub("a", "", rb_clean$value)
rb_clean$value<- gsub("b", "", rb_clean$value)
rb_clean$value<- gsub("c", "", rb_clean$value)
rb_clean$value<- gsub("v", "", rb_clean$value)
rb_clean$value<- gsub("w", "", rb_clean$value)
rb_clean$value<- gsub("u", "", rb_clean$value)
rb_clean$value<- gsub("s", "", rb_clean$value)
rb_clean$value<- gsub("x", "", rb_clean$value)
rb_clean$value<- gsub("z", "", rb_clean$value)
rb_clean$value<- gsub("âˆ¼", "", rb_clean$value)

rb_clean <- rb_clean[, list(drug, value, species, parameter, unit_normal)]
setnames(rb_clean, old = "value", new = "final_value")
rb_clean <- rb_clean[, param_src :="rb_amgen"]
drug_pk <- drug_pk[, param_src :="ppm"]

drug_pk <-rbind(drug_pk, rb_clean, fill = TRUE)

#### scott 1352 ####
scott_1352 <- fread("copy_of_scott_1352.csv")
scott_1352$drug <- tolower(scott_1352$drug)

scott_1352 <- scott_1352[, list(drug, vdss_hum, cl_hum, fu, mrt_h, t_1_2, mw, hba,
                                hbd, tpsa_no, moka_logp)]
scott_1352 <- melt(scott_1352, id.vars = c("drug"),
                   measure.vars = c("vdss_hum", "cl_hum", "fu", "mrt_h", "t_1_2"))
scott_1352 <- as.data.table(scott_1352)
scott_1352 <- scott_1352[variable=="vdss_hum", species :="Human"]
scott_1352 <- scott_1352[variable=="vdss_hum", parameter :="Vss"] # should be Vss or Vd?
scott_1352 <- scott_1352[variable=="vdss_hum", unit_normal :="L/kg"]
scott_1352 <- scott_1352[variable=="vdss_hum", parameter_group :="Vss"]
scott_1352 <- scott_1352[variable=="vdss_hum", route_group :="Oral"] # assumed...may need to be changed
scott_1352 <- scott_1352[variable=="cl_hum", species :="Human"]
scott_1352 <- scott_1352[variable=="cl_hum", parameter :="CL"]
scott_1352 <- scott_1352[variable=="cl_hum", unit_normal :="mL/min"]
scott_1352 <- scott_1352[variable=="cl_hum", parameter_group :="CL"]
scott_1352 <- scott_1352[variable=="cl_hum", route_group :="Oral"]
scott_1352 <- scott_1352[variable=="fu", species :="Human"] # is species human?
scott_1352 <- scott_1352[variable=="fu", parameter :="Fu"]
scott_1352 <- scott_1352[variable=="fu", unit_normal :="%"]
scott_1352 <- scott_1352[variable=="fu", parameter_group :="Fu"]
scott_1352 <- scott_1352[variable=="fu", route_group :="Any"]
scott_1352 <- scott_1352[variable=="mrt_h", species :="Human"]
scott_1352 <- scott_1352[variable=="mrt_h", parameter :="MRT"]
scott_1352 <- scott_1352[variable=="mrt_h", unit_normal :="h"]
scott_1352 <- scott_1352[variable=="mrt_h", parameter_group :="MRT"]
scott_1352 <- scott_1352[variable=="mrt_h", route_group :="Oral"] # assumed...may need to be changed
scott_1352 <- scott_1352[variable=="t_1_2", species :="Human"]
scott_1352 <- scott_1352[variable=="t_1_2", parameter :="T1/2"]
scott_1352 <- scott_1352[variable=="t_1_2", unit_normal :="h"]
scott_1352 <- scott_1352[variable=="t_1_2", parameter_group :="T1/2"]
scott_1352 <- scott_1352[variable=="t_1_2", route_group :="Oral"] # assumed...may need to be changed

#scott_1352 <- scott_1352[variable=="mw", species :="Human"]
#scott_1352 <- scott_1352[variable=="mw", parameter :="Mw"]
#scott_1352 <- scott_1352[variable=="hba", species :="Human"]
#scott_1352 <- scott_1352[variable=="hba", parameter :="Hba"]
#scott_1352 <- scott_1352[variable=="hbd", species :="Human"]
#scott_1352 <- scott_1352[variable=="hbd", parameter :="Hbd"]
#scott_1352 <- scott_1352[variable=="tpsa_no", species :="Human"]
#scott_1352 <- scott_1352[variable=="tpsa_no", parameter :="Tpsa No"]
#scott_1352 <- scott_1352[variable=="moka_logp", species :="Human"]
#scott_1352 <- scott_1352[variable=="moka_logp", parameter :="Moka logp"]

scott_1352 <- scott_1352[, list(drug, value, species, parameter, unit_normal)]
setnames(scott_1352, old = "value", new = "final_value")
scott_1352 <- scott_1352[!is.na(final_value)]
scott_1352 <- scott_1352[, param_src :="scott_1352"]
drug_pk <-rbind(drug_pk, scott_1352, fill = TRUE)

#### wwm rif ####
wwm_rif <- fread("wwm_rif.csv")
ppm_id <- fread("ppium_PFId.csv")

pfid_merge <- merge(wwm_rif, ppm_id, by = "pf_id") # 777/1036 mapped
wwm_rif <- pfid_merge[, list(drug, hfu_p_num, hbp_num)] # bring rif name back to overwrite


wwm_rif <- melt(wwm_rif, id.vars = c("drug"),
                measure.vars = c("hfu_p_num", "hbp_num"))
wwm_rif$drug <- tolower(wwm_rif$drug)

wwm_rif <- as.data.table(wwm_rif)
wwm_rif <- wwm_rif[variable=="hfu_p_num", species :="Human"] # species human?
wwm_rif <- wwm_rif[variable=="hfu_p_num", parameter :="Fu"] # maps to Fu?
wwm_rif <- wwm_rif[variable=="hfu_p_num", parameter_group :="Fu"]
wwm_rif <- wwm_rif[variable=="hfu_p_num", route_group :="Any"]
wwm_rif <- wwm_rif[variable=="hfu_p_num", unit_normal :="%"]
wwm_rif <- wwm_rif[variable=="hbp_num", species :="Human"]
wwm_rif <- wwm_rif[variable=="hbp_num", parameter :="Rb"] # mapped currently to Rb...check this
wwm_rif <- wwm_rif[variable=="hbp_num", parameter_group :="Rbp"]
wwm_rif <- wwm_rif[variable=="hbp_num", route_group :="Any"]

wwm_rif <- wwm_rif[, list(drug, value, species, parameter, unit_normal)]
setnames(wwm_rif, old = "value", new = "final_value")
wwm_rif <- wwm_rif[, param_src :="wwm_rif"]
drug_pk <-rbind(drug_pk, wwm_rif, fill = TRUE)

#### Cancer small molecules and biologics ####
# small molecules
cancer_sm_orig <- fread("small_molecule_nostar_nobloq.csv") # removed drugs with * and rm BLOQ values in this csv
cancer_sm <- cancer_sm_orig[, c(2,6,7,8,9,10,15,17,18,20,21,22,23,24,28)] # take col 15 for cmax mg actually ug
# divide AUC by 1000 to convert to ug 
setnames(cancer_sm, old = c(1:15), new = c("drug", "dose", "dose_unit_norm", "route",
                                           "infusion", "duration", "Cmax", "Tmax",
                                           "T1/2", "AUC", "CL", "Vd", "Plasma protein binding",
                                           "Css", "link"))

cancer_sm$drug <- tolower(cancer_sm$drug)
cancer_sm_melt <- melt(cancer_sm, id.vars = c("drug", "dose","dose_unit_norm", "route","duration", "link"),
                       measure.vars = c("infusion", "Cmax", "Tmax",
                                        "T1/2", "AUC", "CL", "Vd", "Plasma protein binding",
                                        "Css"))
cancer_sm_melt <- as.data.table(cancer_sm_melt)
cancer_sm_melt <- cancer_sm_melt[value != "-"]
cancer_sm_melt <- cancer_sm_melt[value != ""]

cancer_sm_melt <- cancer_sm_melt[, species :="Human"]
cancer_sm_melt <- cancer_sm_melt[variable=="Cmax", unit_normal :="ug/mL"]
cancer_sm_melt <- cancer_sm_melt[variable=="Cmax", parameter_group :="Cmax"]
cancer_sm_melt <- cancer_sm_melt[variable=="Cmax", route_group :="Oral"]
cancer_sm_melt <- cancer_sm_melt[variable=="Tmax", unit_normal :="h"]
cancer_sm_melt <- cancer_sm_melt[variable=="Tmax", parameter_group :="Tmax"]
cancer_sm_melt <- cancer_sm_melt[variable=="Tmax", route_group :="Oral"]
cancer_sm_melt <- cancer_sm_melt[variable=="T1/2", unit_normal :="h"]
cancer_sm_melt <- cancer_sm_melt[variable=="T1/2", parameter_group :="T1/2"]
cancer_sm_melt <- cancer_sm_melt[variable=="T1/2", route_group :="Oral"] # assumed...may need to be changed
cancer_auc <- cancer_sm_melt[variable=="AUC"]
cancer_sm_melt <- cancer_sm_melt[variable!="AUC"]
cancer_auc$value <- as.numeric(cancer_auc$value)
cancer_auc <- cancer_auc[value/1000]
cancer_sm_melt <-rbind.data.frame(cancer_sm_melt, cancer_auc, fill = TRUE)
cancer_sm_melt <- cancer_sm_melt[variable=="AUC", unit_normal :="ug*h/mL"]
cancer_sm_melt <- cancer_sm_melt[variable=="AUC", parameter_group :="AUC"]
cancer_sm_melt <- cancer_sm_melt[variable=="AUC", route_group :="Oral"] # assumed...may need to be changed since saw IV & Oral
cancer_sm_melt <- cancer_sm_melt[variable=="CL", unit_normal :="mL/min"]
cancer_sm_melt <- cancer_sm_melt[variable=="CL", parameter_group :="CL"]
cancer_sm_melt <- cancer_sm_melt[variable=="CL", route_group :="Oral"] # assumed...may need to be changed since saw IV & Oral

setnames(cancer_sm_melt, old = c("variable", "value"), new = c("parameter", "final_value"))
cancer_sm_melt <- cancer_sm_melt[, param_src :="cancer_sm"]
drug_pk <-rbind(drug_pk, cancer_sm_melt, fill = TRUE)

### biologics
cancer_bio_orig <- fread("biologics.csv")
cancer_bio <- cancer_bio_orig[, c(2,7,8,9,10,11,14,16,18,20,21,22,25)] # take col 14 for cmax convert to ug/mL *1000 
# multiply t1/2 by 24 to convert to hours
setnames(cancer_bio, old = c(1:13), new = c("drug", "dose", "dose_unit_norm", "route",
                                            "infusion", "duration", "Cmax", "Tmax",
                                            "T1/2", "AUC", "CL", "Vss", "link"))

cancer_bio$drug <- tolower(cancer_bio$drug)
cancer_bio_melt <- melt(cancer_bio, id.vars = c("drug", "dose","dose_unit_norm", "route","duration", "link"),
                        measure.vars = c("infusion", "Cmax", "Tmax",
                                         "T1/2", "AUC", "CL", "Vss"))
cancer_bio_melt <- as.data.table(cancer_bio_melt)
cancer_bio_melt <- cancer_bio_melt[value != "-"]
cancer_bio_melt <- cancer_bio_melt[value != ""]

cancer_bio_melt <- cancer_bio_melt[, species :="Human"]
cancer_cmax <- cancer_bio_melt[variable=="Cmax"]
cancer_bio_melt <- cancer_bio_melt[variable!="Cmax"]
cancer_cmax$value <- as.numeric(cancer_cmax$value)
cancer_cmax <- cancer_cmax[value*1000]
cancer_bio_melt <-rbind.data.frame(cancer_bio_melt, cancer_cmax, fill = TRUE)
cancer_bio_melt <- cancer_bio_melt[variable=="Cmax", unit_normal :="ug/mL"]
cancer_bio_melt <- cancer_bio_melt[variable=="Cmax", parameter_group :="Cmax"]
cancer_bio_melt <- cancer_bio_melt[variable=="Cmax", route_group :="Oral"]

cancer_bio_melt <- cancer_bio_melt[variable=="Tmax", unit_normal :="h"]
cancer_bio_melt <- cancer_bio_melt[variable=="Tmax", parameter_group :="Tmax"]
cancer_bio_melt <- cancer_bio_melt[variable=="Tmax", route_group :="Oral"]

cancer_t12 <- cancer_bio_melt[variable=="T1/2"]
cancer_bio_melt <- cancer_bio_melt[variable!="T1/2"]
cancer_t12$value <- as.numeric(cancer_t12$value)
cancer_t12 <- cancer_t12[value*24]
cancer_bio_melt <-rbind.data.frame(cancer_bio_melt, cancer_t12, fill = TRUE)
cancer_bio_melt <- cancer_bio_melt[variable=="T1/2", unit_normal :="h"]
cancer_bio_melt <- cancer_bio_melt[variable=="T1/2", route_group :="Oral"] # assumed...may need to be changed

cancer_bio_melt <- cancer_bio_melt[variable=="AUC", unit_normal :="ug*h/mL"]
cancer_bio_melt <- cancer_bio_melt[variable=="AUC", parameter_group :="AUC"]
cancer_bio_melt <- cancer_bio_melt[variable=="AUC", route_group :="Oral"] # assumed...may need to be changed since saw IV & Oral
cancer_bio_melt <- cancer_bio_melt[variable=="CL", unit_normal :="mL/min"]
cancer_bio_melt <- cancer_bio_melt[variable=="CL", parameter_group :="CL"]
cancer_bio_melt <- cancer_bio_melt[variable=="CL", route_group :="Oral"] # assumed...may need to be changed since saw IV & Oral

setnames(cancer_bio_melt, old = c("variable", "value"), new = c("parameter", "final_value"))
cancer_bio_melt <- cancer_bio_melt[, param_src :="cancer_bio"]
drug_pk <-rbind(drug_pk, cancer_bio_melt, fill = TRUE)

#### pk 670 list ####
#setwd("C:/Users/CAPUND/Desktop/")
#download.file("https://chemoinfo.ipmc.cnrs.fr/TMP/tmp.65617/e-Drug3D_1930_PK_v2.txt", "pk_670.txt") # remove '#' sign from column 1
#pk_670_orig <- read.table("pk_670.txt", sep=";", header=T)
# reading in csv preferred since manually edited, Natasha added MW 
pk_670_orig <- fread("pk_670.csv")
pk_670_mw <- as.data.table(fread("pk_670_nk.csv"))
pk_670_mw <- pk_670_mw[, list(ID, `MW (g/mol)`)]

pk_670_merge <- merge(pk_670_orig, pk_670_mw, by = "ID", all = TRUE)
pk_670 <- pk_670_merge[, list(Name, Route, `VD(hour)`, `Cl(liter/hour)`, `t1/2(hour)`,
                              `PPB(percentage)`, Cmax, Cmax_unit, `Tmax(hour)`, `MW (g/mol)`)]

setnames(pk_670, old = c(1:10), new = c("drug", "route", "Vd", "CL",
                                        "T1/2", "Fu", "Cmax", "Cmax_unit",
                                        "Tmax", "MW"))
pk_670$drug <- tolower(pk_670$drug)
pk_670$Cmax_unit <- tolower(pk_670$Cmax_unit)
pk_670$route <- tolower(pk_670$route)
pk_670_melt <- melt(pk_670, id.vars = c("drug", "route","Cmax_unit", "MW"),
                    measure.vars = c("Vd", "CL", "T1/2", "Fu", "Cmax", "Tmax"))
pk_670_melt <- as.data.table(pk_670_melt)
pk_670_melt <- pk_670_melt[!is.na(value)]

pk_670_melt <- pk_670_melt[, species :="Human"]
pk_670_melt <- pk_670_melt[variable=="Vd", unit_normal :="%"]
pk_670_melt <- pk_670_melt[variable=="Vd", route_group :="Oral"]
pk_670_melt <- pk_670_melt[variable=="Vd", parameter_group :="Vd"]
# Cl conversion
cl_670 <- pk_670_melt[variable=="CL"]
pk_670_melt <- pk_670_melt[variable!="CL"]
cl_670$value <- as.numeric(cl_670$value)
cl_670 <- cl_670[value*(50/3)] # unit converion here
pk_670_melt <-rbind.data.frame(pk_670_melt, cl_670, fill = TRUE)
pk_670_melt <- pk_670_melt[variable=="CL", unit_normal :="mL/min"]
pk_670_melt <- pk_670_melt[variable=="CL", parameter_group :="CL"]
pk_670_melt <- pk_670_melt[variable=="CL", route_group :="Oral"] # assumed...may need to be changed since saw IV & Oral
pk_670_melt <- pk_670_melt[variable=="T1/2", unit_normal :="h"]
pk_670_melt <- pk_670_melt[variable=="T1/2", parameter_group :="T1/2"]
pk_670_melt <- pk_670_melt[variable=="T1/2", route_group :="Oral"] # assumed...may need to be changed

# pbb to Fu conversion
fu_670 <- pk_670_melt[variable=="Fu"]
pk_670_melt <- pk_670_melt[variable!="Fu"]
fu_670$value <- as.numeric(fu_670$value)
fu_670 <- fu_670[100-value] # unit converion here
pk_670_melt <-rbind.data.frame(pk_670_melt, fu_670, fill = TRUE)
pk_670_melt <- pk_670_melt[variable=="Fu", parameter_group :="Fu"]
pk_670_melt <- pk_670_melt[variable=="Fu", route_group :="Any"]

# Cmax using cmax units listed until we can settle on conversions
pk_670_melt <- pk_670_melt[variable=="Cmax", unit_normal := Cmax_unit]
pk_670_melt <- pk_670_melt[value != ""]
pk_670_melt <- pk_670_melt[value != "  "]
# Cmax conversions and unit corrections
pk_670_melt$value<- gsub("BELOW", "", pk_670_melt$value)
pk_670_melt$value<- gsub("NOT", "", pk_670_melt$value)
pk_670_melt$value<- gsub("TRUE", "", pk_670_melt$value)
pk_670_melt$value<- gsub(" ", "", pk_670_melt$value)
pk_670_melt$value <- as.numeric(pk_670_melt$value)
pk_670_melt$MW <- as.numeric(pk_670_melt$MW)

# Cmax conversions with MW from Natasha
pk_670_melt[Cmax_unit == "nanomolar" & variable == "Cmax",  value := (value*MW)/1e6] # nM to ug/ml
pk_670_melt[Cmax_unit == "micromolar" & variable== "Cmax",  value := (value*MW)/1e3] # uM to ug/ml
pk_670_melt[Cmax_unit == "picomolar" & variable== "Cmax",  value := (value*MW*1e3)] # pM to ug/ml? Check this???
pk_670_melt[Cmax_unit == "millimolar" & variable== "Cmax",  value := (value*MW)] # mM to ug/ml 
# convert all to unit normal of ug/mL
pk_670_melt[Cmax_unit == "nanomolar" & variable == "Cmax", unit_normal := "ug/mL"]
pk_670_melt[Cmax_unit == "micromolar" & variable == "Cmax", unit_normal := "ug/mL"]
pk_670_melt[Cmax_unit == "picomolar" & variable == "Cmax", unit_normal := "ug/mL"]
pk_670_melt[Cmax_unit == "millimolar" & variable == "Cmax", unit_normal := "ug/mL"]

setnames(pk_670_melt, old = c("variable", "value"), new = c("parameter", "final_value"))
pk_670_melt <- pk_670_melt[, param_src :="pk_670"]
pk_670_melt <- pk_670_melt[, list(drug, route, parameter, final_value, species,
                                  unit_normal, route_group, parameter_group, param_src)]
pk_670_melt <- unique(pk_670_melt)

drug_pk <-rbind(drug_pk, pk_670_melt, fill = TRUE)

#### Natasha DILI 123 ####
nat_dili_123_orig <- fread("natasha_dili_123.csv")
#nat_dili_123 <- nat_dili_123_orig[, c(1,5,6,7,8)] # removed pfids 

# there are some extra routes (not oral or IV) do we keep, change, map?

setnames(nat_dili_123_orig, old = c("Converted dose (mg)","unit"), new = c("dose", "unit_normal"))

nat_dili_123_melt <- melt(nat_dili_123_orig, id.vars = c("drug", "route_group", "dose", "link", "unit_normal", "MW"),
                          measure.vars = c("Cmax"))
nat_dili_123_melt <- as.data.table(nat_dili_123_melt)
nat_dili_123_melt <- nat_dili_123_melt[!is.na(value)]
nat_dili_123_melt <- nat_dili_123_melt[, dose_unit_norm := "mg"]
nat_dili_123_melt <- nat_dili_123_melt[, species := "Human"]
# hold for IV and oral routes, Natasha will do 6/31
nat_dili_123_melt <- nat_dili_123_melt[variable=="Cmax", parameter_group := "Cmax"]

# need conversions for different Cmax units
# gsub clean up and numeric
nat_dili_123_melt$value<- gsub("Â", "", nat_dili_123_melt$value)
nat_dili_123_melt$value<- gsub(",", "", nat_dili_123_melt$value)
nat_dili_123_melt$value <- as.numeric(nat_dili_123_melt$value)
# start of conversions
nat_dili_123_melt[unit_normal == "ng/ml", value := value/1000] #units are now ug/ml
nat_dili_123_melt[unit_normal == "mg/l", unit_normal := "ug/ml"]
nat_dili_123_melt[unit_normal == "mmol/L", value := (value*MW)] #mmol/l to ug/ml
nat_dili_123_melt[unit_normal == "pmol/g", value := (value*MW)] # from Natasha
nat_dili_123_melt[unit_normal == "nmol/L", value := (value*MW)/1e6] # same as nM
nat_dili_123_melt[unit_normal == "ug/L", value := value/1000] # ug/l to ug/ml
nat_dili_123_melt[unit_normal == "umol/L",  value := (value*MW)/1e3] # umol/l to ug/ml
nat_dili_123_melt[unit_normal == "uM",  value := (value*MW)/1e3] # uM to ug/ml
nat_dili_123_melt[unit_normal == "mg/L", unit_normal := "ug/ml"]
nat_dili_123_melt[unit_normal == "nM",  value := (value*MW)/1e6] # nM to ug/ml
nat_dili_123_melt[unit_normal == "pmol/mL", value := (value*MW)/1e6] # same as nM
nat_dili_123_melt[unit_normal == "mg/dl", value := value*10] # mg/dl to ug/ml
nat_dili_123_melt[, unit_normal := "ug/ml"] #overwrite all units once normalized

setnames(nat_dili_123_melt, old = c("variable", "value"), new = c("parameter", "final_value"))
nat_dili_123_melt <- nat_dili_123_melt[, param_src :="nat_dili_123"]

drug_pk <-rbind(drug_pk, nat_dili_123_melt, fill = TRUE)

# end of natasha section

drug_pk <- drug_pk[drug != ""]
drug_pk <- drug_pk[drug != "TRUE"]

# select columns we care about
drug_pk <- drug_pk[, list(drug, duration, parameter, species, study_group, doc_srcshort,
                          dose, dose_unit_norm, logdose, adult_healthy, link, final_value,
                          parameter_group, route_group, endpoint, final_value_min, unit_normal, 
                          unit_default, drug_count, full_display_value, dose_typical,
                          log_dose_typical, log_dose_diff, dose_weight, doct_weight,
                          prmt_weight, sgrp_weight, ccmt_weight, final_weight,
                          winner, param_src)]
# if we only want to keep drugs from drug table in comptox_drug
#drug_pk_final <- subset(drug_pk, drug %in% ppium_drugs$drug) 

drug_pk <- unique(drug_pk)
#drug_pk <- drug_pk[parameter_group != "TRUE"]
#test <- drug_pk[param_src == "wwm_rif"]
# start of db creation
library(RMySQL)
zoomapcon <- function(){dbConnect(MySQL(), user='martm255',
                                  password='GiaRose#0509',
                                  dbname='comptox_drug',
                                  host='10.131.112.20')}
zoomapdb <- zoomapcon()

drug_pk.sql1 <- "DROP TABLE IF EXISTS drug_pk;"      
drug_pk.sql2 <- "CREATE TABLE IF NOT EXISTS drug_pk (
drug varchar(300) NOT NULL,
duration varchar(100) NOT NULL,
parameter varchar(30) DEFAULT NULL,
species varchar(50) NOT NULL,
study_group varchar(50) NOT NULL,
doc_srcshort varchar(10) DEFAULT NULL,
dose varchar(20) DEFAULT NULL,
dose_unit_norm varchar(20) NOT NULL,
logdose varchar(20) DEFAULT NULL,
adult_healthy varchar(10) NOT NULL,
link varchar(200) NOT NULL,
final_value varchar(50) DEFAULT NULL,
parameter_group varchar(20) DEFAULT NULL,
route_group varchar(20) DEFAULT NULL,
endpoint varchar(50) DEFAULT NULL,
final_value_min varchar(20) DEFAULT NULL,
unit_default varchar(20) DEFAULT NULL,
unit_normal varchar(20) DEFAULT NULL,
drug_count varchar(10) DEFAULT NULL,
full_display_value varchar(200) DEFAULT NULL,
dose_typical varchar(20) DEFAULT NULL,
log_dose_typical varchar(20) DEFAULT NULL,
log_dose_diff varchar(10) DEFAULT NULL,
dose_weight varchar(10) DEFAULT NULL,
doct_weight varchar(10) DEFAULT NULL,
prmt_weight varchar(10) DEFAULT NULL,
sgrp_weight varchar(10) DEFAULT NULL,
ccmt_weight varchar(10) DEFAULT NULL,
final_weight varchar(10) DEFAULT NULL,
winner varchar(10) DEFAULT NULL,
param_src varchar(20) DEFAULT NULL
) ENGINE = MYISAM DEFAULT CHARSET = utf8;"
drug_pk.sql3 <- "ALTER TABLE drug_pk
ADD KEY drug (drug),
ADD KEY duration (duration),
ADD KEY parameter (parameter),
ADD KEY species (species),
ADD KEY study_group (study_group),
ADD KEY doc_srcshort (doc_srcshort),
ADD KEY dose (dose),
ADD KEY dose_unit_norm (dose_unit_norm),
ADD KEY logdose (logdose),
ADD KEY adult_healthy (adult_healthy),
ADD KEY link (link),
ADD KEY final_value (final_value),
ADD KEY parameter_group (parameter_group),
ADD KEY route_group (route_group),
ADD KEY endpoint (endpoint),
ADD KEY final_value_min (final_value_min),
ADD KEY unit_default (unit_default),
ADD KEY unit_normal (unit_normal),
ADD KEY drug_count (drug_count),
ADD KEY full_display_value (full_display_value),
ADD KEY dose_typical (dose_typical),
ADD KEY log_dose_typical (log_dose_typical),
ADD KEY log_dose_diff (log_dose_diff),
ADD KEY dose_weight (dose_weight),
ADD KEY doct_weight (doct_weight),
ADD KEY prmt_weight (prmt_weight),
ADD KEY sgrp_weight (sgrp_weight),
ADD KEY ccmt_weight (ccmt_weight),
ADD KEY final_weight (final_weight),
ADD KEY winner (winner),
ADD KEY param_src (param_src)
;"
dbSendQuery(conn = zoomapdb, drug_pk.sql1)
dbSendQuery(conn = zoomapdb, drug_pk.sql2)
dbSendQuery(conn = zoomapdb, drug_pk.sql3)

write.table(drug_pk,file="tmp.txt", fileEncoding ="utf8", row.names = FALSE)
drug_pk <- read.table(file="tmp.txt",encoding="latin1", fill = TRUE, header = TRUE)

dbWriteTable(conn  = zoomapdb,
             name  = "drug_pk",
             value = drug_pk,
             row.names = FALSE,
             append = TRUE)
