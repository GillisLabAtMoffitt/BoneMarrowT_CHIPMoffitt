# Import library

library(tidyverse)
library(data.table)


####################################################################################### I ### Load data----
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP_Moffitt BMT")
# 1.1.Load Demographics data -------------------------------------------------------------------------------------
fct_name_repair <- function(colnms) {
  tolower(gsub(" ", "_", colnms))
}

clinical <-
  readxl::read_xlsx(paste0(path, "/data/BMTs at Moffitt for pilot.xlsx"), .name_repair = fct_name_repair)

DR_match <-
  readxl::read_xlsx(paste0(path, "/data/deIDs_MRNs.xlsx") )%>% 
  select(recipient_mrn = "mrn", recipient_id = "Deidentify ID", "donor_mrn", donor_id= "Donor Deidentifier")

patient_samples <-
  readxl::read_xlsx(paste0(path, "/data/Samples.xlsx"),
                    sheet = "available samples")

bmt_info <-
  readxl::read_xlsx(paste0(path, "/data/Samples.xlsx"))

####################################################################################### II ### Data cleaning----
# Clinical---
clinical <- full_join(DR_match, clinical,  by = c("recipient_id" = "deidentify_id")) %>% 
  select(c("recipient_id", "recipient_mrn", "age_bmt", "race", "sex", "prior_transplant", "rec_cmv_testname",
           "rec_cmv_result", "rec_cmv_collected_date", "karnofsky_score", "karnofsky_assessment_time",
           "karnofsky_score_date", "donor_id", "donor_mrn", everything()), -donor_deidentifier
         )

# Modified as.char
clinical <- clinical %>% 
  mutate_at(c("recipient_mrn", "recipient_id", "donor_mrn", "donor_id"), ~ as.character(.))

patient_samples <- patient_samples %>% 
  mutate_at(c("mrn"), ~ as.character(.))
bmt_info <- bmt_info %>% 
  mutate_at(c("mrn"), ~ as.character(.))

# Align samples
patient_samples <- patient_samples %>% 
  distinct(mrn, collectiondt, sampletype, .keep_all = TRUE) %>% 
  arrange(collectiondt)
patient_samples1 <- dcast(setDT(patient_samples), mrn+collectiondt ~ rowid(mrn), 
                            value.var = c("sampleid", "sampletype", "samplefamilyid", 
                                          "tissuetype", "collectionmethod", "currentqty", "units", "storagestatus")) %>% 
  unite(sampleid, sampleid_1:sampleid_16, sep = ", ", na.rm = TRUE, remove = TRUE) %>% 
  unite(sampletype, sampletype_1:sampletype_16, sep = ", ", na.rm = TRUE, remove = TRUE) %>% 
  unite(samplefamilyid, samplefamilyid_1:samplefamilyid_16, sep = ", ", na.rm = TRUE, remove = TRUE) %>% 
  unite(tissuetype, tissuetype_1:tissuetype_16, sep = ", ", na.rm = TRUE, remove = TRUE) %>% 
  unite(collectionmethod, collectionmethod_1:collectionmethod_16, sep = ", ", na.rm = TRUE, remove = TRUE) %>% 
  unite(currentqty, currentqty_1:currentqty_16, sep = "; ", na.rm = TRUE, remove = TRUE) %>% 
  unite(units, units_1:units_16, sep = ", ", na.rm = TRUE, remove = TRUE) %>% 
  unite(storagestatus, storagestatus_1:storagestatus_16, sep = ", ", na.rm = TRUE, remove = TRUE)

patient_samples2 <- dcast(setDT(patient_samples1), mrn ~ rowid(mrn), 
                          value.var = c("collectiondt", "sampleid", "sampletype", "samplefamilyid", 
                                        "tissuetype", "collectionmethod", "currentqty", "units", "storagestatus"))



colnames(patient_samples1)
# Select recipient ID with samples
recipient_bmt <- bmt_info %>% 
  filter(`Patient samples?` == "Yes") %>% 
  rename(recipient_mrn = "mrn")
# recipient_id <- paste(recipient_bmt$mrn, collapse = "|")

# recipient_samples <- patient_samples[grepl(recipient_id, patient_samples$mrn), ] %>% 
#   distinct(mrn, collectiondt, sampletype, .keep_all = TRUE) %>% 
#   rename(recipient_mrn = "mrn") %>% 
# arrange(collectiondt)

# Find recipient sample date after bmt date
recipient_samples <- left_join(recipient_bmt, 
                                patient_samples2, # Merge recipient who has samples with their samples
                                by = c("recipient_mrn" = "mrn"))

# Select donors samples who has recipient sample
recipient_samples <- recipient_samples %>% 
  mutate(first_collectiondt_after_bmt = case_when(
    bmt_date < collectiondt_1 ~ collectiondt_1,
    bmt_date < collectiondt_2 ~ collectiondt_2,
    bmt_date < collectiondt_3 ~ collectiondt_3,
    bmt_date < collectiondt_4 ~ collectiondt_4,
    bmt_date < collectiondt_5 ~ collectiondt_5,
    bmt_date < collectiondt_6 ~ collectiondt_6,
    bmt_date < collectiondt_7 ~ collectiondt_7,
    bmt_date < collectiondt_8 ~ collectiondt_8,
    bmt_date < collectiondt_9 ~ collectiondt_9,
    bmt_date < collectiondt_10 ~ collectiondt_10,
    bmt_date < collectiondt_11 ~ collectiondt_11,
    bmt_date < collectiondt_12 ~ collectiondt_12,
    bmt_date < collectiondt_13 ~ collectiondt_13
  )) %>% 
  mutate(first_sampletype_after_bmt = case_when(
    bmt_date < collectiondt_1 ~ sampletype_1,
    bmt_date < collectiondt_2 ~ sampletype_2,
    bmt_date < collectiondt_3 ~ sampletype_3,
    bmt_date < collectiondt_4 ~ sampletype_4,
    bmt_date < collectiondt_5 ~ sampletype_5,
    bmt_date < collectiondt_6 ~ sampletype_6,
    bmt_date < collectiondt_7 ~ sampletype_7,
    bmt_date < collectiondt_8 ~ sampletype_8,
    bmt_date < collectiondt_9 ~ sampletype_9,
    bmt_date < collectiondt_10 ~ sampletype_10,
    bmt_date < collectiondt_11 ~ sampletype_11,
    bmt_date < collectiondt_12 ~ sampletype_12,
    bmt_date < collectiondt_13 ~ sampletype_13
  )) %>% 
  select(c("recipient_mrn", "donor_mrn", "primedx", "bmt_date", "first_collectiondt_after_bmt", "first_sampletype_after_bmt"),
         everything())

recipient_clin <- left_join(recipient_samples, clinical, by = "recipient_mrn")
  
  
write_csv(recipient_clin, paste0(path, "/output/Samples and clinicals for recipients who has samples2.csv"))



# Cleaning
rm(DR_match, donor_id)








