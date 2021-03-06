library(tidyverse)
library(magrittr)
library(googlesheets4)
#Policy (Kayla - HKhor@schools.nyc.gov) requested the following information: 
#   1. How many programs have been awarded SDY & EDY seats for the same age? 
#   2. How many of these programs have been awarded EDY for the first time? 
#   3. How many of these programs have been awarded SDY for the first time? 
# Currently only needs the total sums of these questions but will need site 
# level data in the future for working with these schools. 
#   
# Need the following datasets: 
# awards.csv (awards data for each site)
# allSites_21.csv (current AllSites list for active sites)
# 
# Working Plan: Merge data together via site id, and then do a series of tests
# to answer the questions above. 

#Reading in Data ----
awards <- read_csv("awards.csv") %>% 
  #don't need elector info for this, getting rid of to reduce VARS
  select(., -c(AssemblyDistrict, CongressionalDistrict, StateSenatorialDistrict,
               AssemblyDistrict_1, CongressionalDistrict_1, StateSenatorialDistrict_1,
               siteIdExists, nonAward, anyAward, naward, `Current HS Delegate`, 
               `COL Seats Awarded`, cc_seats_awarded, hs4_seats_awarded,
               hs3_seats_awarded,	cc_seats_awarded,	cc4_seats_awarded,
               cc3_seats_awarded,	universal_seats_awarded,	hs_tots,	cc_tots,
               cc_infants, `Base RFP` )) %>% 
  filter((`Award Flag` == "Award" | `Award Flag` == "Award COL") & baseRFP == "B")

#11/17: operating under following assumptions: 
#   1. ONLY looking at EDY/SDY 3's & 4's. No HS, no dual awarded (HS/B5), no I/T
#   2. INLCUDING City Transitional Seats in this 
allSites <- read_csv("allSites_21.csv") %>% 
  filter(., Type == "NYCEEC") %>% #only need NYCEECS
  select(., SiteID, Type, art43_DCID = `Art 43 SBCC DCID`, #only need SDY/EDY 3's/4's information. 
         art47_PK_DCID = `Art 47 Preschool Permit DCID`, 
         art47_IT_DCID = `Art 47 I-T Permit DCID`, Borough, Name, SiteAddress, 
         City, State, VendorName, VendorTaxID, 
         `3KFDFY21`, PKFDFY21, PKHDFY21, PK5HRFY21, PKHSEnhancedFY21,
         ChildCare3YrFY21, CT3YrFY21, ChildCare4YrFY21, CT4YrFY21) %>% 
  mutate(`3KFDFY21` = replace_na(`3KFDFY21`, 0))


#Data Manipulation ----
allSites %<>% 
  mutate(., 
         current_SDY_3s = `3KFDFY21`,
         current_SDY_4s = rowSums(select(., PKFDFY21:PKHSEnhancedFY21), 
                                  na.rm = TRUE),
         current_EDY_3s = rowSums(select(., ChildCare3YrFY21:CT3YrFY21), 
                                  na.rm = TRUE),
         current_EDY_4s = rowSums(select(., ChildCare4YrFY21:CT4YrFY21),
                                  na.rm = TRUE)
         ) %>% 
  mutate(.,
         total_Current_SDY = rowSums(select(., current_SDY_3s, current_SDY_4s),
                             na.rm = TRUE),
         total_Current_EDY = rowSums(select(., current_EDY_3s, current_EDY_4s),
                             na.rm = TRUE),
         current_Pgm_Model = case_when(total_Current_SDY > 0 & total_Current_EDY > 0 ~ "edySDY",
                              total_Current_SDY > 0 ~ "sdyOnly", 
                              total_Current_EDY > 0 ~ "edyOnly")
  )

awards %<>% 
  mutate(sdy3_Awarded = SDY3s, 
         sdy4_Awarded = SDY4s,
         edy3_Awarded = CC_3s, 
         edy4_Awarded = CC_4s
         ) %>% 
  mutate(total_SDY_Awarded = rowSums(select(., sdy3_Awarded, sdy4_Awarded), 
                                     na.rm = TRUE),
         total_EDY_Awarded = rowSums(select(., edy3_Awarded, edy4_Awarded),
                                     na.rm = TRUE),
         awarded_Pgm_Model = case_when(total_SDY_Awarded > 0 & total_EDY_Awarded > 0 ~ "edySDY",
                                       total_SDY_Awarded > 0 ~ "sdyOnly",
                                       total_EDY_Awarded > 0 ~ "edyOnly"))

#Data Joins & Manipulation ----

combined <- left_join(awards, allSites, by = c("DoeSiteId" = "SiteID")) %>% 
  filter(., awarded_Pgm_Model == "edySDY") %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(total_CurrentSeats = rowSums(select(., total_Current_SDY, total_Current_EDY), 
                                      na.rm = TRUE),
         total_AwardSeats = rowSums(select(., total_SDY_Awarded, total_EDY_Awarded),
                                    na.rm = TRUE), 
         change_Pgm_Model = if_else(current_Pgm_Model == awarded_Pgm_Model, 
                                    "model_Same", "model_Different")) %>% 
  unite(., modelChange, current_Pgm_Model, awarded_Pgm_Model, sep = "->", 
        remove = FALSE) %>% 
  mutate(.,
         pgmModel_3s = if_else(sdy3_Awarded > 0 & edy3_Awarded > 0, 
                               "edySDY_3", "not_Mixed_3s"),
         pgmModel_4s = if_else(sdy4_Awarded > 0 & edy4_Awarded > 0, 
                               "edySDY_4", "not_Mixed_4s"),
         new_EDY = if_else((current_EDY_3s == 0 & edy3_Awarded > 0) | (current_EDY_4s == 0 & edy4_Awarded > 0),
                           "new_EDY_Program", "alreadyServing_EDY"),
         new_EDY_3s = if_else(current_EDY_3s == 0 & edy3_Awarded > 0, 
                              "edy_3_New", "alreadyServing_EDY3"),
         new_EDY_4s = if_else(current_EDY_4s == 0 & edy4_Awarded > 0, 
                              "edy_4_New", "alreadyServing_EDY4"),
         new_SDY = if_else((current_SDY_3s == 0 & sdy3_Awarded > 0) | (current_SDY_4s == 0 & sdy4_Awarded > 0), 
                           "new_SDY_Program", "alreadyServing_SDY"),
         new_SDY_3s = if_else(current_SDY_3s == 0 & sdy3_Awarded > 0,
                              "sdy_3_New", "alreadyServing_SDY3"), 
         new_SDY_4s = if_else(current_SDY_4s == 0 & sdy4_Awarded > 0, 
                              "sdy_4_New", "alreadyServing_SDY4")
  ) %>%  
  mutate_at(vars(modelChange, pgmModel_3s, pgmModel_4s, new_EDY, new_EDY_3s, 
                        new_EDY_4s, new_SDY, new_SDY_3s, new_SDY_4s), as.factor)


new_EDY_3s <- combined %>% 
  filter(pgmModel_3s == "edySDY_3") %>% 
  count(new_EDY_3s
        )

new_EDY_4s <- combined %>% 
  filter(pgmModel_4s == "edySDY_4") %>% 
  count(new_EDY_4s
        )

new_SDY_3s <- combined %>% 
  filter(pgmModel_3s == "edySDY_3") %>% 
  count(new_SDY_3s
  )

new_SDY_4s <- combined %>% 
  filter(pgmModel_4s == "edySDY_4") %>% 
  count(new_SDY_4s
  )

combined %<>% 
  mutate(., hasSiteID = if_else(DoeSiteId != "#No SiteID", "yes","no"))

combined %>% count(hasSiteID)

table(combined$hasSiteID, combined$new_EDY)
table(combined$hasSiteID[combined$new_EDY_3s == "edySDY_3"], combined$new_EDY_3s[combined$new_EDY_3s == "edySDY_3"])

pgModel_3s <- combined %>% filter(pgmModel_3s == "edySDY_3")
pgModel_4s <- combined %>% filter(pgmModel_4s == "edySDY_4")

table(pgModel_3s$hasSiteID, pgModel_3s$new_EDY_3s)
table(pgModel_3s$hasSiteID, pgModel_3s$new_SDY_3s)
table(pgModel_4s$hasSiteID, pgModel_4s$new_EDY_4s)
table(pgModel_4s$hasSiteID, pgModel_4s$new_SDY_4s)

newEDY3 <- combined %>% filter(new_EDY_3s == "edy_3_New")
test <- combined %>% filter(pgmModel_3s == "edySDY_3")



ss <- gs4_get("https://docs.google.com/spreadsheets/d/1O_3sB88whOcLJNQlZN8C2mG-QXmL8Xddgts8obZlUkQ/edit#gid=0")
sheet_write(combined, ss = ss, sheet = "All Sites EDY-SDY Awarded")
sheet_write(newEDY3, ss = ss, sheet = "Edy3")
sheet_write(test, ss = ss, sheet = "test3s")
