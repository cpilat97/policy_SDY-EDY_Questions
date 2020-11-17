library(tidyverse)
library(magrittr)

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
               AssemblyDistrict_1, CongressionalDistrict_1, StateSenatorialDistrict_1))

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
         current_SDY_4s = rowSums(select(., PKFDFY21:PKHSEnhancedFY21), na.rm = TRUE),
         current_EDY_3s = rowSums(select(., ChildCare3YrFY21:CT3YrFY21), na.rm = TRUE),
         current_EDY_4s = rowSums(select(., ChildCare4YrFY21:CT4YrFY21), na.rm = TRUE)
         ) %>% 
  mutate(.,
         total_SDY = rowSums(select(., current_SDY_3s, current_SDY_4s), na.rm = TRUE),
         total_EDY = rowSums(select(., current_EDY_3s, current_EDY_4s), na.rm = TRUE),
         pgm_Type = case_when(total_SDY > 0 & total_EDY > 0 ~ "edySDY",
                              total_SDY > 0 ~ "sdyOnly", 
                              total_EDY > 0 ~ "edyOnly")
  )


