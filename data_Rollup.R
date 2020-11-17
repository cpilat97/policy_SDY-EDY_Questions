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
  select(., -c(AssemblyDistrict, CongressionalDistrict, StateSenatorialDistrict,
               AssemblyDistrict_1, CongressionalDistrict_1, StateSenatorialDistrict_1))

allSites <- read_csv("allSites_21.csv") %>% 
  filter(., Type == "NYCEEC") %>% 
  select(., SiteID, Type, art43_DCID = `Art 43 SBCC DCID`, 
         art47_PK_DCID = `Art 47 Preschool Permit DCID`, 
         art47_IT_DCID = `Art 47 I-T Permit DCID`, Borough, Name, SiteAddress, 
         City, State, VendorName, VendorTaxID, 
         `3KFDFY21`, PKFDFY21, PKHDFY21, PK5HRFY21, PKHSEnhancedFY21,
         ChildCare3YrFY21, CT3YrFY21, ChildCare4YrFY21, CT4YrFY21)


