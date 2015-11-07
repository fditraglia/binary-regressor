#Test our method on Cohen and Dupas Dataset
setwd("~/binary-regressor/examples/CohenDupas/")
library(foreign)

# Baseline clinic characteristics
base <- read.dta("clinic_level_baselinedata.dta")
# Clinic-level nets sales
sales <- read.dta("clinic_level_nets_sales.dta")
# Follow-up survey
followup <- read.dta("Followup_survey.dta")
# Pre-natal visit survey
pv <- read.dta("PV_survey.dta")

#------------------------------------------------------------------
# Create a dataframe of the following variables for all women in
# the study by merging selected columns of base and pv based on 
# Clinicid
#------------------------------------------------------------------
#   Treatment           equals 1 if woman visits a clinic that was
#                         randomized into treatment group
#   netprice            randomized price at which clinic sells the 
#                         bednets (0, 10, 20, 40)
#   transactionprice    final price paid for net, after discount and
#                         lottery if applicable
#   PV_hbrate           hemoglobin level at prenatal visit
#   PV_buynet           self-report equals 1 if woman says she
#                         bought mosquito net
#------------------------------------------------------------------
clinics <- with(base, data.frame(clinicid, treatment, netprice))
women <- with(pv, data.frame(clinicid, respid, transactionprice, PV_buynet, PV_hbrate)) 
data <- merge(clinics, women)
summary(data)



#------------------------------------------------------------------
# Create a dataframe based on the follow-up survey for all women
# who were selected to participate containing the following by
# merging on respid.
#------------------------------------------------------------------
  
  
  
  
  
  
  
  
  