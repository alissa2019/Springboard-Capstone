
library(tidyverse)
library(ryouready)
library(Hmisc)


#rm(list=ls())

setwd('C:/Users/Alissa Hayes/Desktop/Springboard')

read_data <- function() {
  Quotes <- read_csv('Quote File.csv')
  Quotes$Bind.Count <- Quotes$`Bind Count`
  Quotes<- Quotes %>% 
    filter(Bind.Count ==1)
  
  XML <- read_csv('revised2 xml.csv')
  cancel_file <- read_csv('cancels2.csv')
  
  #Join together with lead and cancel data
  
  Cancels <- inner_join(Quotes, XML, by = c("PHONE_NBR" = "phone"))
  #glimpse(Cancels)
  Cancels2 <- left_join(Cancels, cancel_file, by = "PHONE_NBR")
 return(Cancels2) 
}


#Examine Data
#glimpse(Cancels2)

#Cancelled information has missing values because not all policies have cancelled. 

#Create cancelled column (1 or 0 column)
add_calculated_columns <- function (Cancels2) {
      Cancels2 <-  Cancels2 %>% 
      mutate(Cancelled = ifelse(!is.na(CANCELLATION_EFF_DATE) & lag<365, 1,0))
      
    #glimpse(Cancels2)
    
    #Alter variables names to make them easier to work with:
    #Cancels2$Zip.Code <- Cancels2$`Zip Code`
    Cancels2$Zip.Code <- Cancels2$`Zip Code`
    Cancels2$First.Quote.Date <- Cancels2$`First Quote Date`
    Cancels2$Last.Quote.Date <- Cancels2$`Last Quote Date`
    
    #Keep only needed variables:
    Cancels3 <- Cancels2 %>% 
      select(State, Zip.Code, First.Quote.Date, Last.Quote.Date, EMPLY_EMAIL_ADDR_TXT.x, LQ_FB_PREM_AMT, customer_age,
             marital_status, education, no_of_vehicles, currently_insured.x, gender, risk_profile, number_of_claims, number_of_accidents, policy_expiration_date.x, years_continuous_coverage, months_continuous_coverage, previous_policy_type,
             cost_of_coverage, bi_limit, vehicle_ownership, lead_seller, CANCELLATION_EFF_DATE,
             CANCELLATION_REASON, DATE_CANX_PROCESSED, lag, Cancelled)
    
    
    
    #Format Columns:
    Cancels3$Last.Quote.Date <- as.Date(Cancels3$Last.Quote.Date, format = "%d-%b-%y")
    Cancels3$First.Quote.Date <- as.Date(Cancels3$First.Quote.Date, format = "%d-%b-%y")
    Cancels3$DATE_CANX_PROCESSED <- as.Date(Cancels3$DATE_CANX_PROCESSED, format = "%m/%d/%y")
    Cancels3$CANCELLATION_EFF_DATE <- as.Date(Cancels3$CANCELLATION_EFF_DATE, format = "%m/%d/%y")
    Cancels3$cost_of_coverage <- as.integer(Cancels3$cost_of_coverage)
    Cancels3$LQ_FB_PREM_AMT <- as.integer(Cancels3$LQ_FB_PREM_AMT)
    
    
    #Derived Variable: Cost Per vehicle
    
    Cancels3 <- Cancels3 %>% 
      mutate(cost_per_vehicle = LQ_FB_PREM_AMT/no_of_vehicles)
    
    #Create binary lag variables
    #Cancels3 <- Cancels3 %>% 
      #mutate(lag_0_60 = ifelse(lag<=60, 1, 0), lag_61_90 = ifelse(lag>60 & lag <=90, 1, 0),
             #lag_91_180 = ifelse(lag>90 & lag<=180, 1, 0),
             #lag_181_364 = ifelse(lag>180 & lag<365, 1, 0))
    
    #create same day vs. follow-up sales variable
    
    Cancels3 <- Cancels3 %>% 
      mutate(SameDay_vs_Followup = ifelse(First.Quote.Date == Last.Quote.Date, "SameDay", "Follow-Up"))
    
    #How many unique: length(unique(variable))
    
    #unique <- sapply(Cancels3, function(x) length(unique(x)))
    #unique
    return(Cancels3)
}


  #Change Nulls and blanks to NA

remove_missing <- function(Cancels3) {
  Cancels3$marital_status <- gsub("NULL", NA, Cancels3$marital_status)
Cancels3$education <- gsub("NULL", NA, Cancels3$education)
Cancels3$gender <- gsub("NULL", NA, Cancels3$gender)
Cancels3$risk_profile <- gsub("NULL", NA, Cancels3$risk_profile)
Cancels3$risk_profile <- gsub("None", NA, Cancels3$risk_profile)
Cancels3$months_continuous_coverage <- gsub("NULL", NA, Cancels3$months_continuous_coverage)
Cancels3$bi_limit <- gsub("NULL", NA, Cancels3$bi_limit)
Cancels3$vehicle_ownership <- gsub("NULL", NA, Cancels3$vehicle_ownership)
Cancels3$previous_policy_type <- gsub("NULL", "None", Cancels3$previous_policy_type)
Cancels3$policy_expiration_date.x <- gsub("NULL", NA, Cancels3$policy_expiration_date.x)
Cancels3$policy_expiration_date.x[is.na(Cancels3$policy_expiration_date.x)] <- "No Previous Insurance" 

#Change unrealistic values 
Cancels3$cost_of_coverage[Cancels3$cost_of_coverage<10] <- NA
Cancels3$lag[Cancels3$lag<0] <- 0

#combine year and months of continues coverage into one column total months
Cancels3$years_continuous_coverage <- as.integer(Cancels3$years_continuous_coverage)
Cancels3$months_continuous_coverage <- as.integer(Cancels3$months_continuous_coverage)

Cancels3 <- Cancels3 %>% 
  mutate(Months_Coverage = (years_continuous_coverage*12) + months_continuous_coverage)
  
  ##remove other years and months columns
Cancels3 <- Cancels3[,-(17:18)]

##remove Cancellation Effective Date: We don't use it for anything
Cancels3 <- Cancels3[, -22]
##remove: Date_Canx_Processed: We only used it to determine lag
Cancels3 <- Cancels3[, -23]

#Exclude rows with missing gender data, marital status, education, risk profile
#impute age with mean
Cancels3 <- Cancels3[!is.na(Cancels3$marital_status),]
Cancels3 <- Cancels3[!is.na(Cancels3$gender),]
Cancels3 <- Cancels3[!is.na(Cancels3$education),]
Cancels3 <- Cancels3[!is.na(Cancels3$risk_profile),]
Cancels3 <- Cancels3[!is.na(Cancels3$Zip.Code),]
Cancels3 <- Cancels3[!is.na(Cancels3$vehicle_ownership),]
Cancels3 <- Cancels3[!is.na(Cancels3$lead_seller),]

#For variables where there was not previous insurance, include none as a category rather 
Cancels3$lag[is.na(Cancels3$CANCELLATION_REASON)] <- "Didn't Cancel"

#create a variable that is something we are predicting cancellation reason: 4 categories 
#(Non-Payment, competition, other, didn't cancel)
Cancels3 <- Cancels3 %>% 
  mutate(Cancellation_Reason_DV = ifelse(CANCELLATION_REASON == "Non-Payment", "Non-Payment",
          ifelse(CANCELLATION_REASON == "Competition", "Competition", 
                 ifelse(is.na(CANCELLATION_REASON), "Didn't Cancel", "Other"))))
                        
Cancels3$Cancellation_Reason_DV[is.na(Cancels3$Cancellation_Reason_DV)] <- "Didn't Cancel"
Cancels3$CANCELLATION_REASON[is.na(Cancels3$CANCELLATION_REASON)] <- "Didn't Cancel"

return(Cancels3)
}



#do same thing for lag (time categories, >365 days)
final_cleaning <- function (Cancels3) {
  Cancels3$lag <- as.numeric(Cancels3$lag)
Cancels3 <- Cancels3 %>% 
  mutate(Lag_DV = ifelse(lag< 61, "0-60",
                          ifelse(lag> 60 & lag<=90, "61-90",
                             ifelse(lag>90 & lag<= 180, "91-180",
                                    ifelse(lag>180 & lag<=364, "181-364",
                                           ifelse(lag>364, "Didn't Cancel", NA))))))
  
Cancels3$Lag_DV[is.na(Cancels3$Lag_DV)] <- "Didn't Cancel"


#Missing variables: imputation--replace with mean for numerical; if not create category other
Cancels3$previous_policy_type[is.na(Cancels3$previous_policy_type)] <- "Other"

Cancels3$Months_Coverage <- as.integer(Cancels3$Months_Coverage)
Cancels3$Months_Coverage[is.na(Cancels3$Months_Coverage)] <- mean(Cancels3$Months_Coverage, na.rm = TRUE)

return(Cancels3) }

#Count missing in each column 
#missing <- colSums(is.na(Cancels3))
#missing



wrangling_main <- function() {
  Cancels_Read <- read_data()
 Cancels_add_columns <- add_calculated_columns(Cancels_Read) 
Cancels_Missing <- remove_missing(Cancels_add_columns)
Cancels_Clean <- final_cleaning(Cancels_Missing)

 return(Cancels_Clean)
}

#x <- wrangling_main()


